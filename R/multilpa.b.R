multilpaClass <- if (requireNamespace("jmvcore", quietly = TRUE))
    R6::R6Class("multilpaClass", inherit = multilpaBase, private = list(
        .init = function() {
            self$results$help$setContent(paste0(
                '<details><summary style="cursor:pointer;padding:8px 0;font-weight:600">Help</summary>',
                '<p>Multilevel latent profile analysis for continuous indicators, based on <b>latents</b>.</p>',
                '<ul><li>One row is one individual (or repeated observation). Select continuous indicators and an observed group ID, such as school or class. The group ID is not a predictor.</li>',
                '<li><b>Individual profiles</b> describe indicator patterns. <b>Group-level latent classes</b> describe differences in the proportions of these profiles. For example, 50 observed schools may be summarized by 2 latent group classes. Profile means and variances are shared across group classes; residual covariances are fixed at zero.</li>',
                '<li>Indicators are fitted in their original units. Consider suitable scaling before analysis when variables use very different units. The standardized profile plot only transforms the display: (profile mean minus analyzed-sample mean) / analyzed-sample SD. Tables and likelihoods remain on the original scale.</li>',
                '<li><b>Missing data:</b> complete cases excludes rows missing any indicator. FIML uses available indicators under the model and MAR assumption. Rows missing every indicator or the group ID are excluded in either mode. Entirely excluded groups do not enter estimation.</li>',
                '<li><b>Model comparison:</b> enter comma-separated profiles/group-class pairs, for example <code>2/1, 3/2</code>. The selected model is always included, with at most 12 models per run. All models use the same data and variance setting. One group class is a single-level baseline; one individual profile cannot identify multiple group classes.</li>',
                '<li><b>BIC (groups)</b> uses the number of analyzed observed groups; <b>BIC (individuals)</b> uses analyzed individuals. Report the chosen convention explicitly. Lower AIC/BIC favors fit after its complexity penalty, but model choice also requires stability, class size and substantive interpretation.</li>',
                '<li><b>Modal n/%</b> counts most-likely assignments. <b>Posterior expected n/%</b> sums posterior probabilities. Estimated group-class proportions and conditional composition probabilities are model parameters, not modal percentages.</li>',
                '<li><b>Relative entropy</b> is reported separately for individuals and groups (0-1, higher indicates clearer separation). It is undefined for one class. AvePP is the mean assigned-class posterior among modal members of each class; it is undefined for a class without modal members.</li>',
                '<li>Check convergence, boundary warnings and repeated recovery of the best log-likelihood. Increase random starts if the best solution is not replicated. Nonconverged or boundary selected models produce fit diagnostics only. Numerical convergence does not prove a global maximum or meaningful classes. A small number of observed groups can make group-level estimation unstable.</li>',
                '<li><b>Tables</b> provides a named checkbox for each result table. Select only the tables and plots needed for the current learning or reporting step; multiple outputs may also be selected. All table and main plot choices start unchecked.</li>',
                '<li><b>Run</b> estimates the selected model and optional comparison models, and displays only the checked outputs. Changing data, model settings, table or plot selections, the plot scale, or save options clears the previous results. Click <b>Run</b> after each change; selecting an option alone does not calculate, display or save new results.</li>',
                '<li>Saved classifications are modal assignments and do not account for classification error in subsequent analyses. Group-class values and probabilities are repeated on individuals within the same group; these repetitions are not independent group observations. Excluded rows receive missing saved values. Labels are arbitrary and are consistent across all outputs of the selected model; labels across different fitted models need not correspond.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowLatent/issues" target="_blank">GitHub</a>.</li>'))
            self$results$modelFit$setNote("bic", "BIC (groups): analyzed observed groups; BIC (individuals): analyzed rows. Compare only models fitted to the same data and settings. No automatic model selection.")
            self$results$classification$setNote("sizes", "Modal counts use most-likely assignments; posterior expected sizes sum membership probabilities. AvePP is conditional on modal assignment.")
            self$results$profiles$setNote("scale", "Means and within-profile SDs are model estimates in original units, shared across group classes.")
            self$results$composition$setNote("conditional", "Within each latent group class, conditional individual-profile percentages sum to 100%.")
        },
        .signature = function() {
            n <- c("vars", "group", "nProfiles", "nGroupClasses", "variance", "missing",
                   "nStarts", "maxIter", "seed", "compare", "models",
                   "showModelFit", "showClassification", "showEntropy", "showProfiles",
                   "showGroupClasses", "showComposition", "showStarts",
                   "profilePlot", "compositionPlot", "plotZ", "saveClasses", "saveProbabilities")
            opts <- stats::setNames(lapply(n, function(x) self$options[[x]]), n)
            cols <- unique(c(self$options$group, self$options$vars))
            list(options = opts, data = if (length(cols) && all(cols %in% names(self$data))) as.data.frame(self$data[, cols, drop = FALSE]) else NULL)
        },
        .clearOutputs = function() {
            for (n in c("modelFit", "classification", "entropy", "profiles", "groupClasses", "composition", "starts"))
                { self$results[[n]]$deleteRows(); self$results[[n]]$setVisible(FALSE) }
            for (n in c("profileFigure", "compositionFigure")) {
                self$results[[n]]$setState(NULL); self$results[[n]]$setVisible(FALSE)
            }
            for (n in c("saveClasses", "saveProbabilities"))
                self$results[[n]]$set(keys = character(), titles = character(), descriptions = character(), measureTypes = character())
        },
        .run = function() {
            signature <- private$.signature()
            previous <- self$results$status$state
            if (!isTRUE(self$options$run)) {
                # Keep an already-run result only when data and every user option
                # are unchanged. Selecting a table or plot never runs the analysis.
                if (!is.null(previous) && identical(previous$signature, signature))
                    return()
                private$.clearOutputs()
                self$results$status$setState(NULL)
                self$results$status$setContent("Select variables, a group ID and the required outputs, then click Run. After any option change, click Run again.")
                return()
            }
            private$.clearOutputs()
            self$results$status$setState(NULL)
            self$results$status$setContent("Estimating Multilevel LPA...")
            private$.checkpoint()
            bundle <- tryCatch({
                if (!requireNamespace("latents", quietly = TRUE) || utils::packageVersion("latents") < "1.1.9")
                    stop("latents >= 1.1.9 is required. Rebuild this module with its pinned GitHub dependency.")
                prepared <- .mlpa_prepare(self$data, self$options$vars, self$options$group, self$options$missing)
                pairs <- .mlpa_pairs(self$options$nProfiles, self$options$nGroupClasses, self$options$compare, self$options$models)
                result <- .mlpa_estimate(prepared, pairs, self$options$variance, self$options$missing,
                    self$options$nStarts, self$options$maxIter, self$options$seed,
                    checkpoint = function(i, n) {
                        self$results$status$setContent(sprintf("Estimating model %d of %d...", i, n))
                        private$.checkpoint()
                    })
                selected <- result$fits[[1L]]
                ready <- !is.null(selected) && isTRUE(selected$converged) && !isTRUE(selected$boundary)
                list(signature = signature, prepared = prepared, comparison = result$comparison,
                     warnings = result$warnings, errors = result$errors, ready = ready,
                     tables = if (ready) .mlpa_extract(selected, prepared) else NULL,
                     packageVersion = as.character(utils::packageVersion("latents")))
            }, error = function(e) {
                self$results$status$setContent(paste0("Analysis could not be completed: ", conditionMessage(e)))
                NULL
            })
            if (is.null(bundle)) return()
            self$results$status$setState(bundle)
            private$.render(bundle)
        },
        .fill = function(table, frame) {
            option <- switch(table$name,
                modelFit = "showModelFit", classification = "showClassification",
                entropy = "showEntropy", profiles = "showProfiles",
                groupClasses = "showGroupClasses", composition = "showComposition",
                starts = "showStarts")
            selected <- !is.null(option) && isTRUE(self$options[[option]])
            table$setVisible(selected && nrow(frame) > 0L)
            if (!selected) return(invisible(NULL))
            if (nrow(frame)) for (i in seq_len(nrow(frame)))
                table$addRow(rowKey = as.character(i), values = as.list(frame[i, , drop = FALSE]))
        },
        .render = function(bundle) {
            p <- bundle$prepared
            status <- sprintf("Analyzed: %d individuals in %d observed groups. Excluded: %d of %d rows. Group sizes: %d-%d. latents %s.",
                nrow(p$data), p$nGroups, p$nExcluded, p$nOriginal, min(p$sizes), max(p$sizes), bundle$packageVersion)
            notes <- unique(unlist(lapply(seq_along(bundle$warnings), function(i) {
                w <- bundle$warnings[[i]]
                if (length(w)) paste0("Model ", bundle$comparison$profiles[i], "/", bundle$comparison$groups[i], ": ", w) else character()
            })))
            if (length(notes)) status <- paste(status, paste(notes, collapse = "\n"), sep = "\n")
            private$.fill(self$results$modelFit, bundle$comparison)
            if (!bundle$ready) {
                self$results$status$setContent(paste(status, "Selected model failed, did not converge, or reached a boundary. Interpretation outputs are suppressed. Select Model fit and comparison under Tables and click Run to display fit diagnostics; adjust the model or estimation settings as needed.", sep = "\n"))
                return()
            }
            if (grepl("not replicated", bundle$comparison$status[1L], fixed = TRUE))
                status <- paste(status, "The best likelihood was not replicated: increase random starts before reporting.", sep = "\n")
            self$results$status$setContent(status)
            t <- bundle$tables; c <- t$classification
            levels <- function(x) ifelse(x == "groups", "Group classes", "Individual profiles")
            private$.fill(self$results$classification, data.frame(level = levels(c$level), class = c$class,
                nModal = c$n_modal, modalPercent = 100*c$proportion_modal,
                posteriorN = c$estimated_n, posteriorPercent = 100*c$estimated_proportion, avepp = c$average_posterior))
            e <- t$entropy
            private$.fill(self$results$entropy, data.frame(level = levels(e$level), classes = e$n_classes, units = e$n_units, entropy = e$relative_entropy))
            private$.fill(self$results$profiles, data.frame(profile = t$profiles$profile, indicator = t$profiles$indicator, mean = t$profiles$mean, sd = t$profiles$standard_deviation))
            private$.fill(self$results$groupClasses, data.frame(groupClass = t$groupProportions$group_class, percent = 100*t$groupProportions$group_class_probability))
            private$.fill(self$results$composition, data.frame(groupClass = t$composition$group_class, profile = t$composition$profile, percent = 100*t$composition$probability))
            if (isTRUE(self$options$showStarts)) {
                s <- t$starts
                private$.fill(self$results$starts, data.frame(start=s$start, loglik=s$log_likelihood,
                    converged=ifelse(s$converged, "Yes", "No"), iterations=s$iterations,
                    boundary=if ("boundary" %in% names(s)) ifelse(s$boundary,"Yes","No") else "",
                    error=ifelse(is.na(s$error), "", s$error)))
            }
            plot <- t$profiles
            plot$indicator <- factor(plot$indicator, levels = p$labels)
            plot$profile <- factor(plot$profile)
            if (isTRUE(self$options$plotZ)) {
                centers <- vapply(p$data[p$engineVars], mean, numeric(1), na.rm=TRUE)
                spreads <- vapply(p$data[p$engineVars], stats::sd, numeric(1), na.rm=TRUE)
                ix <- match(as.character(plot$indicator), p$labels)
                plot$mean <- (plot$mean - centers[ix]) / spreads[ix]
            }
            if (isTRUE(self$options$profilePlot)) {
                self$results$profileFigure$setVisible(TRUE)
                self$results$profileFigure$setState(list(data=plot, z=isTRUE(self$options$plotZ)))
            }
            if (isTRUE(self$options$compositionPlot)) {
                self$results$compositionFigure$setVisible(TRUE)
                self$results$compositionFigure$setState(t$composition)
            }
            for (name in c("saveClasses", "saveProbabilities")) {
                output <- .mlpa_output(bundle,
                    saveClasses = name == "saveClasses" && isTRUE(self$options$saveClasses),
                    saveProbabilities = name == "saveProbabilities" && isTRUE(self$options$saveProbabilities))
                if (!length(output$values)) next
                titles <- names(output$values)
                element <- self$results[[name]]
                element$set(keys = titles, titles = paste0("MLPA - ", titles),
                    descriptions = rep("Selected multilevel LPA model; group values repeat within observed groups. Excluded rows are missing.",length(titles)),
                    measureTypes = unname(output$types))
                element$setRowNums(p$rowNames)
                for (i in seq_along(output$values)) element$setValues(index=i, values=output$values[[i]])
            }
        },
        .profilePlot = function(image, ...) {
            s <- image$state
            if (is.null(s)) return(FALSE)
            d <- s$data
            g <- ggplot2::ggplot(d, ggplot2::aes(x=indicator, y=mean, group=profile, colour=profile)) +
                ggplot2::geom_line(linewidth=0.8) + ggplot2::geom_point(size=2.5) +
                ggplot2::labs(x=NULL, y=if (s$z) "Standardized profile mean" else "Profile mean (original units)", colour="Profile") +
                ggplot2::theme_minimal(base_size=12) +
                ggplot2::theme(panel.grid.minor=ggplot2::element_blank(), legend.position="bottom",
                               axis.text.x=ggplot2::element_text(angle=30, hjust=1))
            print(g); TRUE
        },
        .compositionPlot = function(image, ...) {
            d <- image$state
            if (is.null(d)) return(FALSE)
            d$group_class <- factor(d$group_class); d$profile <- factor(d$profile)
            g <- ggplot2::ggplot(d, ggplot2::aes(x=group_class, y=100*probability, fill=profile)) +
                ggplot2::geom_col(width=0.65) +
                ggplot2::scale_y_continuous(breaks=seq(0,100,25), expand=c(0,0)) +
                ggplot2::coord_cartesian(ylim=c(0,100)) +
                ggplot2::labs(x="Group-level latent class", y="Conditional profile composition (%)", fill="Individual profile") +
                ggplot2::theme_minimal(base_size=12) +
                ggplot2::theme(panel.grid.minor=ggplot2::element_blank(), legend.position="bottom")
            print(g); TRUE
        }
    ))
