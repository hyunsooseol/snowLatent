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
                '<li><b>Covariates (R3STEP):</b> select continuous covariates and/or categorical factors and the desired predictor table. Predictors at the same level enter jointly. Factors are automatically treatment-coded into k-1 numeric dummy variables; nominal numeric codes are categories, not scores. Ordered factors are also treated as categories, without a linear-trend assumption. Select each factor\'s reference category in its reference table; if unset, the first observed category in the data level order is used. The last profile or group class is the reference. B is a log-odds coefficient; OR = exp(B). OR is an odds ratio, not a probability or a causal effect. Intercepts are omitted from the predictor tables.</li>',
                '<li>Individual-profile R3STEP uses group-class-specific intercepts and shared predictor slopes when there is more than one latent group class; with one group class it uses the pooled form. Group-class R3STEP operates at the group level. Both use group-clustered sandwich standard errors (CR1). The step-three regression can update membership proportions but retains the original measurement parameters. It does not jointly estimate individual-level and group-level predictors in one regression.</li>',
                '<li><b>Distal outcomes (BCH):</b> select continuous external outcomes and check class means and/or pairwise differences. Each outcome is analyzed separately using classification-error-corrected BCH weights and group-clustered sandwich standard errors. Differences are Class minus Reference. Confidence intervals are pointwise 95% intervals. Adjusted p-values apply within each outcome and level; R3STEP adjustment applies across predictor terms and non-reference classes within each level. No adjustment is made across different outcomes or levels.</li>',
                '<li>External variables must differ from the indicators and group ID. Group-level covariates, factors and outcomes must be constant within each analyzed observed group. External analyses use exactly the rows retained for the basic LPA: no extra rows are deleted and the basic model is not refitted for external-variable missingness. Missing or nonfinite external values stop only the affected external analysis; impute or otherwise prepare such variables before analysis. Indicator FIML does not handle missing predictors or outcomes.</li>',
                '<li>These analyses are associations with the selected model, not causal effects. Standard errors condition on the estimated classification-error probabilities and do not include uncertainty in those probabilities or in class-number selection. Poor classification, small effective classes or too few groups may cause unstable weights, missing standard errors or an error. Inspect warnings; the module never replaces clustered standard errors with independent-observation standard errors. BCH effective n is a weight-based diagnostic, not a literal class count. Group-level external analyses require at least two latent group classes.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowLatent/issues" target="_blank">GitHub</a>.</li>'))
            self$results$modelFit$setNote("bic", "BIC (groups): analyzed observed groups; BIC (individuals): analyzed rows. Compare only models fitted to the same data and settings. No automatic model selection.")
            self$results$classification$setNote("sizes", "Modal counts use most-likely assignments; posterior expected sizes sum membership probabilities. AvePP is conditional on modal assignment.")
            self$results$profiles$setNote("scale", "Means and within-profile SDs are model estimates in original units, shared across group classes.")
            self$results$composition$setNote("conditional", "Within each latent group class, conditional individual-profile percentages sum to 100%.")
            for (name in c("r3Individual", "r3Group")) self$results[[name]]$setNote("method",
                "R3STEP classification-error correction; Covariates and treatment-coded factors entered jointly within level. Predictor comparison identifies the factor category versus its selected reference. Numeric covariates are per 1-unit increase. Reference latent class: last class. Group-clustered sandwich SE (CR1); pointwise 95% CIs. Intercepts omitted. Adjusted p-values cover predictor terms and non-reference classes within this table.")
            for (name in c("bchIndividualMeans", "bchGroupMeans")) self$results[[name]]$setNote("method",
                "BCH classification-error-corrected means in original outcome units; group-clustered sandwich SE (CR1), pointwise 95% CIs. Each outcome analyzed separately. Effective n is a weight-based diagnostic. Means are not tested against zero.")
            for (name in c("bchIndividualPairs", "bchGroupPairs")) self$results[[name]]$setNote("method",
                "BCH; difference = Class minus Reference. Group-clustered sandwich SE (CR1), pointwise 95% CIs. Adjusted p-values apply separately within each outcome at this level; no correction across outcomes or levels.")
        },
        .signature = function() {
            n <- c("vars", "group", "nProfiles", "nGroupClasses", "variance", "missing",
                   "nStarts", "maxIter", "seed", "compare", "models",
                   "showModelFit", "showClassification", "showEntropy", "showProfiles",
                   "showGroupClasses", "showComposition", "showStarts",
                   "profilePlot", "compositionPlot", "plotZ", "saveClasses", "saveProbabilities",
                   "individualCovariates", "groupCovariates", "individualOutcomes", "groupOutcomes", "r3Adjust", "bchAdjust", "showR3Individual", "showR3Group", "showBCHIndividualMeans", "showBCHIndividualPairs", "showBCHGroupMeans", "showBCHGroupPairs",
                   "individualFactors", "groupFactors", "individualRefLevels", "groupRefLevels")
            opts <- stats::setNames(lapply(n, function(x) self$options[[x]]), n)
            cols <- unique(c(self$options$group, self$options$vars,
                self$options$individualCovariates, self$options$groupCovariates,
                self$options$individualOutcomes, self$options$groupOutcomes,
                self$options$individualFactors, self$options$groupFactors))
            list(options = opts, data = if (length(cols) && all(cols %in% names(self$data))) as.data.frame(self$data[, cols, drop = FALSE]) else NULL)
        },
        .clearOutputs = function() {
            for (n in c("modelFit", "classification", "entropy", "profiles", "groupClasses", "composition", "starts",
                         "r3Individual", "r3Group", "bchIndividualMeans", "bchIndividualPairs", "bchGroupMeans", "bchGroupPairs"))
                { self$results[[n]]$deleteRows(); self$results[[n]]$setVisible(FALSE) }
            self$results$externalStatus$setContent("")
            self$results$externalStatus$setVisible(FALSE)
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
                     external = if (ready) .mlpa_external(selected, prepared, self$data, self$options,
                         checkpoint = function() private$.checkpoint()) else NULL,
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
                starts = "showStarts",
                r3Individual = "showR3Individual",
                r3Group = "showR3Group",
                bchIndividualMeans = "showBCHIndividualMeans",
                bchIndividualPairs = "showBCHIndividualPairs",
                bchGroupMeans = "showBCHGroupMeans",
                bchGroupPairs = "showBCHGroupPairs")
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
            if (!is.null(bundle$external)) {
                ext <- bundle$external
                if (length(ext$notes)) {
                    self$results$externalStatus$setContent(paste(unique(ext$notes), collapse="\n"))
                    self$results$externalStatus$setVisible(TRUE)
                }
                for (name in names(ext$frames)) private$.fill(self$results[[name]], ext$frames[[name]])
            }
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


# External-variable analyses for continuous Multilevel LPA only.
# Kept in this backend so multilpa-core.R remains unchanged.
.mlpa_external_data <- function(prepared, data, variables, level) {
    variables <- unique(as.character(variables))
    if (!length(variables)) stop("Select at least one external variable.")
    if (any(variables %in% prepared$labels))
        stop("External variables must not be measurement indicators.")
    if (!all(variables %in% names(data))) stop("An external variable is unavailable.")
    z <- as.data.frame(data[prepared$rows, variables, drop = FALSE])
    if (!all(vapply(z, is.numeric, logical(1))))
        stop("Use numeric external variables. Dummy-code categorical predictors before analysis.")
    bad <- variables[vapply(z, function(v) any(!is.finite(v)), logical(1))]
    if (length(bad)) stop(paste0("Missing or nonfinite external values on the LPA analysis rows: ",
        paste(bad, collapse=", "), ". No extra rows were deleted; the basic LPA is unchanged."))
    if (level == "groups") {
        groups <- prepared$data$group_id
        bad <- variables[vapply(z, function(v)
            any(vapply(split(v, groups), function(g) length(unique(g)) != 1L, logical(1))), logical(1))]
        if (length(bad)) stop(paste0("Group-level variables must be constant within each analyzed group: ",
            paste(bad, collapse=", "), "."))
    }
    # Preserve the engine's indicator/group columns for latents row-alignment checks.
    safe <- paste0("external_", seq_along(variables))
    names(z) <- safe
    frame <- prepared$data
    for (i in seq_along(safe)) frame[[safe[i]]] <- z[[i]]
    list(data=frame, safe=safe, labels=variables)
}

.mlpa_external <- function(fit, prepared, data, options, checkpoint=function() NULL) {
    frames <- list(); notes <- character()
    addNote <- function(text) notes <<- unique(c(notes, text))
    # An external error does not suppress a valid basic LPA or another outcome.
    capture <- function(label, fun) {
        checkpoint()
        tryCatch(withCallingHandlers(fun(), warning=function(w) {
            addNote(paste0(label, ": ", conditionMessage(w)))
            invokeRestart("muffleWarning")
        }), error=function(e) {
            addNote(paste0(label, " could not be completed: ", conditionMessage(e)))
            NULL
        })
    }
    prepare <- function(variables, level) {
        if (any(variables %in% c(options$vars, options$group)))
            stop("External variables must differ from the indicators and observed group ID.")
        if (level == "groups" && fit$n_group_classes < 2L)
            stop("At least two latent group classes are required for group-level external analyses.")
        .mlpa_external_data(prepared, data, variables, level)
    }
    r3 <- function(variables, factors, references, level, key) {
        label <- if (level == "individuals") "Individual-profile R3STEP" else "Group-class R3STEP"
        frame <- capture(label, function() {
            if (any(c(variables,factors) %in% c(options$vars,options$group)))
                stop("Predictors must differ from the indicators and observed group ID.")
            if (level == "groups" && fit$n_group_classes < 2L)
                stop("At least two latent group classes are required for group-level external analyses.")
            z <- .mlpa_predictor_data(prepared,data,variables,factors,references,level)
            for (note in z$notes) addNote(paste0(label, ": ",note))
            byClass <- level == "individuals" && fit$n_group_classes > 1L
            result <- latents::r3step(fit, data=z$data, covariates=z$safe,
                level=level, vcov_type="robust", ci_level=0.95,
                adjust=options$r3Adjust, by_group_class=byClass)
            reference <- attr(result, "reference_class")
            # Group-class intercepts and mixing logits are nuisance parameters,
            # not predictor effects. Keep exactly the supplied predictor slopes.
            result <- result[result$level == level & result$term %in% z$safe, , drop=FALSE]
            if (!nrow(result)) stop("No predictor coefficients were returned.")
            addNote(paste0(label, ": ", nrow(prepared$data), " individuals in ", prepared$nGroups,
                " observed groups; reference class ", reference, "; ",
                if (byClass) "two-level regression with group-class-specific intercepts and shared slopes; " else "",
                "group-clustered SE; multiplicity correction: ", options$r3Adjust, "."))
            data.frame(class=as.integer(sub(".*_", "", result$outcome)),
                reference=as.integer(reference), predictor=z$labels[match(result$term, z$safe)],
                comparison=z$comparisons[match(result$term,z$safe)],
                b=result$estimate, se=result$standard_error, z=result$statistic,
                p=result$p_value, pAdjusted=result$p_adjusted,
                low=result$conf_low, high=result$conf_high,
                odds=exp(result$estimate), oddsLow=exp(result$conf_low), oddsHigh=exp(result$conf_high),
                stringsAsFactors=FALSE)
        })
        if (!is.null(frame)) frames[[key]] <<- frame
    }
    bch <- function(variables, level, means, pairs, meanKey, pairKey) {
        label <- if (level == "individuals") "Individual-profile BCH" else "Group-class BCH"
        if (!length(variables)) {
            addNote(paste0(label, " could not be completed: select at least one continuous outcome."))
            return(invisible(NULL))
        }
        for (variable in unique(variables)) {
            z <- capture(paste0(label, " (", variable, ")"), function() prepare(variable, level))
            if (is.null(z)) next
            for (contrast in c(if (means) "none", if (pairs) "pairs")) {
                result <- capture(paste0(label, " (", variable, "; ", contrast, ")"), function()
                    latents::three_step(fit, data=z$data, outcome=z$safe[1L], level=level,
                        method="bch", ci_level=0.95, contrast=contrast,
                        adjust=options$bchAdjust, vcov_type="cluster"))
                if (is.null(result) || !nrow(result)) next
                if (contrast == "none") {
                    key <- meanKey
                    frame <- data.frame(outcome=variable, class=result$class,
                        mean=result$estimate, se=result$standard_error,
                        low=result$conf_low, high=result$conf_high, effectiveN=result$effective_n,
                        stringsAsFactors=FALSE)
                } else {
                    key <- pairKey
                    frame <- data.frame(outcome=variable, class=result$class,
                        reference=result$reference_class, difference=result$estimate,
                        se=result$standard_error, z=result$statistic,
                        p=result$p_value, pAdjusted=result$p_adjusted,
                        low=result$conf_low, high=result$conf_high, stringsAsFactors=FALSE)
                }
                frames[[key]] <<- if (is.null(frames[[key]])) frame else rbind(frames[[key]], frame)
            }
        }
        addNote(paste0(label, ": original LPA analysis rows retained; group-clustered SE; ",
            "pairwise multiplicity correction within each outcome: ", options$bchAdjust, "."))
    }
    if (isTRUE(options$showR3Individual)) r3(options$individualCovariates, options$individualFactors, options$individualRefLevels, "individuals", "r3Individual")
    if (isTRUE(options$showR3Group)) r3(options$groupCovariates, options$groupFactors, options$groupRefLevels, "groups", "r3Group")
    if (isTRUE(options$showBCHIndividualMeans) || isTRUE(options$showBCHIndividualPairs))
        bch(options$individualOutcomes, "individuals", isTRUE(options$showBCHIndividualMeans),
            isTRUE(options$showBCHIndividualPairs), "bchIndividualMeans", "bchIndividualPairs")
    if (isTRUE(options$showBCHGroupMeans) || isTRUE(options$showBCHGroupPairs))
        bch(options$groupOutcomes, "groups", isTRUE(options$showBCHGroupMeans),
            isTRUE(options$showBCHGroupPairs), "bchGroupMeans", "bchGroupPairs")
    list(frames=frames, notes=notes)
}

# Treatment-code selected factors explicitly, including ordered factors.
# Never interpret numeric category codes as a linear predictor.
.mlpa_predictor_data <- function(prepared, data, covariates, factors, references, level) {
    covariates <- as.character(covariates); factors <- as.character(factors)
    variables <- c(covariates, factors)
    if (!length(variables)) stop("Select at least one covariate or factor.")
    if (anyDuplicated(variables)) stop("Use each predictor once; do not put the same variable in Covariates and Factors.")
    if (any(variables %in% prepared$labels)) stop("Predictors must not be measurement indicators.")
    if (!all(variables %in% names(data))) stop("A predictor is unavailable.")
    raw <- as.data.frame(data[prepared$rows, variables, drop=FALSE])
    for (v in variables) {
        x <- raw[[v]]
        if (anyNA(x) || (is.numeric(x) && any(!is.finite(x))))
            stop(paste0("Missing or nonfinite predictor values on the LPA analysis rows: ", v,
                ". No extra rows were deleted; the basic LPA is unchanged."))
        if (level == "groups" && any(vapply(split(as.character(x), prepared$data$group_id),
            function(g) length(unique(g)) != 1L, logical(1))))
            stop(paste0("Group-level predictors must be constant within each analyzed group: ", v, "."))
    }
    frame <- prepared$data; safe <- labels <- comparisons <- notes <- character()
    add <- function(value, variable, comparison) {
        key <- paste0("external_", length(safe)+1L)
        frame[[key]] <<- as.numeric(value)
        safe <<- c(safe,key); labels <<- c(labels,variable)
        comparisons <<- c(comparisons,comparison)
    }
    for (v in covariates) {
        if (!is.numeric(raw[[v]])) stop(paste0("Covariates must be numeric; put categorical predictors in Factors: ",v,"."))
        add(raw[[v]],v,"Per 1-unit increase")
    }
    for (v in factors) {
        full <- data[[v]]; x <- as.character(raw[[v]])
        levelsAll <- if (is.factor(full)) levels(full) else if (is.numeric(full))
            as.character(sort(unique(full[is.finite(full)]))) else sort(unique(as.character(full[!is.na(full)])))
        observed <- levelsAll[levelsAll %in% unique(x)]
        if (length(observed)<2L) stop(paste0("A factor needs at least two observed categories on the LPA analysis rows: ",v,"."))
        entries <- Filter(function(item) !is.null(item$var) && identical(as.character(item$var),v), references)
        if (length(entries)>1L) stop(paste0("Duplicate reference settings for factor: ",v,"."))
        ref <- if (length(entries)) entries[[1L]]$ref else NULL
        if (is.null(ref) || !length(ref) || is.na(ref[1L]) || !nzchar(as.character(ref[1L]))) ref <- observed[1L]
        ref <- as.character(ref)
        if (length(ref)!=1L || !ref %in% observed)
            stop(paste0("The reference category for ",v," is absent from the LPA analysis rows. Choose an observed reference category."))
        for (lev in setdiff(observed,ref)) add(x==lev,v,paste0(lev," vs ",ref))
        notes <- c(notes,paste0(v,": reference = ",ref,"; treatment coding (",length(observed)-1L," dummy variable(s))."))
    }
    list(data=frame,safe=safe,labels=labels,comparisons=comparisons,notes=notes)
}
