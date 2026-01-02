
ltaClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "ltaClass",
    inherit = ltaBase,
    private = list(
      .htmlwidget = NULL,
      .dataCache = NULL,  
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) |
            length(self$options$factors[[1]]$vars) == 0) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Latent transition analysis based on <b>slca</b> R package.</li>',
            '<li>The MAR(Missing at Random) method is applied to handle missing values.</li>',
            '<li>Model specifications are described in the <a href="https://kim0sun.github.io/slca/" target = "_blank">page</a>.</li>',
            '<li><b>L1[k]</b>: <b>k</b> denotes the number of latent classes for the first latent class variable <b>L1</b>.</li>',
            '<li><b>PI</b>: Class prevalences.</li>',
            '<li><b>TAU</b>: Transition probabilities.</li>',
            '<li><b>RHO</b>: Item response probabilities.</li>',
            '<li>Minor discrepancies between p-values and 95% CIs may occur due to Wald approximation errors.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowLatent/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        
        if (self$options$fit1)
          self$results$fit1$setNote(
            "Note",
            "obj2: LTA with non-invariant model(H1); obj3: LTA with measurement invariance(Ho)."
          )
        if (self$options$reg)
          self$results$reg$setNote("Note",
                                   "It utilizes logistic regression and employs a three-step approach.")
        
        if(isTRUE(self$options$plot)){
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }
        if(isTRUE(self$options$plot1)){
          width <- self$options$width1
          height <- self$options$height1
          self$results$plot1$setSize(width, height)
        }
        if(isTRUE(self$options$plot2)){
          width <- self$options$width2
          height <- self$options$height2
          self$results$plot2$setSize(width, height)
        }
      },
      
      .run = function() {
        
        if (is.null(private$.dataCache)) {
          private$.dataCache <- self$data
        }
        data <- private$.dataCache
        
        factors <- self$options$factors
        nfactors <- length(factors)
        
        if (length(factors[[1]]$vars) < 2) return()
        
        if (nfactors > 1) {
          formulas <- list()
          
          for (factor in factors) {
            vars <- factor[["vars"]]
            label <- factor[["label"]]
            
            # y ~ a + b + c 
            vars <- vapply(vars, function(x)
              jmvcore::composeTerm(x), '')
            ind <- paste0(vars, collapse = '+')
            formula <- as.formula(paste0(label, ' ~ ', ind))
            
            formulas[[label]] <- as.formula(formula)
          }
          
          create_sequential_relations <- function(formulas) {
            relations <- c()
            
            for (i in seq_along(formulas)[-length(formulas)]) {
              left <- sub("\\[.*?\\]", "", names(formulas)[i])   # lc1[2]-> lc1
              left <- sub(" ~.*", "", left)
              right <- sub("\\[.*?\\]", "", names(formulas)[i + 1])
              right <- sub(" ~.*", "", right)
              relation <- paste0(left, " ~ ", right)
              relations <- c(relations, as.formula(relation))
            }
            return(relations)
          }
          
          sequential_relations <- create_sequential_relations(formulas)
          form1 <- c(formulas, sequential_relations)
          
          # ---- helper: latent labels + step labels (L1→L2, ...)
          .get_latent_names <- function(factors) {
            labs <- vapply(factors, function(f) f[["label"]], "")
            # "L1[2]" -> "L1"
            labs <- sub("\\[.*?\\]", "", labs)
            labs
          }
          .make_step_labels <- function(latent_names) {
            if (length(latent_names) < 2)
              return(character())
            paste0(latent_names[-length(latent_names)], "\u2192", latent_names[-1])  # →
          }
          
          latent_names <- .get_latent_names(factors)
          step_labels <- .make_step_labels(latent_names)
          
          # ---- helper: tau(list of matrices) -> stayer/mover summary
          # NOTE: stayer is a "rate" computed as mean(diag(tau)) = sum(diag)/K
          # so that mover = 1 - stayer is always valid (0~1)
          .tau_diag_summary <- function(lta_obj, step_labels, model_label) {
            par <- slca::param(lta_obj)
            tau <- par$tau
            if (is.null(tau) || length(tau) == 0)
              return(NULL)
            
            nstep <- length(tau)
            if (length(step_labels) != nstep)
              step_labels <- paste0("T", seq_len(nstep), "\u2192T", seq_len(nstep) + 1)
            
            out <- vector("list", nstep)
            
            for (i in seq_len(nstep)) {
              m <- tau[[i]]
              k <- min(nrow(m), ncol(m))
              diag_sum <- sum(diag(m[seq_len(k), seq_len(k)]), na.rm = TRUE)
              stayer_rate <- diag_sum / k
              mover_rate <- 1 - stayer_rate
              
              out[[i]] <- data.frame(
                model = model_label,
                transition = step_labels[i],
                stayer = stayer_rate,
                mover = mover_rate,
                stringsAsFactors = FALSE
              )
            }
            
            do.call(rbind, out)
          }
          
          # ---- helper: fit obj2 (non-invariant) / obj3 (invariant) cleanly
          .fit_obj2 <- function() {
            library(magrittr)
            set.seed(1234)
            slca::slca(formula = form1) %>% slca::estimate(data = data)
          }
          .fit_obj3 <- function() {
            cons <- self$options$cons
            cons1 <- unlist(strsplit(cons, ","))
            library(magrittr)
            set.seed(1234)
            slca::slca(formula = form1, constraints = cons1) %>% slca::estimate(data = data)
          }
          
          if (isTRUE(self$options$par2)) {
            library(magrittr)
            set.seed(1234)
            obj2 <- slca::slca(formula = form1) %>%
              slca::estimate(data = data)
            par2 <- slca::param(obj2)
            
            self$results$text3$setContent(par2)
          }
          
          if (isTRUE(self$options$par3)) {
            cons <- self$options$cons
            cons1 <- unlist(strsplit(cons, ","))
            
            library(magrittr)
            set.seed(1234)
            obj3 <- slca::slca(formula = form1, constraints = cons1) %>%
              slca::estimate(data = data)
            par3 <- slca::param(obj3)
            
            self$results$text4$setContent(par3)
          }
          
          if (isTRUE(self$options$fit1)) {
            if (!exists("obj2")) {
              library(magrittr)
              set.seed(1234)
              obj2 <- slca::slca(formula = form1) %>%
                slca::estimate(data = data)
            }
            
            if (!exists("obj3")) {
              cons <- self$options$cons
              cons1 <- unlist(strsplit(cons, ","))
              
              library(magrittr)
              set.seed(1234)
              obj3 <- slca::slca(formula = form1, constraints = cons1) %>%
                slca::estimate(data = data)
            }
            
            comp <- slca::compare(obj2, obj3, test = 'chisq')
            df <- as.data.frame(comp)
            
            table <- self$results$fit1
            for (name in rownames(df)) {
              table$addRow(
                rowKey = name,
                values = list(
                  df      = df[name, 1],
                  loglik  = df[name, 2],
                  aic     = df[name, 3],
                  bic     = df[name, 4],
                  gsq     = df[name, 5],
                  res     = df[name, 6],
                  p       = df[name, 7]
                )
              )
            }
          }
          
          # -------------------------------------------------
          # Transition Plot(s) (tau)
          # plot1 = non-invariant (obj2)  [forced]
          # plot2 = invariant (obj3)      [forced]
          # facet: L1→L2, L2→L3 ... auto from latent labels
          # -------------------------------------------------
          .tau_to_long <- function(lta_obj, step_labels) {
            par <- slca::param(lta_obj)
            tau <- par$tau
            if (is.null(tau) || length(tau) == 0)
              return(NULL)
            
            nstep <- length(tau)
            if (length(step_labels) != nstep)
              step_labels <- paste0("T", seq_len(nstep), "\u2192T", seq_len(nstep) + 1)
            
            out <- vector("list", nstep)
            
            for (i in seq_len(nstep)) {
              tau_mat <- tau[[i]]
              tmp <- as.data.frame(as.table(tau_mat))
              colnames(tmp) <- c("child", "parent", "prob")
              
              # ✅ 핵심: 클래스 순서를 숫자 기준으로 강제
              k_from <- ncol(tau_mat)
              k_to   <- nrow(tau_mat)
              
              tmp$from <- factor(tmp$parent, levels = as.character(seq_len(k_from)))
              tmp$to   <- factor(tmp$child,  levels = as.character(seq_len(k_to)))
              
              tmp$step <- factor(step_labels[i], levels = step_labels)
              tmp <- tmp[, c("step", "from", "to", "prob")]
              out[[i]] <- tmp
            }
            
            do.call(rbind, out)
          }
          
          
          if (isTRUE(self$options$plot1)) {
            # 강제: obj2 (non-invariant)
            obj2_for_plot <- NULL
            if (exists("obj2")) {
              obj2_for_plot <- obj2
            } else {
              obj2_for_plot <- .fit_obj2()
            }
            
            tau_long <- .tau_to_long(obj2_for_plot, step_labels)
            if (!is.null(tau_long)) {
              image <- self$results$plot1
              image$setState(tau_long)
            }
          }
          
          if (isTRUE(self$options$plot2)) {
            # 강제: obj3 (invariant)
            obj3_for_plot <- NULL
            if (exists("obj3")) {
              obj3_for_plot <- obj3
            } else {
              obj3_for_plot <- .fit_obj3()
            }
            
            tau_long <- .tau_to_long(obj3_for_plot, step_labels)
            if (!is.null(tau_long)) {
              image <- self$results$plot2
              image$setState(tau_long)
            }
          }
          
          # -------------------------------------------------
          # ---- TAU table (new): obj2 + obj3 into one table
          # columns: model, transition, from, to, prob
          # -------------------------------------------------
          if (isTRUE(self$options$tau)) {
            
            .fill_tau_table <- function(lta_obj, model_label, step_labels) {
              tau_long <- .tau_to_long(lta_obj, step_labels)
              if (is.null(tau_long) || nrow(tau_long) == 0)
                return()
              
              table <- self$results$tau
              
              for (i in seq_len(nrow(tau_long))) {
                row <- tau_long[i, ]
                rowKey <- paste0(model_label, "_", as.character(row$step), "_", row$from, "_", row$to)
                
                table$addRow(
                  rowKey = rowKey,
                  values = list(
                    model      = model_label,
                    transition = as.character(row$step),
                    from       = as.character(row$from),
                    to         = as.character(row$to),
                    prob       = as.numeric(row$prob)
                  )
                )
              }
            }
            
            # 강제 추정/재사용 (필요할 때만)
            obj2_for_tau <- if (exists("obj2")) obj2 else .fit_obj2()
            obj3_for_tau <- if (exists("obj3")) obj3 else .fit_obj3()
            
            .fill_tau_table(obj2_for_tau, "Non-invariant (H1)", step_labels)
            .fill_tau_table(obj3_for_tau, "Invariant (H0)", step_labels)
          }
          
          # -------------------------------------------------
          # Stayer / Mover summary (diag of TAU)
          # -------------------------------------------------
          if (isTRUE(self$options$stayer)) {
            
            # obj2 (H1)
            obj2_for_stay <- if (exists("obj2")) obj2 else .fit_obj2()
            # obj3 (H0)
            obj3_for_stay <- if (exists("obj3")) obj3 else .fit_obj3()
            
            df_stay <- rbind(
              .tau_diag_summary(obj2_for_stay, step_labels, "Non-invariant (H1)"),
              .tau_diag_summary(obj3_for_stay, step_labels, "Invariant (H0)")
            )
            
            if (!is.null(df_stay) && nrow(df_stay) > 0) {
              tab <- self$results$stay
              for (i in seq_len(nrow(df_stay))) {
                tab$addRow(
                  rowKey = paste0(df_stay$model[i], "_", df_stay$transition[i]),
                  values = list(
                    model = df_stay$model[i],
                    transition = df_stay$transition[i],
                    stayer = df_stay$stayer[i],
                    mover = df_stay$mover[i]
                  )
                )
              }
            }
          }
          
        }
        
        if (length(self$options$covs) >= 1) {
          regform <- self$options$regform
          regform <- as.formula(regform)
          
          if (nfactors > 1) {
            lc_vars <- character()
            lc_counts <- integer()
            
            for (factor in factors) {
              label <- factor[["label"]]
              class_count <- as.integer(gsub(".*\\[(\\d+)\\].*", "\\1", label))
              lc_name <- sub("\\[.*?\\]", "", label)
              lc_vars <- c(lc_vars, lc_name)
              lc_counts <- c(lc_counts, class_count)
            }
            
            formulas <- list()
            
            for (factor in factors) {
              vars <- factor[["vars"]]
              label <- factor[["label"]]
              
              vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
              ind <- paste0(vars, collapse = '+')
              formula <- as.formula(paste0(label, ' ~ ', ind))
              
              formulas[[label]] <- formula
            }
            
            pf_class_count <- lc_counts[1]
            pf_formula <- as.formula(paste0("pf[", pf_class_count, "] ~ ", paste0(lc_vars, collapse = " + ")))
            
            new_form <- c(formulas, list(pf = pf_formula))
            
            cons <- self$options$cons
            if (cons == "") {
              cons1 <- lc_vars
            } else {
              cons1 <- unlist(strsplit(cons, ","))
            }
            
            library(magrittr)
            set.seed(1234)
            
            lcpa_model <- tryCatch({
              slca::slca(formula = new_form, constraints = cons1) %>%
                slca::estimate(data = data)
            }, error = function(e) {
              message("error: ", e$message)
              return(NULL)
            })
            
            # regression using 3-step---
            
            if (!is.null(lcpa_model) && length(self$options$covs) >= 1) {
              regform <- self$options$regform
              regform <- as.formula(regform)
              
              reg <- slca::regress(
                lcpa_model,
                regform,
                imputation = self$options$impu,
                method = self$options$method,
                data = data
              )
              
              # result---              
              coef <- reg$coefficients
              se <- as.vector(reg$std.err)
              wald <- as.vector(coef / se)
              pval <- stats::pnorm(abs(wald), 1, lower.tail = FALSE)
              
              variable_names <- colnames(reg$coefficients)
              class_info <- rep(rownames(reg$coefficients), each = length(variable_names))
              
              reg.df <- data.frame(
                class = class_info[1:length(coef)],
                variable = variable_names,
                coef = as.vector(coef),
                std.err = se,
                wald = wald,
                p.value = pval
              )
              
              table <- self$results$reg
              df <- as.data.frame(reg.df)
              for (name in rownames(df)) {
                table$addRow(
                  rowKey = name,
                  values = list(
                    cla   = df[name, 1],
                    va    = df[name, 2],
                    co    = df[name, 3],
                    se    = df[name, 4],
                    wald  = df[name, 5],
                    p     = df[name, 6]
                  )
                )
              }
              
              # Covariate Effects Plot
              if (isTRUE(self$options$plot)) {
                plot_data <- reg.df
                image <- self$results$plot
                image$setState(plot_data)
              }
            }
          }
        }
        gc()
      },
      
      .plot = function(image,ggtheme, theme,...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        plot_data <- image$state
        
        plot_data$lower <- plot_data$coef - 1.96 * plot_data$std.err
        plot_data$upper <- plot_data$coef + 1.96 * plot_data$std.err
        
        plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x=variable, y=coef, color=class)) +
          ggplot2::geom_point(size=3, position=ggplot2::position_dodge(width=0.7)) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin=lower, ymax=upper),
                                 width=0.2, position=ggplot2::position_dodge(width=0.7)) +
          ggplot2::geom_hline(yintercept=0, linetype="dashed", color="gray50") +
          ggplot2::labs(title="",
                        x="Covariate", y="Coefficient (log-odds)") +
          ggplot2::coord_flip() +
          ggplot2::theme_minimal(base_size=13)
        
        
        print(plot)
        TRUE
      },
      
      .plot1 = function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        tau_long <- image$state
        tau_long$is_diag <- tau_long$from == tau_long$to
        
        p <- ggplot2::ggplot(
          tau_long,
          ggplot2::aes(x = from, y = to, fill = prob)
        ) +
          ggplot2::geom_tile(color = "grey85", linewidth = 0.5) +
          ggplot2::geom_text(
            ggplot2::aes(
              label = sprintf("%.3f", prob),
              fontface = ifelse(is_diag, "bold", "plain")
            ),
            size = 4.6
          ) +
          ggplot2::scale_fill_viridis_c(
            option = "C",
            limits = c(0, 1),
            name = expression(tau)
          ) +
          ggplot2::facet_wrap(~ step, nrow = 1) +
          ggplot2::labs(
            title = "Transition Probabilities (Non-invariant)",
            x = "Latent Class at Previous Time (Parent)",
            y = "Latent Class at Next Time (Child)"
            # ❌ Reading guide 문장 제거
          ) +
          ggplot2::coord_fixed() +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"),
            strip.text = ggplot2::element_text(face = "bold"),
            axis.title = ggplot2::element_text(face = "bold"),
            axis.text  = ggplot2::element_text(size = 12),
            panel.grid = ggplot2::element_blank(),
            legend.position = "right"
          )
        
        print(p)
        TRUE
      },
      
      
      .plot2 = function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        tau_long <- image$state
        tau_long$is_diag <- tau_long$from == tau_long$to
        
        p <- ggplot2::ggplot(
          tau_long,
          ggplot2::aes(x = from, y = to, fill = prob)
        ) +
          ggplot2::geom_tile(color = "grey85", linewidth = 0.5) +
          ggplot2::geom_text(
            ggplot2::aes(
              label = sprintf("%.3f", prob),
              fontface = ifelse(is_diag, "bold", "plain")
            ),
            size = 4.6
          ) +
          ggplot2::scale_fill_viridis_c(
            option = "C",
            limits = c(0, 1),
            name = expression(tau)
          ) +
          ggplot2::facet_wrap(~ step, nrow = 1) +
          ggplot2::labs(
            title = "Transition Probabilities (Invariant)",
            x = "Latent Class at Previous Time (Parent)",
            y = "Latent Class at Next Time (Child)"
            # ❌ Reading guide 문장 제거
          ) +
          ggplot2::coord_fixed() +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"),
            strip.text = ggplot2::element_text(face = "bold"),
            axis.title = ggplot2::element_text(face = "bold"),
            axis.text  = ggplot2::element_text(size = 12),
            panel.grid = ggplot2::element_blank(),
            legend.position = "right"
          )
        
        print(p)
        TRUE
      }
      
      
      
      
    )
  )





# Example with R---

# library(slca)
# library(magrittr)
#
# #LCA---
# data = nlsy97
# set.seed(1234)
# obj <- slca(L1(2) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98) %>%
#   estimate(data = nlsy97)
# par<- slca::param(obj)
# par
# plot(obj)
# summary(obj)
# # Regression--
# set.seed(1234)
# obj0 %>% slca::regress(L1 ~ SEX, nlsy97)
#

# #####################
# data = nlsy97
# set.seed(1234)
# obj1 <- slca(L1(2) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98,
#              L2(2) ~ ESMK_03 + FSMK_03 + DSMK_03 + HSMK_03,
#              L1~L2) %>%
#   estimate(data = nlsy97)
# par1<- slca::param(obj1)
# par1
#
# # Regression--
# set.seed(1234)
# obj1 %>% slca::regress(L1 ~ SEX, nlsy97)

# #########################
# data = nlsy97
# set.seed(1234)
# obj2 <- slca(L1(2) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98,
#              L2(2) ~ ESMK_03 + FSMK_03 + DSMK_03 + HSMK_03,
#              L1~L2,constraints = c("L1", "L2")) %>%
#   estimate(data = nlsy97)
# par2<- slca::param(obj2)
# par2
# plot(obj2)
# # Regression--
# set.seed(1234)
# obj2 %>% slca::regress(L1 ~ SEX, nlsy97)
