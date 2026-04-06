
# 3-step approach using slca R package (Optimized for performance and memory usage)
stepClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "stepClass",
    inherit = stepBase,
    private = list(
      .htmlwidget = NULL,
      .dataCache = NULL,
      .obj = NULL,
      # Store model object for reuse
      .par = NULL,
      # Store parameters for reuse
      .gof = NULL,
      # Store goodness of fit for reuse
      .pos = NULL,
      # Store posterior probabilities for reuse
      .mem = NULL,
      # Store class membership for reuse
      
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
            '<li>Performs 3-step approach using <b>slca</b> R package.</li>',
            '<li>The <b>slca::regress()</b> function is used to explore the influence of external variables on the latent class variable within an estimated <b>slca</b> model.</li>',
            '<li>Model specifications are described in the <a href="https://kim0sun.github.io/slca/" target = "_blank">page</a>.</li>',
            '<li><b>L[k]</b>: <b>k</b> denotes the number of latent classes for the latent variable <b>L</b>.</li>',
            '<li>Minor discrepancies between p-values and 95% CIs may occur due to Wald approximation errors.</li>',
            '<li>If a respondent leaves all items unanswered (all missing values), the membership and posterior results for that case may not be meaningful. Please check your data and remove or handle such cases if necessary.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowLatent/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        
        if (self$options$fit)
          self$results$fit$setNote("Note", "Goodness of fit indices for the latent class model.")
        if (self$options$reg)
          self$results$reg$setNote("Note",
                                   "It utilizes logistic regression and employs a three-step approach.")
      },
      
      # Helper function to create and estimate model (for reuse)
      .getModel = function(data, formula) {
        if (is.null(private$.obj)) {
          library(magrittr)
          set.seed(1234)
          private$.obj <- slca::slca(formula) %>%
            slca::estimate(data = data)
        }
        return(private$.obj)
      },
      
      # Helper function to get parameters (for reuse)
      .getParameters = function(obj) {
        if (is.null(private$.par)) {
          private$.par <- slca::param(obj)
        }
        return(private$.par)
      },
      
      # Helper function to get goodness of fit (for reuse)
      .getGof = function(obj) {
        if (is.null(private$.gof)) {
          private$.gof <- slca::gof(obj)
        }
        return(private$.gof)
      },
      
      # Helper function to get class membership (for reuse)
      .getMembership = function(obj, f) {
        if (is.null(private$.mem)) {
          if (is.null(private$.pos)) {
            private$.pos <- obj[["posterior"]][["marginal"]][[f]]
          }
          private$.mem <- as.factor(as.numeric(factor(apply(
            private$.pos, 1, which.max
          ))))
        }
        return(private$.mem)
      },
      
      # Add dynamic class columns to a jamovi table
      .addClassColumns = function(tab, nclass) {
        for (k in seq_len(nclass)) {
          nm <- paste0("class", k)
          if (is.null(tab[[nm]])) {
            tab$addColumn(
              name = nm,
              title = paste("Class", k),
              type = "number"
            )
          }
        }
      },
      
      # Convert parameter object to text lines
      .extractParText = function(par) {
        txt <- capture.output(print(par))
        txt <- gsub("\t", " ", txt)
        txt <- txt[nzchar(trimws(txt))]
        txt
      },
      
      # Parse PI section into wide format
      .parsePIwide = function(txt) {
        pi_start  <- grep("^PI\\s*:", txt)
        rho_start <- grep("^RHO\\s*:", txt)
        
        if (length(pi_start) == 0 || length(rho_start) == 0 || rho_start[1] <= pi_start[1])
          return(NULL)
        
        block <- txt[(pi_start[1] + 1):(rho_start[1] - 1)]
        block <- block[nzchar(trimws(block))]
        
        cand <- block[grepl("^[[:space:][:digit:].]+$", block)]
        if (length(cand) == 0)
          return(NULL)
        
        vals <- scan(text = cand[length(cand)], quiet = TRUE)
        if (length(vals) == 0)
          return(NULL)
        
        out <- data.frame(label = "Initial probability", stringsAsFactors = FALSE)
        for (k in seq_along(vals))
          out[[paste0("class", k)]] <- as.numeric(vals[k])
        
        out
      },
      
      # Parse RHO section into wide format
      .parseRHOwide = function(txt, vars = NULL) {
        rho_start <- grep("^RHO\\s*:", txt)
        if (length(rho_start) == 0)
          return(NULL)
        
        block <- txt[(rho_start[1] + 1):length(txt)]
        block <- block[nzchar(trimws(block))]
        
        # remove bottom mapping lines such as: V1 V2 V3 ... / L ...
        block <- block[!grepl("^V[0-9]+(\\s+V[0-9]+)+\\s*$", trimws(block))]
        block <- block[!grepl("^L\\s+", trimws(block))]
        
        # keep only lines from the header onward
        class_idx <- grep("^\\s*class\\s*$", block)
        if (length(class_idx) == 0)
          return(NULL)
        
        block <- block[class_idx[1]:length(block)]
        
        # find class ids from a line like: "response   1   2   3"
        hdr_idx <- grep("^\\s*response\\s+", block)
        if (length(hdr_idx) == 0)
          return(NULL)
        
        hdr_parts <- scan(text = block[hdr_idx[1]], what = character(), quiet = TRUE)
        if (length(hdr_parts) < 2)
          return(NULL)
        
        # first token is "response", remaining are class labels
        class_ids <- hdr_parts[-1]
        
        # data lines start after the response/class header
        dat_lines <- block[(hdr_idx[1] + 1):length(block)]
        dat_lines <- dat_lines[nzchar(trimws(dat_lines))]
        
        rows <- list()
        row_i <- 1
        current_item <- NA
        
        for (ln in dat_lines) {
          parts <- scan(text = ln, what = character(), quiet = TRUE)
          if (length(parts) < 2)
            next
          
          first <- parts[1]
          
          # new item line: 1(V1), 2(V1), 1(V2) ...
          if (grepl("^[0-9]+\\(V[0-9]+\\)$", first)) {
            response <- sub("\\(.*$", "", first)
            current_item <- sub("^.*\\((V[0-9]+)\\)$", "\\1", first)
            probs <- suppressWarnings(as.numeric(parts[-1]))
          } else {
            # continuation row for same item
            response <- first
            probs <- suppressWarnings(as.numeric(parts[-1]))
          }
          
          if (is.na(current_item))
            next
          
          if (length(probs) != length(class_ids))
            next
          
          item_label <- current_item
          if (!is.null(vars) && length(vars) > 0) {
            idx <- suppressWarnings(as.integer(sub("^V", "", current_item)))
            if (!is.na(idx) && idx >= 1 && idx <= length(vars))
              item_label <- vars[idx]
          }
          
          one <- data.frame(
            item = item_label,
            response = response,
            stringsAsFactors = FALSE
          )
          
          for (j in seq_along(class_ids))
            one[[paste0("class", j)]] <- as.numeric(probs[j])
          
          rows[[row_i]] <- one
          row_i <- row_i + 1
        }
        
        if (length(rows) == 0)
          return(NULL)
        
        do.call(rbind, rows)
      },
      
      .run = function() {
        
        
        if (!isTRUE(self$options$run))
          return()
        
        # Use cached data if available
        if (is.null(private$.dataCache)) {
          private$.dataCache <- self$data
        }
        data <- private$.dataCache
        
        factors <- self$options$factors
        
        if (length(factors[[1]]$vars) < 2)
          return()
        
        vars <- factors[[1]][["vars"]]
        factors <- factors[[1]]$label  # L1[2]
        
        number <- gsub(".*\\[(\\d+)\\].*", "\\1", factors)
        nc <- as.integer(number)
        
        vars <- vapply(vars, function(x)
          jmvcore::composeTerm(x), '')
        ind <- paste0(vars, collapse = '+')
        formula <- as.formula(paste0(factors, ' ~ ', ind))
        
        # Get or create model
        obj <- private$.getModel(data, formula)
        
        # Get parameters
        par <- private$.getParameters(obj)
        
        f <- sub("\\[.*?\\]", "", factors)   # L1[2]-> L1
        
        # Get class membership
        mem <- private$.getMembership(obj, f)
        
        # Get posterior probabilities if needed
        if (is.null(private$.pos)) {
          private$.pos <- obj[["posterior"]][["marginal"]][[f]]
        }
        pos <- private$.pos
        
        # Class membership output
        if (isTRUE(self$options$member)) {
          if (self$options$member && self$results$member$isNotFilled()) {
            
            self$results$member$setRowNums(rownames(self$data))
            self$results$member$setValues(mem)
            
          }
        }
        
        # Posterior probabilities output
        if (isTRUE(self$options$post)) {
          if (self$options$post && self$results$post$isNotFilled()) {
            keys <- 1:nc
            measureTypes <- rep("continuous", nc)
            
            titles <- paste("Class", keys)
            descriptions <- paste("Class", keys)
            
            self$results$post$set(
              keys = keys,
              titles = titles,
              descriptions = descriptions,
              measureTypes = measureTypes
            )
            
            self$results$post$setRowNums(rownames(self$data))
            
            for (i in 1:nc) {
              scores <- as.numeric(pos[, i])
              self$results$post$setValues(index = i, scores)
            }
          }
        }
        
        # Parameters output -> wide jamovi tables
        if (isTRUE(self$options$par)) {
          
          txt <- private$.extractParText(par)
          
          # PI wide table
          pi_df <- private$.parsePIwide(txt)
          
          if (!is.null(pi_df) && nrow(pi_df) > 0) {
            pi_tab <- self$results$pi
            
            nclass_pi <- sum(grepl("^class[0-9]+$", names(pi_df)))
            private$.addClassColumns(pi_tab, nclass_pi)
            
            vals <- list(label = pi_df$label[1])
            for (k in seq_len(nclass_pi))
              vals[[paste0("class", k)]] <- pi_df[[paste0("class", k)]][1]
            
            pi_tab$addRow(rowKey = 1, values = vals)
          }
          
          # RHO wide table
          rho_df <- private$.parseRHOwide(txt, vars = vars)
          
          if (!is.null(rho_df) && nrow(rho_df) > 0) {
            rho_tab <- self$results$rho
            
            nclass_rho <- sum(grepl("^class[0-9]+$", names(rho_df)))
            private$.addClassColumns(rho_tab, nclass_rho)
            
            for (i in seq_len(nrow(rho_df))) {
              vals <- list(
                item = rho_df$item[i],
                response = rho_df$response[i]
              )
              
              for (k in seq_len(nclass_rho))
                vals[[paste0("class", k)]] <- rho_df[[paste0("class", k)]][i]
              
              rho_tab$addRow(
                rowKey = i,
                values = vals
              )
            }
          }
        }
        
        # Goodness of fit output
        if (isTRUE(self$options$fit)) {
          gof <- private$.getGof(obj)
          self$results$fit$setRow(
            rowNo = 1,
            values = list(
              class   = nc,
              df      = gof$Df,
              loglik  = gof$logLik,
              aic     = gof$AIC,
              bic     = gof$BIC,
              gsq     = gof$Gsq
            )
          )
        }
        
        # Regression analysis
        if (length(self$options$covs) >= 1) {
          if (isTRUE(self$options$reg)) {
            regform <- self$options$regform
            regform <- as.formula(regform)
            set.seed(1234)
            reg <- slca::regress(
              obj,
              regform,
              imputation = self$options$impu,
              method = self$options$method,
              data = data
            )
            
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
          
          if (isTRUE(self$options$reg1)) {
            regform <- self$options$regform1
            regform <- as.formula(regform)
            
            set.seed(1234)
            reg <- slca::regress(
              obj,
              regform,
              imputation = self$options$impu1,
              method = self$options$method1,
              data = data
            )
            
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
            
            table <- self$results$reg1
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
            if (isTRUE(self$options$plot1)) {
              plot_data <- reg.df
              image <- self$results$plot1
              image$setState(plot_data)
            }
            
          }
          
          # Free memory
          reg.df <- NULL
          df <- NULL
        }
        
        # Manual garbage collection
        gc(verbose = FALSE)
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
      
      .plot1 = function(image,ggtheme, theme,...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        plot_data <- image$state
        
        plot_data$lower <- plot_data$coef - 1.96 * plot_data$std.err
        plot_data$upper <- plot_data$coef + 1.96 * plot_data$std.err
        
        plot1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x=variable, y=coef, color=class)) +
          ggplot2::geom_point(size=3, position=ggplot2::position_dodge(width=0.7)) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin=lower, ymax=upper),
                                 width=0.2, position=ggplot2::position_dodge(width=0.7)) +
          ggplot2::geom_hline(yintercept=0, linetype="dashed", color="gray50") +
          ggplot2::labs(title="",
                        x="Covariate", y="Coefficient (log-odds)") +
          ggplot2::coord_flip() +
          ggplot2::theme_minimal(base_size=13)
        
        
        print(plot1)
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
