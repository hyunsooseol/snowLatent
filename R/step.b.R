
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
      
      .run = function() {
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
            self$results$member$setValues(mem)
            self$results$member$setRowNums(rownames(data))
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
            
            self$results$post$setRowNums(rownames(data))
            
            for (i in 1:nc) {
              scores <- as.numeric(pos[, i])
              self$results$post$setValues(index = i, scores)
            }
          }
        }
        
        # Parameters output
        if (isTRUE(self$options$par)) {
          self$results$text1$setContent(par)
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
          }
          
          # Free memory
          reg.df <- NULL
          df <- NULL
        }
        
        # Manual garbage collection
        gc(verbose = FALSE)
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
