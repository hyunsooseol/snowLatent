
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
      },
      
      .run = function() {
        if (is.null(private$.dataCache)) {
          private$.dataCache <- self$data
        }
        data <- private$.dataCache
        
        factors <- self$options$factors
        nfactors <- length(factors)
        
        if (length(factors[[1]]$vars) < 2)
          return()
        
        if (nfactors == 1) {
          vars <- factors[[1]][["vars"]]
          factors <- factors[[1]]$label  # L1[2]
          
          number <- gsub(".*\\[(\\d+)\\].*", "\\1", factors)
          nc <- as.integer(number)
          
          vars <- vapply(vars, function(x)
            jmvcore::composeTerm(x), '')
          ind <- paste0(vars, collapse = '+')
          formula <- as.formula(paste0(factors, ' ~ ', ind))
          
          library(magrittr)
          set.seed(1234)
          obj <- slca::slca(formula) %>%
            slca::estimate(data = data)
          
          par <- slca::param(obj)
          
          f <- sub("\\[.*?\\]", "", factors)   # L1[2]-> L1
          pos <- obj[["posterior"]][["marginal"]][[f]]
          
          mem <- as.numeric(factor(apply(pos, 1, which.max)))
          mem <- as.factor(mem)
          
          if (isTRUE(self$options$member)) {
            if (self$options$member
                && self$results$member$isNotFilled()) {
              
              self$results$member$setRowNums(rownames(self$data))
              self$results$member$setValues(mem)
              
            }
          }
          
          if (isTRUE(self$options$post)) {
            if (self$options$post
                && self$results$post$isNotFilled()) {
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
          
          if (isTRUE(self$options$par)) {
            self$results$text1$setContent(par)
          }
          
          if (isTRUE(self$options$fit)) {
            gof <- slca::gof(obj)
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
        }
        
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
        }
        
        if (length(self$options$covs) >= 1) {
          regform <- self$options$regform
          regform <- as.formula(regform)
          
          if (!exists("obj")) {
            if (length(self$options$factors) == 1) {
              vars <- factors[[1]][["vars"]]
              factorLabel <- factors[[1]]$label
              number <- gsub(".*\\[(\\d+)\\].*", "\\1", factorLabel)
              nc <- as.integer(number)
              
              vars <- vapply(vars, function(x)
                jmvcore::composeTerm(x), '')
              ind <- paste0(vars, collapse = '+')
              formula <- as.formula(paste0(factorLabel, ' ~ ', ind))
              
              library(magrittr)
              set.seed(1234)
              obj <- slca::slca(formula) %>%
                slca::estimate(data = data)
            } else {
              library(magrittr)
              set.seed(1234)
              obj <- slca::slca(form1) %>%
                slca::estimate(data = data)
            }
          }
          
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
        gc()
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
