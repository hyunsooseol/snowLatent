
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import slca
#' @importFrom slca slca
#' @importFrom slca estimate
#' @importFrom slca param
#' @importFrom slca regress
#' @import ggplot2
#' @export


ltaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "ltaClass",
    inherit = ltaBase,
    private = list(
      .htmlwidget = NULL, 
      
      .init = function() {
        
        # private$.htmlwidget <- HTMLWidget$new()
        # 
        # if (is.null(self$data) | is.null(self$options$factors)) {
        # 
        #   self$results$instructions$setVisible(visible = TRUE)
        # 
        # }
        # 
        # self$results$instructions$setContent(
        #   private$.htmlwidget$generate_accordion(
        #     title="Instructions",
        #     content = paste(
        #       '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
        #       '<div style="text-align:justify;">',
        #       '<ul>',
        #       '<li><b>tidySEM</b> R package is described in the <a href="https://cjvanlissa.github.io/tidySEM/articles/LCGA.html" target = "_blank">page</a>.</li>',
        #       '<li>Please set <b>Thresholds=TRUE</b> when analyzing ordinal data.</li>',
        #       '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
        #       '</ul></div></div>'
        # 
        #     )
        # 
        #   )
        # )

        if(isTRUE(self$options$plot1)){
          
          width <- self$options$width1
          height <- self$options$height1
          self$results$plot1$setSize(width, height)
        }
        
        if(isTRUE(self$options$plot)){
          
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }      
 
      },
      
 
  .run = function() {

    # if (is.null(self$options$factors) ||
    #     length(self$options$factors) < 3) return()
    
    
     data <- self$data
     data <- jmvcore::naOmit(data)
     
     factors <- self$options$factors
    
     # is.list?---
     if (!is.list(factors)) {
       stop("factors is not a list")
     }
     
     # Calculate the number of factors before modifying
     nfactors <- length(factors)
 
  if(nfactors==1){    
      vars <- factors[[1]][["vars"]]
      factors <- factors[[1]]$label
    
      # y~a+b+c---
      vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
      ind <- paste0(vars, collapse = '+')
      formula <- as.formula(paste0(factors, ' ~ ', ind))
      
      
     library(magrittr)
        set.seed(1234)
        obj<- slca::slca(formula) %>%
              slca::estimate(data=data)
        par<- slca::param(obj)
        
       if(length(self$options$covs)>=1){
        #LCA regression---
        #reg<- slca::regress(nlsy_smoke,smk98 ~ SEX, nlsy97)
         
          form1 <- self$options$form1
          form1 <- as.formula(form1)
      
         reg <- slca::regress(obj, 
                             form1,
                             method=self$options$method,
                             data=data)
         self$results$text1$setContent(reg)
       
        }  
  
        
        }    
       
   if(nfactors>1){
     
     # Assuming 'factors' is a list of lists with each sublist containing 'vars' and 'label'
     formulas <- list()
     
     for (factor in factors) {
       vars <- factor[["vars"]]
       label <- factor[["label"]]
       
       # y~a+b+c---
       vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
       ind <- paste0(vars, collapse = '+')
       formula <- as.formula(paste0(label, ' ~ ', ind))
       
       # # Store the formula in the list
       # formulas[[label]] <- formula
       
       # Convert the string to a formula object and store it
       formulas[[label]] <- as.formula(formula)
       
     }
     
     # # Assuming 'formulas' is a list of formulas created previously
     # formula_strings <- sapply(formulas, function(f) deparse(f))
      
     # # Combine the formulas into a single string with commas
     # combined_formulas <- paste(formula_strings, collapse = ", ")
     
     # Create a data frame or matrix with rbind
     #lta.formula <- rbind(combined_formulas)
     
     #self$results$text2$setContent(formulas)
     
     
     #connecting factors with '~'---
     # Assuming 'factors' is a list of lists with each sublist containing 'label'
     factor_labels <- sapply(factors, function(factor) factor[["label"]]) 
     
     # Initialize an empty string for the single formula
     single_connection_formula <- ""
     
     # Loop to create only one connection formula
     for (i in seq_along(factor_labels)) {
       for (j in seq_along(factor_labels)) {
         if (i != j) {
           single_connection_formula <- paste0(factor_labels[i], " ~ ", factor_labels[j])
           break  # Exit the inner loop after creating one formula
         }
       }
       if (single_connection_formula != "") {
         break  # Exit the outer loop if a formula has been created
       }
     }
    
     # Remove brackets from the formula
     single_connection_formula <- gsub("\\[.*?\\]", "", single_connection_formula)
     
     #self$results$text2$setContent(single_connection_formula)
     single_connection_formula <- as.formula(single_connection_formula)
    
     formula_list <- list(formulas[[1]], formulas[[2]], single_connection_formula)
       
     #self$results$text3$setContent(formula_list)
     
     # Defining constraints---
     cons <- self$options$cons
     cons1 <- strsplit(self$options$cons, ',')[[1]]
    
     
      library(magrittr)
      set.seed(1234)
      obj<- slca::slca(formula = formula_list, constraints = cons1) %>%
        slca::estimate(data=data)
      par<- slca::param(obj)

      self$results$text2$setContent(par)
     
     
   }   
      
      
      
      
    #---
    #res <- private$.computeRES()
    #---   
    
    # if(length(self$options$covs)>=1){
    #   
    #   factors <- self$options$factors
    #   form2 <- self$options$form2
    #   
    #   if (!inherits(form2, "formula")) {
    #     form2 <- as.formula(form2)
    #   }
    #   
    #   set.seed(1234)
    #   reg<- slca::regress(res$obj,
    #                       form2,
    #                       method=self$options$method,
    #                       data=data)
    #   
    #   wald <- reg$coefficients / reg$std.err
    #   pval <- stats::pnorm(abs(wald), 1, lower.tail = FALSE)
    #  
    #   self$results$text1$setContent(reg[["coefficients"]])
    #   self$results$text2$setContent(reg[["std.err"]])
    #   self$results$text3$setContent(wald)
    #   self$results$text4$setContent(pval)
    #     }
   
    
    }

  # .computeRES = function() {
  #   
  #   # R example---
  #   
  #   # library(slca)
  #   # library(magrittr)
  #   # 
  #   # data = nlsy97
  #   # nlsy_smoke <- slca(smk98(2) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98) %>%
  #   #   estimate(data = nlsy97)
  #   # summary(nlsy_smoke)
  #   # param(nlsy_smoke)
  #   # 
  #   # regress(nlsy_smoke,smk98 ~ SEX, nlsy97)
  #   # 
  #   # ####################
  #   # library(slca)
  #   # library(magrittr)
  #   # data <- gss7677[gss7677$RACE == "BLACK",]
  #   # 
  #   # model_stat <- slca(status(3) ~ PAPRES + PADEG + MADEG) %>%
  #   #   estimate(data = data)
  #   # summary(model_stat)
  #   # param(model_stat)
  #   # 
  #   # model_tol <- slca(tol(4) ~ TOLRAC + TOLCOM + TOLHOMO + TOLATH + TOLMIL) %>%
  #   #   estimate(data = data)
  #   # summary(model_tol)
  #   # param(model_tol)
  #   # 
  #   # model_lta <- slca(
  #   #   status(3) ~ PAPRES + PADEG + MADEG,
  #   #   tol(4) ~ TOLRAC + TOLCOM + TOLHOMO + TOLATH + TOLMIL,
  #   #   status ~ tol
  #   # ) %>% estimate(data = data)
  #   # summary(model_lta)
  #   # param(model_lta)
  #   # 
  #   # regress(model_lta, status ~ SEX, data)
  #   # 
  #   # ###################
  #   # 
  #   # # Standard LCA
  #   # slca(lc[3] ~ y1 + y2 + y3)
  #   # # Latent transition analysis (LTA)
  #   # slca(l1[3] ~ y11 + y21 + y31,
  #   #      l2[3] ~ y12 + y22 + y32,
  #   #      l1 ~ l2)
  #   # 
  #   # # LTA with measurement invariance
  #   # slca(l1[3] ~ y11 + y21 + y31,
  #   #      l2[3] ~ y12 + y22 + y32,
  #   #      l1 ~ l2, constraints = c("l1", "l2"))
  #   # 
  #   # ######################
  #   data <- private$.cleanData()
  #   
  #   factors <- self$options$factors
  #   vars <- factors$vars
  #   
  #   factors <- vapply(factors, function(x) jmvcore::composeTerm(x), '')
  #   vars <- paste0(vars, collapse='+')
  #   formula <- as.formula(paste0('(', factors, ') ~', vars))
  #   
  #   self$results$text$setContent(formula)
  #   
  #   
  #   
  #   
  #   form1 <- self$options$form1
  #   
  #   if (!inherits(form1, "formula")) {
  #     form1 <- as.formula(form1)
  #   }
  #   
  #   #self$results$text$setContent(form1)
  #   
  #   library(magrittr)
  #   set.seed(1234)
  #   obj<- slca::slca(form1) %>% 
  #         slca::estimate(data=data)
  #   par<- slca::param(obj)
  #   
  #   #self$results$text$setContent(res[["rho"]])
  #   
  #    retlist <- list(obj=obj, par=par)
  #    return(retlist)
  # }, 
    
  ### Helper functions =================================     
  # 
  # .cleanData = function() {
  # 
  #   data <- list()
  # 
  #   if( !is.null(self$options$covs) )
  # 
  #     for (cov in self$options$covs)
  #       data[[cov]] <- jmvcore::toNumeric(self$data[[cov]])
  # 
  #   for (var in self$options$vars)
  # 
  #     data[[var]] <- jmvcore::toNumeric(self$data[[var]])
  # 
  #   attr(data, 'row.names') <- seq_len(length(data[[1]]))
  #   attr(data, 'class') <- 'data.frame'
  # 
  #   if( !is.null(self$options$covs))
  #     for (cov in self$options$covs)
  #       data <- data[!is.na(data[[cov]]), ]
  # 
  #   return(data)
  # }
      )
)
  
 