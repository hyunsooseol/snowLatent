
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import slca
#' @import magrittr
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
        
        private$.htmlwidget <- HTMLWidget$new()

        if (is.null(self$data) | length(self$options$factors[[1]]$vars) == 0) {

          self$results$instructions$setVisible(visible = TRUE)

        }

        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title="Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li><b>slca</b> R package is described in the <a href="https://CRAN.R-project.org/package=slca" target = "_blank">page</a>.</li>',
              '<li><b>L1[k]</b>: <b>k</b> denotes the number of latent classes for the first latent class variable <b>L1</b>.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowLatent/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'

            )

          )
        )

      },
      
 
  .run = function() {

     data <- self$data
     data <- jmvcore::naOmit(data)
     
     factors <- self$options$factors
     
     # Calculate the number of factors before modifying
     nfactors <- length(factors)

     if (length(factors[[1]]$vars) < 2) return()

  if(nfactors==1){    
      
      vars <- factors[[1]][["vars"]]
      factors <- factors[[1]]$label  #L1[2]
     
      # y~a+b+c---
      vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
      ind <- paste0(vars, collapse = '+')
      formula <- as.formula(paste0(factors, ' ~ ', ind))
      
      
        library(magrittr)
        set.seed(1234)
        obj<- slca::slca(formula) %>%
              slca::estimate(data=data)
        par<- slca::param(obj)
      
        # Additional outputs: Estimated parameters---
        
        if(isTRUE(self$options$par)){
          self$results$text1$setContent(par) 
        }
        
      # Posterior prob. and membership---
      #obj[["posterior"]][["marginal"]][["L1"]]
      f <- sub("\\[.*?\\]", "", factors)   # L1[2]-> L1
      pos <- obj[["posterior"]][["marginal"]][[f]]
      #self$results$text3$setContent(pos)
      
      # class membership---
      mem <- as.numeric(factor(apply(pos, 1, which.max)))
      mem<- as.factor(mem)
      
      # Class membership---
      if(isTRUE(self$options$member)){
        
        if (self$options$member
            && self$results$member$isNotFilled()) {
          
          self$results$member$setValues(mem)
          self$results$member$setRowNums(rownames(data))
        }
      }
      
      # Posterior prob.---
      
      if(isTRUE(self$options$post)){
    
        if (self$options$post
            && self$results$post$isNotFilled()) {
          
          keys <- 1:self$options$nc
          measureTypes <- rep("continuous", self$options$nc)
          
          titles <- paste("Class", keys)
          descriptions <- paste("Class", keys)
          
          self$results$post$set(
            keys=keys,
            titles=titles,
            descriptions=descriptions,
            measureTypes=measureTypes
          )                
          
          self$results$post$setRowNums(rownames(data))
          
          for (i in 1:self$options$nc) {
            scores <- as.numeric(pos[, i])
            self$results$post$setValues(index=i, scores)
          }
        }
      } 
  } 
   
  if(nfactors>1){
    
    #### Formulas #################################
    
    # Assuming 'factors' is a list of lists with each sublist containing 'vars' and 'label'
    formulas <- list()
    
    for (factor in factors) {
      vars <- factor[["vars"]]
      label <- factor[["label"]]
      
      # y ~ a + b + c ---
      vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
      ind <- paste0(vars, collapse = '+')
      formula <- as.formula(paste0(label, ' ~ ', ind))
      
      # Convert the string to a formula object and store it
      formulas[[label]] <- as.formula(formula)
    }
    
    # Sequential relations---
    create_sequential_relations <- function(formulas) {
      relations <- c()
      
      for (i in seq_along(formulas)[-length(formulas)]) {
       
        left <- sub("\\[.*?\\]", "", formulas[i])   # lc1[2]-> lc1
        left <- sub(" ~.*", "", left)               
        right <- sub("\\[.*?\\]", "", formulas[i + 1]) 
        right <- sub(" ~.*", "", right)             
        relation <- paste0(left, " ~ ", right)
        relations <- c(relations, as.formula(relation))  
      }
      
      return(relations)
    }
    
    # Sequential relations 
    sequential_relations <- create_sequential_relations(formulas)
    form1 <- c(formulas, sequential_relations)
   
    # Estimated parameters---
    # JLCPA---
    if(isTRUE(self$options$par1)){
 
      library(magrittr)
      set.seed(1234)
      obj1<- slca::slca(formula = formulas) %>%
        slca::estimate(data=data)
      par1<- slca::param(obj1)
     
      self$results$text2$setContent(par1) 
         }
    
    #LTA---
    if(isTRUE(self$options$par2)){
 
      library(magrittr)
      set.seed(1234)
      obj2<- slca::slca(formula = form1) %>%
        slca::estimate(data=data)
      par2<- slca::param(obj2)
     
      self$results$text3$setContent(par2) 
         }
    
    # LTA with Measurement invariance---
    if(isTRUE(self$options$par3)){
      
      # Defining constraints(Measurement invariance)---
      cons <- self$options$cons
      cons1 <- unlist(strsplit(cons, ","))
    
      #Num.of classes Should be equal before proceeding!!!
      
      library(magrittr)
      set.seed(1234)
      obj3<- slca::slca(formula = form1,
                       constraints=cons1) %>%
        slca::estimate(data=data)
      par3<- slca::param(obj3)

     self$results$text4$setContent(par3) 
      }
               }   
 
 # Regression---
 if(length(self$options$covs)>=1){
       
      #LCA regression---
       #reg<- slca::regress(nlsy_smoke,smk98 ~ SEX, nlsy97)
       regform <- self$options$regform
       regform <- as.formula(regform)
      
       if(length(self$options$factors)==1){
         
         library(magrittr)
         set.seed(1234)
         obj<- slca::slca(formula) %>%
           slca::estimate(data=data)
         
       }
       
       if(length(self$options$factors)>1){
         
         library(magrittr)
         set.seed(1234)
         obj<- slca::slca(form1) %>%
           slca::estimate(data=data)
         
       }
       
       reg <- slca::regress(obj, 
                            regform,
                            imputation = self$options$impu,
                            method=self$options$method,
                            data=data)
       
       # coef, std.err, wald, p-value---
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
       
       #self$results$text3$setContent(reg.df)
       # Logistic regression table---
       
       table <- self$results$reg
       
       df <- as.data.frame(reg.df)
       names <- dimnames(df)[[1]]
       
       for (name in names) {
         row <- list()
         
         row[["cla"]]   <-  df[name, 1]
         row[["va"]]   <-  df[name, 2]
         row[["co"]] <-  df[name, 3]
         row[["se"]] <-  df[name, 4]
         row[["wald"]] <- df[name, 5]
         row[["p"]] <-   df[name, 6]
         
         table$addRow(rowKey=name, values=row)
         
       }
       
     }       

     # Example with R---

     # library(slca)
     # library(magrittr)
     # 
     # #LCA---
     # data = nlsy97
     # set.seed(1234)
     # obj0 <- slca(L1(2) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98) %>% 
     #   estimate(data = nlsy97)
     # par0<- slca::param(obj0)
     # par0
     # 
     # # Regression--
     # set.seed(1234)
     # obj0 %>% slca::regress(L1 ~ SEX, nlsy97)
     # 
     # #JLCA---
     # data = nlsy97
     # set.seed(1234)
     # obj <- slca(L1(2) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98,
     #             L2(2) ~ ESMK_03 + FSMK_03 + DSMK_03 + HSMK_03) %>%
     #   estimate(data = nlsy97)
     # par<- slca::param(obj)
     # par
     # 
     # # Regression--
     # set.seed(1234)
     # obj %>% slca::regress(L2 ~ SEX, nlsy97)
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
     # 
     # # Regression--
     # set.seed(1234)
     # obj0 %>% slca::regress(L1 ~ SEX, nlsy97)
     
     
          
 }))
  
 