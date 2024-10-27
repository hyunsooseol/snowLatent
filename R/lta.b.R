
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
        # if (is.null(self$data) | is.null(self$options$vars)) {
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
        # 
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
      
      #---------
  .run = function() {

    if (is.null(self$options$vars) ||
        length(self$options$vars) < 3) return()
    
    
    data <- private$.cleanData()
    
    #---
    res <- private$.computeFIT()
    #---   
    
    if(length(self$options$covs)>=1){
      
      vars <- self$options$vars
      form2 <- self$options$form2
      
      if (!inherits(form2, "formula")) {
        form2 <- as.formula(form2)
      }
      
      set.seed(1234)
      reg<- slca::regress(res$obj,
                          form2,
                          method=self$options$method,
                          data=data)
      
      wald <- reg$coefficients / reg$std.err
      pval <- stats::pnorm(abs(wald), 1, lower.tail = FALSE)
     
      self$results$text1$setContent(reg[["coefficients"]])
      self$results$text2$setContent(reg[["std.err"]])
      self$results$text3$setContent(wald)
      self$results$text4$setContent(pval)
        }
   
    
    },
  
  
  .computeFIT = function() {
    
    # R example---
    
    # library(slca)
    # library(magrittr)
    # 
    # data = nlsy97
    # nlsy_smoke <- slca(smk98(2) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98) %>%
    #   estimate(data = nlsy97)
    # summary(nlsy_smoke)
    # param(nlsy_smoke)
    # 
    # regress(nlsy_smoke,smk98 ~ SEX, nlsy97)
    # 
    # #########
    # library(glca)
    # data("gss08")
    # # LCA
    # lca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
    #            data = gss08, nclass = 3, n.init = 1)
    # 
    # slca::regress(lca, smk98 ~ SEX, nlsy97)
    # #???????????
    # 
    # ####################
    # library(slca)
    # library(magrittr)
    # data <- gss7677[gss7677$RACE == "BLACK",]
    # 
    # model_stat <- slca(status(3) ~ PAPRES + PADEG + MADEG) %>%
    #   estimate(data = data)
    # summary(model_stat)
    # param(model_stat)
    # 
    # model_tol <- slca(tol(4) ~ TOLRAC + TOLCOM + TOLHOMO + TOLATH + TOLMIL) %>%
    #   estimate(data = data)
    # summary(model_tol)
    # param(model_tol)
    # 
    # model_lta <- slca(
    #   status(3) ~ PAPRES + PADEG + MADEG,
    #   tol(4) ~ TOLRAC + TOLCOM + TOLHOMO + TOLATH + TOLMIL,
    #   status ~ tol
    # ) %>% estimate(data = data)
    # summary(model_lta)
    # param(model_lta)
    # 
    # regress(model_lta, status ~ SEX, data)
    # 
    # ###################
    # 
    # # Standard LCA
    # slca(lc[3] ~ y1 + y2 + y3)
    # # Latent transition analysis (LTA)
    # slca(l1[3] ~ y11 + y21 + y31,
    #      l2[3] ~ y12 + y22 + y32,
    #      l1 ~ l2)
    # 
    # # LTA with measurement invariance
    # slca(l1[3] ~ y11 + y21 + y31,
    #      l2[3] ~ y12 + y22 + y32,
    #      l1 ~ l2, constraints = c("l1", "l2"))
    # 
    # ######################
    data <- private$.cleanData()
    
    vars <- self$options$vars
    form1 <- self$options$form1
    
    if (!inherits(form1, "formula")) {
      form1 <- as.formula(form1)
    }
    
    #self$results$text$setContent(form1)
    
    library(magrittr)
    set.seed(1234)
    obj<- slca::slca(form1) %>% 
          slca::estimate(data=data)
    par<- slca::param(obj)
    
    #self$results$text$setContent(res[["rho"]])
    
     retlist <- list(obj=obj, par=par)
     return(retlist)
  }, 
    
  ### Helper functions =================================     
  
  .cleanData = function() {
    
    data <- list()
    
    if( !is.null(self$options$covs) )
      
      for (cov in self$options$covs)
        data[[cov]] <- jmvcore::toNumeric(self$data[[cov]])
    
    for (var in self$options$vars)
      
      data[[var]] <- jmvcore::toNumeric(self$data[[var]])
    
    attr(data, 'row.names') <- seq_len(length(data[[1]]))
    attr(data, 'class') <- 'data.frame'
    
    if( !is.null(self$options$covs))
      for (cov in self$options$covs)
        data <- data[!is.na(data[[cov]]), ]
    
    return(data)
  }
      )
)
  
 