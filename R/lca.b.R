
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import stats
#' @import glca
#' @importFrom glca glca
#' @importFrom glca item
#' @importFrom glca gofglca
#' @export




lcaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lcaClass",
    inherit = lcaBase,
    private = list(
      .init = function() {
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(
          "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
           
            <p><b>To get started:</b></p>
            <p>_____________________________________________________________________________________________</p>
            <p>1. Latent Class Analysis(LCA) based on glca R package.</p>
            <p>2. The rationale of snowLatent module is described in the <a href='https://docs.google.com/viewer?a=v&pid=sites&srcid=a29yZWEuYWMua3J8a3VzdGF0bGFifGd4OjU0Nzc0NjU4OGJkODVjNDk'>documentation</a>.</p>
            <p>3. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowLatent/issues'  target = '_blank'>GitHub</a>.</p>
            
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
         if (self$options$comp)
             self$results$comp$setNote(
                 "Note",
                 "p: Bootstrap p value; H0: Model fit data adequately."
             )
        
        
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
        
        
        
      },
      
      .run = function() {
        
        ready <- TRUE
        
        if (is.null(self$options$vars) ||
            length(self$options$vars) < 2)
          
          ready <- FALSE
        
        if (ready) {
          
          data <- private$.cleanData()
          
          results <- private$.compute(data)               
          
          # populate fit table------------
          
          private$.populateFitTable(results)
          
          # populate Model comparison-----------
          
          private$.populateModelTable(results)
          
          # populate class probability table-----
          
          private$.populateClassTable(results)
          
          # populate item probabilities---------
          
          private$.populateItemTable(results)
          
          # populate posterior probabilities--
          
          private$.populatePosteriorOutputs(data)
          
          # populate class prevalences plot--
          
         # private$.populateClassPlot(results)
          
        
          
        }
      },
        
        
      .compute = function(data) {
        
        
        ######## glca R package ################
        
        # library(glca)
        # data("gss08")
        # 
        # lca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ AGE,
        #            data = gss08, nclass = 3, n.init = 1)
        # summary(lca)
        
        ################################
        
        
        vars<- self$options$vars
        covs<- self$options$covs
        
        nc <- self$options$nc
        nb <- self$options$nb
       
        data<- as.data.frame(data)
        
        # data[[covs]] <- jmvcore::toNumeric(data[[covs]])
        
       
        ############ Construct formula###################        
          
        vars <- colnames(data)
        vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
        vars <- paste0(vars, collapse=',')
      
        if (is.null(covs)){
      
          # formula with no covariate variables----------     
      
        formula <- as.formula(paste0('glca::item(', vars, ')~1'))
       
        }
   
        if (length(covs) > 0) {

          covs<- self$options$covs
          
          if (is.null(covs))
            return()
          
          #-----------------------------------------------
          varNames <- c(covs, vars)
          data <- jmvcore::select(self$data, varNames)
          
           for (var in vars)
               data[[var]] <- jmvcore::toNumeric(data[[var]])
           
          # exclude rows with missings in the covariate variables
          
          data <- data[!is.na(data[[covs]]), ]
          #-----------------------------------------------
          
          # Handling covariate variables ??? ---------

          # data <- colnames(data)
          # covs <- colnames(data)
          #covs <- colnames(data[covs])

         # covs<- data[covs] # ?????
      
          # Covariate formula is OK-----------------------------

      formula <- as.formula(paste0('glca::item(', vars, ')~', paste(data[,1], collapse= "+")))

         
        }

       
   ################### LCA model estimates############################

          lca = glca::glca(formula=formula,
                           data=data,
                        # group= data[[group]],
                           nclass = nc,
                           na.rm = TRUE,
                           n.init=1)
    #################################################################
        #group: Argument that indicates group variable which has the same length as manifest items
        #on the formula. If group = NULL (default), LCA or LCR is fitted.

        
       # self$results$text$setContent(lca)
      
        #fit measure----------
        
        loglik<- lca$gof$loglik
        aic<- lca$gof$aic
        caic<- lca$gof$caic
        bic<- lca$gof$bic
        entropy<- lca$gof$entropy
        df<- lca$gof$df
        gsq<- lca$gof$Gsq
            
   
      ######## LCA with no covariates##############
      
        gam<- lca[["param"]][["gamma"]]
          
        row.names(gam) <- 1:nrow(gam)  
        gam <- as.data.frame(gam)
        gam <- t(gam)
        gam <- as.data.frame(gam)
          
        # item probabilities------
          
        item<- lca[["param"]][["rho"]][["ALL"]]
          
        # Class Prevalences plot----------
          
         image <- self$results$plot1
        #  
        #  vars <- length(self$options$vars) 
        #  
        #  width <- 100 + vars * 100
        #  
        #  image$setSize(width, 700)
        # 
        #image$setSize(100 + 100 * length(self$options$vars), 200)
        image$setSize(100 + 100 * length(self$options$vars), 700)  
         
        image$setState(lca)
          
          
        ############## Model comparison######################
          
             out <- NULL
             
             for (i in 2:self$options$nc) {
               
               lca = glca::glca(formula, data = data, nclass = nc, n.init=1)
          
               fit<- glca::gofglca(lca, test = "boot", nboot = nb)
               fit <- fit[["gtable"]]
               
              if (is.null(out)) {
                out <- fit
              } else {
                out <- rbind(out, fit)
              }
            }

            out <- out
            
            row.names(out) <- 1:nrow(out)  
            
            res <- as.data.frame(out)
           
           res['class'] <- 2:nc
           
           res<- res[, c(9,1,2,3,4,5,6,7,8)]
           
        #  self$results$text$setContent(res)
            
            
            results <-
              list(
                'loglik'=loglik,
                'aic'=aic,
                'caic'=caic,
                'bic'=bic,
                'entropy'=entropy,
                'df'=df,
                'gsq'=gsq,
                'res'=res,
                'gam'= gam,
                'item'=item
                
                )
          
            
      },   
      
      
      ################ populating Tables################################
      
   # populating fit table-------------   
   
   .populateFitTable = function(results) {
     
     table <- self$results$fit
     
     class <- self$options$nc
     
     loglik<- results$loglik
     aic<- results$aic
     caic<- results$caic
     bic<- results$bic
     entropy<- results$entropy
     df<- results$df
     gsq<- results$gsq
     
   
     row <- list()
     
     row[['class']] <- class
     row[['loglik']] <- loglik
     row[['aic']] <- aic
     row[['caic']] <- caic
     row[['bic']] <- bic
     row[['entropy']] <- entropy
     row[['df']] <- df
     row[['gsq']] <- gsq
     
     table$setRow(rowNo = 1, values = row)
     
     
   },
   
   # Model comparison----------------
   
   
      .populateModelTable = function(results) {
            
        
        nc <- self$options$nc
         
        table <- self$results$comp
         
        
         res <- results$res
         
         class <- res[,1]
         loglik <- res[,2]
         aic <- res[,3]
         caic <- res[,4]
         bic <- res[,5] 
         entropy <- res[,6]
         df <- res[,7] 
         gsq <- res[,8] 
         p <- res[,9]
         
       
         names <- dimnames(res)[[1]]
         
       
         for (name in names) {
           
           row <- list()
           
           row[['class']] <- res[name,1]
           row[["loglik"]]   <-  res[name, 2]
           row[["aic"]] <-  res[name, 3]
           row[["caic"]] <-  res[name, 4]
           row[["bic"]] <-  res[name, 5]
           row[["entropy"]] <-  res[name, 6]
           row[["df"]] <-  res[name, 7]
           row[["gsq"]] <-  res[name, 8]
           row[["p"]] <-  res[name, 9]
           
           
           table$addRow(rowKey=name, values=row)
           
         }     
         
      },
         
      .populateClassTable= function(results) {  
         
        table <- self$results$cp
        
        gam <- results$gam
        names<- dimnames(gam)[[1]]
         
       
         for (name in names) {
           
           row <- list()
           
           row[['value']] <- gam[name,1]
           
           table$addRow(rowKey=name, values=row)
           
         }
         
      },
       
    
      # posterior probability----------
         
      .populatePosteriorOutputs= function(data) {
        
        nc<- self$options$nc
        
        data <- jmvcore::naOmit(data)
        
        data<- as.data.frame(data)
        
        vars <- colnames(data)
        vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
        vars <- paste0(vars, collapse=',')
        
        formula <- as.formula(paste0('glca::item(', vars, ')~1'))
        
        
        ################ LCA model estimates############################ 
        
        lca = glca::glca(formula, data = data, nclass = nc, n.init=1)
        ###############################################################
        
        pos<- lca$posterior$ALL
        
        
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
      },
      
      # item probabilities----------
      
      .populateItemTable= function(results) {
        
        
        res <- results$item
        
        self$results$text1$setContent(res)
        
      },   

   .plot1 = function(image, ...) {
     
     lca <- image$state
     
     if (is.null(lca))
       return()
      
     par(mfcol = c(2, 1))
      plot1 <- plot(lca, ask=FALSE)
      
      print(plot1)
      
      TRUE
      
      },
      
 
      
      
      ###############################################################
     #      lcr = glca::glca(formula1, data = data, nclass = class,n.init=1)
     #      
     #      self$results$text1$setContent(summary(lcr))
     #      
     #    }  
          
     ### Helper functions =================================     
     
     .cleanData = function() {
       
       items <- self$options$vars
       
       data <- list()
       
       for (item in items)
         data[[item]] <-
         jmvcore::toNumeric(self$data[[item]])
       
       attr(data, 'row.names') <- seq_len(length(data[[1]]))
       attr(data, 'class') <- 'data.frame'
       
       data <- jmvcore::naOmit(data)
       
       return(data)
     }
     
     
    )
)

  
