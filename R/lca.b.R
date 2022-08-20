
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
            <p>1. jamovi treats all variables as qualitative/categorical/nominal.</p>
            <p>2. Variables must contain only integer values, and must be coded with consecutive values from 1 to the maximum number.</p>
            <p>3. The results of <b> Class membership </b> will be displayed in the datasheet.</p>
            <p>4. The output columm can NOT be used as an input to the same analysis.</p>
            <P>5. To analyze 'Profile' analysis, click the LCA analysis again.</p>
            <p>6. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub</a>.</p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
         if (self$options$comp)
             self$results$comp$setNote(
                 "Note",
                 "Bootstrap sample was used for obtaining p values"
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
        
        
        vars<- self$options$vars
        covs <- self$options$covs
        nc <- self$options$nc
        nb <- self$options$nb
       # cluster <- self$options$cluster
        
        data<- as.data.frame(data)
        
         ### Construct formula-----------        
          vars <- colnames(data)
          vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
          vars <- paste0(vars, collapse=',')
         
          formula <- as.formula(paste0('glca::item(', vars, ')~1'))
          
       
          ################ LCA model estimates############################ 
          
          lca = glca::glca(formula, data = data, nclass = nc, n.init=1)
          
          
          ############################################################### 
          
          gam<- lca[["param"]][["gamma"]]
          
          row.names(gam) <- 1:nrow(gam)  
          gam <- as.data.frame(gam)
          gam <- t(gam)
          gam <- as.data.frame(gam)
          
          # item probabilities------
          
          item<- lca[["param"]][["rho"]][["ALL"]]
          
          # Class Prevalences plot----------
          
          image <- self$results$plot1
          image$setState(lca)
          
          
          ############## Model comparison######################
          
             out <- NULL
             
             for (i in 1:self$options$nc) {
               
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
            
            results <-
              list(
                'res'=res,
                'gam'= gam,
                'item'=item
                
                )
          
            
      },   
      
      
      # Model comparison table----------
      
      
      .populateModelTable = function(results) {
            
        
        nc <- self$options$nc
         
        table <- self$results$comp
         
        
         res <- results$res
         
        
         aic <- res[,2]
         caic <- res[,3]
         bic <- res[,4] 
         entropy <- res[,5]
         df <- res[,6] 
         gsq <- res[,7] 
         p <- res[,8]
         
         
         names <- dimnames(res)[[1]]
         
         for (name in names) {
           
           row <- list()
           
           row[['nc']] <- nc
           row[["loglik"]]   <-  res[name, 1]
           row[["aic"]] <-  res[name, 2]
           row[["caic"]] <-  res[name, 3]
           row[["bic"]] <-  res[name, 4]
           row[["entropy"]] <-  res[name, 5]
           row[["df"]] <-  res[name, 6]
           row[["gsq"]] <-  res[name, 7]
           row[["p"]] <-  res[name, 8]
           
           
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
        
      
      plot1 <- plot(lca)
      
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

  
