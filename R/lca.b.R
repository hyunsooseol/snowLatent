
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
        
        
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
        
        
        
      },
      
      .run = function() {
        
       #----------------------------
        
        data <- self$data
      #data <- jmvcore::naOmit(data)
        
        # Cleaning data----------
         
        items <- self$options$vars
        
        data <- list()
        
       for (item in items)
          data[[item]] <-
          jmvcore::toNumeric(self$data[[item]])
        
        attr(data, 'row.names') <- seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        
        data <- jmvcore::naOmit(data)
        #------------------------------
        
        vars<- self$options$vars
        covs <- self$options$covs
        
        nc <- self$options$nc
       # cluster <- self$options$cluster
        
        data<- as.data.frame(data)
        
         ### Construct formula-----------        
          vars <- colnames(data)
          vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
          vars <- paste0(vars, collapse=',')
         
          formula <- as.formula(paste0('glca::item(', vars, ')~1'))
          
       
          ############## Model comparison######################
          
             out <- NULL
             
             for (i in 1:self$options$nc) {
               
               lca = glca::glca(formula, data = data, nclass = nc, n.init=1)
          
               fit<- glca::gofglca(lca, test = "boot", nboot = 100)
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
            
            #self$results$text1$setContent(res)

######################################################################
        
         # populating model comparison--------
         
         table <- self$results$comp
         
        
         loglik <- res[,1]
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
         
         ################ LCA model estimates############################ 
         
         lca = glca::glca(formula, data = data, nclass = nc, n.init=1)
         
         
         ############################################################### 
         
         gam<- lca[["param"]][["gamma"]]
         
         row.names(gam) <- 1:nrow(gam)  
         gam <- as.data.frame(gam)
         gam <- t(gam)
         gam <- as.data.frame(gam)
         
         names<- dimnames(gam)[[1]]
         
         #creating table--------
         
         table <- self$results$cp
         
         for (name in names) {
           
           row <- list()
           
           row[['value']] <- gam[name,1]
           
           table$addRow(rowKey=name, values=row)
           
         }
         
         
         
         
         
         
         
         
         
      ###############################################################
     #      lcr = glca::glca(formula1, data = data, nclass = class,n.init=1)
     #      
     #      self$results$text1$setContent(summary(lcr))
     #      
     #    }  
          
                 
          
          
          
      
      }
        
        )
)