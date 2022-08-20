
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
        cov <- self$options$cov
        
        nc <- self$options$nc
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
        
       # self$results$text$setContent(summary(lca))
      
        # Fit measure-----------
          
          nc <- self$options$nc
          
          aic<- lca[["gof"]][["aic"]]
          
          bic<- lca[["gof"]][["bic"]]
          
          caic <- lca[["gof"]][["caic"]]
          
          entropy<- lca[["gof"]][["entropy"]]
          
          # gsq<- lca[["gof"]][["Gsq"]]
          # loglik <- lca[["gof"]][["loglik"]]
          
          # Fit measure table-------------
          
          table <- self$results$fit
          
          row <- list()
          
          row[['nc']] <- nc
          row[['aic']] <- aic
          row[['bic']] <- bic
          row[['caic']] <- caic
          row[['entropy']] <- entropy
          
          table$setRow(rowNo = 1, values = row)
          
          #---------------------------------
          # Model comparison-----------------
          
          out <- NULL
          
          for (i in 1:self$options$nc) {
            
            lca = glca::glca(formula, data = data, nclass = nc, n.init=1)
            
           
            aic<- lca[["gof"]][["aic"]]
            
            bic<- lca[["gof"]][["bic"]]
            
            caic <- lca[["gof"]][["caic"]]
            
            entropy<- lca[["gof"]][["entropy"]]
            
            
            
            df<- data.frame(aic,bic,caic, entropy)
            
            
            if (is.null(out)) {
              out <- df
            } else {
              out <- rbind(out, df)
            }
          }
          
          out <- out
          
          
          #Populate model comparison----------------
          
          table <- self$results$comp
          
          nc <- self$options$nc
         
          fit<- data.frame(out)
          
         # self$results$text$setContent(fit)
         
          names <- dimnames(fit)[[1]]
          
          
          for (name in names) {
            
            row <- list()
            
            row[["aic"]]   <-  fit[name, 1]
            row[["bic"]] <-  fit[name, 2]
            row[["caic"]] <-  fit[name, 3]
            row[["entropy"]] <-  fit[name, 4]
            
            table$addRow(rowKey=name, values=row)
            
          }  
      
          
             
       
     #      if(self$options$lcr==TRUE){
     #      
     #        dat<- jmvcore::select(data,self$options$cov)
     #      
     # #   formula1 <- as.formula(paste0('glca::item(', vars, ') ~ vars'))
     #    
     #  #formula1<- jmvcore:::composeFormula(self$options$vars, self$options$cov)
     #      
     #        formula1 <- as.formula(paste0('glca::item(', vars, '),'),paste0(self$options$cov, collapse ="+"))      
     #      ###############################################################
     #      lcr = glca::glca(formula1, data = data, nclass = class,n.init=1)
     #      
     #      self$results$text1$setContent(summary(lcr))
     #      
     #    }  
          
          
          
          
          
      
      }
        
        )
)
