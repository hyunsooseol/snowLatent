
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
        
        data <- jmvcore::naOmit(data)
        
        vars<- self$options$vars
        
        class<- self$options$class
        
        #cluster <- self$options$cluster
        
        data<- as.data.frame(data)
        
         
        # vars <- colnames(data)
          vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
          vars <- paste0(vars, collapse=',')
         formula <- as.formula(paste0('glca::item(', vars, ')~1'))
        # 
        
        ################ Model Estimates############################ 
        
        # Model 1: LCA
        lca = glca::glca(formula,
                   data = data, nclass = class)
        
        res<- summary(lca)
        
        ############################################################### 
        
        self$results$text$setContent(res)
         
      
      }
        
        )
)
