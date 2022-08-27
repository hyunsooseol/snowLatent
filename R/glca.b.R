
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import stats
#' @import glca
#' @importFrom glca glca
#' @importFrom glca item
#' @importFrom glca gofglca
#' @export
#' 
glcaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "glcaClass",
    inherit = glcaBase,
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
        
        if (is.null(self$options$group) || is.null(self$options$vars) || length(self$options$vars) < 2)
          
          ready <- FALSE
        
        if (ready) {
          
          data <- private$.cleanData()
          
          results <- private$.compute(data)               
          
          # populate Fit table----------
          
          private$.populateFitTable(results)
          
          # populate Model comparison-----------
          
          private$.populateModelTable(results)
          
          # populate Class probability table-----
          
          #private$.populateClassTable(results)
          
         # populate class prevalences by group table-------
          
          private$.populateCgTable(results)
          
          
           # populate item probabilities---------
          
          private$.populateItemTable(results)
          
          # populate posterior probabilities--
          
          private$.populatePosTable(results)
          
         
        }
      },
      
      
      .compute = function(data) {
        
        if (is.null(self$options$group))
          return()
        
        
        ######## glca R package ################
        
        # library(glca)
        # data("gss08")
        # 
        # lca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
        #       group= DEGREE,            
        #       data = gss08, 
        #       nclass = 3)
        # summary(lca)
        
        ################################
        
        # Constructing formula----------------        
        
        vars <- colnames(data[, -1] )
        vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
        vars <- paste0(vars, collapse=',')
        
        # formula with no covariates variables----------     
        
        formula <- as.formula(paste0('glca::item(', vars, ')~1'))
        group <- data[, 1]
        
        nc <- self$options$nc
        nb <- self$options$nb
       
        
        ################### LCA model estimates############################
        
        lca = glca::glca(formula=formula,
                         group= group,
                         data=data,
                         nclass = nc,
                         n.init=1)
      #################################################################
      #group: Argument that indicates group variable which has the same length as manifest items
      #on the formula. If group = NULL (default), LCA or LCR is fitted.
        
        
      # self$results$text$setContent(lca)
        
        
        # fit measure----------
        
        loglik<- lca$gof$loglik
        aic<- lca$gof$aic
        caic<- lca$gof$caic
        bic<- lca$gof$bic
        entropy<- lca$gof$entropy
        df<- lca$gof$df
        gsq<- lca$gof$Gsq
        
       
        # Class prevalences by group----------
        
        class.group <- lca[["param"]][["gamma"]]
        
        #self$results$text$setContent(class.group)
        
        # item probability---------
        
        item<- lca[["param"]][["rho"]]
        
        # posterior probability---------
        
        post <- lca[["posterior"]]
        
        
        # Class Prevalences plot----------
        
        image <- self$results$plot1
        
        vars <- length(self$options$vars) 
        
        width <- 100 + vars * 100
        
        image$setSize(width, 700)
        
        
        #image$setSize(100 + 100 * length(self$options$vars), 200)
        
        image$setState(lca)
        
        
        # Model comparison----------
        
        out <- NULL
        
        for (i in 2:self$options$nc) {
          
          lca = glca::glca(formula, data = data, 
                           group=group,
                           nclass = nc, n.init=1)
          
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
            'class.group'=class.group,
            'item'=item,
            'post'=post
            
          )
        
        
      },   
      
      
      ################ populating Tables################################
      
      # populate Model Fit table-------------   
      
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
      
      # populate Model comparison table------------
      
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
      
      # populate class prevalences by group table---------------
      
      .populateCgTable= function(results) {  
        
        table <- self$results$preval
        
        cg<- results$class.group
        
        cg<- as.data.frame(cg)
        
        names <- dimnames(cg)[[1]]
        dims <- dimnames(cg)[[2]]
        
        for (dim in dims) {
          
          table$addColumn(name = paste0(dim),
                          type = 'number')
        }
        
        
        for (name in names) {
          
          row <- list()
          
          for(j in seq_along(dims)){
            
            row[[dims[j]]] <- cg[name,j]
            
          }
          
          table$addRow(rowKey=name, values=row)
          
        }
        
      },
      
      
      # posterior probability----------
      
      # .populatePosteriorOutputs= function(data) {
      #   
      #   nc<- self$options$nc
      #   
      #   # --------------------------------------------------
      #   # NOTE:
      #   # Excludes the GROUP variable, which is inserted
      #   # as the first variable in the .cleandata()function
      #   # --------------------------------------------------
      #   df <- jmvcore::naOmit(data[, -1])
      #   
      #   vars <- colnames(df)
      #   vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
      #   vars <- paste0(vars, collapse=',')
      #   formula <- as.formula(paste0('glca::item(', vars, ') ~ 1'))
      #   
      #  # group <- data[, 1]
      #   
      #   ################ LCA model estimates############################ 
      #   
      #   lca = glca::glca(formula=formula, 
      #                    data=df, 
      #                   # group=group,
      #                    nclass=nc, 
      #                    n.init=1)
      #   ###############################################################
      #   pos<- lca$posterior$ALL
      #   
      #   #self$results$text$setContent(pos)
      #   
      #   if (self$options$post && self$results$post$isNotFilled()) {
      #     
      #     keys <- 1:self$options$nc
      #     measureTypes <- rep("continuous", self$options$nc)
      #     titles <- paste("Class", keys)
      #     descriptions <- paste("Class", keys)
      #     
      #     self$results$post$set(
      #       keys=keys,
      #       titles=titles,
      #       descriptions=descriptions,
      #       measureTypes=measureTypes
      #     )                
      #     
      #     self$results$post$setRowNums(rownames(data))
      #     
      #     for (i in 1:self$options$nc) {
      #       
      #       scores <- as.numeric(pos[, i])
      #       self$results$post$setValues(index=i, scores)
      #     }
      #   }
      #   
      # },
      # 
        
      
      # item probabilities----------

      .populateItemTable= function(results) {


        res <- results$item

        self$results$text1$setContent(res)

      },

      # posterior probability----------
      
      .populatePosTable= function(results) {
        
        
        post <- results$post
        
        self$results$text2$setContent(post)
        
      },
      
      
      
      ######## plot#######################
      .plot1 = function(image, ...) {
        
        lca <- image$state
        
        if (is.null(lca))
          return()
        
        par(mfcol = c(2, 1))
        plot1 <- plot(lca, ask=FALSE)
        
        print(plot1)
        
        TRUE
        
      },
      
      
      ### Helper functions =================================     
      
      .cleanData = function() {
        
        vars <- self$options$vars
        groupVarName <- self$options$group
        data <- list()
        
        data[[groupVarName]] <- jmvcore::toNumeric(self$data[[groupVarName]])
        
        for (item in vars)
          data[[item]] <- jmvcore::toNumeric(self$data[[item]])
        
        attr(data, 'row.names') <- seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        
        data <- data[!is.na(data[[groupVarName]]), ]
        
        return(data)
      }
      
      
    )
)
