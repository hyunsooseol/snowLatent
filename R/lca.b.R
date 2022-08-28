
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
          
          
          # populate Model comparison(Absolute model fit)-----------
          
          private$.populateModelTable(results)
          
          # populate Relative model fit---------------
          
          
          private$.populateRelTable(results)
          
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
        
        nc <- self$options$nc
        nb <- self$options$nb
       
      ############ Construct formula###################        
          
        vars <- self$options$vars
        vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
        vars <- paste0(vars, collapse=',')
        formula <- as.formula(paste0('glca::item(', vars, ') ~ 1'))
        
        if( !is.null(self$options$covs) ) {
          covs <- self$options$covs
          covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
          covs <- paste0(covs, collapse='+')
          
          formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
        } 
        
   ################### LCA model estimates############################

          lca = glca::glca(formula=formula,data=data,nclass=nc)

    #################################################################
     
        
      
        #fit measure----------
        
        loglik<- lca$gof$loglik
        aic<- lca$gof$aic
        caic<- lca$gof$caic
        bic<- lca$gof$bic
        entropy<- lca$gof$entropy
        df<- lca$gof$df
        gsq<- lca$gof$Gsq
            
   
      ######## LCA with no covariates##############
      
        # Attention:
        # Capture if 1 or more covariates have 
        # not been selected to avoid 0 length error.
        
        if( is.null(self$options$covs) ) {
          
          gam <- lca[["param"]][["gamma"]]
          row.names(gam) <- 1:nrow(gam)  
          gam <- as.data.frame(gam)
          gam <- t(gam)
          gam <- as.data.frame(gam)
        } else
          gam <- NaN
        
       
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
          
        
        #Good codes for model fit####################################
        # 
        # library(glca)
        # data("gss08")
        # data <- gss08
        # args <- list(test = "boot", seed = 1)
        # f <- item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1
        # inpclas = 4
        # for(nc in 2:inpclas)
        #   args[[nc+1]] <- glca::glca(formula = f, data = data, nclass = nc, seed = 1)
        # res <- do.call(glca::gofglca, args)
        # res
        
        args <- list(test = "boot", nboot=nb)
        inpclas = self$options$nc
        
        for(nc in 2:inpclas)
          args[[nc+1]] <- glca::glca(formula = formula, data = data, nclass = nc)
        res <- do.call(glca::gofglca, args)
        
        
        gtable <- res[["gtable"]] #Absolute model fit
        
        dtable<- res[["dtable"]]  # Relative model fit 
        
        
        #Goodness of fit----------------------------
        
        # if(self$options$nc ==2){
        #   
        #   res <- glca::gofglca(glca::glca(formula, data = data, nclass = 2),
        #                         test='boot', nboot = nb)
        #   
        # } else if(self$options$nc ==3){
        # 
        # res <- glca::gofglca(glca::glca(formula, data = data, nclass = 2),
        #                      glca::glca(formula, data = data, nclass = 3),
        #                      test='boot', nboot = nb)
        # 
        # 
        # } else if(self$options$nc ==4){
        #   
        #   
        #   res <- glca::gofglca(glca::glca(formula, data = data, nclass = 2),
        #                        glca::glca(formula, data = data, nclass = 3),
        #                        glca::glca(formula, data = data, nclass = 4),
        #                        test='boot', nboot = nb) 
        #   
        #   
        # } else if(self$options$nc ==5){
        #   
        #   
        #   res <- glca::gofglca(glca::glca(formula, data = data, nclass = 2),
        #                        glca::glca(formula, data = data, nclass = 3),
        #                        glca::glca(formula, data = data, nclass = 4),
        #                        glca::glca(formula, data = data, nclass = 5),
        #                        test='boot', nboot = nb) 
        #   
        #   
        # } else if(self$options$nc ==6){
        #   
        #   
        #   res <- glca::gofglca(glca::glca(formula, data = data, nclass = 2),
        #                        glca::glca(formula, data = data, nclass = 3),
        #                        glca::glca(formula, data = data, nclass = 4),
        #                        glca::glca(formula, data = data, nclass = 5),
        #                        glca::glca(formula, data = data, nclass = 6),
        #                        test='boot', nboot = nb) 
        #   
        #   
        # } else if(self$options$nc ==7){
        #   
        #   
        #   res <- glca::gofglca(glca::glca(formula, data = data, nclass = 2),
        #                        glca::glca(formula, data = data, nclass = 3),
        #                        glca::glca(formula, data = data, nclass = 4),
        #                        glca::glca(formula, data = data, nclass = 5),
        #                        glca::glca(formula, data = data, nclass = 6),
        #                        glca::glca(formula, data = data, nclass = 7),
        #                        test='boot', nboot = nb) 
        #   
        #   
        # } else if(self$options$nc ==8){
        #   
        #   
        #   res <- glca::gofglca(glca::glca(formula, data = data, nclass = 2),
        #                        glca::glca(formula, data = data, nclass = 3),
        #                        glca::glca(formula, data = data, nclass = 4),
        #                        glca::glca(formula, data = data, nclass = 5),
        #                        glca::glca(formula, data = data, nclass = 6),
        #                        glca::glca(formula, data = data, nclass = 7),
        #                        glca::glca(formula, data = data, nclass = 8),
        #                        test='boot', nboot = nb) 
        #   
        #   
        # } else if(self$options$nc ==9){
        #   
        #   
        #   res <- glca::gofglca(glca::glca(formula, data = data, nclass = 2),
        #                        glca::glca(formula, data = data, nclass = 3),
        #                        glca::glca(formula, data = data, nclass = 4),
        #                        glca::glca(formula, data = data, nclass = 5),
        #                        glca::glca(formula, data = data, nclass = 6),
        #                        glca::glca(formula, data = data, nclass = 7),
        #                        glca::glca(formula, data = data, nclass = 8),
        #                        glca::glca(formula, data = data, nclass = 9),
        #                        test='boot', nboot = nb) 
        #   
        #   
        # } else if(self$options$nc ==10){
        #   
        #   
        #   res <- glca::gofglca(glca::glca(formula, data = data, nclass = 2),
        #                        glca::glca(formula, data = data, nclass = 3),
        #                        glca::glca(formula, data = data, nclass = 4),
        #                        glca::glca(formula, data = data, nclass = 5),
        #                        glca::glca(formula, data = data, nclass = 6),
        #                        glca::glca(formula, data = data, nclass = 7),
        #                        glca::glca(formula, data = data, nclass = 8),
        #                        glca::glca(formula, data = data, nclass = 9),
        #                        glca::glca(formula, data = data, nclass = 10),
        #                        test='boot', nboot = nb) 
        #   
        #   
        # }
        # 
        # gtable <- res[["gtable"]] #Absolute model fit
        # 
        # dtable<- res[["dtable"]]  # Relative model fit                 
        # 
         
            results <-
              list(
                'loglik'=loglik,
                'aic'=aic,
                'caic'=caic,
                'bic'=bic,
                'entropy'=entropy,
                'df'=df,
                'gsq'=gsq,
                'gam'= gam,
                'item'=item,
                'gtable'=gtable,
                'dtable'=dtable
                
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
   
   # Model comparison(Absolute model fit)----------------
   
   
      .populateModelTable = function(results) {
            
        
        nc <- self$options$nc
         
        table <- self$results$comp
         
        
         gtable <- results$gtable
         
         g<- as.data.frame(gtable)
         
         loglik <- g[,1]
         aic <- g[,2]
         caic <- g[,3]
         bic <- g[,4] 
         entropy <- g[,5]
         df <- g[,6] 
         gsq <- g[,7] 
         p <- g[,8]
         
       
         names <- dimnames(g)[[1]]
         
       
         for (name in names) {
           
           row <- list()
           
           row[["loglik"]]   <-  g[name, 1]
           row[["aic"]] <-  g[name, 2]
           row[["caic"]] <-  g[name, 3]
           row[["bic"]] <-  g[name, 4]
           row[["entropy"]] <-  g[name, 5]
           row[["df"]] <-  g[name, 6]
           row[["gsq"]] <-  g[name, 7]
           row[["p"]] <-  g[name, 8]
           
           
           table$addRow(rowKey=name, values=row)
           
         }     
         
      },
  
  # Populate relative model fit---------------
  
  .populateRelTable = function(results) {
   
   if(self$options$nc<3)
     return()
    
    nc <- self$options$nc
    
    table <- self$results$rel
    
    
    dtable <- results$dtable
    
    d<- as.data.frame(dtable)
    
   para <- d[,1]
   loglik<- d[,2]
   df<- d[,3]
   dev<- d[,4]
   p<- d[,5]
   
   names <- dimnames(d)[[1]]
   
   
   for (name in names) {
     
     row <- list()
     
     row[["para"]] <-  d[name, 1]
     row[["loglik"]] <-  d[name, 2]
     row[["df"]] <-  d[name, 3]
     row[["dev"]] <-  d[name, 4]
     row[["p"]] <-d[name, 5]
   
     table$addRow(rowKey=name, values=row)
     
   }
    
  },
   # size of Class--------------------------------------
          
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

  
