
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import stats
#' @import glca
#' @import ggplot2
#' @importFrom glca glca
#' @importFrom data.table melt
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
            <p>1. Latent Class Analysis(LCA) based on <b>glca</b> R package.</p>
            <p>2. The rationale of snowLatent module is described in the <a href='https://docs.google.com/viewer?a=v&pid=sites&srcid=a29yZWEuYWMua3J8a3VzdGF0bGFifGd4OjU0Nzc0NjU4OGJkODVjNDk'>documentation</a>.</p>
            <p>3. The result table does not printed if the results from glca R package are not available.</p>
            <p>4. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowLatent/issues'  target = '_blank'>GitHub</a>.</p>
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
          
          
          # logistic table-----------
          
          private$.populateLogTable(results)
          
          
          # populate item probabilities---------
          
          private$.populateItemTable(results)
          
          
          # populate posterior probabilities--
          
          private$.populatePosteriorOutputs(results)
          
          # populate class membership--
          
          private$.populateMemberOutputs(results)
          
          # populate item probabilities---------
          
          private$.populateGamTable(results)
          
        
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

          lca = glca::glca(formula=formula,data=data,
                           nclass=nc, seed = 1)

    #################################################################
        
        self$results$text$setContent(lca)
       
        #fit measure----------
        
        loglik<- lca$gof$loglik
        aic<- lca$gof$aic
        caic<- lca$gof$caic
        bic<- lca$gof$bic
        entropy<- lca$gof$entropy
        df<- lca$gof$df
        gsq<- lca$gof$Gsq
           
        fit <- data.frame(loglik,aic,caic,bic,entropy,df,gsq) 
   
      ######## Marginal prvalences for latent class##############
      
        # # Attention:
        # # Capture if 1 or more covariates have 
        # # not been selected to avoid 0 length error.
        # 
        # if( is.null(self$options$covs) ) {
        #   
        #   gam <- lca[["param"]][["gamma"]]
        #   row.names(gam) <- 1:nrow(gam)  
        #   gam <- as.data.frame(gam)
        #   gam <- t(gam)
        #   gam <- as.data.frame(gam)
        # } else
        #   gam <- NaN
        
        
        gam <- lapply(lca$posterior, colMeans)
        gam<- gam$ALL
        gam <- as.data.frame(gam)
        
        
        # item probabilities------
          
        item<- lca[["param"]][["rho"]][["ALL"]]
       # item<- do.call("rbind", lapply(item, as.data.frame))  
        
        # gamma probability----------
        
        gamma <- lca[["param"]][["gamma"]]
        
        
        # logistic regression coefficients-------------
        
        if( !is.null(self$options$covs) ) {
        
          coef<- coef(lca)
          
        }
        
        
        # populate output results-------------
        
        
        pos<- lca$posterior$ALL
        
        # class membership-------------
        
        
        pos$Membership <- as.numeric(factor(apply(pos, 1, which.max)))
        
         mem <- pos$Membership
        
         # mem<- as.factor(mem)
         
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
          
        # Item by class plot---------
        
        image1 <- self$results$plot2
        
        ic <- data.table::melt(lca$param$rho)
        colnames(ic) <-c("Class", "Factor_level", "value", "L1")
        
        image1$setState(ic)
        
        
        
        #Good codes for model fit####################################
        
        args <- list(test = "boot", nboot=nb)
      
        for(n in 2:self$options$nc)
          args[[n+1]] <- glca::glca(formula = formula, 
                                     data = data, 
                                     nclass = n, 
                                     seed = 1)
        
        res <- do.call(glca::gofglca, args)
        
        gtable <- res[["gtable"]] #Absolute model fit
        
        if(is.null(res$dtable)) {
          dtable <- NULL 
        } else {
          dtable <- res[["dtable"]] # Relative model fit 
        }
        
         
      results <-
              list(
                'fit'=fit,
                'gam'= gam,
                'item'=item,
                'gtable'=gtable,
                'dtable'=dtable,
                'pos'=pos,
               'coef'= coef,
               'mem'=mem,
               'gamma'=gamma
                )
          
            
      },   
      
      
      ################ populating Tables################################
      
   # populating fit table-------------   
   
   .populateFitTable = function(results) {
     
     table <- self$results$fit
     
     fit <- results$fit
     class <- self$options$nc 
     
     row <- list()
     
     row[['class']] <- class
     row[['loglik']] <- fit[,1]
     row[['aic']] <- fit[,2]
     row[['caic']] <- fit[,3]
     row[['bic']] <- fit[,4]
     row[['entropy']] <- fit[,5]
     row[['df']] <- fit[,6]
     row[['gsq']] <- fit[,7]
     
     table$setRow(rowNo = 1, values = row)
     
     
   },
   
   # Model comparison(Absolute model fit)----------------
   
   
      .populateModelTable = function(results) {
       
       
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
   
    if(self$options$nc<3 | is.null(results$dtable))
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
   # Marginal prevalences for latent classes--------------------------------------
          
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
         
      .populatePosteriorOutputs= function(results) {
        
        # nc<- self$options$nc
        # 
        # data <- jmvcore::naOmit(data)
        # 
        # data<- as.data.frame(data)
        # 
        # vars <- colnames(data)
        # vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
        # vars <- paste0(vars, collapse=',')
        # 
        # formula <- as.formula(paste0('glca::item(', vars, ')~1'))
        # 
        # 
        # ################ LCA model estimates############################ 
        # 
        # lca = glca::glca(formula, data = data, nclass = nc, n.init=1)
        # ###############################################################
        # 
        # pos<- lca$posterior$ALL
        
        pos <- results$pos
        
        
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
      
  .populateMemberOutputs= function(results) {
    
    
    mem <- results$mem
   
    mem<- as.factor(mem)
    
     if (self$options$member
        && self$results$member$isNotFilled()) {
    
    
    self$results$member$setValues(mem)
    
    self$results$member$setRowNums(rownames(data))
    
    }
  },
  
  # populate multinomial logistic regression-----
  
  .populateLogTable= function(results) {
  
    
    if(is.null(self$options$covs))
      return()
  
    table <- self$results$coef
    
    coef <- results$coef
    coef<- coef[[1]]
    codf<- do.call("rbind", lapply(coef, as.data.frame))
    
   
    names<- dimnames(codf)[[1]]
    
    for (name in names) {
      
      row <- list()
      
      row[["odds"]]   <-  codf[name, 1]
      row[["co"]] <-  codf[name, 2]
      row[["error"]] <-  codf[name, 3]
      row[["t"]] <-  codf[name, 4]
      row[["p"]] <-  codf[name, 5]
    
      table$addRow(rowKey=name, values=row)
      
    }     
    
  },
  
        # item probabilities----------
      
      .populateItemTable= function(results) {
        
       
        tables <- self$results$item
        
        vars <- self$options$vars
        
        for(i in seq_along(vars)){
          
          item <- results$item[[ vars[i] ]]
          
         
          table <- tables[[i]]
          
          names<- row.names(item)
          dims <- colnames(item)
          
          #item<- as.data.frame(item)
          #names<- dimnames(item)[[1]]
          #dims <- dimnames(item)[[2]]
          
         
           for (dim in dims) {

             table$addColumn(name = paste0(dim),
                             type = 'text',
                             combineBelow=TRUE)
           }

          
          for (name in names) {
            
            row <- list()
            
            for(j in seq_along(dims)){
              
              row[[dims[j]]] <- item[name,j]
              
            }
            
            table$addRow(rowKey=name, values=row)
            
          }
          
        }
        
        },   

  # gamma probabilities----------
  
  .populateGamTable= function(results) {
    
    if (!self$options$gamma)
      return()
    
    res <- results$gamma
    
    options(max.print = 1000000)
    
    self$results$text2$setContent(res)
    
  }, 
  
  # plot-------------------------------------
  
   .plot1 = function(image, ...) {
     
     lca <- image$state
     
     if (is.null(lca))
       return()
      
     par(mfcol = c(2, 1))
      plot1 <- plot(lca, ask=FALSE)
      
      print(plot1)
      
      TRUE
      
      },
      
 # item by class plot-----------
 
 .plot2 = function(image1, ggtheme, theme, ...) {    
   
   ic <- image1$state
   
   plot2 <- ggplot2::ggplot(ic, ggplot2::aes(x = Class, y = value, fill = Factor_level)) + 
     ggplot2::geom_bar(stat = "identity", position = "stack")+ 
     ggplot2::facet_wrap(~ L1)+
     ggplot2::scale_x_discrete("Class", expand = c(0, 0)) +
     ggplot2::scale_y_continuous("Proportion", expand = c(0, 0)) +
     ggplot2::theme_bw()
   
   
   plot2 <- plot2+ggtheme
   print(plot2)
   TRUE
  
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

  
