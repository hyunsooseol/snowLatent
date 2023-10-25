
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import stats
#' @import glca
#' @import ggplot2
#' @importFrom glca glca
#' @importFrom reshape2 melt
#' @importFrom glca item
#' @importFrom ggplot2 ggplot
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
           
            <h2><b>Instructions</b></h2>
            <p>_____________________________________________________________________________________________</p>
            <p>1. Latent Class Analysis(LCA) based on <b>glca</b> R package.</p>
            <p>2. The result table does not printed if the results from glca R package are not available.</p>
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
        
        
        if(isTRUE(self$options$plot1)){
          
          width <- self$options$width
        #  width <- max(min(width, 800),200)
          height <- self$options$height
         # height <- max(min(height, 600),150)
          self$results$plot1$setSize(width, height)
        }
        
        
        # if(isTRUE(self$options$plot3)){
        #   
        #   width <- self$options$width
        #   width <- max(min(width, 800),200)
        #   height <- self$options$height
        #   height <- max(min(height, 600),150)
        #   self$results$plot3$setSize(width, height)
        # }
        
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
          #private$.populateLogTable(results)
          
          
          # populate item probabilities---------
          
          private$.populateItemTable(results)
          
          
          # populate posterior probabilities--
          
         # private$.populatePosteriorOutputs(results)
          
          # populate class membership--
          
        #  private$.populateMemberOutputs(results)
          
          # populate item probabilities---------
          
       #   private$.populateGamTable(results)
          
        
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
        
        #if( !is.null(self$options$covs) ) {
        if(length(self$options$covs)>=1){
          
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
        
      
        # logistic regression coefficients-------------
        
        if(length(self$options$covs)>=1){
          #if( !is.null(self$options$covs) ) {
          #coef<- coef(lca)
          
          table <- self$results$coef
          
          #coef <- results$coef
          coef<- coef(lca)
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
          
        }
        
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
     #   image$setSize(100 + 100 * length(self$options$vars), 700)  
         
        image$setState(lca)
          
        # Item by class plot---------
        
        image1 <- self$results$plot2
        
        ic <- reshape2::melt(lca$param$rho)
        colnames(ic) <-c("Class", "Level", "value", "L1")
        
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
        
          # new <- c(2:self$options$nc)
          #add column called new
          #dtable <- cbind(dtable, new)
         
          }
        
       
        if(self$options$nc>2){
        
        # Elbow plot-------------
        
        out1 <- gtable[,c(2:4)]
        
        
        cla <- c(2:self$options$nc)
        
        out1 <- data.frame(out1,cla)
        
        # self$results$text1$setContent(out1)
        
        colnames(out1) <- c('AIC',
                            'CAIC','BIC','Class')
        
        elbow <- reshape2::melt(out1,
                                id.vars='Class',
                                variable.name="Fit",
                                value.name='Value')
        
        image2 <- self$results$plot3
        image2$setState(elbow )
        
        }
        
        
        # adding class--------
        
        gtable<- as.data.frame(gtable)
        dtable<- as.data.frame(dtable)
        
        #define new column to add
        new <- c(2:self$options$nc)
       
        #add column called new
        gtable <- cbind(gtable, new)
      #  dtable <- cbind(dtable, new1)
        
        if(!is.null(res$dtable)){
        
        new <- c(2:self$options$nc)
        #add column called new
        dtable <- cbind(dtable, new)
        }
        
        # populate output results-------------
        pos<- lca$posterior$ALL
        
        # class membership-------------
        mem <- as.numeric(factor(apply(pos, 1, which.max)))
        mem<- as.factor(mem)
        
        if(isTRUE(self$options$post)){
          
          # pos <- self$results$post
          
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
        
        if(isTRUE(self$options$member)){
          
          
          if (self$options$member
              && self$results$member$isNotFilled()) {
            
            
            self$results$member$setValues(mem)
            
            self$results$member$setRowNums(rownames(data))
            
          }
          
        }
        
        
        if(isTRUE(self$options$gamma)){
          # gamma probability----------
          gamma <- lca[["param"]][["gamma"]]
          
          options(max.print = 1000000)
          self$results$text2$setContent(gamma)
        }
        #self$results$text$setContent(dtable)
        
        results <-
              list(
                'fit'=fit,
                'gam'= gam,
                'item'=item,
                'gtable'=gtable,
                'dtable'=dtable
              #  'pos'=pos,
              # 'coef'= coef,
              # 'mem'=mem,
              # 'gamma'=gamma
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
         
         # loglik <- g[,1]
         # aic <- g[,2]
         # caic <- g[,3]
         # bic <- g[,4]
         # entropy <- g[,5]
         # df <- g[,6]
         # gsq <- g[,7]
         # p <- g[,8]
         # class <- g[,9]

         names <- dimnames(g)[[1]]
         
       
         for (name in names) {
           
           row <- list()
           
           row[["class"]]   <-  g[name, 9]
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
    
    #new <- c(2:self$options$nc)
    #add column called new
    #dtable <- cbind(dtable, new)
    
    d<- as.data.frame(dtable)
    
    
   # para <- d[,1]
   # loglik<- d[,2]
   # df<- d[,3]
   # dev<- d[,4]
   # p<- d[,5]
   
   names <- dimnames(d)[[1]]
   
   
   for (name in names) {
     
     row <- list()
     
     row[["class"]] <-  d[name, 6]
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
         
      # .populatePosteriorOutputs= function(results) {
      #   
      #   if(!self$options$post) return()
      #   
      #   pos <- results$pos
      #   
      #   if (self$options$post
      #       && self$results$post$isNotFilled()) {
      #    
      #     keys <- 1:self$options$nc
      #     measureTypes <- rep("continuous", self$options$nc)
      #     
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
      #       scores <- as.numeric(pos[, i])
      #       self$results$post$setValues(index=i, scores)
      #     }
      #     
      #     
      #   }
      # },
      # 
  # .populateMemberOutputs= function(results) {
  #   
  #   if(!self$options$member) return()
  #   
  #   mem <- results$mem
  #  
  #   mem<- as.factor(mem)
  #   
  #    if (self$options$member
  #       && self$results$member$isNotFilled()) {
  #   
  #   
  #   self$results$member$setValues(mem)
  #   
  #   self$results$member$setRowNums(rownames(data))
  #   
  #   }
  # },
  # 
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
  
  # .populateGamTable= function(results) {
  #   
  #   if (!self$options$gamma)
  #     return()
  #   
  #   res <- results$gamma
  #   
  #   options(max.print = 1000000)
  #   
  #   self$results$text2$setContent(res)
  #   
  # }, 
  
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
   
   plot2 <- ggplot2::ggplot(ic, ggplot2::aes(x = Class, y = value, fill = Level)) + 
     ggplot2::geom_bar(stat = "identity", position = "stack")+ 
     ggplot2::facet_wrap(~ L1)+
     ggplot2::scale_x_discrete("Class", expand = c(0, 0)) +
     ggplot2::scale_y_continuous("Probability", expand = c(0, 0)) +
     ggplot2::theme_bw()
   
   plot2 <- plot2+ggtheme
   
   if (self$options$angle > 0) {
     plot2 <- plot2 + ggplot2::theme(
       axis.text.x = ggplot2::element_text(
         angle = self$options$angle, hjust = 1
       )
     )
   }
   
   print(plot2)
   TRUE
  
 },
     
 
 .plot3 = function(image2, ggtheme, theme,...) {
   
   if(self$options$nc <3)
     return()
   
   elbow <- image2$state
   
   
   # plot3 <- ggplot2::ggplot(elbow,ggplot2::aes(x = Class, y = Value, group = Fit))+
   #   ggplot2::geom_line(size=1.1,ggplot2::aes(color=Fit))+
   #   ggplot2::geom_point(size=3,ggplot2::aes(color=Fit))
   #   ggplot2::scale_x_continuous(breaks = seq(1, length(elbow$Class), by = 1))
   
   plot3 <- ggplot2::ggplot(elbow, ggplot2::aes(x = Class, y = Value, color = Fit)) +
     ggplot2::geom_line(size = 1.1) +
     ggplot2::geom_point(size = 3) +
     ggplot2::scale_x_continuous(breaks = seq(1, length(elbow$Class), by = 1))
   
   
   
   plot3 <- plot3+ggtheme
   
   
   print(plot3)
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

  
