
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import stats
#' @import glca
#' @importFrom glca glca
#' @importFrom glca item
#' @importFrom glca gofglca
#' @importFrom stringr str_interp
#' @export
#' 

mlcaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mlcaClass",
    inherit = mlcaBase,
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
            <p>2. When group and cluster(>1) are given, the multilevel latent class models will be fitted.</p>
            <p>3. The rationale of snowLatent module is described in the <a href='https://docs.google.com/viewer?a=v&pid=sites&srcid=a29yZWEuYWMua3J8a3VzdGF0bGFifGd4OjU0Nzc0NjU4OGJkODVjNDk'>documentation</a>.</p>
            <p>4. The result table does not printed if the results from glca R package are not available.</p>
            <p>5. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowLatent/issues'  target = '_blank'>GitHub</a>.</p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
        if (self$options$comp1)
          self$results$comp1$setNote(
            "Note",
            "p: Bootstrap p value; H0: Model fit data adequately."
          )
        
        if (self$options$gof)
          self$results$gof$setNote(
            "Note",
            "Model1: Coeff.inv=TRUE; Model2: Coeff.inv=FALSE."
          )
        
        
        if (self$options$ci)
          self$results$ci$setNote(
            "Note",
            "Model1: Coeff.inv=TRUE; Model2: Coeff.inv=FALSE."
          )
        
        
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
        
        
      },
      
      .run = function() {
        
        ready <- TRUE
        
        if (is.null(self$options$group) || is.null(self$options$vars) || 
            length(self$options$vars) < 2)
          
          ready <- FALSE
        
        if (ready) {
          
          data <- private$.cleanData()
          
          results <- private$.compute(data)               
          
          # populate Fit table----------
          
          private$.populateFitTable(results)
          
         
          # Absolute model fit for cluster-----------
          
          private$.populateModel1Table(results)
          
          # populate Relative model fit for cluster---------------
          
          private$.populateRel1Table(results)
          
          
          # Marginal prevalences for latent clusters------
          
          private$.populateMarginTable(results)
          
          # Marginal prevalences for latent classes----
          
          
          private$.populateClaTable(results)
          
          #Class prevalences by cluster------------
          
          private$.populateCrossTable(results)
          
          # populate  gof for coeff ---------------
          
       #   private$.populateGofTable(results)
          
          # populate equality coefficients ---------------
          
        #   private$.populateCiTable(results)
          
        
          # populate item probabilities---------
          
          private$.populateItemTable(results)
          
         
          # populate posterior probabilities--
          
          private$.populatePosTable(results)
          
          # populate cluster membership--
          
          private$.populateMemTable(results)
          
           #logistic table-----------
          
            private$.populateLogTable(results)
          
          # cluster prob.(gamma)----------
          
          private$.populateGamTable(results)
          
          
        }
      },
      
      
      .compute = function(data) {
        
        if (is.null(self$options$group))
          return()
        
        
        ########  Multilevel glca R package ################
        
        # library(glca)
        # mlca = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ SEX,
        #             group = SCH_ID, data = nyts18, 
        #             nclass = 3, ncluster = 2, seed=1)
        # 
        ################################
        
        # Constructing formula----------------        
        
        vars <- self$options$vars
        #vars <- colnames(data[, -1] )
        
        vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
        vars <- paste0(vars, collapse=',')
        
        # formula with no covariates----------     
        
        formula <- as.formula(paste0('glca::item(', vars, ')~1'))
       
        # With covariates-------------
        
        if( !is.null(self$options$covs) ) {
          
          covs <- self$options$covs
          covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
          covs <- paste0(covs, collapse='+')
          
          formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
        }
       
        group <- data[, 1]
        
        nc <- self$options$nc
        nb <- self$options$nb
        nclust<- self$options$nclust 
       
        ################### LCA model estimates############################
        
        lca = glca::glca(formula=formula,
                         group= group,
                         data=data,
                         nclass = nc, 
                         ncluster = nclust,
                         seed = 1)
        
        #################################################################
        
         self$results$text$setContent(lca)
       
        # Model fit measure----------
        
        loglik<- lca$gof$loglik
        aic<- lca$gof$aic
        caic<- lca$gof$caic
        bic<- lca$gof$bic
        entropy<- lca$gof$entropy
        df<- lca$gof$df
        gsq<- lca$gof$Gsq
        
        fit <- data.frame(loglik,aic,caic,bic,entropy,df,gsq)
        
       
        # CLUSTER: Absolute and relative model fit-------
        
        args <- list(test = "boot", nboot=nb)

        for(n in 2:self$options$nclust)

          args[[n+1]] <- glca::glca(formula = formula,
                                       group=group,
                                       data = data,
                                       nclass = nc,
                                       ncluster = n,
                                       seed = 1)

        res <- do.call(glca::gofglca, args)

        #self$results$text$setContent(res)

        
        gtable1 <- res[["gtable"]] #Absolute model fit

        if(is.null(res$gtable)) {
          gtable1 <- NULL
        } else {
          gtable1 <- res[["gtable"]]   
        }
        
        
        if(is.null(res$dtable)) {
          dtable1 <- NULL
        } else {
          dtable1 <- res[["dtable"]]   # Relative model fit
        }


####  Invariance of equality  ########################################################       
        
           lca2 <- try(glca::glca(formula = formula, 
                             group = group, 
                             data = data, 
                             nclass = nc, 
                             ncluster = nclust,
                             coeff.inv = FALSE,
                             seed= 1)
           )
        
        if(jmvcore::isError(lca2)){
         
          err_string <- stringr::str_interp(
            " Error in if (maxdiff < eps) break : Covariates can not be computed."
          )
          stop(err_string)
          
          } 
        
        if (! jmvcore::isError(lca2) ){
          
          cf <- glca::gofglca(lca, lca2, test = "chisq", nboot = nb)
          
         # self$results$text$setContent(cf)
          
          ci.g <- cf[["gtable"]]
          ci.d <- cf[["dtable"]]
         
        } 
         
   ######## Goodness of fit table----------  
          
          if(isTRUE(self$options$gof)){
         
          table <- self$results$gof  
  
          if(is.null(ci.g))
            return()
          
          g<- as.data.frame(ci.g)
          
          loglik <- g[,1]
          aic <- g[,2]
          caic <- g[,3]
          bic <- g[,4]
          entropy <- g[,5]
          df <- g[,6]
          gsq <- g[,7]
          
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
            
            table$addRow(rowKey=name, values=row)
            
          }
          
          }
          
          
  ############ Testing equality of coefficients-------------
          
          if(isTRUE(self$options$ci)){
         
            table <- self$results$ci
            
            if(is.null(ci.d))
              return()
           
          d<- as.data.frame(ci.d)
          
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
        
          
          }
          
################################################################
          
        # Marginal prevalences for latent cluster------
        
        clust <- lca[["param"]][["delta"]]
        clust<- as.data.frame(clust)
        
        # Marginal prevalences for latent classes ----------
        # cla <- colMeans(do.call(rbind, lca[["posterior"]][["class"]]))
        # cla<- as.data.frame(cla)

        if( is.null(lca[["posterior"]][["class"]]) ) {
          cla <- colMeans(do.call(rbind, lca[["posterior"]]))
        } else {
          cla <- colMeans(do.call(rbind, lca[["posterior"]][["class"]]))
        }

        cla <- as.data.frame(cla)

        # Class prevalences by cluster-----------
        #cross<- lca[["posterior"]][["wclass"]]
        
        if( is.null(lca[["posterior"]][["wclass"]]) ) {
          cross <- NULL 
        } else {
          cross <- lca[["posterior"]][["wclass"]]
        }
        
        # item response probability---------
        
        item<- lca[["param"]][["rho"]]
        
        #item<- do.call("rbind", lapply(item, as.data.frame))
        
        
        # posterior probability---------
        
        post <-lca[["posterior"]][["class"]]
        
        # Cluster membership------------
        
        member<- lca[["posterior"]][["cluster"]]
        
        # logistic regression coef.--------
        
        co <- lca[["coefficient"]]
        
        
        # cluster prob.(gamma)----------
        
        gamma <- lca[["param"]][["gamma"]]
        
        
        # Class Prevalences plot----------
        
        image <- self$results$plot1
        
        vars <- length(self$options$vars) 
        
        width <- 100 + vars * 100
        
        image$setSize(width, 700)
        
        
        #image$setSize(100 + 100 * length(self$options$vars), 200)
        
        image$setState(lca)
        
       ##############################
        #example with args
        
        # # --- Class prevalences (Marginal prevalences for latent classes) --- #
        # args <- list()
        # args[[1]] <- lca[["posterior"]][["1"]]
        # args[[2]] <- lca[["posterior"]][["2"]]
        # 
        # cla <- colMeans(do.call(rbind, args))
        # cla <- as.data.frame(cla)
        
        ###################################
        
        
        results <-
          list(
           
            'fit'=fit,
            'cla'=cla,
            'cross'=cross,
            'item'=item,
            'post'=post,
            'gtable1'=gtable1,
            'dtable1'=dtable1,
            'clust'=clust,
            'member'=member,
            'co'=co,
           'gamma'=gamma
           
          )
        
        
      },   
      
      
      ################ populating Tables################################
     
        # Model Fit table-------------   
      
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
      
       
      # Absolute model fit for cluster------------
      
      .populateModel1Table = function(results) {
        
        if (!self$options$comp1)
             return()
        
        table <- self$results$comp1
        
        gtable1 <- results$gtable1
        
        if(is.null(gtable1))
        return()
        
        g<- as.data.frame(gtable1)
       
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
      
      # Relative model fit for cluster---------------
      
      .populateRel1Table = function(results) {
        
        if (!self$options$rel1)
          return()
        
        table <- self$results$rel1
        
        dtable1 <- results$dtable1
        
        if (is.null(dtable1))
          return()
        
        d<- as.data.frame(dtable1)
        
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
      
      
      # Marginal prevalences for latent cluster---------
      
      .populateMarginTable= function(results) {  
        
        if (!self$options$margin)
          return()
        
        table <- self$results$margin
        
        clust<- results$clust
      
      
        names<- dimnames(clust)[[1]]
        
        
        for (name in names) {
          
          row <- list()
          
          row[['value']] <- clust[name,1]
          
          table$addRow(rowKey=name, values=row)
          
        }
        
      },
      
      # Marginal prevalences for latent classes----------
      
      .populateClaTable= function(results){
        
        if (!self$options$cla)
          return()
        
        table <- self$results$cla
        
        cla<- results$cla
        
        names<- dimnames(cla)[[1]]
        
        for (name in names) {
          
          row <- list()
          
          row[['value']] <- cla[name,1]
          
          table$addRow(rowKey=name, values=row)
        
        
      }
      
      },
      
      # Class prevalences by cluster----------------------
      
      .populateCrossTable=function(results){
        
        if (!self$options$cross)
          return()
        
        
        table <- self$results$cross
        
        cross<- results$cross
        
        names<- dimnames(cross)[[1]]
        dims <- dimnames(cross)[[2]]
        
        
        for (dim in dims) {
          
          table$addColumn(name = paste0(dim),
                          type = 'character')
        }
        
        
        for (name in names) {
          
          row <- list()
          
          for(j in seq_along(dims)){
            
            row[[dims[j]]] <- cross[name,j]
            
          }
          
          table$addRow(rowKey=name, values=row)
          
        }
        
        
      },
      
      # cluster membership-----------
      
      .populateMemTable= function(results) {
      
        if (!self$options$member)
          return()
        
        member <- results$member
        
        m<- as.data.frame(member)
       #------------------------------------- 
        
        m$Membership <- as.numeric(factor(apply(m, 1, which.max)))
        
        #---------------------------------
        table <- self$results$member
        
        names<- dimnames(m)[[1]]
        
        dims <- dimnames(m)[[2]]
        
        for (dim in dims) {
          
          table$addColumn(name = paste0(dim),
                          type = 'number')
        }
        
        
        for (name in names) {
          
          row <- list()
          
          for(j in seq_along(dims)){
            
            row[[dims[j]]] <- m[name,j]
            
          }
          
          table$addRow(rowKey=name, values=row)
          
        }
        
        
      },
       
      # item probabilities----------
      
      .populateItemTable= function(results) {
        
        if (!self$options$item)
          return()
        
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
      

# posterior probability----------
      
      .populatePosTable= function(results) {
        
        if(!self$options$post)
          return()
        
        post <- results$post
        
        options(max.print = 1000000)
        
        self$results$text2$setContent(post)
        
      },
      
      # logistic table----------

      .populateLogTable=function(results){

        if(!self$options$co)
          return()

        co <- results$co

        self$results$text3$setContent(co)


      },

      # Gamma probability---------
      
      .populateGamTable=function(results){
        
        if(!self$options$gamma)
          return()
        
        gamma <- results$gamma
        
        options(max.print = 1000000)
        
        self$results$text4$setContent(gamma)
        
        
      },
      
      
      ######## plot#######################
      .plot1 = function(image, ...) {
        
        lca <- image$state
        
        if (is.null(lca))
          return()
        
        par(mfcol = c(3, 1))
        plot1 <- plot(lca, ask=FALSE)
        
        print(plot1)
        
        TRUE
        
      },
      
# # GOF for coefficients----------------------
# 
# .populateGofTable = function(results) {
#   
#   if (!self$options$gof)
#     return()
#   
#   table <- self$results$gof  
#   
#   ci.g <- results$ci.g
#   
#   if(is.null(ci.g))
#     return()   
#   
#   g<- as.data.frame(ci.g)
#   
#   loglik <- g[,1]
#   aic <- g[,2]
#   caic <- g[,3]
#   bic <- g[,4]
#   entropy <- g[,5]
#   df <- g[,6]
#   gsq <- g[,7]
#   
#   names <- dimnames(g)[[1]]
#   
#   
#   for (name in names) {
#     
#     row <- list()
#     
#     row[["loglik"]]   <-  g[name, 1]
#     row[["aic"]] <-  g[name, 2]
#     row[["caic"]] <-  g[name, 3]
#     row[["bic"]] <-  g[name, 4]
#     row[["entropy"]] <-  g[name, 5]
#     row[["df"]] <-  g[name, 6]
#     row[["gsq"]] <-  g[name, 7]
#     
#     table$addRow(rowKey=name, values=row)
#     
#   }
#   
# },
# 

# Equality of coefficients table--------

# .populateCiTable = function(results) {
#   
#   if (!self$options$ci)
#     return()
#   
#   table <- self$results$ci
#   ci.d <- results$ci.d
#   
#   if(is.null(ci.d))
#     return()
#   
#   d<- as.data.frame(ci.d)
#   
#   para <- d[,1]
#   loglik<- d[,2]
#   df<- d[,3]
#   dev<- d[,4]
#   p<- d[,5]
#   
#   names <- dimnames(d)[[1]]
#   
#   
#   for (name in names) {
#     
#     row <- list()
#     
#     row[["para"]] <-  d[name, 1]
#     row[["loglik"]] <-  d[name, 2]
#     row[["df"]] <-  d[name, 3]
#     row[["dev"]] <-  d[name, 4]
#     row[["p"]] <-d[name, 5]
#     
#     table$addRow(rowKey=name, values=row)
#     
#   }
#   
# },

      ########################################################
      .cleanData = function() {
        
        vars <- self$options$vars
        covs <- self$options$covs
        
        groupVarName <- self$options$group
        data <- list()
        
        data[[groupVarName]] <- jmvcore::toNumeric(self$data[[groupVarName]])
        
        for (item in vars)
          data[[item]] <- jmvcore::toNumeric(self$data[[item]])
        for (item in covs)
          data[[item]] <- jmvcore::toNumeric(self$data[[item]])
        
        attr(data, 'row.names') <- seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        
        data <- data[!is.na(data[[groupVarName]]), ]
        
        return(data)
      }
      
    )
)
