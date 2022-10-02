
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
        
        if (self$options$mi)
          self$results$mi$setNote(
            "Note",
            "Model1: Invariance is TRUE; Model2: Invariance is FALSE."
          )
        
        if (self$options$ci)
          self$results$ci$setNote(
            "Note",
            "Model1: Equality is TRUE; Model2: Equality is FALSE."
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
          
          # Absolute model fit for class-----------
          
          private$.populateModelTable(results)
          
          # populate Relative model fit for class---------------
          
          private$.populateRelTable(results)
          
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
          
          # populate measurement invariance---------------
          
          private$.populateMiTable(results)
          
          # populate coefficients invariance---------------
          
          private$.populateCiTable(results)
          
        
          # populate item probabilities---------
          
          private$.populateItemTable(results)
          
          
          # cluster probabilities--------
          
         # private$.populateClpTable(results)
          
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
        # Multilevel LCA (MLCA)
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
       
        # fit measure----------
        
        loglik<- lca$gof$loglik
        aic<- lca$gof$aic
        caic<- lca$gof$caic
        bic<- lca$gof$bic
        entropy<- lca$gof$entropy
        df<- lca$gof$df
        gsq<- lca$gof$Gsq
        
        #self$results$text$setContent(loglik)
        
        
        # Goodnes of fit for class-------------------------
        
        args <- list(test = "boot", nboot=nb)
        
        inpclas = self$options$nc
        
        for(nc in 2:inpclas)
          
          args[[nc+1]] <- glca::glca(formula = formula, 
                                     group=group, 
                                     data = data, 
                                     nclass = nc,
                                     ncluster = nclust,
                                     seed = 1)
        
        res <- do.call(glca::gofglca, args)
        
        gtable <- res[["gtable"]] #Absolute model fit
        
        dtable<- res[["dtable"]]  # Relative model fit 
        
        if(is.null(res$dtable)){
          
          dtable<- NULL 
          
          # res<- gofglca(lca2, lca3, lca4, test = "boot", seed = 1)
          # Warning message:
          #   In gofglca(lca2, lca3, lca4, test = "boot", seed = 1) :
          #   Since responses are different, deviance table does not printed.
          # 
        }
        
        # Goodness of fit for cluster(Selecting optimal cluster)-------
        
        
        args <- list(test = "boot", nboot=nb)
        
        nclu = self$options$nclust
        
        for(nclu in 2:nclu)
          
          args[[nclu+1]] <- glca::glca(formula = formula, 
                                       group=group, 
                                       data = data, 
                                       nclass = nc,
                                       ncluster=nclu,
                                       seed = 1)
        
        res1 <- do.call(glca::gofglca, args)
        
        
        gtable1 <- res1[["gtable"]] #Absolute model fit
        
        dtable1<- res1[["dtable"]]  # Relative model fit 
        
        if(is.null(res$dtable)){
          
          dtable<- NULL 
          
          # res<- gofglca(lca2, lca3, lca4, test = "boot", seed = 1)
          # Warning message:
          #   In gofglca(lca2, lca3, lca4, test = "boot", seed = 1) :
          #   Since responses are different, deviance table does not printed.
          # 
        }
        
        
        # Measurement invariance--------------
        
        if(self$options$nc>=2){
          
          
          mglca2<- glca::glca(formula = formula, 
                              group=group, 
                              data = data, 
                              nclass = nc, 
                              ncluster = nclust,
                              seed = 1)
          
          mglca3<- glca::glca(formula = formula, 
                              group=group, 
                              data = data, 
                              nclass = nc, 
                              ncluster = nclust,
                              measure.inv=FALSE,
                              seed = 1)
          
          mi<- glca::gofglca(mglca2, mglca3, test = "chisq")
          
          mi.d<- mi[["dtable"]]
          
        }
        
        # Equality of coefficients--------------
        
        if(self$options$nc>=2){
          
          
          mglca2<- glca::glca(formula = formula, 
                              group=group, 
                              data = data, 
                              nclass = nc, 
                              ncluster = nclust,
                              seed = 1)
          
          mglca3<- glca::glca(formula = formula, 
                              group=group, 
                              data = data, 
                              nclass = nc, 
                              ncluster = nclust,
                              coeff.inv = FALSE,
                              seed = 1)
          
          ci<- glca::gofglca(mglca2, mglca3, test = "chisq")
          
          ci.d<- ci[["dtable"]]
          
        }
        
        # Cluster(Marginal prevalences for latent cluster)------
        
        clust <- lca[["param"]][["delta"]]
        clust<- as.data.frame(clust)
        
        # Class prevalences(Marginal prevalences for latent classes) ----------
        
        cla <- colMeans(do.call(rbind, lca[["posterior"]][["class"]]))
        cla<- as.data.frame(cla)
       
        # Class prevalences by cluster-----------
        
        cross<- lca[["posterior"]][["wclass"]]
        
        # item probability---------
        
        item<- lca[["param"]][["rho"]]
        
        item<- do.call("rbind", lapply(item, as.data.frame))
        
        # cluster posterior probability---------
        
       # clp<-lca[["posterior"]][["cluster"]]
        
        # posterior probability---------
        
        post <-lca[["posterior"]][["class"]]
        
        # Cluster membership------------
        
        member<- lca$posterior$cluster
        
        # logistic regression coef.--------
        
        co <- lca[["coefficient"]][["Level1"]]
        
        
        # cluster prob.(gamma)----------
        
        gamma <- lca[["param"]][["gamma"]]
        
        
        # Class Prevalences plot----------
        
        image <- self$results$plot1
        
        vars <- length(self$options$vars) 
        
        width <- 100 + vars * 100
        
        image$setSize(width, 700)
        
        
        #image$setSize(100 + 100 * length(self$options$vars), 200)
        
        image$setState(lca)
        
       
        
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
            'cla'=cla,
            'cross'=cross,
            'item'=item,
            'post'=post,
            'gtable'=gtable,
            'dtable'=dtable,
            'gtable1'=gtable1,
            'dtable1'=dtable1,
            'clust'=clust,
            'member'=member,
            'co'=co,
            'mi.d'=mi.d,
            'ci.d'=ci.d,
            'gamma'=gamma
           
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
      
      # populate Absolute model fit table for class------------
      
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
      
      # Populate relative model fit for class---------------
      
      .populateRelTable = function(results) {
        
        
        if(is.null(results$dtable))
          return()
        
        
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
      
      # populate Absolute model fit table for cluster------------
      
      .populateModel1Table = function(results) {
        
        if(is.null(results$dtable))
          return()
        
       
        table <- self$results$comp1
        
        
        gtable1 <- results$gtable1
        
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
      
      # Populate relative model fit for cluster---------------
      
      .populateRel1Table = function(results) {
        
        if(is.null(results$dtable))
          return()
        
        table <- self$results$rel1
        
        
        dtable1 <- results$dtable1
        
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
      
      
      # populate marginal prevalences for latent classes---
      
      .populateMarginTable= function(results) {  
        
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
      
      # invariance check------------------
      
      .populateMiTable = function(results) {
        
        if(self$options$nc<3)
          return()
        
      
        table <- self$results$mi
        
        dtable <- results$mi.d
        
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
      
      # invariance for coeff.----------------------
      
      .populateCiTable = function(results) {
        
        if(self$options$nc<3)
          return()
        
       table <- self$results$ci
       
       dtable <- results$ci.d
        
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
      
      # populate cluter membership-----------
      
      .populateMemTable= function(results) {
      
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
        
        table <- self$results$item
        item <- results$item
        
        names<- dimnames(item)[[1]]
        
        dims <- dimnames(item)[[2]]
        
        for (dim in dims) {
          
          table$addColumn(name = paste0(dim),
                          type = 'character')
        }
        
        
        for (name in names) {
          
          row <- list()
          
          for(j in seq_along(dims)){
            
            row[[dims[j]]] <- item[name,j]
            
          }
          
          table$addRow(rowKey=name, values=row)
          
        }
        
        
      },
    
      # posterior probability----------
      
      .populatePosTable= function(results) {
        
        if(!self$options$post)
          return()
        
        post <- results$post
        
        self$results$text2$setContent(post)
        
      },
      
      .populateLogTable=function(results){
        
        if(!self$options$co)
          return()
        
        co <- results$co
        
        self$results$text3$setContent(co)
        
        
      },
      
      .populateGamTable=function(results){
        
        if(!self$options$gamma)
          return()
        
        gamma <- results$gamma
        
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
