
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
        
        if (is.null(self$options$group) || is.null(self$options$vars) || 
            length(self$options$vars) < 2)
          
          ready <- FALSE
        
        if (ready) {
          
          data <- private$.cleanData()
          
          results <- private$.compute(data)               
          
          # populate Fit table----------
          
          private$.populateFitTable(results)
          
          # populate Model comparison(Absolute model fit)-----------
          
          private$.populateModelTable(results)
          
          # populate Relative model fit---------------
          
          private$.populateRelTable(results)
          
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
        
        lca = glca::glca(formula=formula,group= group,
                         data=data,nclass = nc)
                        
      #################################################################
      
      # self$results$text$setContent(lca)
        
        
        # fit measure----------
        
        loglik<- lca$gof$loglik
        aic<- lca$gof$aic
        caic<- lca$gof$caic
        bic<- lca$gof$bic
        entropy<- lca$gof$entropy
        df<- lca$gof$df
        gsq<- lca$gof$Gsq
        
       # Goodnes of fit--------------------------
        
         args <- list(test = "boot", nboot=nb)
         inpclas = self$options$nc
         
         for(nc in 2:inpclas)
           args[[nc+1]] <- glca::glca(formula = formula, group=group, data = data, nclass = nc)
         res <- do.call(glca::gofglca, args)
        
        
         gtable <- res[["gtable"]] #Absolute model fit
         
         dtable<- res[["dtable"]]  # Relative model fit 
        
        
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
            'post'=post,
            'gtable'=gtable,
            'dtable'=dtable
            
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
      
      # populate Absolute model fit table------------
      
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
        
        par(mfcol = c(3, 1))
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
