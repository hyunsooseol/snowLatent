
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import stats
#' @import glca
#' @import ggplot2
#' @importFrom glca glca
#' @importFrom glca item
#' @importFrom glca gofglca
#' @importFrom stringr str_interp
#' @importFrom ggplot2 ggplot
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
            <p>1. Latent Class Analysis(LCA) based on <b>glca</b> R package.</p>
            <p>2. The rationale of snowLatent module is described in the <a href='https://docs.google.com/viewer?a=v&pid=sites&srcid=a29yZWEuYWMua3J8a3VzdGF0bGFifGd4OjU0Nzc0NjU4OGJkODVjNDk'>documentation</a>.</p>
            <p>3. The result table does not printed if the results from glca R package are not available.</p>
            <p>4. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowLatent/issues'  target = '_blank'>GitHub</a>.</p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
        if (self$options$mia)
          self$results$mia$setNote(
            "Note",
            "Model1: measure.inv=TRUE; Model2: measure.inv=FALSE."
          )
        
      
        if (self$options$mir)
          self$results$mir$setNote(
            "Note",
            "Model1: measure.inv=TRUE; Model2: measure.inv=FALSE."
          )

        if (self$options$cia)
          self$results$cia$setNote(
            "Note",
            "Model1: measure.inv=TRUE; Model2: measure.inv=FALSE."
          )
        
        
        if (self$options$cir)
          self$results$cir$setNote(
            "Note",
            "Model1: coeff.inv=TRUE; Model2: coeff.inv=FALSE."
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
          
          
          # populate fit table------------
          
          private$.populateFitTable(results)
          
          
          # Measurement invariance(Absolute model fit)-----------
          
          private$.populateMiaTable(results)
          
          # Measurement invariance(Relative model fit)---------------
          
          
          private$.populateMirTable(results)
          
          # Equality of coefficients(Absolute model fit)-----------
          
          private$.populateCiaTable(results)
          
          # Equality of coefficients(Relative model fit)---------------
          
          
          private$.populateCirTable(results)
          
          
          
          # class probability table-----
          
          private$.populateClassTable(results)
          
        
          #  class prevalences by group table-------
          
          private$.populateCgTable(results)
          
          
           # item probabilities---------
          
          private$.populateItemTable(results)
          
          #  Gamma probabilities---------
          
          private$.populateGamTable(results)
          
         
          #  posterior probabilities--
          
          private$.populatePosTable(results)
          
 
          # logistic table-----------
          
           private$.populateLogiTable(results)
          
        }
      },
      
      
      .compute = function(data) {
        
         # if (is.null(self$options$group))
         #   return()
         # 
        
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
        vars <- self$options$vars
        
       # vars <- colnames(data[, -1] )
        vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
        vars <- paste0(vars, collapse=',')
        
        # formula with no covariates ----------     
        
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
        # nb <- self$options$nb
        
      ################### LCA model estimates############################
        
        lca = glca::glca(formula=formula,
                         group= group,
                         data=data,
                         nclass = nc, 
                         seed = 1)
                        
      #################################################################
      
      self$results$text$setContent(lca)
        
        # Model fit-------------
        
        loglik<- lca$gof$loglik
        aic<- lca$gof$aic
        caic<- lca$gof$caic
        bic<- lca$gof$bic
        entropy<- lca$gof$entropy
        df<- lca$gof$df
        gsq<- lca$gof$Gsq
        
        fit <- data.frame(loglik,aic,caic,bic,entropy,df,gsq)
        
        # Measurement invariance----------------
        
          mglca2 <- glca::glca(formula = formula, 
                               group = group, 
                               data = data, 
                               nclass = nc, 
                               seed = 1)
          
          
          mglca3 <-try(glca::glca(formula = formula, 
                               group = group, 
                               data = data, 
                               nclass = nc, 
                               measure.inv = FALSE,
                               seed = 1)
          
          )
          
          if(jmvcore::isError(mglca3)){
            
            err_string <- stringr::str_interp(
              " Error in if (maxdiff < eps) break : Covariates can not be computed."
            )
            stop(err_string)
            
          } 
          
          if (! jmvcore::isError(mglca3) ){
          
          mi <- glca::gofglca(mglca2, mglca3, test = "chisq")
          
          #self$results$text$setContent(mi)
          
          
          mi.g <- mi[["gtable"]] #Absolute model fit
          
         
          if(is.null(mi$dtable)) {
            mi.d <- NULL 
          } else {
            mi.d <- mi[["dtable"]] # Relative model fit
          }
          
          }
           # Equality of coefficients ---             
         
          
          mglca4 <- try(glca::glca(formula = formula, 
                               group = group, 
                               data = data, 
                               nclass = nc, 
                               coeff.inv = FALSE,
                               seed = 1)
          )
                        
                        
                        
          if(jmvcore::isError(mglca4)){
            
            err_string <- stringr::str_interp(
              " Error in if (maxdiff < eps) break : Covariates can not be computed."
            )
            stop(err_string)
            
          } 
          
          if (! jmvcore::isError(mglca4) ){
          
          ci <- glca::gofglca(mglca2, mglca4, test = "chisq")
          
          ci.g <- ci[["gtable"]] #Absolute model fit
         
          if(is.null(ci$dtable)) {
            ci.d <- NULL 
          } else {
            ci.d <- ci[["dtable"]] # Relative model fit
          }
        
          }
      
       
          # Class prevalences by group----------
        
         prev <-  as.matrix(do.call(rbind, lapply(lca[["posterior"]], colMeans)))
        
       
        # item probability---------
        
        item<- lca[["param"]][["rho"]]
        
        # gamma probability----------
         
         gamma <- lca[["param"]][["gamma"]]
         
         # posterior probability---------
        
        post <- lca[["posterior"]]
        
        
         # Marginal prvalences for latent class-----
         
         margin<- colMeans(do.call(rbind, lca[["posterior"]]))
         
        # self$results$text3$setContent(margin)
         
         # logistic regression -------------
        
        co<- lca$coefficient
        
        if(is.null(co)) {
          co <- NULL 
        } else {
          co<- lca$coefficient
        }
      
       
         # Class Prevalences plot----------
        
        image <- self$results$plot1
        
        vars <- length(self$options$vars) 
        
        width <- 100 + vars * 100
        
        image$setSize(width, 700)
        
        
        #image$setSize(100 + 100 * length(self$options$vars), 200)
        
        image$setState(lca)
        
        # Item probabilities by group plot(default:Measure.inv=TRUE)--------
        
        ic<- lca[["param"]][["rho"]]
        ic <- reshape2::melt(ic) 
        colnames(ic) <-c("Class", "Level", "value", "Variable", "Group") 
        image2 <- self$results$plot2
        image2$setState(ic )
        
        # Item probabilities by group plot(Measure.inv=FALSE)--------
        
        if(self$options$plot3==TRUE){
          
          mglca3 <-glca::glca(formula = formula, 
                                  group = group, 
                                  data = data, 
                                  nclass = nc, 
                                  measure.inv = FALSE,
                                  seed = 1)
          
          icf<- mglca3[["param"]][["rho"]]
          icf <- reshape2::melt(icf) 
          colnames(icf) <-c("Class", "Level", "value", "Variable", "Group") 
          image3 <- self$results$plot3
          image3$setState(icf )
          
          
        }
        
        
        results <-
          list(
           'fit'=fit,
            'prev'=prev,
            'item'=item,
            'post'=post,
            'margin'=margin,
             'mi.g'=mi.g,
            'mi.d'=mi.d,
            'ci.g'=ci.g,
            'ci.d'=ci.d,
            'gamma'=gamma,
            'co'=co
          
            )
        
        
      },   
      
      
      ################ Tables################################
  
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
     
     
     # absolute model fit table--------------------
     
      .populateMiaTable = function(results) {
        
        if (!self$options$mia)
          return()
        
        table <- self$results$mia
      
        gtable <- results$mi.g
        
        if(is.null(gtable))
          return()
        
        g<- as.data.frame(gtable)
      
       
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
        
      },
      
      # Populate relative model fit---------------
      
      .populateMirTable = function(results) {
        
        if (!self$options$mir)
          return()
       
        table <- self$results$mir
        
        dtable <- results$mi.d
        
        if(is.null(dtable))
          return()
        
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
    
      #-----------------------
      
      .populateCiaTable = function(results) {
        
        if(is.null(self$options$covs))
          return() 
       
      table <- self$results$cia
      ctable <- results$ci.g
        
      if(is.null(ctable))
        return()  
      
      g<- as.data.frame(ctable)
        
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
        
      },
      
      # Populate relative model fit---------------
      
      .populateCirTable = function(results) {
        
        
        if(is.null(self$options$covs))
          return()
      
        table <- self$results$cir
        cdtable <- results$ci.d
        
        if(is.null(cdtable))
          return()
        
        d<- as.data.frame(cdtable)
        
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
          
          if (!self$options$marginal)
            return()
          
          table <- self$results$marginal
          
          margin <- results$margin
          
          mar<- as.data.frame(margin)
          names<- dimnames(mar)[[1]]
          
          for (name in names) {
            
            row <- list()
            
            row[['value']] <- mar[name,1]
            
            table$addRow(rowKey=name, values=row)
            
          }
          
          
          
        },
        
      # class prevalences by group table---------------
      
      .populateCgTable= function(results) {

        if (!self$options$preval)
          return()
        
        
        table <- self$results$preval

        cg<- results$prev

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


      # item probabilities----------

      .populateItemTable= function(results) {

        if (!self$options$item)
          return()
          
        res <- results$item

        self$results$text1$setContent(res)

      },

      # gamma probabilities----------
      
      .populateGamTable= function(results) {
        
        if (!self$options$gamma)
          return()
        
        res <- results$gamma
        
        options(max.print = 1000000)
        
        self$results$text4$setContent(res)
        
      }, 
      
      
      # posterior probability----------
      
      .populatePosTable= function(results) {
        
        if (!self$options$post)
          return()
        
        post <- results$post
        
        options(max.print = 1000000)
        
        self$results$text2$setContent(post)
        
      },
      

      # Logistic reg. table----------


      .populateLogiTable= function(results) {

        if (!self$options$co)
          return()

        co <- results$co

        self$results$text3$setContent(co)

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
      
     # item probailities by group plot-----------
     
     .plot2 = function(image2, ggtheme, theme, ...) {    
       
       ic <- image2$state
       
       if (is.null(ic))
         return()
       
       plot2 <- ggplot2::ggplot(ic, ggplot2::aes(x = Variable, y = value, fill = Level)) + 
         ggplot2::geom_bar(stat = "identity", position = "stack")+ 
         ggplot2::facet_wrap(ggplot2::vars(Group,Class) )+
         ggplot2::scale_x_discrete("Variable", expand = c(0, 0)) +
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
     
     
     # item probailities by group plot(Measure.inv=FALSE)-----------
     
     .plot3 = function(image3, ggtheme, theme, ...) {    
       
       ic <- image3$state
       
       if (is.null(ic))
         return()
       
       plot3 <- ggplot2::ggplot(ic, ggplot2::aes(x = Variable, y = value, fill = Level)) + 
         ggplot2::geom_bar(stat = "identity", position = "stack")+ 
         ggplot2::facet_wrap(ggplot2::vars(Group,Class) )+
         ggplot2::scale_x_discrete("Variable", expand = c(0, 0)) +
         ggplot2::scale_y_continuous("Probability", expand = c(0, 0)) +
         ggplot2::theme_bw()
       
       plot3 <- plot3+ggtheme
       
       if (self$options$angle > 0) {
         plot3 <- plot3 + ggplot2::theme(
           axis.text.x = ggplot2::element_text(
             angle = self$options$angle, hjust = 1
           )
         )
       }
       
       print(plot3)
       TRUE
       
       
     },
     
     
     
      ### Helper functions =================================     
      
      # .cleanData = function() {
      #   
      #   vars <- self$options$vars
      #   groupVarName <- self$options$group
      #   data <- list()
      #   
      #   data[[groupVarName]] <- jmvcore::toNumeric(self$data[[groupVarName]])
      #   
      #   for (item in vars)
      #     data[[item]] <- jmvcore::toNumeric(self$data[[item]])
      #   
      #   attr(data, 'row.names') <- seq_len(length(data[[1]]))
      #   attr(data, 'class') <- 'data.frame'
      #   
      #   data <- data[!is.na(data[[groupVarName]]), ]
      #   
      #   return(data)
      # }
      
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
