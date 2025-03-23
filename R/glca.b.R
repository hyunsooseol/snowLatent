

glcaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "glcaClass",
    inherit = glcaBase,
    private = list(
      .allCache = NULL,      
      .htmlwidget = NULL,
      
      .init = function() {
        
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title="Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li>Latent Class Analysis(LCA) based on <b>glca</b> R package.</li>',
              '<li>The MAR(Missing at Random) method is applied to handle missing values.</li>',
              '<li>If you select the Equality of coefficients option, the logistic regression result does not appear in the screen.</li>',
              '<li>The result table does not printed if the results from glca R package are not available.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowLatent/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'
              
            )
            
          )
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
            "Model2: measure.inv=TRUE; Model3: measure.inv=FALSE."
          )
        if (self$options$cir)
          self$results$cir$setNote(
            "Note",
            "Model2: coeff.inv=TRUE; Model3: coeff.inv=FALSE."
          )
        if(isTRUE(self$options$plot1)){
          
          width <- self$options$width
          height <- self$options$height
         
          self$results$plot1$setSize(width, height)
        }
        if(isTRUE(self$options$plot2)){
          
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$plot2$setSize(width, height)
        }
        if(isTRUE(self$options$plot3)){
          
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$plot3$setSize(width, height)
        }
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
      },
      
  .run = function() {
        
        if (is.null(self$options$group) || is.null(self$options$vars) || 
            length(self$options$vars) < 2) return()
        
        ######## Example: glca R package ################
        
        # library(glca)
        # data("gss08")
        # 
        # lca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
        #       group= DEGREE,            
        #       data = gss08, 
        #       nclass = 3)
        # summary(lca)
        
        ################################
    vars <- self$options$vars
    group <- self$options$group
    covs <- self$options$covs
    nc <- self$options$nc      
    
    #---
    data <- private$.cleanData()

    #lca <- private$.computeLCA()
    if (is.null(private$.allCache)) {
      private$.allCache <- private$.computeLCA()
    }
    lca<- private$.allCache     
    self$results$text$setContent(lca) 
        
        # Model fit
        if(isTRUE(self$options$fit)){
       
        table <- self$results$fit
        class <- self$options$nc 
        
        loglik<- lca$gof$loglik
        aic<- lca$gof$aic
        caic<- lca$gof$caic
        bic<- lca$gof$bic
        entropy<- lca$gof$entropy
        df<- lca$gof$df
        gsq<- lca$gof$Gsq
        
        fit <- data.frame(loglik,aic,caic,bic,entropy,df,gsq)
        
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
        }
        
        #Model Comparison----
        mc <- private$.computeMEAI()
      
          if(isTRUE(self$options$mia)){
            table <- self$results$mia
            gtable <- mc$mi.g
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

            # if(self$options$test=='boot'){
            #   boot <- g[,8]
            # }

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

              # if(self$options$test=='boot'){
              #   row[["boot"]] <-  g[name, 8]
              # }

              table$addRow(rowKey=name, values=row)
            }
          }
          
          if(isTRUE(self$options$mir)){
           
            table <- self$results$mir
            dtable <- mc$mi.d
            
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
          }
       # Equality of coefficients----
        eq <- private$.computeEQ()

        if(isTRUE(self$options$cia)){

          if(is.null(self$options$covs))
            return()

          table <- self$results$cia
          ctable <- eq$ci.g

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

          # if(self$options$test1=='boot'){
          #   boot <- g[,8]
          # }

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

            # if(self$options$test1=='boot'){
            # 
            #   row[["boot"]] <-  g[name, 8]
            # }

            table$addRow(rowKey=name, values=row)
          }
        }

        if(isTRUE(self$options$cir)){

          if(is.null(self$options$covs))
            return()

          table <- self$results$cir
          cdtable <- eq$ci.d

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
        }

  #################################################
      
        if(isTRUE(self$options$co)){
       # logistic regression -------------
       co<- lca$coefficient
       
       if(is.null(co)) {
         co <- NULL 
       } else {
         co<- lca$coefficient
       }
       self$results$text3$setContent(co)
     }
        
        if(isTRUE(self$options$marginal)){
        # Marginal prvalences for latent class-----
        margin<- colMeans(do.call(rbind, lca[["posterior"]]))
        
        table <- self$results$marginal
        mar<- as.data.frame(margin)
        names<- dimnames(mar)[[1]]
        
        for (name in names) {
          row <- list()
          row[['value']] <- mar[name,1]
          table$addRow(rowKey=name, values=row)
        }
      }  
        
        if(isTRUE(self$options$preval)){  
          # Class prevalences by group----------
          prev <-  as.matrix(do.call(rbind, lapply(lca[["posterior"]], colMeans)))
          
          table <- self$results$preval
          cg<- prev
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
        }
        
        if(isTRUE(self$options$item)){

        # item probability---------
        item<- lca[["param"]][["rho"]]
       self$results$text1$setContent(item)
        }
        
        if(isTRUE(self$options$gamma)){
          # gamma probability----------
          gamma <- lca[["param"]][["gamma"]]
          options(max.print = 1000000)
          self$results$text4$setContent(gamma) 
        }
        
        if(isTRUE(self$options$post)){
          # posterior probability---------
          post <- lca[["posterior"]]
          options(max.print = 1000000)
          self$results$text2$setContent(post) 
        }
 
##########################################################
 
        # Item probabilities by group plot(default:Measure.inv=TRUE)--------
        ic<- lca[["param"]][["rho"]]
        ic <- reshape2::melt(ic) 
        colnames(ic) <-c("Class", "Level", "value", "Variable", "Group") 

        image2 <- self$results$plot2
        image2$setState(ic )
        },
      
      .plot1 = function(image, ...) {
        
        if (is.null(self$options$group) || is.null(self$options$vars) || 
            length(self$options$vars) < 2) return()
        
        data <- private$.cleanData()
        
        vars <- self$options$vars
        group <- self$options$group
        covs <- self$options$covs
        nc <- self$options$nc  
        
        #lca <- private$.computeLCA()
        lca<- private$.allCache
        
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
       
       if (is.null(self$options$group) || is.null(self$options$vars) || 
           length(self$options$vars) < 2) return()
       
       data <- private$.cleanData()
       
       vars <- self$options$vars
       group <- self$options$group
       covs <- self$options$covs
       nc <- self$options$nc  
       
       mglca3<- private$.computeIP()
      
       icf<- mglca3[["param"]][["rho"]]
       icf <- reshape2::melt(icf) 
       colnames(icf) <-c("Class", "Level", "value", "Variable", "Group") 
       
       ic <- icf
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
        
        #apply MAR for missing values---
        #data <- data[!is.na(data[[groupVarName]]), ]
        
        return(data)
      },
     
     .computeLCA = function() {
       
       data <- private$.cleanData()
       
       vars <- self$options$vars
       group <- self$options$group
       covs <- self$options$covs
       nc <- self$options$nc  
       
       # vars <- colnames(data[, -1] )
       vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
       vars <- paste0(vars, collapse=',')
       
       # formula with no covariates ----------     
       formula <- as.formula(paste0('glca::item(', vars, ')~1'))
       
       # With covariates-------------
       #if( !is.null(self$options$covs) ) {
       if(length(self$options$covs)>=1){
         
         covs <- self$options$covs
         covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
         covs <- paste0(covs, collapse='+')
         
         formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
       }
       
       group <- data[, 1]
       
       ################### LCA model estimates############################
       
       lca = glca::glca(formula=formula,
                        group= group,
                        data=data,
                        nclass = nc, 
                        seed = 1)
       
       return(lca)
     },
     
     #Model Comparison---
     .computeMEAI = function() {
     
       data <- private$.cleanData()
       
       vars <- self$options$vars
       group <- self$options$group
       covs <- self$options$covs
       nc <- self$options$nc  
       
       # vars <- colnames(data[, -1] )
       vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
       vars <- paste0(vars, collapse=',')
       
       # formula with no covariates ----------     
       formula <- as.formula(paste0('glca::item(', vars, ')~1'))
       
       # With covariates-------------
       #if( !is.null(self$options$covs) ) {
       if(length(self$options$covs)>=1){
         
         covs <- self$options$covs
         covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
         covs <- paste0(covs, collapse='+')
         
         formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
       }
       
       group <- data[, 1]
    
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
                               seed = 1))
       
       if(jmvcore::isError(mglca3)){

         err_string <- stringr::str_interp(
           " Error in if (maxdiff < eps) break : Covariates can not be computed."
         )
         stop(err_string)

       }

       if (! jmvcore::isError(mglca3) ){
        
         mi <- glca::gofglca(mglca2, mglca3, test = 'chisq')
         
         mi.g <- mi[["gtable"]] #Absolute model fit
         
         
         if(is.null(mi$dtable)) {
           mi.d <- NULL 
         } else {
           mi.d <- mi[["dtable"]] # Relative model fit
         }
         
      }
       retlist <- list(mi.g=mi.g, mi.d=mi.d)
       return(retlist)
     },
     
     #Equality of coefficients---
     .computeEQ=function(){

       data <- private$.cleanData()

       vars <- self$options$vars
       group <- self$options$group
       covs <- self$options$covs
       nc <- self$options$nc

       # vars <- colnames(data[, -1] )
       vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
       vars <- paste0(vars, collapse=',')

       # formula with no covariates ----------
       formula <- as.formula(paste0('glca::item(', vars, ')~1'))

       # With covariates-------------
       #if( !is.null(self$options$covs) ) {
       if(length(self$options$covs)>=1){

         covs <- self$options$covs
         covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
         covs <- paste0(covs, collapse='+')

         formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
       }

       group <- data[, 1]
       #-----------------------

       lca = glca::glca(formula=as.formula(paste0('glca::item(', vars, ')~1')),
                        group= group,
                        data=data,
                        nclass = nc,
                        seed = 1)

       mglca4 <- glca::glca(formula = formula,
                            group = group,
                            data = data,
                            nclass = nc,
                            seed = 1)

      mglca5 <- try(glca::glca(formula = formula,
                              group = group,
                              data = data,
                              nclass = nc,
                              coeff.inv = FALSE,
                              seed = 1))

      if(jmvcore::isError(mglca4)){

        err_string <- stringr::str_interp(
          " Error in if (maxdiff < eps) break : Covariates can not be computed."
        )
        stop(err_string)

      }

      if (! jmvcore::isError(mglca4) ){

        ci <- glca::gofglca(lca, mglca4, mglca5, test = 'chisq')

        ci.g <- ci[["gtable"]] #Absolute model fit

        if(is.null(ci$dtable)) {
          ci.d <- NULL
        } else {
          ci.d <- ci[["dtable"]] # Relative model fit
        }

      }

      retlist <- list(ci.g=ci.g, ci.d=ci.d)
      return(retlist)


     },

    #Item probabilities by group plot(Measure.inv=FALSE)--------

     .computeIP=function(){
       
       data <- private$.cleanData()
       
       vars <- self$options$vars
       group <- self$options$group
       covs <- self$options$covs
       nc <- self$options$nc  
       
       # vars <- colnames(data[, -1] )
       vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
       vars <- paste0(vars, collapse=',')
       
       # formula with no covariates ----------     
       formula <- as.formula(paste0('glca::item(', vars, ')~1'))
       
       # With covariates-------------
       #if( !is.null(self$options$covs) ) {
       if(length(self$options$covs)>=1){
         
         covs <- self$options$covs
         covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
         covs <- paste0(covs, collapse='+')
         
         formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
       }
       
       group <- data[, 1]
       
       mglca3 <-glca::glca(formula = formula, 
                           group = group, 
                           data = data, 
                           nclass = nc, 
                           measure.inv = FALSE,
                           seed = 1)
       
       return(mglca3)
       
     }
     )
)
