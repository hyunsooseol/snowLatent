
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
        
        if (is.null(self$options$vars) ||
            length(self$options$vars) < 2)
          
          ready <- FALSE
        
        if (ready) {
          
          data <- private$.cleanData()
          
          results <- private$.compute(data)               
          
          # populate Model comparison-----------
          
          private$.populateModelTable(results)
          
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
        
        
        vars<- self$options$vars
        covs<- self$options$covs
        
        nc <- self$options$nc
        nb <- self$options$nb
       
        data<- as.data.frame(data)
        
        # data[[covs]] <- jmvcore::toNumeric(data[[covs]])
        
       
        ############ Construct formula###################        
          
        vars <- colnames(data)
        vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
        vars <- paste0(vars, collapse=',')
      
        if (is.null(covs)){
      
          # formula with no covariate variables----------     
      
        formula <- as.formula(paste0('glca::item(', vars, ')~1'))
       
        }
   
        if (length(covs) > 0) {

          covs<- self$options$covs
          
          if (is.null(covs))
            return()
          
          #-----------------------------------------------
          varNames <- c(covs, vars)
          data <- jmvcore::select(self$data, varNames)
          
           for (var in vars)
               data[[var]] <- jmvcore::toNumeric(data[[var]])
           
          # exclude rows with missings in the covariate variables
          
          data <- data[!is.na(data[[covs]]), ]
          #-----------------------------------------------
          
          # Handling covariate variables ??? ---------

          # data <- colnames(data)
          # covs <- colnames(data)
          #covs <- colnames(data[covs])

         # covs<- data[covs] # ?????
      
          # Covariate formula is OK-----------------------------

      formula <- as.formula(paste0('glca::item(', vars, ')~', paste(data[[covs]], collapse= "+")))

         
        }

        self$results$text$setContent(formula)
       
        
   ################### LCA model estimates############################

          lca = glca::glca(formula=formula,
                           data=data,
                        # group= data[[group]],
                           nclass = nc,
                           n.init=1)
    #################################################################
        #group: Argument that indicates group variable which has the same length as manifest items
        #on the formula. If group = NULL (default), LCA or LCR is fitted.

        
       # self$results$text$setContent(lca)
          
   
      ######## LCA with no covariates##############
      
        gam<- lca[["param"]][["gamma"]]
          
        row.names(gam) <- 1:nrow(gam)  
        gam <- as.data.frame(gam)
        gam <- t(gam)
        gam <- as.data.frame(gam)
          
        # item probabilities------
          
        item<- lca[["param"]][["rho"]][["ALL"]]
          
        # Class Prevalences plot----------
          
        image <- self$results$plot1
          
        vars <- length(self$options$vars)
          
        width <- 300 + vars * 30
          
        image$setSize(width, 500)
          
        image$setState(lca)
          
          
        ############## Model comparison######################
          
             out <- NULL
             
             for (i in 1:self$options$nc) {
               
               lca = glca::glca(formula, data = data, nclass = nc, n.init=1)
          
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
            
            results <-
              list(
                'res'=res,
                'gam'= gam,
                'item'=item
                
                )
          
            
      },   
      
      
      ################ populating Tables################################
      
      
      .populateModelTable = function(results) {
            
        
        nc <- self$options$nc
         
        table <- self$results$comp
         
        
         res <- results$res
         
        
         aic <- res[,2]
         caic <- res[,3]
         bic <- res[,4] 
         entropy <- res[,5]
         df <- res[,6] 
         gsq <- res[,7] 
         p <- res[,8]
         
         
         names <- dimnames(res)[[1]]
         
         for (name in names) {
           
           row <- list()
           
           row[['nc']] <- nc
           row[["loglik"]]   <-  res[name, 1]
           row[["aic"]] <-  res[name, 2]
           row[["caic"]] <-  res[name, 3]
           row[["bic"]] <-  res[name, 4]
           row[["entropy"]] <-  res[name, 5]
           row[["df"]] <-  res[name, 6]
           row[["gsq"]] <-  res[name, 7]
           row[["p"]] <-  res[name, 8]
           
           
           table$addRow(rowKey=name, values=row)
           
         }     
         
      },
         
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
        
        plot.glca <- function(x, ...){
          oldmar <- par()$mar
          on.exit(par(mar = oldmar))
          
          #  if (ask) grDevices::devAskNewPage(TRUE)
          
          model <- x$model
          param <- x$param
          post <- x$posterior
          
          par(mar = c(5.1, 4.1, 4.1, 5.1))
          
          # rho
          if (all(model$R == 2L)) { # binary plot
            if (model$measure.inv | model$G == 1L) {
              if (model$W > 1L) rho <- param$rho
              else rho <- param$rho[[1]]
              irp <- sapply(rho, function(i) i[,1L])
              # grDevices::dev.hold()
              plot(x = 1L:model$M, y = c(0L, rep(1L, model$M - 1L)),
                   xlim = c(0.8, model$M + 0.2), ylim = c(-0.1, 1.1),
                   xlab = "Manifest Items", ylab = "",
                   type = "n", xaxt = "n", yaxt = "n")
              axis(side=1, at = 1:model$M, labels = x$var.names$y.name)
              axis(side=2, at = (0:5)/5, las = "1")
              for (c in 1:model$C) {
                lines(1:model$M, irp[c,], type = "b", pch = c)
              }
              legend("topleft", pch = 1:model$C, inset = c(1,0),
                     legend = rownames(irp), xpd = TRUE, bg = "white")
              title("Item Response Probabilities by Class")
              #grDevices::dev.flush()
            } else {
              for (g in 1:model$G) {
                irp <- sapply(param$rho[[g]], function(i) i[,1])
                # grDevices::dev.hold()
                plot(x = 1:model$M, y = c(0, rep(1, model$M - 1)),
                     xlim = c(0.8, model$M + 0.2), ylim = c(-0.1, 1.1),
                     xlab = "Manifest Items", ylab = "",
                     type = "n", xaxt = "n", yaxt = "n")
                axis(side=1, at = 1:model$M, labels = x$var.names$y.name)
                axis(side=2, at = (0:5)/5, las = 1)
                for (c in 1:model$C) {
                  lines(1:model$M, irp[c,], type = "b", pch = c)
                }
                legend(x = model$M + 0.2, y = 1, pch = 1:model$C,
                       legend = rownames(irp), xpd = TRUE, bg = "white")
                title(paste0("Item Response Probabilities by Class", "\n(Group : ",
                             x$var.names$g.names[g],")"))
                #grDevices::dev.flush()
              }
            }
          } else { # polytomous plot
            par(mar = c(3.5, 4.1, 4.1, 4.5))
            if (model$measure.inv | model$G == 1) {
              if (model$W > 1) rho <- param$rho
              else rho <- param$rho[[1]]
              for (m in 1:model$M) {
                # grDevices::dev.hold()
                xpos <- barplot(t(rho[[m]]), beside = TRUE, ylim = c(0, 1))
                legend("topleft", inset = c(1, 0), legend = colnames(rho[[m]]),
                       fill = grDevices::gray.colors(ncol(rho[[m]])),
                       xpd = TRUE, bg = "white")
                title(paste0("Item Response Probabilities by Class", "\n(Item : ",
                             names(rho)[m],")"))
                #grDevices::dev.flush()
              }
            } else {
              for (g in 1:model$G) {
                rho <- param$rho[[g]]
                for (m in 1:model$M) {
                  #  grDevices::dev.hold()
                  xpos <- barplot(t(rho[[m]]), beside = TRUE, ylim = c(0, 1))
                  legend("topleft", inset = c(1, 0), legend = colnames(rho[[m]]),
                         fill = grDevices::gray.colors(ncol(rho[[m]])),
                         xpd = TRUE, bg = "white")
                  title(paste0("Item Response Probabilities by Class", "\n(Item : ",
                               names(rho)[m]," / Group : ", x$var.names$g.names[g], ")"))
                  #grDevices::dev.flush()
                }
              }
            }
          }
          
        }
        
        
      plot1 <- plot.glca(lca)
      
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
       
       items <- self$options$vars
       
       data <- list()
       
       for (item in items)
         data[[item]] <-
         jmvcore::toNumeric(self$data[[item]])
       
       attr(data, 'row.names') <- seq_len(length(data[[1]]))
       attr(data, 'class') <- 'data.frame'
       
       data <- jmvcore::naOmit(data)
       
       return(data)
     }
     
     
    )
)

  
