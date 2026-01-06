
glcaClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "glcaClass",
    inherit = glcaBase,
    private = list(
      .modelCache = list(),  
      .dataCache = NULL,     
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
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
        ))
        
        if (self$options$mia)
          self$results$mia$setNote("Note",
                                   "Model1: measure.inv=TRUE; Model2: measure.inv=FALSE.")
        if (self$options$mir)
          self$results$mir$setNote("Note",
                                   "Model1: measure.inv=TRUE; Model2: measure.inv=FALSE.")
        if (self$options$cia)
          self$results$cia$setNote("Note",
                                   "Model2: measure.inv=TRUE; Model3: measure.inv=FALSE.")
        if (self$options$cir)
          self$results$cir$setNote("Note", "Model2: coeff.inv=TRUE; Model3: coeff.inv=FALSE.")
        
      },
      
      .run = function() {
        if (is.null(self$options$group) || is.null(self$options$vars) ||
            length(self$options$vars) < 3) return()
        
        if (is.null(private$.dataCache)) {
          private$.dataCache <- private$.cleanData()
        }
        data <- private$.dataCache
        
        vars <- self$options$vars
        group <- self$options$group
        covs <- self$options$covs
        nc <- self$options$nc
        
        modelKey <- paste(paste(vars, collapse=","), nc, group, sep="|")
        
        if (is.null(private$.modelCache[[modelKey]])) {
          private$.modelCache[[modelKey]] <- private$.computeLCA()
        }
        
        lca <- private$.modelCache[[modelKey]]
        
        self$results$text$setContent(lca)
        
        if (isTRUE(self$options$fit)) {
          self$results$fit$setRow(rowNo = 1, values = c(list(class = nc), 
                                                        as.list(lca$gof[c("loglik", "AIC", "BIC", "entropy", "df", "Gsq")])))
        }
        
        if (isTRUE(self$options$mia) || isTRUE(self$options$mir)) {
          mcKey <- paste(modelKey, "mc", sep="|")
          if (is.null(private$.modelCache[[mcKey]])) {
            private$.modelCache[[mcKey]] <- private$.computeMEAI()
          }
          mc <- private$.modelCache[[mcKey]]
          
          if (isTRUE(self$options$mia)) {
            table <- self$results$mia
            g <- as.data.frame(mc$mi.g)
            if (!is.null(g)) {
              row_names <- rownames(g)
              for (name in row_names) {
                table$addRow(
                  rowKey = name,
                  values = list(
                    loglik  = g[name, 1],
                    aic     = g[name, 2],
                    #caic    = g[name, 3],
                    bic     = g[name, 3],
                    entropy = g[name, 4],
                    df      = g[name, 5],
                    gsq     = g[name, 6]
                  )
                )
              }
            }
          }
          
          if (isTRUE(self$options$mir)) {
            table <- self$results$mir
            d <- as.data.frame(mc$mi.d)
            if (!is.null(d)) {
              row_names <- rownames(d)
              for (name in row_names) {
                table$addRow(rowKey = name,
                             values = list(
                               para   = d[name, 1],
                               loglik = d[name, 2],
                               df     = d[name, 3],
                               dev    = d[name, 4],
                               p      = d[name, 5]
                             ))
              }
            }
          }
        }
        
        if (isTRUE(self$options$cia) || isTRUE(self$options$cir)) {
          if (is.null(self$options$covs))
            return()
          
          eqKey <- paste(modelKey, "eq", sep="|")
          if (is.null(private$.modelCache[[eqKey]])) {
            private$.modelCache[[eqKey]] <- private$.computeEQ()
          }
          eq <- private$.modelCache[[eqKey]]
          
          if (isTRUE(self$options$cia)) {
            table <- self$results$cia
            g <- as.data.frame(eq$ci.g)
            #self$results$text$setContent(g)
            
            if (!is.null(g)) {
              row_names <- rownames(g)
              row_names <- row_names[row_names != "1"]
              
              for (name in row_names) {
                table$addRow(
                  rowKey = name,
                  values = list(
                    loglik  = g[name, 1],
                    aic     = g[name, 2],
                    bic     = g[name, 3],
                    entropy = g[name, 4],
                    df      = g[name, 5],
                    gsq     = g[name, 6]
                  )
                )
              }
            }
          }
          
          if (isTRUE(self$options$cir)) {
            table <- self$results$cir
            d <- as.data.frame(eq$ci.d)
            #self$results$text$setContent(d)
            
            if (!is.null(d)) {
              row_names <- rownames(d)
              row_names <- row_names[row_names != "1"]
              
              for (name in row_names) {
                table$addRow(rowKey = name,
                             values = list(
                               para   = d[name, 1],
                               loglik = d[name, 2],
                               df     = d[name, 3],
                               dev    = d[name, 4],
                               p      = d[name, 5]
                             ))
              }
            }
          }
        }
        
        if (isTRUE(self$options$co)) {
          #co <- lca$coefficient
          co<- coef(lca)
          if (!is.null(co)) {
            self$results$text3$setContent(co)
          }
        }
        
        if (isTRUE(self$options$marginal)) {
          marginalKey <- paste(modelKey, "marginal", sep="|")
          
          if (is.null(private$.modelCache[[marginalKey]])) {
            private$.modelCache[[marginalKey]] <- colMeans(do.call(rbind, lca[["posterior"]]))
          }
          margin <- private$.modelCache[[marginalKey]]
          
          table <- self$results$marginal
          mar <- as.data.frame(margin)
          names <- dimnames(mar)[[1]]
          
          for (name in names) {
            row <- list()
            row[['value']] <- mar[name, 1]
            table$addRow(rowKey = name, values = row)
          }
        }
        
        if (isTRUE(self$options$preval)) {
          prevalKey <- paste(modelKey, "preval", sep="|")
          
          if (is.null(private$.modelCache[[prevalKey]])) {
            private$.modelCache[[prevalKey]] <- as.matrix(do.call(rbind, lapply(lca[["posterior"]], colMeans)))
          }
          prev <- private$.modelCache[[prevalKey]]
          
          table <- self$results$preval
          cg <- as.data.frame(prev)
          names <- dimnames(cg)[[1]]
          dims <- dimnames(cg)[[2]]
          
          for (dim in dims) {
            table$addColumn(name = paste0(dim), type = 'number')
          }
          for (name in names) {
            row <- list()
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- cg[name, j]
            }
            table$addRow(rowKey = name, values = row)
          }
        }
        
        if (isTRUE(self$options$item)) {
          itemKey <- paste(modelKey, "item", sep="|")
          
          if (is.null(private$.modelCache[[itemKey]])) {
            private$.modelCache[[itemKey]] <- lca[["param"]][["rho"]]
          }
          item <- private$.modelCache[[itemKey]]
          self$results$text1$setContent(item)
        }
        
        if (isTRUE(self$options$gamma)) {
          gammaKey <- paste(modelKey, "gamma", sep="|")
          
          if (is.null(private$.modelCache[[gammaKey]])) {
            private$.modelCache[[gammaKey]] <- lca[["param"]][["gamma"]]
          }
          gamma <- private$.modelCache[[gammaKey]]
          options(max.print = 1000000)
          self$results$text4$setContent(gamma)
        }
        
        if (isTRUE(self$options$post)) {
          postKey <- paste(modelKey, "post", sep="|")
          
          if (is.null(private$.modelCache[[postKey]])) {
            private$.modelCache[[postKey]] <- lca[["posterior"]]
          }
          post <- private$.modelCache[[postKey]]
          options(max.print = 1000000)
          self$results$text2$setContent(post)
        }
        
        # Item probabilities by group plot(default:Measure.inv=TRUE)
        if (isTRUE(self$options$plot2)) {
          icKey <- paste(modelKey, "ic", sep="|")
          
          if (is.null(private$.modelCache[[icKey]])) {
            ic <- lca[["param"]][["rho"]]
            ic <- reshape2::melt(ic)
            colnames(ic) <- c("Class", "Level", "value", "Variable", "Group")
            private$.modelCache[[icKey]] <- ic
          }
          ic <- private$.modelCache[[icKey]]
          
          image2 <- self$results$plot2
          image2$setState(ic)
        }
        private$.clearMemory()
      },
      
      .plot1 = function(image, ...) {
        if (!self$options$plot1)
          return(FALSE)
        
        vars <- self$options$vars
        group <- self$options$group
        nc <- self$options$nc
        modelKey <- paste(paste(vars, collapse=","), nc, group, sep="|")
        
        if (is.null(private$.modelCache[[modelKey]])) {
          private$.modelCache[[modelKey]] <- private$.computeLCA()
        }
        lca <- private$.modelCache[[modelKey]]
        
        par(mfcol = c(3, 1))
        plot1 <- plot(lca, ask = FALSE)
        
        print(plot1)
        TRUE
      },
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        
        ic <- image2$state
        
        plot2 <- ggplot2::ggplot(ic, ggplot2::aes(x = Variable, y = value, fill = Level)) +
          ggplot2::geom_bar(stat = "identity", position = "stack") +
          ggplot2::facet_wrap(ggplot2::vars(Group, Class)) +
          ggplot2::scale_x_discrete("Variable", expand = c(0, 0)) +
          ggplot2::scale_y_continuous("Probability", expand = c(0, 0)) +
          ggplot2::theme_bw()
        
        plot2 <- plot2 + ggtheme
        
        if (self$options$angle > 0) {
          plot2 <- plot2 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        print(plot2)
        TRUE
      },
      
      .plot3 = function(image3, ggtheme, theme, ...) {
        if (!self$options$plot3)
          return(FALSE)
        
        vars <- self$options$vars
        group <- self$options$group
        nc <- self$options$nc
        modelKey <- paste(paste(vars, collapse=","), nc, group, "ip", sep="|")
        
        if (is.null(private$.modelCache[[modelKey]])) {
          private$.modelCache[[modelKey]] <- private$.computeIP()
        }
        mglca3 <- private$.modelCache[[modelKey]]
        
        icfKey <- paste(modelKey, "icf", sep="|")
        
        if (is.null(private$.modelCache[[icfKey]])) {
          icf <- mglca3[["param"]][["rho"]]
          icf <- reshape2::melt(icf)
          colnames(icf) <- c("Class", "Level", "value", "Variable", "Group")
          private$.modelCache[[icfKey]] <- icf
        }
        ic <- private$.modelCache[[icfKey]]
        
        plot3 <- ggplot2::ggplot(ic, ggplot2::aes(x = Variable, y = value, fill = Level)) +
          ggplot2::geom_bar(stat = "identity", position = "stack") +
          ggplot2::facet_wrap(ggplot2::vars(Group, Class)) +
          ggplot2::scale_x_discrete("Variable", expand = c(0, 0)) +
          ggplot2::scale_y_continuous("Probability", expand = c(0, 0)) +
          ggplot2::theme_bw()
        
        plot3 <- plot3 + ggtheme
        
        if (self$options$angle > 0) {
          plot3 <- plot3 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        
        print(plot3)
        TRUE
      },
      
      .cleanData = function() {
        vars <- self$options$vars
        covs <- self$options$covs
        
        groupVarName <- self$options$group
        
        n <- nrow(self$data)
        data <- list()
        
        data[[groupVarName]] <- jmvcore::toNumeric(self$data[[groupVarName]])
        
        for (item in vars)
          data[[item]] <- jmvcore::toNumeric(self$data[[item]])
        
        if (!is.null(covs)) {
          for (item in covs)
            data[[item]] <- jmvcore::toNumeric(self$data[[item]])
        }
        
        attr(data, 'row.names') <- seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        
        return(data)
      },
      
      .computeLCA = function() {
        data <- private$.dataCache
        if (is.null(data)) {
          data <- private$.cleanData()
          private$.dataCache <- data
        }
        
        vars <- self$options$vars
        group <- self$options$group
        covs <- self$options$covs
        nc <- self$options$nc
        
        vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
        vars <- paste0(vars, collapse = ',')
        
        formula <- as.formula(paste0('glca::item(', vars, ')~1'))
        
        if (length(self$options$covs) >= 1) {
          covs <- self$options$covs
          covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
          covs <- paste0(covs, collapse = '+')
          
          formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
        }
        
        group <- data[, 1]
        
        lca <- glca::glca(
          formula = formula,
          group = group,
          data = data,
          nclass = nc,
          seed = 1
        )
        
        return(lca)
      },
      
      .computeMEAI = function() {
        vars <- self$options$vars
        group <- self$options$group
        nc <- self$options$nc
        modelKey <- paste(paste(vars, collapse=","), nc, group, sep="|")
        
        if (is.null(private$.modelCache[[modelKey]])) {
          private$.modelCache[[modelKey]] <- private$.computeLCA()
        }
        mglca2 <- private$.modelCache[[modelKey]]
        
        data <- private$.dataCache
        if (is.null(data)) {
          data <- private$.cleanData()
          private$.dataCache <- data
        }
        
        vars <- self$options$vars
        covs <- self$options$covs
        
        vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
        vars <- paste0(vars, collapse = ',')
        
        formula <- as.formula(paste0('glca::item(', vars, ')~1'))
        
        if (length(self$options$covs) >= 1) {
          covs <- self$options$covs
          covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
          covs <- paste0(covs, collapse = '+')
          
          formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
        }
        
        group <- data[, 1]
        
        # measure.inv=FALSE 
        miFalseKey <- paste(modelKey, "mi_false", sep="|")
        if (is.null(private$.modelCache[[miFalseKey]])) {
          mglca3 <- try(glca::glca(
            formula = formula,
            group = group,
            data = data,
            nclass = nc,
            measure.inv = FALSE,
            seed = 1
          ))
          
          if (jmvcore::isError(mglca3)) {
            err_string <- stringr::str_interp(" Error in if (maxdiff < eps) break : Covariates can not be computed.")
            stop(err_string)
          }
          
          private$.modelCache[[miFalseKey]] <- mglca3
        } else {
          mglca3 <- private$.modelCache[[miFalseKey]]
        }
        
        mi <- glca::gofglca(mglca2, mglca3, test = 'chisq')
        
        mi.g <- mi[["gtable"]] 
        
        if (is.null(mi$dtable)) {
          mi.d <- NULL
        } else {
          mi.d <- mi[["dtable"]] 
        }
        
        retlist <- list(mi.g = mi.g, mi.d = mi.d)
        return(retlist)
      },
      
      .computeEQ = function() {
        vars <- self$options$vars
        group <- self$options$group
        nc <- self$options$nc
        modelKey <- paste(paste(vars, collapse=","), nc, group, sep="|")
        
        if (is.null(private$.modelCache[[modelKey]])) {
          private$.modelCache[[modelKey]] <- private$.computeLCA()
        }
        lca <- private$.modelCache[[modelKey]]
        
        data <- private$.dataCache
        if (is.null(data)) {
          data <- private$.cleanData()
          private$.dataCache <- data
        }
        
        vars <- self$options$vars
        covs <- self$options$covs
        
        vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
        vars <- paste0(vars, collapse = ',')
        
        formula <- as.formula(paste0('glca::item(', vars, ')~1'))
        
        if (length(self$options$covs) >= 1) {
          covs <- self$options$covs
          covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
          covs <- paste0(covs, collapse = '+')
          
          formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
        }
        
        group <- data[, 1]
        
        model4Key <- paste(modelKey, "model4", sep="|")
        if (is.null(private$.modelCache[[model4Key]])) {
          mglca4 <- glca::glca(
            formula = formula,
            group = group,
            data = data,
            nclass = nc,
            seed = 1
          )
          private$.modelCache[[model4Key]] <- mglca4
        } else {
          mglca4 <- private$.modelCache[[model4Key]]
        }
        
        model5Key <- paste(modelKey, "model5", sep="|")
        if (is.null(private$.modelCache[[model5Key]])) {
          mglca5 <- try(glca::glca(
            formula = formula,
            group = group,
            data = data,
            nclass = nc,
            coeff.inv = FALSE,
            seed = 1
          ))
          
          if (jmvcore::isError(mglca5)) {
            err_string <- stringr::str_interp(" Error in if (maxdiff < eps) break : Covariates can not be computed.")
            stop(err_string)
          }
          
          private$.modelCache[[model5Key]] <- mglca5
        } else {
          mglca5 <- private$.modelCache[[model5Key]]
        }
        
        ci <- glca::gofglca(lca, mglca4, mglca5, test = 'chisq')
        
        ci.g <- ci[["gtable"]] 
        
        if (is.null(ci$dtable)) {
          ci.d <- NULL
        } else {
          ci.d <- ci[["dtable"]] 
        }
        
        retlist <- list(ci.g = ci.g, ci.d = ci.d)
        return(retlist)
      },
      
      .computeIP = function() {
        data <- private$.dataCache
        if (is.null(data)) {
          data <- private$.cleanData()
          private$.dataCache <- data
        }
        
        vars <- self$options$vars
        group <- self$options$group
        covs <- self$options$covs
        nc <- self$options$nc
        
        vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
        vars <- paste0(vars, collapse = ',')
        
        formula <- as.formula(paste0('glca::item(', vars, ')~1'))
        
        if (length(self$options$covs) >= 1) {
          covs <- self$options$covs
          covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
          covs <- paste0(covs, collapse = '+')
          
          formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
        }
        
        group <- data[, 1]
        
        # measure.inv=FALSE 
        mglca3 <- glca::glca(
          formula = formula,
          group = group,
          data = data,
          nclass = nc,
          measure.inv = FALSE,
          seed = 1
        )
        return(mglca3)
      },
      
      .clearMemory = function() {
        gc()
      }
    )
  )
