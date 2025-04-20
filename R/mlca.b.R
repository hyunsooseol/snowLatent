
mlcaClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "mlcaClass",
    inherit = mlcaBase,
    private = list(
      .cache = list(),  
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
            '<li>The result table does not printed if the results from glca R package are not available.</li>',
            '<li>The Raltive model fit option requires the number of clusters to be greater than 2. Check box should be unchecked.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowLatent/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        
        if (self$options$rel1)
          self$results$rel1$setNote("Note", "p: Chi-Square p value.")
        
        if (self$options$gof)
          self$results$gof$setNote("Note", "Model 2: Coeff.inv=TRUE; Model 3: Coeff.inv=FALSE.")
        if (self$options$ci)
          self$results$ci$setNote("Note",
                                  "p: Chi-Square p value; Model 2:Coeff.inv=TRUE; Model 3: Coeff.inv=FALSE.")
        
        if (isTRUE(self$options$plot1)) {
          width <- self$options$width
          height <- self$options$height
          self$results$plot1$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot2)) {
          width <- self$options$width1
          height <- self$options$height1
          self$results$plot2$setSize(width, height)
        }
        if (isTRUE(self$options$plot3)) {
          width <- self$options$width2
          height <- self$options$height2
          self$results$plot3$setSize(width, height)
        }
      },
      
      .run = function() {
        if (is.null(self$options$group) || is.null(self$options$vars) ||
            length(self$options$vars) < 3) return()
        
        if (is.null(private$.dataCache)) {
          private$.dataCache <- private$.cleanData()
        }
        data <- private$.dataCache
        
        if (is.null(private$.cache$lca))
          private$.cache$lca <- private$.computeLCA()
        if (is.null(private$.cache$clu))
          private$.cache$clu <- private$.computeCLUST()
        if (is.null(private$.cache$inv))
          private$.cache$inv <- private$.computeINV()
        
        lca <- private$.cache$lca
        clu <- private$.cache$clu
        inv <- private$.cache$inv
        self$results$text$setContent(lca)
        
        if (isTRUE(self$options$co)) {
          co <- lca$coefficient
          self$results$text3$setContent(co)
        }
        
        # Model fit
        if (isTRUE(self$options$fit)) {
          table <- self$results$fit
          table$setRow(
            rowNo = 1,
            values = list(
              class   = self$options$nc,
              loglik  = lca$gof$loglik,
              aic     = lca$gof$aic,
              caic    = lca$gof$caic,
              bic     = lca$gof$bic,
              entropy = lca$gof$entropy,
              df      = lca$gof$df,
              gsq     = lca$gof$Gsq
            )
          )
        }
        
        if (isTRUE(self$options$comp1)) {
          table <- self$results$comp1
          if (!is.null(clu$gtable1)) {
            g <- as.data.frame(clu$gtable1)
            clusters <- 2:self$options$nclust
            
            for (i in seq_along(clusters)) {
              table$addRow(
                rowKey = rownames(g)[i],
                values = list(
                  cluster = clusters[i],
                  loglik  = g[i, 1],
                  aic     = g[i, 2],
                  caic    = g[i, 3],
                  bic     = g[i, 4],
                  entropy = g[i, 5],
                  df      = g[i, 6],
                  gsq     = g[i, 7]
                )
              )
            }
          }
        }
        
        if (isTRUE(self$options$rel1)) {
          table <- self$results$rel1
          if (!is.null(clu$dtable1)) {
            d <- as.data.frame(clu$dtable1)
            clusters <- 2:self$options$nclust
            for (i in seq_along(clusters)) {
              table$addRow(
                rowKey = rownames(d)[i],
                values = list(
                  cluster = clusters[i],
                  para    = d[i, 1],
                  loglik  = d[i, 2],
                  df      = d[i, 3],
                  dev     = d[i, 4],
                  p       = d[i, 5]
                )
              )
            }
          }
        }
        
        if (isTRUE(self$options$cla)) {
          table <- self$results$cla
          
          cla <- tryCatch({
            colMeans(do.call(rbind, lca[["posterior"]][["class"]]))
          }, error = function(e) {
            NULL
          })
          
          if (!is.null(cla)) {
            cla <- as.data.frame(cla)
            names <- dimnames(cla)[[1]]
            for (name in names) {
              row <- list()
              row[['value']] <- cla[name, 1]
              table$addRow(rowKey = name, values = row)
            }
          }
        }
        
        if (isTRUE(self$options$cross)) {
          table <- self$results$cross
          
          cross <- tryCatch({
            as.matrix(do.call(rbind, lapply(lca$posterior, colMeans)))
          }, error = function(e) {
            NULL
          })
          
          if (!is.null(cross)) {
            names <- dimnames(cross)[[1]]
            dims <- dimnames(cross)[[2]]
            
            for (dim in dims) {
              table$addColumn(name = paste0(dim), type = 'character')
            }
            for (name in names) {
              row <- list()
              for (j in seq_along(dims)) {
                row[[dims[j]]] <- cross[name, j]
              }
              table$addRow(rowKey = name, values = row)
            }
          }
        }
        
        if (isTRUE(self$options$mpc)) {
          table <- self$results$mpc
          
          mpc <- tryCatch({
            colMeans(lca$posterior$cluster)
          }, error = function(e) {
            NULL
          })
          
          if (!is.null(mpc)) {
            mpc <- as.data.frame(mpc)
            names <- dimnames(mpc)[[1]]
            
            for (name in names) {
              row <- list()
              row[['value']] <- mpc[name, 1]
              table$addRow(rowKey = name, values = row)
            }
          }
        }
        
        if (isTRUE(self$options$cpc)) {
          table <- self$results$cpc
          
          cpc <- tryCatch({
            lca$posterior$wclass
          }, error = function(e) {
            tryCatch({
              as.matrix(do.call(rbind, lapply(lca$posterior, colMeans)))
            }, error = function(e) {
              NULL
            })
          })
          
          if (!is.null(cpc)) {
            names <- dimnames(cpc)[[1]]
            dims <- dimnames(cpc)[[2]]
            
            for (dim in dims) {
              table$addColumn(name = paste0(dim), type = 'character')
            }
            for (name in names) {
              row <- list()
              for (j in seq_along(dims)) {
                row[[dims[j]]] <- cpc[name, j]
              }
              table$addRow(rowKey = name, values = row)
            }
          }
        }
        
        # Elbow 
        if (self$options$nclust > 2 && isTRUE(self$options$plot3)) {
          out1 <- clu$gtable1[, c(2:4)]
          cla <- c(2:self$options$nclust)
          out1 <- data.frame(out1, cla)
          colnames(out1) <- c('AIC', 'CAIC', 'BIC', 'Cluster')
          elbow <- reshape2::melt(
            out1,
            id.vars = 'Cluster',
            variable.name = "Fit",
            value.name = 'Value'
          )
          image2 <- self$results$plot3
          image2$setState(elbow)
        }
        
        if (isTRUE(self$options$gof) && !is.null(self$options$covs)) {
          table <- self$results$gof
          if (!is.null(inv$ci.g)) {
            g <- as.data.frame(inv$ci.g)
            for (name in rownames(g)) {
              table$addRow(
                rowKey = name,
                values = list(
                  loglik  = g[name, 1],
                  aic     = g[name, 2],
                  caic    = g[name, 3],
                  bic     = g[name, 4],
                  entropy = g[name, 5],
                  df      = g[name, 6],
                  gsq     = g[name, 7]
                )
              )
            }
          }
        }
        
        if (!is.null(self$options$covs) && isTRUE(self$options$ci)) {
          table <- self$results$ci
          if (!is.null(inv$ci.d)) {
            d <- as.data.frame(inv$ci.d)
            for (name in rownames(d)) {
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
        
        if (isTRUE(self$options$cn)) {
          member <- lca[["posterior"]][["cluster"]]
          m <- as.data.frame(member)
          m$Membership <- as.numeric(factor(apply(m, 1, which.max)))
          
          table <- self$results$cn
          names <- dimnames(m)[[1]]
          dims <- dimnames(m)[[2]]
          
          for (dim in dims) {
            table$addColumn(name = paste0(dim), type = 'number')
          }
          for (name in names) {
            row <- list()
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- m[name, j]
            }
            table$addRow(rowKey = name, values = row)
          }
        }
        
        if (isTRUE(self$options$plot2)) {
          image2 <- self$results$plot2
          ic <- lca[["param"]][["rho"]]
          ic <- reshape2::melt(ic)
          colnames(ic) <- c("Class", "Level", "value", "L1")
          image2$setState(ic)
        }
        
        if (isTRUE(self$options$item)) {
          tables <- self$results$item
          
          itemprob <- lca$param$rho
          vars <- self$options$vars
          
          for (i in seq_along(vars)) {
            item <- itemprob[[vars[i]]]
            
            table <- tables[[i]]
            names <- row.names(item)
            dims <- colnames(item)
            
            for (dim in dims) {
              table$addColumn(name = paste0(dim),
                              type = 'text',
                              combineBelow = TRUE)
            }
            for (name in names) {
              row <- list()
              for (j in seq_along(dims)) {
                row[[dims[j]]] <- item[name, j]
              }
              table$addRow(rowKey = name, values = row)
            }
          }
        }
        
        gc()
      },
      
      .plot1 = function(image, ...) {
        if (!self$options$plot1)
          return(FALSE)
        
        lca <- private$.cache$lca
        
        par(mfcol = c(3, 1))
        plot1 <- plot(lca, ask = FALSE)
        print(plot1)
        TRUE
      },
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (!self$options$plot2 || is.null(image2$state))
          return(FALSE)
        
        ic <- image2$state
        
        plot2 <- ggplot2::ggplot(ic, ggplot2::aes(x = Class, y = value, fill = Level)) +
          ggplot2::geom_bar(stat = "identity", position = "stack") +
          ggplot2::facet_wrap( ~ L1) +
          ggplot2::scale_x_discrete("Class", expand = c(0, 0)) +
          ggplot2::scale_y_continuous("Probability", expand = c(0, 0)) +
          ggplot2::theme_bw()
        plot2 <- plot2 + ggtheme
        
        if (self$options$angle > 0) {
          plot2 <- plot2 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        print(plot2)
        TRUE
      },
      
      .plot3 = function(image2, ggtheme, theme, ...) {
        if (!self$options$plot3 || is.null(image2$state))
          return(FALSE)
        
        elbow <- image2$state
        
        plot3 <- ggplot2::ggplot(elbow, ggplot2::aes(x = Cluster, y = Value, color = Fit)) +
          ggplot2::geom_line(size = 1.1) +
          ggplot2::geom_point(size = 3) +
          ggplot2::scale_x_continuous(breaks = seq(1, length(elbow$Cluster), by = 1))
        
        plot3 <- plot3 + ggtheme
        print(plot3)
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
        nclust <- self$options$nclust
        
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
        
        lca = glca::glca(
          formula = formula,
          group = group,
          data = data,
          nclass = nc,
          ncluster = nclust,
          seed = 1
        )
        return(lca)
      },
      
      .computeCLUST = function() {
        data <- private$.dataCache
        if (is.null(data)) {
          data <- private$.cleanData()
          private$.dataCache <- data
        }
        
        vars <- self$options$vars
        group <- self$options$group
        covs <- self$options$covs
        nc <- self$options$nc
        nclust <- self$options$nclust
        
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
        
        args <- list(test = "chisq")
        
        for (n in 2:self$options$nclust)
          args[[n]] <- glca::glca(
            formula = formula,
            group = group,
            data = data,
            nclass = nc,
            ncluster = n,
            seed = 1
          )
        
        res <- do.call(glca::gofglca, args)
        
        if (is.null(res$gtable)) {
          gtable1 <- NULL
        } else {
          gtable1 <- res[["gtable"]]
        }
        
        if (is.null(res$dtable)) {
          dtable1 <- NULL
        } else {
          dtable1 <- res[["dtable"]]
        }
        
        retlist <- list(gtable1 = gtable1, dtable1 = dtable1)
        return(retlist)
      },
      
      .computeINV = function() {
        data <- private$.dataCache
        if (is.null(data)) {
          data <- private$.cleanData()
          private$.dataCache <- data
        }
        
        vars <- self$options$vars
        group <- self$options$group
        covs <- self$options$covs
        nc <- self$options$nc
        nclust <- self$options$nclust
        
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
        
        ci.g <- NULL
        ci.d <- NULL
        
        if (!is.null(self$options$covs)) {
          lca0 = glca::glca(
            formula = as.formula(paste0('glca::item(', vars, ')~1')),
            group = group,
            data = data,
            nclass = nc,
            ncluster = nclust,
            seed = 1
          )
          
          lca = glca::glca(
            formula = formula,
            group = group,
            data = data,
            nclass = nc,
            ncluster = nclust,
            seed = 1
          )
          
          lca2 <- try(glca::glca(
            formula = formula,
            group = group,
            data = data,
            nclass = nc,
            ncluster = nclust,
            coeff.inv = FALSE,
            seed = 1
          ))
          
          if (jmvcore::isError(lca2)) {
            err_string <- stringr::str_interp(" Error in if (maxdiff < eps) break : Covariates can not be computed.")
            stop(err_string)
          }
          
          cf <- glca::gofglca(lca0, lca, lca2, test = "chisq")
          
          ci.g <- cf[["gtable"]]
          ci.d <- cf[["dtable"]]
        }
        
        retlist <- list(ci.g = ci.g, ci.d = ci.d)
        return(retlist)
      }
    )
  )
