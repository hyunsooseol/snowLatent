
lcaClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "lcaClass",
    inherit = lcaBase,
    private = list(
      .modelCache = NULL,       
      .fitCache = NULL,         
      .compCache = NULL,        
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
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowLatent/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        
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
        if (is.null(self$options$vars) || length(self$options$vars) < 3)
          return()
        
        data <- private$.cleanData()
        
        if (is.null(private$.modelCache)) {
          private$.modelCache <- private$.computeLCA()
        }
        
        self$results$text$setContent(private$.modelCache)
        
        private$.populateFitTable()
        private$.populateModelTable()
        private$.populateRelTable()
        private$.populateClassTable()
        private$.populateItemTable()
        
        if (length(self$options$covs) >= 1) {
          private$.populateCoefTable()
        }
        
        private$.handlePosterior()
        
        if (isTRUE(self$options$gamma)) {
          gamma <- private$.modelCache[["param"]][["gamma"]]
          options(max.print = 1000000)
          self$results$text2$setContent(gamma)
        }
      },
      
      .populateFitTable = function() {
        if (is.null(private$.fitCache)) {
          v <- c("loglik", "AIC", "BIC", "entropy", "df", "Gsq")
          gof_data <- private$.modelCache$gof
          private$.fitCache <- as.data.frame(replace(gof_data[v], sapply(gof_data[v], is.null), list(NA_real_)))
        }
        
        table <- self$results$fit
        table$setRow(
          rowNo = 1,
          values = list(
            class = self$options$nc,
            loglik = private$.fitCache[1, "loglik"],
            AIC = private$.fitCache[1, "AIC"],
            BIC = private$.fitCache[1, "BIC"],
            entropy = private$.fitCache[1, "entropy"],
            df = private$.fitCache[1, "df"],
            gsq = private$.fitCache[1, "Gsq"]
          )
        )
      },
      
      .populateModelTable = function() {
        if (is.null(private$.compCache)) {
          private$.compCache <- private$.computeModelComparison()
        }
        
        if (is.null(private$.compCache$gtable))
          return()
        
        table <- self$results$comp
        g <- as.data.frame(private$.compCache$gtable)
        
        #self$results$text$setContent(g)
        
        for (i in seq_len(nrow(g))) {
          table$addRow(
            rowKey = rownames(g)[i],
            values = list(
              class = g[i, 7],
              loglik = g[i, 1],
              aic = g[i, 2],
              bic = g[i, 3],
              entropy = g[i, 4],
              df = g[i, 5],
              gsq = g[i, 6]
            )
          )
        }
      },
      
      .populateRelTable = function() {
        if (self$options$nc < 3 || is.null(private$.compCache) || is.null(private$.compCache$dtable))
          return()
        
        table <- self$results$rel
        d <- as.data.frame(private$.compCache$dtable)
        
        for (i in seq_len(nrow(d))) {
          table$addRow(
            rowKey = rownames(d)[i],
            values = list(
              class = d[i, 6],
              para = d[i, 1],
              loglik = d[i, 2],
              df = d[i, 3],
              dev = d[i, 4],
              p = d[i, 5]
            )
          )
        }
      },
      
      .populateClassTable = function() {
        gam <- lapply(private$.modelCache$posterior, colMeans)
        gam <- as.data.frame(gam$ALL)
        
        table <- self$results$cp
        for (i in seq_len(nrow(gam))) {
          table$addRow(rowKey = rownames(gam)[i], values = list(value = gam[i, 1]))
        }
      },
      
      .populateItemTable = function() {
        tables <- self$results$item
        vars <- self$options$vars
        item <- private$.modelCache[["param"]][["rho"]][["ALL"]]
        
        for (i in seq_along(vars)) {
          table <- tables[[i]]
          names <- row.names(item[[vars[i]]])
          dims <- colnames(item[[vars[i]]])
          
          for (dim in dims) {
            table$addColumn(name = paste0(dim),
                            type = 'text',
                            combineBelow = TRUE)
          }
          for (name in names) {
            row <- list()
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- item[[vars[i]]][name, j]
            }
            table$addRow(rowKey = name, values = row)
          }
        }
      },
      
      .populateCoefTable = function() {
        table <- self$results$coef
        coef <- coef(private$.modelCache)
        coef <- coef[[1]]
        codf <- do.call("rbind", lapply(coef, as.data.frame))
        names <- dimnames(codf)[[1]]
        
        for (name in names) {
          row <- list()
          row[["odds"]] <- codf[name, 1]
          row[["co"]] <- codf[name, 2]
          row[["error"]] <- codf[name, 3]
          row[["t"]] <- codf[name, 4]
          row[["p"]] <- codf[name, 5]
          table$addRow(rowKey = name, values = row)
        }
      },
      
      .handlePosterior = function() {
        data <- private$.cleanData()
        pos <- private$.modelCache$posterior$ALL
        
        if (isTRUE(self$options$member)) {
          mem <- as.numeric(factor(apply(pos, 1, which.max)))
          mem <- as.factor(mem)
          
          if (self$options$member && self$results$member$isNotFilled()) {
            
            self$results$member$setRowNums(rownames(self$data))
            self$results$member$setValues(mem)
            
          }
        }
        
        if (isTRUE(self$options$post)) {
          if (self$options$post && self$results$post$isNotFilled()) {
            keys <- 1:self$options$nc
            measureTypes <- rep("continuous", self$options$nc)
            
            titles <- paste("Class", keys)
            descriptions <- paste("Class", keys)
            
            self$results$post$set(
              keys = keys,
              titles = titles,
              descriptions = descriptions,
              measureTypes = measureTypes
            )
            self$results$post$setRowNums(rownames(self$data))
            
            for (i in 1:self$options$nc) {
              scores <- as.numeric(pos[, i])
              self$results$post$setValues(index = i, scores)
            }
          }
        }
      },
      
      .computeModelComparison = function() {
        data <- private$.cleanData()
        nc <- self$options$nc
        covs <- self$options$covs
        vars <- self$options$vars
        
        vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
        vars <- paste0(vars, collapse = ',')
        formula <- as.formula(paste0('glca::item(', vars, ') ~ 1'))
        
        if (length(covs) >= 1) {
          covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
          covs <- paste0(covs, collapse = '+')
          formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
        }
        
        args <- list(test = "chisq")
        
        for (n in 2:self$options$nc)
          args[[n]] <- glca::glca(
            formula = formula,
            data = data,
            nclass = n,
            seed = 1
          )
        
        res <- do.call(glca::gofglca, args)
        
        gtable <- res[["gtable"]] # Absolute model fit
        
        if (is.null(res$dtable)) {
          dtable <- NULL
        } else {
          dtable <- res[["dtable"]] # Relative model fit
        }
        
        if (!is.null(gtable)) {
          gtable <- as.data.frame(gtable)
          new <- c(2:self$options$nc)
          gtable <- cbind(gtable, new)
        }
        
        if (!is.null(dtable)) {
          dtable <- as.data.frame(dtable)
          new <- c(2:self$options$nc)
          dtable <- cbind(dtable, new)
        }
        
        elbow <- NULL
        if (self$options$nc > 2 && !is.null(gtable)) {
          out1 <- gtable[, c(2:3)]
          cla <- c(2:self$options$nc)
          out1 <- data.frame(out1, cla)
          #self$results$text$setContent(out1)
          colnames(out1) <- c('AIC', 'BIC', 'Class')
          elbow <- reshape2::melt(
            out1,
            id.vars = 'Class',
            variable.name = "Fit",
            value.name = 'Value'
          )
          
          if (isTRUE(self$options$plot3)) {
            image2 <- self$results$plot3
            image2$setState(elbow)
          }
        }
        
        return(list(
          'gtable' = gtable,
          'dtable' = dtable,
          'elbow' = elbow
        ))
      },
      
      .plot1 = function(image, ...) {
        if (!self$options$plot1)
          return(FALSE)
        
        if (is.null(private$.modelCache))
          private$.modelCache <- private$.computeLCA()
        
        par(mfcol = c(2, 1))
        plot1 <- plot(private$.modelCache, ask = FALSE)
        print(plot1)
        TRUE
      },
      
      .plot2 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        ic <- image1$state
        
        plot2 <- ggplot2::ggplot(ic, ggplot2::aes(x = Class, y = value, fill = Level)) +
          ggplot2::geom_bar(stat = "identity", position = "stack") +
          ggplot2::facet_wrap(~ L1) +
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
        if (self$options$nc < 3)
          return()
        if (is.null(image2$state))
          return(FALSE)
        
        elbow <- image2$state
        plot3 <- ggplot2::ggplot(elbow, ggplot2::aes(x = Class, y = Value, color = Fit)) +
          ggplot2::geom_line(size = 1.1) +
          ggplot2::geom_point(size = 3) +
          ggplot2::scale_x_continuous(breaks = seq(1, length(elbow$Class), by = 1))
        
        plot3 <- plot3 + ggtheme
        print(plot3)
        TRUE
      },
      
      .cleanData = function() {
        data <- list()
        
        if (!is.null(self$options$covs))
          for (cov in self$options$covs)
            data[[cov]] <- jmvcore::toNumeric(self$data[[cov]])
        
        for (var in self$options$vars)
          data[[var]] <- jmvcore::toNumeric(self$data[[var]])
        
        attr(data, 'row.names') <- seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        
        return(data)
      },
      
      .computeLCA = function() {
        data <- private$.cleanData()
        nc <- self$options$nc
        covs <- self$options$covs
        vars <- self$options$vars
        
        vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
        vars <- paste0(vars, collapse = ',')
        formula <- as.formula(paste0('glca::item(', vars, ') ~ 1'))
        
        if (length(covs) >= 1) {
          covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
          covs <- paste0(covs, collapse = '+')
          formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
        }
        
        lca <- glca::glca(
          formula = formula,
          data = data,
          nclass = nc,
          seed = 1
        )
        
        if (isTRUE(self$options$plot2)) {
          ic <- reshape2::melt(lca$param$rho)
          colnames(ic) <- c("Class", "Level", "value", "L1")
          image1 <- self$results$plot2
          image1$setState(ic)
        }
        
        return(lca)
      }
    )
  )