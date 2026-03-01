profileClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "profileClass",
  inherit = profileBase,
  private = list(
    .htmlwidget = NULL,

    # ------------------------------------------------------------
    # Helpers: error bars (t-based 95% CI only)
    # ------------------------------------------------------------
    .err_df = function(df, value_col = "Value") {
      # df: long data frame with columns Group, Variable, Value

      # mean
      m <- aggregate(df[[value_col]],
                     by = list(Group = df$Group, Variable = df$Variable),
                     FUN = mean, na.rm = TRUE)
      names(m)[3] <- "mean"

      # sd
      s <- aggregate(df[[value_col]],
                     by = list(Group = df$Group, Variable = df$Variable),
                     FUN = stats::sd, na.rm = TRUE)
      names(s)[3] <- "sd"

      # n
      n <- aggregate(df[[value_col]],
                     by = list(Group = df$Group, Variable = df$Variable),
                     FUN = function(x) sum(!is.na(x)))
      names(n)[3] <- "n"

      out <- Reduce(function(a,b) merge(a,b, by = c("Group","Variable"), all = TRUE),
                    list(m, s, n))

      out$se <- out$sd / sqrt(out$n)

      # t-based 95% CI half-width: t_{.975, df=n-1} * SE
      out$df <- pmax(out$n - 1, 1)
      out$tcrit <- stats::qt(0.975, df = out$df)
      out$ci <- out$tcrit * out$se

      out
    },

    .add_errorbars = function(p, errdf) {

      if (isTRUE(self$options$ci)) {

        # guard: drop rows where CI cannot be computed
        errdf <- errdf[is.finite(errdf$ci) & is.finite(errdf$mean), , drop = FALSE]

        p <- p + ggplot2::geom_errorbar(
          data = errdf,
          inherit.aes = FALSE,
          ggplot2::aes(
            x = Variable,
            ymin = mean - ci,
            ymax = mean + ci,
            color = factor(Group),
            group = Group
          ),
          width = 0.10,
          linewidth = 0.7,
          linetype = "dashed"
        )
      }

      p
    },

    .init = function() {

      private$.htmlwidget <- HTMLWidget$new()

      if (is.null(self$data) | is.null(self$options$vars) | is.null(self$options$group)) {
        self$results$instructions$setVisible(visible = TRUE)
      }

      self$results$instructions$setContent(
        private$.htmlwidget$generate_accordion(
          title="Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>95% confidence intervals are shown for profile plots only and are not applied to box plot.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowLatent/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        )
      )
    },

    .run = function() {

      if (is.null(self$data) | is.null(self$options$vars) | is.null(self$options$group))
        return()

      group <- self$options$group
      vars  <- self$options$vars
      data  <- self$data
      data  <- jmvcore::naOmit(data)

      formula <- jmvcore::constructFormula(self$options$group, self$options$vars)
      formula <- as.formula(formula)

      #### ANALYSIS##############################################
      group.means <- MASS::lda(formula, data=data)
      ########################################################

      # --------------------------------------------------------
      # Group means table
      # --------------------------------------------------------
      gm    <- group.means$means
      names <- dimnames(gm)[[1]]

      table <- self$results$mc
      for (i in seq_along(vars)) {
        var <- vars[[i]]
        table$addColumn(name = paste0(var),
                        type = 'number',
                        format = 'zto')
      }

      for (name in names) {
        row <- list()
        for (j in seq_along(vars)) {
          var <- vars[[j]]
          row[[var]] <- gm[name, j]
        }
        table$addRow(rowKey=name, values=row)
      }

      # --------------------------------------------------------
      # plot1: mean profile (state = list(mean, raw))
      # --------------------------------------------------------
      plotMean1 <- reshape2::melt(gm, id.vars=self$options$group)
      colnames(plotMean1) <- c("Group","Variable","Value")

      raw1 <- reshape2::melt(data[, c(group, vars), drop=FALSE], id.vars = group)
      colnames(raw1) <- c("Group","Variable","Value")

      image1 <- self$results$plot1
      image1$setState(list(mean = plotMean1, raw = raw1))

      # --------------------------------------------------------
      # plot2: box plot (use only group+vars)
      # --------------------------------------------------------
      if (isTRUE(self$options$plot2)) {

        df2 <- data[, c(group, vars), drop = FALSE]
        df2 <- jmvcore::naOmit(df2)

        d <- reshape2::melt(df2, id.vars = group)
        colnames(d) <- c("Group","Variable","Value")

        image2 <- self$results$plot2
        image2$setState(d)
      }

      # --------------------------------------------------------
      # plot3: z profile (state = list(mean, raw_z))
      # --------------------------------------------------------
      if (isTRUE(self$options$plot3)) {

        dfZ <- data[, c(group, vars), drop = FALSE]
        dfZ <- jmvcore::naOmit(dfZ)

        # numeric + z standardize within variable (whole sample)
        for (v in vars) {
          dfZ[[v]] <- jmvcore::toNumeric(dfZ[[v]])
          dfZ[[v]] <- as.numeric(scale(dfZ[[v]]))
        }

        # group means of z
        zgm <- aggregate(
          dfZ[, vars, drop = FALSE],
          by = list(Group = dfZ[[group]]),
          FUN = mean,
          na.rm = TRUE
        )

        plotMean3 <- reshape2::melt(zgm, id.vars = "Group")
        colnames(plotMean3) <- c("Group","Variable","Value")

        zlong <- reshape2::melt(dfZ, id.vars = group)
        colnames(zlong) <- c("Group","Variable","Value")

        image3 <- self$results$plot3
        image3$setState(list(mean = plotMean3, raw = zlong))
      }
    },

    # ------------------------------------------------------------
    # plot1: mean profile
    # ------------------------------------------------------------
    .plot1 = function(image, ggtheme, theme, ...) {
      if (is.null(image$state))
        return(FALSE)

      st       <- image$state
      plotMean <- st$mean
      plotRaw  <- st$raw

      p <- ggplot2::ggplot(
        plotMean,
        ggplot2::aes(x=Variable, y=Value, group=Group)
      ) +
        ggplot2::geom_line(linewidth=1.2, ggplot2::aes(color=factor(Group))) +
        ggplot2::xlab("Variable") +
        ggplot2::ylab("Mean value") +
        ggplot2::labs(color="Group") +
        ggtheme

      # points
      if (isTRUE(self$options$points)) {
        p <- p + ggplot2::geom_point(size=4, ggplot2::aes(color=factor(Group)))
      }

      # t-based 95% CI error bars (only)
      if (isTRUE(self$options$ci)) {
        errdf <- private$.err_df(plotRaw, "Value")
        p <- private$.add_errorbars(p, errdf)
      }

      if (self$options$angle > 0) {
        p <- p + ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle=self$options$angle, hjust=1)
        )
      }

      print(p)
      TRUE
    },

    # ------------------------------------------------------------
    # plot3: z profile
    # ------------------------------------------------------------
    .plot3 = function(image3, ggtheme, theme, ...) {
      if (is.null(image3$state))
        return(FALSE)

      st       <- image3$state
      plotMean <- st$mean
      plotRaw  <- st$raw

      p <- ggplot2::ggplot(
        plotMean,
        ggplot2::aes(x=Variable, y=Value, group=Group)
      ) +
        ggplot2::geom_hline(yintercept=0, linetype="dashed", linewidth=0.6) +
        ggplot2::geom_line(linewidth=1.2, ggplot2::aes(color=factor(Group))) +
        ggplot2::xlab("Variable") +
        ggplot2::ylab("Standardized value (z)") +
        ggplot2::labs(color="Group") +
        ggtheme

      # points
      if (isTRUE(self$options$points)) {
        p <- p + ggplot2::geom_point(size=4, ggplot2::aes(color=factor(Group)))
      }

      # t-based 95% CI error bars (only)
      if (isTRUE(self$options$ci)) {
        errdf <- private$.err_df(plotRaw, "Value")
        p <- private$.add_errorbars(p, errdf)
      }

      if (self$options$angle > 0) {
        p <- p + ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle=self$options$angle, hjust=1)
        )
      }

      print(p)
      TRUE
    },

    # ------------------------------------------------------------
    # plot2: box plot
    # ------------------------------------------------------------
    .plot2 = function(image2, ggtheme, theme, ...) {

      if (is.null(image2$state))
        return(FALSE)

      d <- image2$state

      plot2 <- ggplot2::ggplot(d,
                               ggplot2::aes(x = Group,
                                            y = Value,
                                            color = Group)) +
        ggplot2::geom_boxplot() +
        ggplot2::facet_wrap(~Variable) +
        ggtheme

      # points (jitter overlay)
      if (isTRUE(self$options$points)) {
        plot2 <- plot2 + ggplot2::geom_jitter(width = 0.15, alpha = 0.45, size = 1.6)
      }

      # me 옵션이 TRUE일 때만 평균값 표시
      if (isTRUE(self$options$me)) {
        plot2 <- plot2 +
          ggplot2::stat_summary(fun = mean,
                                geom = "point",
                                shape = 23,
                                size = 3,
                                fill = "red",
                                color = "red")
      }

      if (self$options$angle > 0) {
        plot2 <- plot2 + ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = self$options$angle, hjust = 1
          )
        )
      }

      print(plot2)
      TRUE
    }
  )
)
