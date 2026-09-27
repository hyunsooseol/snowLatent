
wordClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "wordClass",
  inherit = wordBase,
  private = list(
    
    .htmlwidget = NULL,
    
    .init = function() {
      
      private$.htmlwidget <- HTMLWidget$new()
      
      if (is.null(self$data) | is.null(self$options$words) | is.null(self$options$freq)) {
        self$results$instructions$setVisible(visible = TRUE)
      }
      
      self$results$instructions$setContent(
        private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowLatent/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        )
      )
      
    },
    
    #-----------------------------------------------------
    
    .run = function() {
      
      if (is.null(self$options$words) | is.null(self$options$freq))
        return()
      
    },
    
    #-----------------------------------------------------
    # Wordcloud plot
    
    .plot = function(image, ggtheme, theme, ...) {
      
      if (is.null(self$options$words) | is.null(self$options$freq))
        return()
      
      words <- self$options$words
      freq <- self$options$freq
      
      minf <- self$options$minf
      max <- self$options$max
      min <- self$options$min
      maxw <- self$options$maxw
      rot <- self$options$rot
      
      data <- self$data
      data <- jmvcore::naOmit(data)
      data <- as.data.frame(data)
      
      wordValues <- as.character(data[[words]])
      freqValues <- suppressWarnings(
        as.numeric(as.character(data[[freq]]))
      )
      
      valid <- !is.na(wordValues) &
        !is.na(freqValues) &
        is.finite(freqValues)
      
      wordValues <- wordValues[valid]
      freqValues <- freqValues[valid]
      
      if (length(wordValues) == 0)
        return()
      
      colors <- RColorBrewer::brewer.pal(8, "Dark2")
      
      set.seed(1234)
      
      plot <- wordcloud::wordcloud(
        words = wordValues,
        freq = freqValues,
        min.freq = minf,
        max.words = maxw,
        scale = c(max, min),
        rot.per = rot,
        random.order = FALSE,
        colors = colors
      )
      
      print(plot)
      TRUE
    },
    
    #-----------------------------------------------------
    # Word frequencies plot
    
    .plot1 = function(image, ggtheme, theme, ...) {
      
      if (is.null(self$options$words) | is.null(self$options$freq))
        return()
      
      words <- self$options$words
      freq <- self$options$freq
      maxn <- self$options$maxn
      
      data <- self$data
      data <- jmvcore::naOmit(data)
      data <- as.data.frame(data)
      
      # Selected variables
      Words <- as.character(data[[words]])
      
      Frequency <- suppressWarnings(
        as.numeric(as.character(data[[freq]]))
      )
      
      # Keep valid values only
      valid <- !is.na(Words) &
        nzchar(Words) &
        !is.na(Frequency) &
        is.finite(Frequency)
      
      Words <- Words[valid]
      Frequency <- Frequency[valid]
      
      if (length(Words) == 0)
        return()
      
      df <- data.frame(
        Words = Words,
        Frequency = Frequency,
        stringsAsFactors = FALSE
      )
      
      # Sort by frequency in descending order
      df <- df[
        order(df$Frequency, decreasing = TRUE),
        ,
        drop = FALSE
      ]
      
      # Select top N words
      if (!is.null(maxn) && is.finite(maxn) && maxn > 0) {
        n <- min(as.integer(maxn), nrow(df))
        df <- df[seq_len(n), , drop = FALSE]
      }
      
      # Preserve frequency order on the x-axis
      df$Words <- factor(
        df$Words,
        levels = rev(unique(df$Words))
      )
      
      set.seed(1234)
      
      plot1 <- ggplot2::ggplot(
        data = df,
        ggplot2::aes(
          x = Words,
          y = Frequency
        )
      ) +
        ggplot2::geom_bar(
          stat = "identity",
          fill = "steelblue"
        ) +
        ggplot2::geom_text(
          ggplot2::aes(label = Frequency),
          vjust = 1.6,
          color = "white",
          size = 3.5
        )
      
      plot1 <- plot1 + ggtheme
      
      if (self$options$angle > 0) {
        plot1 <- plot1 +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = self$options$angle,
              hjust = 1
            )
          )
      }
      
      print(plot1)
      TRUE
    }
    
  )
)