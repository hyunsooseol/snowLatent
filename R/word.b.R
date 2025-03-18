
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @export

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
        
        # self$results$instructions$setContent(
        #   "<html>
        #     <head>
        #     </head>
        #     <body>
        #     <div class='instructions'>
        #     <p>____________________________________________________________________________________</p>
        #     <p> Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowLatent/issues'  target = '_blank'>GitHub.</a></p>
        #     <p>____________________________________________________________________________________</p>
        #     
        #     </div>
        #     </body>
        #     </html>"
        # )
        
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title="Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowLatent/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'
              
            )
            
          )
        )         
        
         if(isTRUE(self$options$plot)){
           width <- self$options$width
           height <- self$options$height
           self$results$plot$setSize(width, height)
         }

         if(isTRUE(self$options$plot1)){
           width <- self$options$width1
           height <- self$options$height1
           self$results$plot1$setSize(width, height)
         }

      },
      
      #-----------------------------------------------------
      
      .run = function() {
        
        
        if (is.null(self$options$words) | is.null(self$options$freq)) return()
        
         # words <- self$options$words
         # freq <- self$options$freq
        # 
        # minf <- self$options$minf
        # max <- self$options$max
        # min <- self$options$min
        # rot <- self$options$rot
        # 
        # data <- self$data
        # data <- jmvcore::naOmit(data)
        # data <- as.data.frame(data)
       
        #data$words <- as.character(data$words)
        #data$freq <- as.numeric(as.character(data$freq)
       
        #Wordcloud analysis---
        # w <- wordcloud::wordcloud(words = as.character(data[,words]),
        #                           freq = as.numeric(as.character(data[,freq]),
        #                           min.freq = minf,
        #                           scale=c(max, min),
        #                           rot.per=rot,
        #                           colors=RColorBrewer::brewer.pal(8, "Dark2")))
        # 
        # self$results$text$setContent(w) 
        #  # plot data function---------
        #  
        #  image<-self$results$plot
        #  image$setState(w)
      
        # Most frequent words---
        # Example---
        # barplot(data[1:10,]$freq, 
        #         las = 2, 
        #         names.arg = data[1:10,]$word,
        #         col ="lightblue", 
        #         main ="Most frequent words",
        #         ylab = "Word frequencies")
       
        
        },        
      
      .plot = function(image,ggtheme, theme,...) {
 
    if (is.null(self$options$words) | is.null(self$options$freq)) return()
        
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
        
        colors <- RColorBrewer::brewer.pal(8, "Dark2")
        
        set.seed(1234)
        plot <- wordcloud::wordcloud(words = as.character(data[,words]),
                                  freq = as.numeric(as.character(data[,freq])),
                                  min.freq = minf,
                                  max.words = maxw,
                                  scale=c(max, min),
                                  rot.per=rot,
                                  random.order = FALSE,
                                  colors=colors)
        
        print(plot)
        TRUE
      },
    
    .plot1 = function(image,ggtheme, theme,...) {
      
      if (is.null(self$options$words) | is.null(self$options$freq)) return()
      
      # words <- self$options$words
      # freq <- self$options$freq
      
      maxn <- self$options$maxn
    
      data <- self$data
      data <- jmvcore::naOmit(data)
      data <- as.data.frame(data)
     
      #Change the name of variables---
      names(data) <- c("word", "freq")
      #self$results$text$setContent(data[1:maxn,]$freq)
      Words<- data[1:maxn,]$word
      Words<- as.vector(Words) 
      
       Frequency<- data[1:maxn,]$freq
       Frequency<- as.vector(Frequency)
       
       df<- data.frame(Words, Frequency)
        
       #self$results$text$setContent(df)
      
       df$Words <- factor(df$Words, levels = df$Words[order(df$Frequency)])
       Words<- stats::reorder(Words, dplyr::desc(Frequency))
       
       library(ggplot2)
       set.seed(1234)
       plot1<- ggplot2::ggplot(data=df, ggplot2::aes(x=Words, y=Frequency)) +
         ggplot2::geom_bar(stat = "identity", fill = "steelblue")+
         ggplot2::geom_text(aes(label=Frequency), vjust=1.6, color="white", size=3.5)
      
      plot1 <- plot1+ggtheme
      
      if (self$options$angle > 0) {
        plot1 <- plot1 + ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = self$options$angle, hjust = 1
          )
        )
      }
      
      print(plot1)
      TRUE  
    }
 
    )
)