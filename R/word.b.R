
# This file is a generated template, your changes will not be overwritten
#' @importFrom  wordcloud wordcloud
#' @importFrom RColorBrewer::brewer.pal
#' @import ggplot2
#' @import wordcloud
#' @import RColorBrewer
#' @export

wordClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "wordClass",
    inherit = wordBase,
    private = list(
      
      .init = function() {
        
        if (is.null(self$data) | is.null(self$options$words) | is.null(self$options$freq)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(
          "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>____________________________________________________________________________________</p>
            <p>1. You must enter row number(s) of <b>4</b> or more.</p>
            <p>2. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowLatent/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
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
        
         words <- self$options$words
         freq <- self$options$freq
        # 
        # minf <- self$options$minf
        # max <- self$options$max
        # min <- self$options$min
        # rot <- self$options$rot
        # 
        data <- self$data
        data <- jmvcore::naOmit(data)
        data <- as.data.frame(data)
       
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
      
      words <- self$options$words
      freq <- self$options$freq
      
      maxn <- self$options$maxn
    
      data <- self$data
      data <- jmvcore::naOmit(data)
      data <- as.data.frame(data)
     
      #Change the name of variables---
      names(data) <- c("word", "freq")
      
      
       plot1<- barplot(data[1:maxn,]$freq, 
               las = 2, 
               names.arg = data[1:maxn,]$word,
               main ="Most frequent words",
               ylab = "Word frequencies")
      
      plot1 <- plot1+ggtheme
      print(plot1)
      TRUE  
    }
 
    )
)