
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import PupillometryR geom_flat_violin
#' @importFrom ggplot2 ggplot
#' @importFrom reshape2 melt
#' @export


rainClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "rainClass",
    inherit = rainBase,
    private = list(
      .init = function() {
        
        if (is.null(self$data) | is.null(self$options$vars) | is.null(self$options$group)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(
          "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
           
            <p>_____________________________________________________________________________________________</p>
            <p> Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowLatent/issues'  target = '_blank'>GitHub</a>.</p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
        if(isTRUE(self$options$plot)){

          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }
        # 
        # if(isTRUE(self$options$plot2)){
        #   
        #   width <- self$options$width1
        #   height <- self$options$height1
        #   self$results$plot2$setSize(width, height)
        #}

      },
      
      .run = function() {
        
        if (is.null(self$data) | is.null(self$options$vars) | is.null(self$options$group))
          return()
        
        group <- self$options$group
        vars <- self$options$vars

        data <- self$data
        data <- jmvcore::naOmit(data)
        data <- data.frame(data)
        

        # reshape to long for ggplot
        
        plotData <-  reshape2::melt(data, id.vars=self$options$group)
        colnames(plotData) <- c("Group","Variable","Value")
        #self$results$text$setContent(plotData1)
        
        # plot data function---------
        
        image   <-  self$results$plot
        image$setState(plotData)
      },        
        
      .plot = function(image, ggtheme, theme, ...) {

        if (is.null(image$state))
          return(FALSE)
        
        plotData <- image$state
        
        plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = Variable, y = Value, fill = Group)) +
          PupillometryR::geom_flat_violin(ggplot2::aes(fill = Group), 
                                          position = ggplot2::position_nudge(x = .2, y = 0), 
                                          adjust = 1.5, trim = FALSE, alpha = .5, colour = NA,
                                          width = 1.0, scale = "width") +
          ggplot2::geom_point(ggplot2::aes(x = as.numeric(factor(Variable))-.15, y = Value, colour = Group), 
                              position = ggplot2::position_jitter(width = .05), size = 3,alpha=0.7, shape = 20)+
          ggplot2::geom_boxplot(ggplot2::aes(x = Variable, y = Value, fill=Group), 
                                outlier.shape = NA, alpha = .5, width = .25, colour = "black")+
          
          ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = self$options$angle, hjust = 1
            )
          )
        }
        
        print(plot)
        TRUE
      }

      
      
        )
)
