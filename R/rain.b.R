
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @export


rainClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "rainClass",
    inherit = rainBase,
    private = list(
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$vars) | is.null(self$options$group)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        # self$results$instructions$setContent(
        #   "<html>
        #     <head>
        #     </head>
        #     <body>
        #     <div class='instructions'>
        #    
        #     <p>_____________________________________________________________________________________________</p>
        #     <p> Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowLatent/issues'  target = '_blank'>GitHub</a>.</p>
        #     <p>_____________________________________________________________________________________________</p>
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
        
        #####################################
        library(ggplot2)
        library(dplyr)
        
        
        "%||%" <- function(a, b) {
          if (!is.null(a))
            a
          else
            b
        }
        
        GeomFlatViolin <-
          ggplot2::ggproto(
            "GeomFlatViolin",
            ggplot2::Geom,
            setup_data = function(data, params) {
              data$width <- data$width %||%
                params$width %||% (ggplot2::resolution(data$x, FALSE) * 0.9)
              
              # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
              data %>%
                dplyr::group_by(.data = ., group) %>%
                dplyr::mutate(
                  .data = .,
                  ymin = min(y),
                  ymax = max(y),
                  xmin = x,
                  xmax = x + width / 2
                )
            },
            
            draw_group = function(data, panel_scales, coord)
            {
              # Find the points for the line to go all the way around
              data <- base::transform(data,
                                      xminv = x,
                                      xmaxv = x + violinwidth * (xmax - x))
              
              # Make sure it's sorted properly to draw the outline
              newdata <-
                base::rbind(
                  dplyr::arrange(.data = base::transform(data, x = xminv), y),
                  dplyr::arrange(.data = base::transform(data, x = xmaxv), -y)
                )
              
              # Close the polygon: set first and last point the same
              # Needed for coord_polar and such
              newdata <- rbind(newdata, newdata[1,])
              
              ggplot2:::ggname("geom_flat_violin",
                               GeomPolygon$draw_panel(newdata, panel_scales, coord))
            },
            
            draw_key = ggplot2::draw_key_polygon,
            
            default_aes = ggplot2::aes(
              weight = 1,
              colour = "grey20",
              fill = "white",
              size = 0.5,
              alpha = NA,
              linetype = "solid"
            ),
            
            required_aes = c("x", "y")
          )
        
        
        geom_flat_violin <-
          function(mapping = NULL,
                   data = NULL,
                   stat = "ydensity",
                   position = "dodge",
                   trim = TRUE,
                   scale = "area",
                   show.legend = NA,
                   inherit.aes = TRUE,
                   ...) {
            ggplot2::layer(
              data = data,
              mapping = mapping,
              stat = stat,
              geom = GeomFlatViolin,
              position = position,
              show.legend = show.legend,
              inherit.aes = inherit.aes,
              params = list(trim = trim,
                            scale = scale,
                            ...)
            )
          }
        
        ##################################
        
        plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = Variable, y = Value, fill = Group)) +
          
                geom_flat_violin(ggplot2::aes(fill = Group), 
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
