
# This file is a generated template, your changes will not be overwritten
#' @importFrom  fmsb radarchart
#' @importFrom scales rescale
#' @import ggplot2
#' @import scales
#' @export


radarClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "radarClass",
    inherit = radarBase,
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
            <p>____________________________________________________________________________________</p>
            <p> Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowLatent/issues'  target = '_blank'>GitHub.</a></p>
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

        
      },
      
      #-----------------------------------------------------
      
  .run = function() {

        if (!is.null(self$options$vars)) {
          
          vars <- self$options$vars
          data <- self$data
          data <- jmvcore::naOmit(data)
          
          # Handling id----------
          
          #  Assuming your data frame is named 'data'
          # row.names(data) <- data$City
          # data <- data[, -1]
          
          
          if ( ! is.null(self$options$labels)) {
            
            rownames(data) <- data[[self$options$labels]]
            data[[self$options$labels]] <- NULL
            
          }
          
          for (i in seq_along(vars))
            data[[i]] <- jmvcore::toNumeric(data[[i]])
          
        
     # Data Handling------------
    
    df_scaled <- round(apply(data, 2, scales::rescale), 2)
    df_scaled <- as.data.frame(df_scaled)
    
    # Variables summary
    # Get the minimum and the max of every column  
    col_max <- apply(df_scaled, 2, max)
    col_min <- apply(df_scaled, 2, min)
    # Calculate the average profile 
    col_mean <- apply(df_scaled, 2, mean)
    # Put together the summary of columns
    col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))
    
    
    # Bind variables summary to the data
    df_scaled2 <- as.data.frame(rbind(col_summary, df_scaled))
    
    self$results$text$setContent(df_scaled2)
    
    ##### plot-------------------
    
    image <- self$results$plot
    image$setState(df_scaled2) 
    
        }
  },
  
  # Radar chart plot---------------
  
  .plot = function(image, ...) {
    
    if (is.null(image$state))
      return(FALSE)
   
    i <- self$options$num
    df_scaled2 <- image$state
  
    # Produce a radar-chart for each one---------
    # for (i in 4:nrow(df_scaled2)) {
    #   fmsb::radarchart(
    #     df_scaled2[c(1:3, i), ],
    #     pfcol = c("#99999980",NA),
    #     pcol= c(NA,2), plty = 1, plwd = 2,
    #     title = row.names(df_scaled2)[i]
    #   )
    # }  
    
    plot <- fmsb::radarchart(df_scaled2[c(1:3, i), ],
        pfcol = c("#99999980",NA),
        pcol= c(NA,2), plty = 1, plwd = 2,
        title = row.names(df_scaled2)[i])
    
  print(plot)
  TRUE
   
    
  }
 
        )
)
