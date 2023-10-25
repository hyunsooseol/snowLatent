
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom ggplot2 ggplot
#' @importFrom reshape2 melt
#' @export

profileClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "profileClass",
    inherit = profileBase,
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
        
        if(isTRUE(self$options$plot1)){
          
          width <- self$options$width
          width <- max(min(width, 800),200)
          height <- self$options$height
          height <- max(min(height, 600),150)
          self$results$plot1$setSize(width, height)
        }
      
        if(isTRUE(self$options$plot2)){
          
          width <- self$options$width1
          width <- max(min(width, 800),200)
          height <- self$options$height1
          height <- max(min(height, 600),150)
          self$results$plot2$setSize(width, height)
        }
        
        
        
        
      },
      
      
              .run = function() {

          if(is.null(self$options$group))
            return()
          
          group <- self$options$group
          
          vars <- self$options$vars
          
          
          data <- self$data
          
          data <- jmvcore::naOmit(data)
          
          
          formula <- jmvcore::constructFormula(self$options$group, self$options$vars)
          formula <- as.formula(formula)
          
          #### ANALYSIS##############################################
          
          
           group.means<- MASS::lda(formula, data=data)
          
          ########################################################
          
          # Group means---------------
          
          gm <- group.means$means
          
          vars <- self$options$vars 
          
          names<- dimnames(gm)[[1]]
          
          table <- self$results$mc
          
          for (i in seq_along(vars)) {
            
            var <- vars[[i]]
            
            table$addColumn(name = paste0(var),
                            type = 'number',
                            format = 'zto')
            
          }
          
          for (name in names) {
            
            row <- list()
            
            
            for(j in seq_along(vars)){
              
              var <- vars[[j]]
              
              row[[var]] <- gm[name, j]
              
            }
            
            table$addRow(rowKey=name, values=row)
            
            
          }
          
          # reshape to long for ggplot

            plotData1 <-  reshape2::melt(gm, id.vars=self$options$group)
            
            #self$results$text$setContent(plotData1)
          
           colnames(plotData1) <- c("Group","Variable","Value")
         
           # plot data function---------

            image   <-  self$results$plot1
            image$setState(plotData1)


            # Box plot-------------------
            
            if(isTRUE(self$options$plot2)){
              
              x <- self$options$vars
              y <- self$options$group
              x<- self$data[x]
              y<- self$data[y]
              # state <- list(pre, tar)
              # image2 <- self$results$plot2 
              # image2$setState(state)
              # xCol <- jmvcore::toNumeric(self$data[x])
              # yCol <- jmvcore::toNumeric(self$data[y])
              
              d <- cbind(x, y)
              d <- jmvcore::naOmit(data)
              
              d <-  reshape2::melt(data, id.vars=self$options$group)
              colnames(d) <- c("Group","Variable","Value")
              
              #self$results$text$setContent(data)
              
              image2 <- self$results$plot2
              image2$setState(d) 
              
            }
            
          
        },
        
        .plot1 = function(image, ggtheme, theme, ...) {
          
          
          if (is.null(image$state))
            return(FALSE)
          
          plotData1 <- image$state
          
          plot1<-ggplot2::ggplot(plotData1, 
                 ggplot2::aes(x=Variable, 
                              y=Value, 
                              group=Group))+
            ggplot2::geom_line(size=1.2,ggplot2::aes(color=factor(Group)))+
            ggplot2::geom_point(size=4,ggplot2::aes(color=factor(Group)))+
            ggplot2::xlab("Variable") +
            ggplot2::ylab("Mean value") +  
            ggplot2::labs(color = "Group")+
            ggtheme
            
            
            
            if (self$options$angle > 0) {
              plot1 <- plot1 + ggplot2::theme(
                axis.text.x = ggplot2::element_text(
                  angle = self$options$angle, hjust = 1
                )
              )
            }
            
            print(plot1)
            TRUE
          
        },
        
        .plot2 = function(image2,ggtheme, theme,...) {
          
          if (is.null(image2$state))
            return(FALSE)
          
          # pre <- image2$state[[1]]
          # tar <- image2$state[[2]]
          # pre <- image2$state[[1]]
          # tar <- as.factor(image2$state[[2]])
          # plot2<- caret::featurePlot(pre, tar, "box")
          
          d <- image2$state
          
          plot2 <- ggplot2::ggplot(d, 
                                   ggplot2::aes(x=Group, 
                                                y=Value, 
                                                color=Group))+  
            ggplot2::geom_boxplot()+ 
            ggplot2::facet_wrap(~Variable)+
           
            ggtheme
          
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
