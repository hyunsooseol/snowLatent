
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
          
          
          
            #  data <- self$data
            #  data <- as.data.frame(data)
            #  data <- jmvcore::naOmit(data)
            # 
            # 
            #  vars <- self$options$vars
            #  nVars <- length(vars)
            # 
            #  group<- self$options$group
            # 
            # 
            #  for (var in vars) {
            #    data[[var]] <-as.numeric(as.character(data[[var]]))
            #  }
            # 
            # 
            # # Using aggregate to calculate mean across class variable-----
            # ave <-  stats::aggregate(data[,self$options$vars], list(data[,self$options$group]), mean)
            # 
            #  self$results$text$setContent(ave)
            # 
            #  table <- self$results$mc
            #  
            #  names<- levels(ave[,1])
            #  
            #  
            # # The means of class table-------
            # 
            # ave1 <- ave[,-1]
            # 
            # 
            # for (i in seq_along(vars)) {
            # 
            #   var <- vars[[i]]
            # 
            #   table$addColumn(name = paste0(var),
            #                   type = 'number',
            #                   format = 'zto')
            # 
            # }
            # 
            # for (i in 1:3) {
            # 
            #   row <- list()
            # 
            # 
            #   for(j in seq_along(vars)){
            # 
            #     var <- vars[[j]]
            # 
            #     row[[var]] <- ave1[i, j]
            # 
            #   }
            # 
            #   table$addRow(rowKey=i, values=row)
            # 
            # 
            # }
            # 

            # reshape to long for ggplot

            plotData1 <-  reshape2::melt(gm, id.vars=self$options$group)
            
            #self$results$text$setContent(plotData1)
          
           colnames(plotData1) <- c("group","variable","value")
          
          
          
          # names(plotData1)[1]   <- self$options$group
          # names(plotData1)[2]   <-  "variable"

          #self$results$text$setContent(plotData1)

            # plot data function---------

            image   <-  self$results$plot1
            image$setState(plotData1)


          
        },
        
        .plot1 = function(image, ggtheme, theme, ...) {
          
          
          if(is.null(self$options$group))
            return()
          
          plotData1 <- image$state
          
          plot1<-ggplot2::ggplot(plotData1, 
                 ggplot2::aes(x=variable, 
                              y=value, 
                              group=group))+
            ggplot2::geom_line(size=1.2,ggplot2::aes(color=factor(group)))+
            ggplot2::geom_point(size=4,ggplot2::aes(color=factor(group)))+
            ggplot2::xlab("") +
            ggplot2::ylab("Mean value") +  
            
            ggtheme
            
            # plot1 <-
            #   ggplot2::ggplot(plotData1,
            #   ggplot2::aes_string(x = "variable",y = "value",
            #            group = as.factor("group"),
            #            colour = as.factor("group"))) +
            #   ggplot2::geom_path(size = 1.2) +
            #   ggplot2::geom_point(size = 4) +
            #   ggplot2::xlab("") +
            #   ggplot2::ylab("Mean value") +
            #   ggtheme
            # 
            
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
