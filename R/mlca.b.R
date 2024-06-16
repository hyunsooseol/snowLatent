
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import stats
#' @import glca
#' @importFrom ggplot2 ggplot
#' @importFrom glca glca
#' @importFrom glca item
#' @importFrom glca gofglca
#' @importFrom stringr str_interp
#' @export
#' 

mlcaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mlcaClass",
    inherit = mlcaBase,
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
           
            <p>_____________________________________________________________________________________________</p>
            <p>1. Latent Class Analysis(LCA) based on <b>glca</b> R package.</p>
            <p>2. The result table does not printed if the results from glca R package are not available.</p>
            <p>3. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowLatent/issues'  target = '_blank'>GitHub</a>.</p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
        if (self$options$comp1)
          self$results$comp1$setNote(
            "Note",
            "p: Bootstrap p value; H\u2090: Model fit data adequately."
          )
        
        if (self$options$rel1)
          self$results$rel1$setNote(
            "Note",
            "p: Bootstrap p value."
          )
       
        if (self$options$gof)
          self$results$gof$setNote(
            "Note",
            "Model 2: Coeff.inv=TRUE; Model 3: Coeff.inv=FALSE."
          )
        
        
        if (self$options$ci)
          self$results$ci$setNote(
            "Note",
            "p: Chi-Square p value; Model 2:Coeff.inv=TRUE; Model 3: Coeff.inv=FALSE."
          )
        
        
        if(isTRUE(self$options$plot1)){
          
          width <- self$options$width
          height <- self$options$height
         
          self$results$plot1$setSize(width, height)
        }
        
        if(isTRUE(self$options$plot2)){
          
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$plot2$setSize(width, height)
        }
        
          if(isTRUE(self$options$plot3)){
          
          width <- self$options$width2
          height <- self$options$height2
          
          self$results$plot3$setSize(width, height)
        }
  
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
  
      },
      
      .run = function() {
      
        ########  Multilevel glca R package ################
        
        # library(glca)
        # f <- item(starts.with = "E") ~ SEX
        # mlca = glca::glca(f,group = SCH_ID, data = nyts18, 
        #                   nclass = 3, ncluster = 3, seed=1)
        # 
        # summary(mlca)
        # 
        
        if (is.null(self$options$group) || is.null(self$options$vars) || 
            length(self$options$vars) < 2) return()
 
          
          data <- private$.cleanData()
  
          vars <- self$options$vars
          group <- self$options$group
          covs <- self$options$covs
          nc <- self$options$nc  
          nb <- self$options$nb
          nclust <- self$options$nclust
          
##############################
lca <- private$.computeLCA()
self$results$text$setContent(lca) 

# Item probability(rho)---                    

if(isTRUE(self$options$item)){ 
    item<- lca$param$rho
    self$results$text5$setContent(item)          
}
 
 # logistic table----------

# if(isTRUE(self$options$cluster)){
#       beta <- lca$param$beta
#       self$results$text4$setContent(beta)
# }  

if(isTRUE(self$options$co)){
    co <- lca$coefficient
    self$results$text3$setContent(co)
          }
          

# Model fit
if(isTRUE(self$options$fit)){
            
            table <- self$results$fit
            class <- self$options$nc 
            
            # Model fit measure----------
            loglik<- lca$gof$loglik
            aic<- lca$gof$aic
            caic<- lca$gof$caic
            bic<- lca$gof$bic
            entropy<- lca$gof$entropy
            df<- lca$gof$df
            gsq<- lca$gof$Gsq
            
            fit <- data.frame(loglik,aic,caic,bic,entropy,df,gsq)
           
            row <- list()
            
            row[['class']] <- class
            row[['loglik']] <- fit[,1]
            row[['aic']] <- fit[,2]
            row[['caic']] <- fit[,3]
            row[['bic']] <- fit[,4]
            row[['entropy']] <- fit[,5]
            row[['df']] <- fit[,6]
            row[['gsq']] <- fit[,7]
            
            table$setRow(rowNo = 1, values = row)
          
          }


##############################          
clu <- private$.computeCLUST()

# Absolute model fit for cluster
        
if(isTRUE(self$options$comp1)){
            
            table <- self$results$comp1
            gtable1 <- clu$gtable1
            
            if(is.null(gtable1))
              return()
            
            gtable1<- as.data.frame(gtable1)
            #define new column to add
            new <- c(2:self$options$nclust)
            #add column called new
            g <- cbind(gtable1, new)
            names <- dimnames(g)[[1]]
          
            for (name in names) {
               row <- list()
              
              row[["cluster"]]   <-  g[name, 9]
              row[["loglik"]]   <-  g[name, 1]
              row[["aic"]] <-  g[name, 2]
              row[["caic"]] <-  g[name, 3]
              row[["bic"]] <-  g[name, 4]
              row[["entropy"]] <-  g[name, 5]
              row[["df"]] <-  g[name, 6]
              row[["gsq"]] <-  g[name, 7]
              row[["p"]] <-  g[name, 8]
 
              table$addRow(rowKey=name, values=row)
            }
        }
          
#Relative model fit for cluster     
# If Number of clusters are less than 3, the checkbox should be unchecked !!!

if(isTRUE(self$options$rel1)){            
           
            table <- self$results$rel1
            dtable1 <- clu$dtable1
            
            if (is.null(dtable1)) return()
            
            dtable1<- as.data.frame(dtable1)
            #define new column to add
            new <- c(2:self$options$nclust)
            #add column called new
            d <- cbind(dtable1, new)
            names <- dimnames(d)[[1]]

            for (name in names) {
              
              row <- list()
              
              row[["cluster"]] <-  d[name, 6]
              row[["para"]] <-  d[name, 1]
              row[["loglik"]] <-  d[name, 2]
              row[["df"]] <-  d[name, 3]
              row[["dev"]] <-  d[name, 4]
              row[["p"]] <-d[name, 5]
              
              table$addRow(rowKey=name, values=row)
             }
 
            }

# Marginal prevalences for latent classes----------
          
if(isTRUE(self$options$cla)){
            
            table <- self$results$cla
            
            # if( is.null(lca[["posterior"]][["class"]]) ) {
            #   cla <- colMeans(do.call(rbind, lca[["posterior"]]))
            # } else {
            #   cla <- colMeans(do.call(rbind, lca[["posterior"]][["class"]]))
            # }
            
            cla <- tryCatch({
              colMeans(do.call(rbind, lca[["posterior"]][["class"]]))
            }, error = function(e) {
              NULL
            })
            
            cla <- as.data.frame(cla)
            names<- dimnames(cla)[[1]]
            
            for (name in names) {
              row <- list()
              row[['value']] <- cla[name,1]
              table$addRow(rowKey=name, values=row)
            }
            
          }

# Class prevalences by group

# Only group = SCH_LEV can be applied !!!
#prev = as.matrix(do.call(rbind, lapply(lca$posterior, colMeans)))

if(isTRUE(self$options$cross)){

            table <- self$results$cross

            cross <- tryCatch({
              as.matrix(do.call(rbind, lapply(lca$posterior, colMeans)))
            }, error = function(e) {
              NULL
            })
            
            names<- dimnames(cross)[[1]]
            dims <- dimnames(cross)[[2]]

            for (dim in dims) {
              table$addColumn(name = paste0(dim),
                              type = 'character')
            }
            for (name in names) {
              row <- list()
              for(j in seq_along(dims)){
                row[[dims[j]]] <- cross[name,j]
              }
              table$addRow(rowKey=name, values=row)
            }
          }


# Marginal prevalences for latent clusters

if(isTRUE(self$options$mpc)){
  
  table <- self$results$mpc
  
  mpc <- tryCatch({
    colMeans(lca$posterior$cluster)
  }, error = function(e) {
    NULL
  })
  
  mpc <- as.data.frame(mpc)
  names<- dimnames(mpc)[[1]]
  
  for (name in names) {
    row <- list()
    row[['value']] <- mpc[name,1]
    table$addRow(rowKey=name, values=row)
 
}
}

# Class prevalences by cluster---

if(isTRUE(self$options$cpc)){
  
  table <- self$results$cpc
 
  cross <- tryCatch({
    as.matrix(do.call(rbind, lapply(lca$posterior, colMeans)))
  }, error = function(e) {
    NULL
  })
  
  names<- dimnames(cross)[[1]]
  dims <- dimnames(cross)[[2]]
  
  for (dim in dims) {
    table$addColumn(name = paste0(dim),
                    type = 'character')
  }
  for (name in names) {
    row <- list()
    for(j in seq_along(dims)){
      row[[dims[j]]] <- cross[name,j]
    }
    table$addRow(rowKey=name, values=row)
  }
}


# Elbow plot------------------------------
        
 if(self$options$nclust>2){
          
          # Elbow plot-------------
          
          out1 <- clu$gtable1[,c(2:4)]
          cla <- c(2:self$options$nclust)
          out1 <- data.frame(out1,cla)
 
          colnames(out1) <- c('AIC',
                              'CAIC','BIC','Cluster')
          
          elbow <- reshape2::melt(out1,
                                  id.vars='Cluster',
                                  variable.name="Fit",
                                  value.name='Value')
          
          image2 <- self$results$plot3
          image2$setState(elbow )
          
        }
        
        # # adding class--------
        # gtable1<- as.data.frame(clu$gtable1)
        # dtable1<- as.data.frame(clu$dtable1)
        # 
        # #define new column to add
        # new <- c(2:self$options$nclust)
        # 
        # #add column called new
        # gtable1 <- cbind(clu$gtable1, new)

########################

inv <- private$.computeINV()        
     
# Goodness of fit table----------  

if(isTRUE(self$options$gof)){
        
          if(!is.null(self$options$covs)){
            
            table <- self$results$gof  
            
            if(is.null(inv$ci.g))
              return()
            
            g<- as.data.frame(inv$ci.g)
           
            loglik <- g[,1]
            aic <- g[,2]
            caic <- g[,3]
            bic <- g[,4]
            entropy <- g[,5]
            df <- g[,6]
            gsq <- g[,7]
            
            names <- dimnames(g)[[1]]
            
            for (name in names) {
              row <- list()
              
              row[["loglik"]]   <-  g[name, 1]
              row[["aic"]] <-  g[name, 2]
              row[["caic"]] <-  g[name, 3]
              row[["bic"]] <-  g[name, 4]
              row[["entropy"]] <-  g[name, 5]
              row[["df"]] <-  g[name, 6]
              row[["gsq"]] <-  g[name, 7]
              
              table$addRow(rowKey=name, values=row)
            }
            
          }

        }
                 
#Testing equality of coefficients-------------

if(!is.null(self$options$covs)&&isTRUE(self$options$ci)){
  
            table <- self$results$ci
            
            if(is.null(inv$ci.d))
              return()
           
          d<- as.data.frame(inv$ci.d)
          
          para <- d[,1]
          loglik<- d[,2]
          df<- d[,3]
          dev<- d[,4]
          p<- d[,5]
          
          names <- dimnames(d)[[1]]

          for (name in names) {
            row <- list()
            
            row[["para"]] <-  d[name, 1]
            row[["loglik"]] <-  d[name, 2]
            row[["df"]] <-  d[name, 3]
            row[["dev"]] <-  d[name, 4]
            row[["p"]] <-d[name, 5]
            
            table$addRow(rowKey=name, values=row)
           }
        
          }
        

# Class prevalences for latent clusters
# lca$posterior$wclass

if(isTRUE(self$options$cpc)){
  
  table <- self$results$cpc
  
  cpc <- tryCatch({
     lca$posterior$wclass
  }, error = function(e) {
    NULL
  })
  
  names<- dimnames(cpc)[[1]]
  dims <- dimnames(cpc)[[2]]
  
  for (dim in dims) {
    table$addColumn(name = paste0(dim),
                    type = 'character')
  }
  for (name in names) {
    row <- list()
    for(j in seq_along(dims)){
      row[[dims[j]]] <- cpc[name,j]
    }
    table$addRow(rowKey=name, values=row)
  }
  
}

# Cluster number---

if(isTRUE(self$options$cn)){
  
  # # data<- lca[["posterior"]][["cluster"]]
  # # data$cluster<- factor(apply(data, 1, which.max))
  # 
  # cn <- tryCatch({
  #   dat<- lca[["posterior"]][["cluster"]]
  #   dat$Membership<- factor(apply(dat, 1, which.max))
  #   cn <- dat
  # }, error = function(e) {
  #   NULL
  # })
  # 
  # self$results$text6$setContent(cn) 

      member<- lca[["posterior"]][["cluster"]]
      m<- as.data.frame(member)
      m$Membership <- as.numeric(factor(apply(m, 1, which.max)))
   
   table <- self$results$cn
   names<- dimnames(m)[[1]]
   dims <- dimnames(m)[[2]]
    
   for (dim in dims) {
      table$addColumn(name = paste0(dim),
                      type = 'number')
    }
    for (name in names) {
      row <- list()
      for(j in seq_along(dims)){
        row[[dims[j]]] <- m[name,j]
      }
    table$addRow(rowKey=name, values=row)
            }
  
  }


# Item by class plot---------

if(isTRUE(self$options$plot2)){

image2 <- self$results$plot2
ic<- lca[["param"]][["rho"]]
ic <- reshape2::melt(ic) 
colnames(ic) <-c("Class", "Level", "value", "L1") 
image2$setState(ic)
}

  
},


#Plot#######################

.plot1 = function(image, ...) {
        
        if (is.null(self$options$group) || is.null(self$options$vars) || 
            length(self$options$vars) < 2) return()
        
        data <- private$.cleanData()
        
        vars <- self$options$vars
        group <- self$options$group
        covs <- self$options$covs
        nc <- self$options$nc  
        nb <- self$options$nb
        nclust <- self$options$nclust
        
        lca <- private$.computeLCA()
        
        par(mfcol = c(3, 1))
        plot1 <- plot(lca, ask=FALSE)
        
        print(plot1)
        TRUE
      },
  
# item by class plot-----------

.plot2 = function(image2, ggtheme, theme, ...) {    
  
  ic <- image2$state
  
  plot2 <- ggplot2::ggplot(ic, ggplot2::aes(x = Class, y = value, fill = Level)) + 
    ggplot2::geom_bar(stat = "identity", position = "stack")+ 
    ggplot2::facet_wrap(~ L1)+
    ggplot2::scale_x_discrete("Class", expand = c(0, 0)) +
    ggplot2::scale_y_continuous("Probability", expand = c(0, 0)) +
    ggplot2::theme_bw()
  
  
    plot2 <- plot2+ggtheme
  
  if (self$options$angle > 0) {
    plot2 <- plot2 + ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = self$options$angle, hjust = 1
      )
    )
  }
  
  print(plot2)
  TRUE
  
},

.plot3 = function(image2, ggtheme, theme,...) {
  
  if(self$options$nclust <3)
    return()
  
  elbow <- image2$state
  
  
  # plot3 <- ggplot2::ggplot(elbow,ggplot2::aes(x = Class, y = Value, group = Fit))+
  #   ggplot2::geom_line(size=1.1,ggplot2::aes(color=Fit))+
  #   ggplot2::geom_point(size=3,ggplot2::aes(color=Fit))
  #   ggplot2::scale_x_continuous(breaks = seq(1, length(elbow$Class), by = 1))
  
  plot3 <- ggplot2::ggplot(elbow, ggplot2::aes(x = Cluster, y = Value, color = Fit)) +
    ggplot2::geom_line(size = 1.1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_x_continuous(breaks = seq(1, length(elbow$Cluster), by = 1))
  
  
  
  plot3 <- plot3+ggtheme
  
  
  print(plot3)
  TRUE
  
},

#Private function##########################################################
        
.cleanData = function() {
        
        vars <- self$options$vars
        covs <- self$options$covs
        
        groupVarName <- self$options$group
        data <- list()
        
        data[[groupVarName]] <- jmvcore::toNumeric(self$data[[groupVarName]])
        
        for (item in vars)
          data[[item]] <- jmvcore::toNumeric(self$data[[item]])
        for (item in covs)
          data[[item]] <- jmvcore::toNumeric(self$data[[item]])
        
        attr(data, 'row.names') <- seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        
        data <- data[!is.na(data[[groupVarName]]), ]
        
        return(data)
      },

.computeLCA = function() { 
  
  data <- private$.cleanData()
  
  vars <- self$options$vars
  group <- self$options$group
  covs <- self$options$covs
  nc <- self$options$nc  
  nb <- self$options$nb
  nclust <- self$options$nclust
  
  # Constructing formula----------------        
  
  vars <- self$options$vars
  #vars <- colnames(data[, -1] )
  
  vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
  vars <- paste0(vars, collapse=',')
  
  # formula with no covariates----------     
  
  formula <- as.formula(paste0('glca::item(', vars, ')~1'))
  
  # With covariates-------------
  
  #if( !is.null(self$options$covs) ) {
  
  if(length(self$options$covs)>=1){ 
    covs <- self$options$covs
    covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
    covs <- paste0(covs, collapse='+')
    
    formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
  }
  
  group <- data[, 1]
 
  ################### LCA model estimates############################
  
  lca = glca::glca(formula=formula,
                   group= group,
                   data=data,
                   nclass = nc, 
                   ncluster = nclust,
                   seed = 1)
  
  #################################################################
 
  return(lca)

},

.computeCLUST = function() {
  
  data <- private$.cleanData()
  
  vars <- self$options$vars
  group <- self$options$group
  covs <- self$options$covs
  nc <- self$options$nc  
  nb <- self$options$nb
  nclust <- self$options$nclust
  
  # Constructing formula----------------        
  
  vars <- self$options$vars
  #vars <- colnames(data[, -1] )
  
  vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
  vars <- paste0(vars, collapse=',')
  
  # formula with no covariates----------     
  
  formula <- as.formula(paste0('glca::item(', vars, ')~1'))
  
  # With covariates-------------
  
  #if( !is.null(self$options$covs) ) {
  
  if(length(self$options$covs)>=1){ 
    covs <- self$options$covs
    covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
    covs <- paste0(covs, collapse='+')
    
    formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
  }
  
  group <- data[, 1]
  
  # CLUSTER: Absolute and relative model fit-------
  
  args <- list(test = "boot", nboot=nb)
  
  for(n in 2:self$options$nclust)
    
    args[[n+1]] <- glca::glca(formula = formula,
                              group=group,
                              data = data,
                              nclass = nc,
                              ncluster = n,
                              seed = 1)
  
  res <- do.call(glca::gofglca, args)
  
  gtable1 <- res[["gtable"]] #Absolute model fit
  
  if(is.null(res$gtable)) {
    gtable1 <- NULL
  } else {
    gtable1 <- res[["gtable"]]   
  }
  
  
  if(is.null(res$dtable)) {
    dtable1 <- NULL
  } else {
    dtable1 <- res[["dtable"]]   # Relative model fit
  }
  
  retlist <- list(gtable1=gtable1, dtable1=dtable1)
  return(retlist)

},

.computeINV = function() {
  
  data <- private$.cleanData()
  
  vars <- self$options$vars
  group <- self$options$group
  covs <- self$options$covs
  nc <- self$options$nc  
  nb <- self$options$nb
  nclust <- self$options$nclust
  
  # Constructing formula----------------        
  
  vars <- self$options$vars
  #vars <- colnames(data[, -1] )
  
  vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
  vars <- paste0(vars, collapse=',')
  
  # formula with no covariates----------     
  
  formula <- as.formula(paste0('glca::item(', vars, ')~1'))
  
  # With covariates-------------
  
  #if( !is.null(self$options$covs) ) {
  
  if(length(self$options$covs)>=1){ 
    covs <- self$options$covs
    covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
    covs <- paste0(covs, collapse='+')
    
    formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
  }
  
  group <- data[, 1]
  
  #Invariance of equality---       

  if(!is.null(self$options$covs)){
    
    lca0 = glca::glca(formula=as.formula(paste0('glca::item(', vars, ')~1')),
                      group= group,
                      data=data,
                      nclass = nc, 
                      ncluster = nclust,
                      seed = 1)

    lca = glca::glca(formula=formula,
                     group= group,
                     data=data,
                     nclass = nc, 
                     ncluster = nclust,
                     seed = 1)
    
    lca2 <- try(glca::glca(formula = formula, 
                           group = group, 
                           data = data, 
                           nclass = nc, 
                           ncluster = nclust,
                           coeff.inv = FALSE,
                           seed= 1)
    )
    
    if(jmvcore::isError(lca2)){
      
      err_string <- stringr::str_interp(
        " Error in if (maxdiff < eps) break : Covariates can not be computed."
      )
      stop(err_string)
      
    } 
    
    if (! jmvcore::isError(lca2) ){
      
      cf <- glca::gofglca(lca0, lca, lca2, test = "chisq", nboot = nb)
      
      # self$results$text$setContent(cf)
      
      ci.g <- cf[["gtable"]]
      ci.d <- cf[["dtable"]]
      
    } 
    
  }
  
  retlist <- list(ci.g=ci.g, ci.d=ci.d)
  return(retlist)
  
}


################################################################
# Marginal prevalences for latent cluster------

#clust <- lca[["param"]][["delta"]]
#clust<- as.data.frame(clust)

# Marginal prevalences for latent classes ----------
# cla <- colMeans(do.call(rbind, lca[["posterior"]][["class"]]))
# cla<- as.data.frame(cla)

# if( is.null(lca[["posterior"]][["class"]]) ) {
#   cla <- colMeans(do.call(rbind, lca[["posterior"]]))
# } else {
#   cla <- colMeans(do.call(rbind, lca[["posterior"]][["class"]]))
# }
# 
# cla <- as.data.frame(cla)

# Class prevalences by cluster-----------
#cross<- lca[["posterior"]][["wclass"]]
# 
# if( is.null(lca[["posterior"]][["wclass"]]) ) {
#   cross <- NULL 
# } else {
#   cross <- lca[["posterior"]][["wclass"]]
# }
# 
# item response probability---------

#item<- lca[["param"]][["rho"]]

#item<- do.call("rbind", lapply(item, as.data.frame))


# posterior probability---------

#post <-lca[["posterior"]][["class"]]


# logistic regression coef.--------

#  co <- lca[["coefficient"]]

# cluster prob.(gamma)----------

#gamma <- lca[["param"]][["gamma"]]

# Class Prevalences plot----------
# image <- self$results$plot1
# image$setState(lca)


# # cluster membership-----------
# 
#   if(isTRUE(self$options$member)){
#   
#     member<- lca[["posterior"]][["cluster"]]
#     m<- as.data.frame(member)
#     m$Membership <- as.numeric(factor(apply(m, 1, which.max)))
#   
#   table <- self$results$member
#   names<- dimnames(m)[[1]]
#   dims <- dimnames(m)[[2]]
#   
#   for (dim in dims) {
#     table$addColumn(name = paste0(dim),
#                     type = 'number')
#   }
#   for (name in names) {
#     row <- list()
#     for(j in seq_along(dims)){
#       row[[dims[j]]] <- m[name,j]
#     }
#   table$addRow(rowKey=name, values=row)
#           }
#         }

# item probabilities----------

# if(isTRUE(self$options$item)){



# tables <- self$results$item
# vars <- self$options$vars
# 
# for(i in seq_along(vars)){
#      
#          item <- item[[ vars[i] ]]
#       
#          table <- tables[[i]]
#           names<- row.names(item)
#           dims <- colnames(item)
# 
#   for (dim in dims) {
#          table$addColumn(name = paste0(dim),
#                     type = 'text',
#                     combineBelow=TRUE)
#   }
#       for (name in names) {
#          row <- list()
#          for(j in seq_along(dims)){
#            row[[dims[j]]] <- item[name,j]
#          }
#          table$addRow(rowKey=name, values=row)
#        }
#      }

#       }


    )
)
