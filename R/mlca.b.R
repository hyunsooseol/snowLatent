
mlcaClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "mlcaClass",
    inherit = mlcaBase,
    private = list(
      .cache = list(),  
      .dataCache = NULL,  
      .htmlwidget = NULL,
      
.init = function() {
  private$.htmlwidget <- HTMLWidget$new()
  

  if (is.null(self$data) | is.null(self$options$vars)) {
    self$results$instructions$setVisible(visible = TRUE)
  }
  
  self$results$instructions$setContent(
    private$.htmlwidget$generate_accordion(
      title = "Instructions",
      content = paste(
        '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
        '<div style="text-align:justify;">',
        '<ul>',
        '<li>Latent Class Analysis(LCA) based on <b>glca</b> R package.</li>',
        '<li>The MAR(Missing at Random) method is applied to handle missing values.</li>',
        '<li>The Relative model fit option requires more than two clusters. This option should be unchecked when two clusters are specified.</li>',
        '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowLatent/issues" target="_blank">GitHub</a>.</li>',
        '</ul></div></div>'
      )
    )
  )
  
  if (self$options$rel1)
    self$results$rel1$setNote("Note", "p: Chi-Square p value.")
  
  if (self$options$gof)
    self$results$gof$setNote("Note", "Model 2: Coeff.inv=TRUE; Model 3: Coeff.inv=FALSE.")
  
  if (self$options$ci)
    self$results$ci$setNote(
      "Note",
      "p: Chi-Square p value; Model 2:Coeff.inv=TRUE; Model 3: Coeff.inv=FALSE."
    )
  
},

.run = function() {
  
  if (!isTRUE(self$options$run))
    return()
  
  if (is.null(self$options$group) ||
      is.null(self$options$vars) ||
      length(self$options$vars) < 3)
    return()
  
  # Apple spinner 표시
  self$results$progressBarHTML$setVisible(TRUE)
  self$results$progressBarHTML$setContent(
    appleSpinnerH('Performing multilevel LCA...')
  )
  
  # 화면에 spinner를 먼저 반영
  private$.checkpoint()
  
  # 현재 Run 기준으로 데이터와 모델 초기화
  private$.dataCache <- NULL
  private$.cache <- list()
  
  private$.dataCache <- private$.cleanData()
  
  # LCA 적합
  private$.cache$lca <- private$.computeLCA()
  
  # 클러스터 비교
  private$.cache$clu <- private$.computeCLUST()
  
  # 공변량/불변성
  private$.cache$inv <- private$.computeINV()
  
  lca <- private$.cache$lca
  clu <- private$.cache$clu
  inv <- private$.cache$inv
  
  # 이후 기존 결과 출력 코드
  self$results$text$setContent(lca)
  
  if (isTRUE(self$options$co)) {
    co <- coef(lca)
    private$.populateCoTable(co)
  }
  
  # Model fit
  if (isTRUE(self$options$fit)) {
    table <- self$results$fit
    table$setRow(
      rowNo = 1,
      values = list(
        class   = self$options$nc,
        loglik  = lca$gof$loglik,
        aic     = lca$gof$AIC,
        bic     = lca$gof$BIC,
        entropy = lca$gof$entropy,
        df      = lca$gof$df,
        gsq     = lca$gof$Gsq
      )
    )
  }
  
  if (isTRUE(self$options$comp1)) {
    table <- self$results$comp1
    if (!is.null(clu$gtable1)) {
      g <- as.data.frame(clu$gtable1)
      clusters <- 2:self$options$nclust
      
      for (i in seq_along(clusters)) {
        table$addRow(
          rowKey = rownames(g)[i],
          values = list(
            cluster = clusters[i],
            loglik  = g[i, 1],
            aic     = g[i, 2],
            bic     = g[i, 3],
            entropy = g[i, 4],
            df      = g[i, 5],
            gsq     = g[i, 6]
          )
        )
      }
    }
  }
  
  if (isTRUE(self$options$rel1)) {
    table <- self$results$rel1
    if (!is.null(clu$dtable1)) {
      d <- as.data.frame(clu$dtable1)
      clusters <- 2:self$options$nclust
      for (i in seq_along(clusters)) {
        table$addRow(
          rowKey = rownames(d)[i],
          values = list(
            cluster = clusters[i],
            para    = d[i, 1],
            loglik  = d[i, 2],
            df      = d[i, 3],
            dev     = d[i, 4],
            p       = d[i, 5]
          )
        )
      }
    }
  }
  
  if (isTRUE(self$options$cla)) {
    table <- self$results$cla
    cla <- tryCatch({
      colMeans(do.call(rbind, lca[["posterior"]][["class"]]))
    }, error = function(e) NULL)
    
    if (!is.null(cla)) {
      cla <- as.data.frame(cla)
      names <- dimnames(cla)[[1]]
      for (name in names) {
        row <- list()
        row[['value']] <- cla[name, 1]
        table$addRow(rowKey = name, values = row)
      }
    }
  }
  
  if (isTRUE(self$options$cross)) {
    table <- self$results$cross
    
    cross <- tryCatch({
      
      classPosterior <- lca$posterior$class
      
      if (is.null(classPosterior))
        return(NULL)
      
      # 각 관측 집단 내에서 개인별 class posterior의 평균 계산
      result <- lapply(
        classPosterior,
        function(x) {
          colMeans(
            as.matrix(x),
            na.rm = TRUE
          )
        }
      )
      
      result <- as.matrix(
        do.call(rbind, result)
      )
      
      # 집단명이 존재하면 행 이름으로 유지
      if (!is.null(names(classPosterior)) &&
          length(names(classPosterior)) == nrow(result)) {
        rownames(result) <- names(classPosterior)
      }
      
      result
      
    }, error = function(e) NULL)
    
    if (!is.null(cross)) {
      names <- rownames(cross)
      dims <- colnames(cross)
      
      for (dim in dims) {
        table$addColumn(
          name = paste0(dim),
          type = 'character'
        )
      }
      
      for (name in names) {
        row <- list()
        
        for (j in seq_along(dims)) {
          row[[dims[j]]] <- cross[name, j]
        }
        
        table$addRow(
          rowKey = name,
          values = row
        )
      }
    }
  }
  
  if (isTRUE(self$options$mpc)) {
    table <- self$results$mpc
    mpc <- tryCatch({ colMeans(lca$posterior$cluster) }, error = function(e) NULL)
    if (!is.null(mpc)) {
      mpc <- as.data.frame(mpc)
      names <- dimnames(mpc)[[1]]
      for (name in names) {
        row <- list()
        row[['value']] <- mpc[name, 1]
        table$addRow(rowKey = name, values = row)
      }
    }
  }
  
  if (isTRUE(self$options$cpc)) {
    table <- self$results$cpc
    cpc <- tryCatch({
      lca$posterior$wclass
    }, error = function(e) {
      tryCatch({
        as.matrix(do.call(rbind, lapply(lca$posterior, colMeans)))
      }, error = function(e) NULL)
    })
    
    if (!is.null(cpc)) {
      names <- dimnames(cpc)[[1]]
      dims <- dimnames(cpc)[[2]]
      for (dim in dims) {
        table$addColumn(name = paste0(dim), type = 'character')
      }
      for (name in names) {
        row <- list()
        for (j in seq_along(dims)) row[[dims[j]]] <- cpc[name, j]
        table$addRow(rowKey = name, values = row)
      }
    }
  }
  
  # Elbow 
  if (self$options$nclust > 2 && isTRUE(self$options$plot3)) {
    out1 <- clu$gtable1[, c(2:3)]
    cla <- c(2:self$options$nclust)
    out1 <- data.frame(out1, cla)
    colnames(out1) <- c('AIC', 'BIC', 'Cluster')
    elbow <- reshape2::melt(out1, id.vars = 'Cluster', variable.name = "Fit", value.name = 'Value')
    image2 <- self$results$plot3
    image2$setState(elbow)
  }
  
  if (isTRUE(self$options$gof) && !is.null(self$options$covs)) {
    table <- self$results$gof
    if (!is.null(inv$ci.g)) {
      g <- as.data.frame(inv$ci.g)
      ro <- rownames(g); ro <- ro[ro != "1"]
      for (name in ro) {
        table$addRow(
          rowKey = name,
          values = list(
            loglik  = g[name, 1],
            aic     = g[name, 2],
            bic     = g[name, 3],
            entropy = g[name, 4],
            df      = g[name, 5],
            gsq     = g[name, 6]
          )
        )
      }
    }
  }
  
  if (!is.null(self$options$covs) && isTRUE(self$options$ci)) {
    table <- self$results$ci
    if (!is.null(inv$ci.d)) {
      d <- as.data.frame(inv$ci.d)
      ro <- rownames(d); ro <- ro[ro != "1"]
      for (name in ro) {
        table$addRow(rowKey = name,
                     values = list(
                       para   = d[name, 1],
                       loglik = d[name, 2],
                       df     = d[name, 3],
                       dev    = d[name, 4],
                       p      = d[name, 5]
                     ))
      }
    }
  }
  
  if (isTRUE(self$options$cn)) {
    member <- lca[["posterior"]][["cluster"]]
    m <- as.data.frame(member)
    m$Membership <- as.numeric(factor(apply(m, 1, which.max)))
    
    table <- self$results$cn
    names <- dimnames(m)[[1]]
    dims <- dimnames(m)[[2]]
    for (dim in dims) table$addColumn(name = paste0(dim), type = 'number')
    for (name in names) {
      row <- list()
      for (j in seq_along(dims)) row[[dims[j]]] <- m[name, j]
      table$addRow(rowKey = name, values = row)
    }
  }
  
  if (isTRUE(self$options$plot2)) {
    image2 <- self$results$plot2
    ic <- lca[["param"]][["rho"]]
    ic <- reshape2::melt(ic)
    colnames(ic) <- c("Class", "Level", "value", "L1")
    image2$setState(ic)
  }
  
  if (isTRUE(self$options$item)) {
    tables <- self$results$item
    itemprob <- lca$param$rho
    vars <- self$options$vars
    
    for (i in seq_along(vars)) {
      item <- itemprob[[vars[i]]]
      table <- tables[[i]]
      names <- row.names(item)
      dims <- colnames(item)
      for (dim in dims) {
        table$addColumn(name = paste0(dim),
                        type = 'text',
                        combineBelow = TRUE)
      }
      for (name in names) {
        row <- list()
        for (j in seq_along(dims)) row[[dims[j]]] <- item[name, j]
        table$addRow(rowKey = name, values = row)
      }
    }
  }
  
  self$results$progressBarHTML$setVisible(FALSE)
},

.plot1 = function(image, ...) {
  if (!isTRUE(self$options$plot1))
    return(FALSE)
  
  lca <- private$.cache$lca
  
  if (is.null(lca))
    return(FALSE)
  
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar), add = TRUE)
  
  result <- tryCatch({
    
    graphics::par(mfcol = c(3, 1))
    plot(lca, ask = FALSE)
    
    TRUE
    
  }, error = function(e) {
    
    graphics::par(mfrow = c(1, 1))
    graphics::plot.new()
    
    graphics::text(
      x = 0.5,
      y = 0.55,
      labels = "Profile plot could not be generated.",
      cex = 1
    )
    
    graphics::text(
      x = 0.5,
      y = 0.43,
      labels = "Some estimated values were non-finite.",
      cex = 0.85
    )
    
    TRUE
  })
  
  result
},

.plot2 = function(image2, ggtheme, theme, ...) {
  if (!self$options$plot2 || is.null(image2$state))
    return(FALSE)
  
  ic <- image2$state
  
  plot2 <- ggplot2::ggplot(ic, ggplot2::aes(x = Class, y = value, fill = Level)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::facet_wrap( ~ L1) +
    ggplot2::scale_x_discrete("Class", expand = c(0, 0)) +
    ggplot2::scale_y_continuous("Probability", expand = c(0, 0)) +
    ggplot2::theme_bw()
  plot2 <- plot2 + ggtheme
  
  if (self$options$angle > 0) {
    plot2 <- plot2 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
  }
  print(plot2)
  TRUE
},

.plot3 = function(image2, ggtheme, theme, ...) {
  if (!self$options$plot3 || is.null(image2$state))
    return(FALSE)
  
  elbow <- image2$state
  
  plot3 <- ggplot2::ggplot(elbow, ggplot2::aes(x = Cluster, y = Value, color = Fit)) +
    ggplot2::geom_line(size = 1.1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_x_continuous(breaks = seq(1, length(elbow$Cluster), by = 1))
  
  plot3 <- plot3 + ggtheme
  print(plot3)
  TRUE
},

.cleanData = function() {
  vars <- self$options$vars
  covs <- self$options$covs
  
  groupVarName <- self$options$group
  data <- list()
  
  data[[groupVarName]] <- jmvcore::toNumeric(self$data[[groupVarName]])
  
  for (item in vars)
    data[[item]] <- jmvcore::toNumeric(self$data[[item]])
  
  if (!is.null(covs)) {
    for (item in covs)
      data[[item]] <- jmvcore::toNumeric(self$data[[item]])
  }
  
  attr(data, 'row.names') <- seq_len(length(data[[1]]))
  attr(data, 'class') <- 'data.frame'
  
  return(data)
},

.computeLCA = function() {
  data <- private$.dataCache
  if (is.null(data)) {
    data <- private$.cleanData()
    private$.dataCache <- data
  }
  
  vars <- self$options$vars
  group <- self$options$group
  covs <- self$options$covs
  nc <- self$options$nc
  nclust <- self$options$nclust
  
  vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
  vars <- paste0(vars, collapse = ',')
  
  formula <- as.formula(paste0('glca::item(', vars, ')~1'))
  
  if (!is.null(self$options$covs) && length(self$options$covs) >= 1) {
    covs <- self$options$covs
    covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
    covs <- paste0(covs, collapse = '+')
    
    formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
  }
  
  group <- data[, 1]
  
  lca = glca::glca(
    formula = formula,
    group = group,
    data = data,
    nclass = nc,
    ncluster = nclust,
    seed = 1
  )
  return(lca)
},

.computeCLUST = function() {
  data <- private$.dataCache
  if (is.null(data)) {
    data <- private$.cleanData()
    private$.dataCache <- data
  }
  
  vars <- self$options$vars
  group <- self$options$group
  covs <- self$options$covs
  nc <- self$options$nc
  nclust <- self$options$nclust
  
  vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
  vars <- paste0(vars, collapse = ',')
  
  formula <- as.formula(paste0('glca::item(', vars, ')~1'))
  
  if (!is.null(self$options$covs) && length(self$options$covs) >= 1) {
    covs <- self$options$covs
    covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
    covs <- paste0(covs, collapse = '+')
    
    formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
  }
  group <- data[, 1]
  
  args <- list(test = "chisq")
  
  for (n in 2:self$options$nclust)
    args[[n]] <- glca::glca(
      formula = formula,
      group = group,
      data = data,
      nclass = nc,
      ncluster = n,
      seed = 1
    )
  
  res <- do.call(glca::gofglca, args)
  
  if (is.null(res$gtable)) {
    gtable1 <- NULL
  } else {
    gtable1 <- res[["gtable"]]
  }
  
  if (is.null(res$dtable)) {
    dtable1 <- NULL
  } else {
    dtable1 <- res[["dtable"]]
  }
  
  retlist <- list(gtable1 = gtable1, dtable1 = dtable1)
  return(retlist)
},

.computeINV = function() {
  data <- private$.dataCache
  if (is.null(data)) {
    data <- private$.cleanData()
    private$.dataCache <- data
  }
  
  vars <- self$options$vars
  group <- self$options$group
  covs <- self$options$covs
  nc <- self$options$nc
  nclust <- self$options$nclust
  
  vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
  vars <- paste0(vars, collapse = ',')
  
  formula <- as.formula(paste0('glca::item(', vars, ')~1'))
  
  if (!is.null(self$options$covs) && length(self$options$covs) >= 1) {
    covs <- self$options$covs
    covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
    covs <- paste0(covs, collapse = '+')
    
    formula <- as.formula(paste0('glca::item(', vars, ') ~ ', covs))
  }
  group <- data[, 1]
  
  ci.g <- NULL
  ci.d <- NULL
  
  if (!is.null(self$options$covs)) {
    lca0 = glca::glca(
      formula = as.formula(paste0('glca::item(', vars, ')~1')),
      group = group,
      data = data,
      nclass = nc,
      ncluster = nclust,
      seed = 1
    )
    
    lca = glca::glca(
      formula = formula,
      group = group,
      data = data,
      nclass = nc,
      ncluster = nclust,
      seed = 1
    )
    
    lca2 <- try(glca::glca(
      formula = formula,
      group = group,
      data = data,
      nclass = nc,
      ncluster = nclust,
      coeff.inv = FALSE,
      seed = 1
    ))
    
    if (jmvcore::isError(lca2)) {
      err_string <- stringr::str_interp(" Error in if (maxdiff < eps) break : Covariates can not be computed.")
      stop(err_string)
    }
    
    cf <- glca::gofglca(lca0, lca, lca2, test = "chisq")
    
    ci.g <- cf[["gtable"]]
    ci.d <- cf[["dtable"]]
  }
  
  retlist <- list(ci.g = ci.g, ci.d = ci.d)
  return(retlist)
},

.populateCoTable = function(co) {
  table <- self$results$coTable
  
  as_num <- function(x) {
    if (is.null(x))
      return(NA_real_)
    
    if (length(x) == 0)
      return(NA_real_)
    
    if (is.data.frame(x))
      x <- x[[1]]
    
    x <- x[1]
    
    if (is.numeric(x))
      return(as.numeric(x))
    
    x <- trimws(as.character(x))
    x <- gsub(",", "", x)
    
    suppressWarnings({
      out <- as.numeric(x)
    })
    if (!is.na(out))
      return(out)
    
    x2 <- gsub("^[<>= ]+", "", x)
    suppressWarnings(as.numeric(x2))
  }
  
  fmt_comparison <- function(x) {
    if (length(x) == 0 || is.null(x) || all(is.na(x)))
      return(NA_character_)
    
    x <- x[1]
    if (!nzchar(x))
      return(NA_character_)
    
    x <- gsub("`", "", x, fixed = TRUE)
    x <- gsub("\\^", "", x)
    x <- gsub("Class([0-9]+)[[:space:]]*/[[:space:]]*([0-9]+)", "Class\\1 / \\2", x)
    x
  }
  
  norm_name <- function(x) {
    x <- tolower(x)
    x <- gsub("[[:space:]_.]+", "", x)
    x
  }
  
  find_col <- function(nms, type) {
    nn <- norm_name(nms)
    
    if (type == "or") {
      hit <- which(nn %in% c("oddsratio", "or"))
    } else if (type == "est") {
      hit <- which(nn %in% c("coefficient", "estimate", "coef"))
    } else if (type == "se") {
      hit <- which(nn %in% c("stderr", "stderror", "se"))
    } else if (type == "stat") {
      hit <- which(nn %in% c("tvalue", "zvalue", "waldz", "waldt"))
    } else if (type == "p") {
      hit <- which(grepl("^pr", nn) | nn %in% c("p", "pvalue"))
    } else {
      hit <- integer(0)
    }
    
    if (length(hit) > 0)
      return(nms[hit[1]])
    
    NULL
  }
  
  add_df <- function(df, path) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0)
      return()
    
    rn <- rownames(df)
    if (is.null(rn))
      rn <- rep("", nrow(df))
    
    nms <- names(df)
    
    col_or   <- find_col(nms, "or")
    col_est  <- find_col(nms, "est")
    col_se   <- find_col(nms, "se")
    col_stat <- find_col(nms, "stat")
    col_p    <- find_col(nms, "p")
    
    level_hits   <- path[grepl("^Level", path)]
    cluster_hits <- path[grepl("^Cluster", path)]
    comp_hits    <- path[grepl("^Class", gsub("`", "", path))]
    
    level_name   <- if (length(level_hits) > 0) level_hits[1] else NA_character_
    cluster_name <- if (length(cluster_hits) > 0) cluster_hits[1] else NA_character_
    comp_name    <- fmt_comparison(comp_hits)
    
    cluster_num <- suppressWarnings(as.integer(gsub("[^0-9]+", "", cluster_name)))
    
    for (i in seq_len(nrow(df))) {
      term_name <- rn[i]
      if (is.null(term_name) || is.na(term_name) || !nzchar(term_name))
        term_name <- paste0("Term", i)
      
      lvl_label  <- if (!is.na(level_name) && nzchar(level_name)) level_name else "Level"
      clu_label  <- if (!is.na(cluster_num)) as.character(cluster_num) else as.character(i)
      comp_label <- if (!is.na(comp_name) && nzchar(comp_name)) comp_name else "Comparison"
      
      table$addRow(
        rowKey = paste(lvl_label, clu_label, comp_label, term_name, i, sep = "_"),
        values = list(
          level      = if (!is.na(level_name)) level_name else "",
          cluster    = if (!is.na(cluster_num)) cluster_num else NA_integer_,
          comparison = if (!is.na(comp_name)) comp_name else "",
          term       = term_name,
          or         = if (!is.null(col_or))   as_num(df[[col_or]][i])   else NA_real_,
          est        = if (!is.null(col_est))  as_num(df[[col_est]][i])  else NA_real_,
          se         = if (!is.null(col_se))   as_num(df[[col_se]][i])   else NA_real_,
          z          = if (!is.null(col_stat)) as_num(df[[col_stat]][i]) else NA_real_,
          p          = if (!is.null(col_p))    as_num(df[[col_p]][i])    else NA_real_
        )
      )
    }
  }
  
  walk <- function(x, path = character()) {
    if (is.data.frame(x)) {
      add_df(x, path)
      return(invisible(NULL))
    }
    
    if (is.list(x)) {
      nms <- names(x)
      if (is.null(nms))
        nms <- rep("", length(x))
      
      for (i in seq_along(x)) {
        nm <- nms[i]
        if (is.null(nm) || is.na(nm))
          nm <- ""
        walk(x[[i]], c(path, nm))
      }
    }
  }
  
  walk(co)
}


)
)


appleSpinnerH <- function(message = '') {
  paste0(
    '<div style="text-align:center;padding:24px;">',
    
    '<style>',
    '@keyframes snowsoftAppleDotPulse {',
    '0%, 80%, 100% { transform: scale(0.72); opacity: 0.55; }',
    '40% { transform: scale(1.20); opacity: 1; }',
    '}',
    '</style>',
    
    '<div style="margin-bottom:10px;">',
    
    '<span style="',
    'display:inline-block;',
    'width:12px;',
    'height:12px;',
    'margin:0 5px;',
    'border-radius:50%;',
    'background:#007AFF;',
    'animation:snowsoftAppleDotPulse 1.2s infinite ease-in-out;',
    'vertical-align:middle;',
    '"></span>',
    
    '<span style="',
    'display:inline-block;',
    'width:12px;',
    'height:12px;',
    'margin:0 5px;',
    'border-radius:50%;',
    'background:#34C759;',
    'animation:snowsoftAppleDotPulse 1.2s infinite ease-in-out;',
    'animation-delay:0.15s;',
    'vertical-align:middle;',
    '"></span>',
    
    '<span style="',
    'display:inline-block;',
    'width:12px;',
    'height:12px;',
    'margin:0 5px;',
    'border-radius:50%;',
    'background:#FF9500;',
    'animation:snowsoftAppleDotPulse 1.2s infinite ease-in-out;',
    'animation-delay:0.30s;',
    'vertical-align:middle;',
    '"></span>',
    
    '</div>',
    
    '<div style="font-size:12px;color:#666;">',
    message,
    '</div>',
    
    '</div>'
  )
}