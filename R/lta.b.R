

ltaClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "ltaClass",
    inherit = ltaBase,
    private = list(
      .htmlwidget = NULL,
      .dataCache = NULL,  
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) |
            length(self$options$factors[[1]]$vars) == 0) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title = "Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li>Latent transition analysis based on <b>slca</b> R package.</li>',
              '<li>The MAR(Missing at Random) method is applied to handle missing values.</li>',
              '<li>Model specifications are described in the <a href="https://kim0sun.github.io/slca/" target="_blank">page</a>.</li>',
              '<li><b>L1[k]</b>: <b>k</b> denotes the number of latent classes for the first latent class variable <b>L1</b>.</li>',
              '<li><b>PI</b>: Class prevalences.</li>',
              '<li><b>TAU</b>: Transition probabilities.</li>',
              '<li><b>RHO</b>: Item response probabilities.</li>',
              '<li>Class labels are automatically aligned by minimizing differences between item-response probability profiles. The same aligned labels are used in the PI, TAU, and RHO tables and in all transition and stability plots. Under the non-invariant model, class meanings may still differ across time, so diagonal transition probabilities should be interpreted with caution.</li>',
              '<li>Average stay probability is the unweighted mean of the diagonal probabilities in the profile-aligned transition matrix. Average switching probability is 1 minus the average stay probability.</li>',
              '<li>Minor discrepancies between p-values and 95% CIs may occur due to Wald approximation errors.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowLatent/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'
            )
          )
        )
        
        if (self$options$fit1) {
          self$results$fit1$setNote(
            "Note",
            "obj2: LTA with non-invariant model(H1); obj3: LTA with measurement invariance(H0)."
          )
        }
        
        self$results$stay$setNote(
          "Note",
          paste(
            "(1) Average stay probability is the unweighted mean of the diagonal",
            "probabilities in the profile-aligned transition matrix;",
            "(2) Average switching probability is 1 minus the average stay probability."
          )
        )
        
        if (self$options$reg) {
          self$results$reg$setNote(
            "Note",
            "It utilizes logistic regression and employs a three-step approach."
          )
        }
        
      },
      
      .normalizeRefClass = function(ref, nc) {
        if (is.null(ref) || length(ref) == 0)
          return(NULL)
        
        ref <- suppressWarnings(as.integer(ref))
        
        # ref = 0 은 auto reference로 처리
        if (is.na(ref) || ref < 1 || ref > nc)
          return(NULL)
        
        ref
      },
      
      .reorderByReference = function(obj, latentName, ref, nc) {
        ref <- private$.normalizeRefClass(ref, nc)
        
        # NULL이면 auto: 기존 객체 그대로 사용
        if (is.null(ref))
          return(obj)
        
        # 선택한 class를 마지막으로 보냄
        ord <- c(setdiff(seq_len(nc), ref), ref)
        
        args <- c(list(x = obj), stats::setNames(list(ord), latentName))
        obj2 <- do.call(reorder, args)
        
        obj2
      },
      
      .getRefNoteText = function(ref, nc) {
        ref_raw <- suppressWarnings(as.integer(ref))
        ref2 <- private$.normalizeRefClass(ref, nc)
        
        if (is.null(ref2)) {
          if (!is.na(ref_raw) && ref_raw > 0)
            return(paste0("Reference class input ", ref_raw,
                          " is invalid for a ", nc,
                          "-class model; class ", nc, " (auto) was used."))
          
          return(paste0("Reference class: ", nc, " (auto)"))
        }
        
        paste0("Reference class: ", ref2)
      },
      
      .getDisplayClassLabels = function(ref, nc) {
        ref2 <- private$.normalizeRefClass(ref, nc)
        
        if (is.null(ref2)) {
          ref2 <- nc
          ord <- seq_len(nc)
        } else {
          ord <- c(setdiff(seq_len(nc), ref2), ref2)
        }
        
        paste0(ord[seq_len(nc - 1)], "/", ref2)
      },
      
      .run = function() {
        
        if (!isTRUE(self$options$run))
          return()
        
        if (is.null(private$.dataCache)) {
          private$.dataCache <- self$data
        }
        
        data <- private$.dataCache
        
        factors <- self$options$factors
        nfactors <- length(factors)
        
        if (length(factors[[1]]$vars) < 2) return()
        
        if (nfactors > 1) {
          formulas <- list()
          
          for (factor in factors) {
            vars <- factor[["vars"]]
            label <- factor[["label"]]
            
            # y ~ a + b + c 
            vars <- vapply(vars, function(x)
              jmvcore::composeTerm(x), '')
            ind <- paste0(vars, collapse = '+')
            formula <- as.formula(paste0(label, ' ~ ', ind))
            
            formulas[[label]] <- as.formula(formula)
          }
          
          create_sequential_relations <- function(formulas) {
            relations <- c()
            
            for (i in seq_along(formulas)[-length(formulas)]) {
              left <- sub("\\[.*?\\]", "", names(formulas)[i])   # lc1[2]-> lc1
              left <- sub(" ~.*", "", left)
              right <- sub("\\[.*?\\]", "", names(formulas)[i + 1])
              right <- sub(" ~.*", "", right)
              relation <- paste0(left, " ~ ", right)
              relations <- c(relations, as.formula(relation))
            }
            return(relations)
          }
          
          sequential_relations <- create_sequential_relations(formulas)
          form1 <- c(formulas, sequential_relations)
          
          # ---- helper: latent labels + step labels (L1→L2, ...)
          .get_latent_names <- function(factors) {
            labs <- vapply(factors, function(f) f[["label"]], "")
            # "L1[2]" -> "L1"
            labs <- sub("\\[.*?\\]", "", labs)
            labs
          }
          .make_step_labels <- function(latent_names) {
            if (length(latent_names) < 2)
              return(character())
            paste0(latent_names[-length(latent_names)], "\u2192", latent_names[-1])  # →
          }
          
          latent_names <- .get_latent_names(factors)
          step_labels <- .make_step_labels(latent_names)
          .format_class_label <- function(x) {
            x <- as.character(x)
            ifelse(grepl("^Class\\s*", x, ignore.case = TRUE), x, paste("Class", x))
          }
          
          .pi_to_long <- function(lta_obj, latent_names, class_maps = NULL) {
            par <- slca::param(lta_obj)
            pi_raw <- par$pi
            
            if (is.null(pi_raw))
              return(NULL)
            
            if (is.list(pi_raw)) {
              if (length(pi_raw) >= 1 && is.numeric(pi_raw[[1]])) {
                pi_vec <- pi_raw[[1]]
              } else {
                pi_vec <- unlist(pi_raw, use.names = TRUE)
              }
            } else {
              pi_vec <- pi_raw
            }
            
            pi_vec <- as.numeric(pi_vec)
            k <- length(pi_vec)
            display_class <- seq_len(k)
            
            if (!is.null(class_maps) &&
                length(class_maps) >= 1 &&
                length(class_maps[[1]]) == k) {
              display_class <- class_maps[[1]]
            }
            
            out <- data.frame(
              time = latent_names[1],
              class = .format_class_label(display_class),
              prob = pi_vec,
              stringsAsFactors = FALSE
            )
            
            out[order(display_class), , drop = FALSE]
          }
          
          .tau_to_long <- function(lta_obj, step_labels, class_maps = NULL) {
            par <- slca::param(lta_obj)
            tau <- par$tau
            
            if (is.null(tau) || length(tau) == 0)
              return(NULL)
            
            out <- list()
            idx <- 1
            
            for (s in seq_along(tau)) {
              tau_mat <- tau[[s]]
              
              if (is.null(tau_mat) || is.null(dim(tau_mat)))
                next
              
              parent_map <- seq_len(ncol(tau_mat))
              child_map <- seq_len(nrow(tau_mat))
              
              if (!is.null(class_maps)) {
                if (length(class_maps) >= s &&
                    length(class_maps[[s]]) == ncol(tau_mat))
                  parent_map <- class_maps[[s]]
                
                if (length(class_maps) >= s + 1 &&
                    length(class_maps[[s + 1]]) == nrow(tau_mat))
                  child_map <- class_maps[[s + 1]]
              }
              
              step_lab <- if (length(step_labels) >= s)
                step_labels[s]
              else
                paste0("L", s, " → L", s + 1)
              
              for (i in seq_len(nrow(tau_mat))) {
                for (j in seq_len(ncol(tau_mat))) {
                  out[[idx]] <- data.frame(
                    step = step_lab,
                    from = as.character(parent_map[j]),
                    to = as.character(child_map[i]),
                    prob = as.numeric(tau_mat[i, j]),
                    stringsAsFactors = FALSE
                  )
                  idx <- idx + 1
                }
              }
            }
            
            if (length(out) == 0)
              return(NULL)
            
            out <- do.call(rbind, out)
            out$from_num <- suppressWarnings(as.integer(out$from))
            out$to_num <- suppressWarnings(as.integer(out$to))
            out <- out[
              order(
                match(out$step, unique(out$step)),
                out$to_num,
                out$from_num
              ),
              c("step", "from", "to", "prob"),
              drop = FALSE
            ]
            rownames(out) <- NULL
            out
          }
          
          .rho_to_long <- function(lta_obj,
                                   latent_names,
                                   factors,
                                   class_maps = NULL) {
            par <- slca::param(lta_obj)
            rho <- par$rho
            
            if (is.null(rho) || length(rho) == 0)
              return(NULL)
            
            if (!is.list(rho))
              rho <- list(rho)
            
            ntime <- length(latent_names)
            if (length(rho) == 1 && ntime > 1)
              rho <- rep(rho, ntime)
            
            time_labels <- latent_names
            if (length(time_labels) < ntime)
              time_labels <- paste0("L", seq_len(ntime))
            
            out <- list()
            idx <- 1
            
            for (t in seq_len(ntime)) {
              rho_mat <- rho[[t]]
              if (is.null(rho_mat) || is.null(dim(rho_mat)))
                next
              
              rn <- rownames(rho_mat)
              if (is.null(rn))
                rn <- as.character(seq_len(nrow(rho_mat)))
              
              cn <- colnames(rho_mat)
              if (is.null(cn))
                cn <- as.character(seq_len(ncol(rho_mat)))
              
              vars_t <- factors[[min(t, length(factors))]][["vars"]]
              if (is.null(vars_t))
                vars_t <- character()
              
              current_item <- NA_character_
              current_var  <- NA_character_
              row_info <- vector("list", length(rn))
              
              for (r in seq_along(rn)) {
                rr <- trimws(as.character(rn[r]))
                hit <- regmatches(rr, regexec("^(.+?)\\((V\\d+)\\)$", rr))[[1]]
                
                if (length(hit) == 3) {
                  response_label <- trimws(hit[2])
                  current_item   <- hit[3]
                  
                  item_num <- suppressWarnings(as.integer(sub("^V", "", current_item)))
                  if (!is.na(item_num) && item_num >= 1 && item_num <= length(vars_t))
                    current_var <- vars_t[item_num]
                  else
                    current_var <- current_item
                } else {
                  response_label <- rr
                }
                
                if (is.na(current_item) || current_item == "")
                  current_item <- paste0("V", r)
                
                if (is.na(current_var) || current_var == "")
                  current_var <- current_item
                
                row_info[[r]] <- data.frame(
                  time     = time_labels[t],
                  item     = current_item,
                  variable = current_var,
                  response = response_label,
                  stringsAsFactors = FALSE
                )
              }
              
              row_info <- do.call(rbind, row_info)
              
              display_class <- seq_len(ncol(rho_mat))
              if (!is.null(class_maps) &&
                  length(class_maps) >= t &&
                  length(class_maps[[t]]) == ncol(rho_mat)) {
                display_class <- class_maps[[t]]
              }
              
              for (c in seq_len(ncol(rho_mat))) {
                tmp <- row_info
                tmp$class <- .format_class_label(display_class[c])
                tmp$prob  <- as.numeric(rho_mat[, c])
                tmp <- tmp[, c("time", "item", "variable", "response", "class", "prob")]
                out[[idx]] <- tmp
                idx <- idx + 1
              }
            }
            
            if (length(out) == 0)
              return(NULL)
            
            do.call(rbind, out)
          }
          
          # ---- helper: align latent-class labels using RHO profiles
          .rho_matrices <- function(lta_obj, ntime = NULL) {
            par <- slca::param(lta_obj)
            rho <- par$rho
            
            if (is.null(rho))
              return(NULL)
            
            if (!is.list(rho))
              rho <- list(rho)
            
            rho <- lapply(rho, function(x) {
              if (is.null(x) || is.null(dim(x)))
                return(NULL)
              as.matrix(x)
            })
            
            rho <- Filter(Negate(is.null), rho)
            
            if (length(rho) == 0)
              return(NULL)
            
            if (!is.null(ntime) && length(rho) == 1 && ntime > 1)
              rho <- rep(rho, ntime)
            
            rho
          }
          
          .profile_distance <- function(x, y) {
            n <- min(length(x), length(y))
            
            if (n == 0)
              return(Inf)
            
            x <- as.numeric(x[seq_len(n)])
            y <- as.numeric(y[seq_len(n)])
            ok <- is.finite(x) & is.finite(y)
            
            if (!any(ok))
              return(Inf)
            
            sqrt(mean((x[ok] - y[ok])^2))
          }
          
          # Exact minimum-cost assignment without adding a package dependency.
          # Returns p where target[, p[j]] corresponds to reference[, j].
          .minimum_profile_assignment <- function(reference, target) {
            reference <- as.matrix(reference)
            target <- as.matrix(target)
            
            k_ref <- ncol(reference)
            k_tar <- ncol(target)
            
            if (is.null(k_ref) || is.null(k_tar) ||
                k_ref < 1 || k_tar < 1 || k_ref != k_tar)
              return(seq_len(min(k_ref, k_tar)))
            
            k <- k_ref
            cost <- matrix(Inf, nrow = k, ncol = k)
            
            for (i in seq_len(k)) {
              for (j in seq_len(k)) {
                cost[i, j] <- .profile_distance(
                  reference[, i],
                  target[, j]
                )
              }
            }
            
            if (any(!is.finite(apply(cost, 1, min))))
              return(seq_len(k))
            
            best_cost <- Inf
            best_perm <- seq_len(k)
            
            search_assignment <- function(i, available, current_perm, current_cost) {
              if (current_cost >= best_cost)
                return(invisible(NULL))
              
              if (i > k) {
                best_cost <<- current_cost
                best_perm <<- current_perm
                return(invisible(NULL))
              }
              
              ordered <- available[order(cost[i, available])]
              
              for (j in ordered) {
                search_assignment(
                  i + 1,
                  setdiff(available, j),
                  c(current_perm, j),
                  current_cost + cost[i, j]
                )
              }
              
              invisible(NULL)
            }
            
            search_assignment(
              i = 1,
              available = seq_len(k),
              current_perm = integer(),
              current_cost = 0
            )
            
            best_perm
          }
          
          .align_class_profiles <- function(lta_obj,
                                            ntime,
                                            first_reference = NULL) {
            rho <- .rho_matrices(lta_obj, ntime)
            
            if (is.null(rho) || length(rho) == 0)
              return(NULL)
            
            if (length(rho) < ntime)
              rho <- c(rho, rep(list(rho[[length(rho)]]), ntime - length(rho)))
            
            rho <- rho[seq_len(ntime)]
            maps <- vector("list", ntime)
            aligned_profiles <- vector("list", ntime)
            
            k1 <- ncol(rho[[1]])
            
            if (!is.null(first_reference) &&
                ncol(first_reference) == k1) {
              p1 <- .minimum_profile_assignment(first_reference, rho[[1]])
            } else {
              p1 <- seq_len(k1)
            }
            
            map1 <- integer(k1)
            map1[p1] <- seq_len(k1)
            maps[[1]] <- map1
            aligned_profiles[[1]] <- rho[[1]][, p1, drop = FALSE]
            
            if (ntime >= 2) {
              for (t in 2:ntime) {
                current <- rho[[t]]
                previous <- aligned_profiles[[t - 1]]
                
                if (ncol(current) != ncol(previous)) {
                  kt <- ncol(current)
                  pt <- seq_len(kt)
                } else {
                  pt <- .minimum_profile_assignment(previous, current)
                }
                
                mapt <- integer(length(pt))
                mapt[pt] <- seq_along(pt)
                maps[[t]] <- mapt
                aligned_profiles[[t]] <- current[, pt, drop = FALSE]
              }
            }
            
            list(
              maps = maps,
              profiles = aligned_profiles
            )
          }
          
          .tau_to_aligned_long <- function(lta_obj,
                                           step_labels,
                                           class_maps) {
            par <- slca::param(lta_obj)
            tau <- par$tau
            
            if (is.null(tau) || length(tau) == 0)
              return(NULL)
            
            out <- list()
            idx <- 1
            
            for (s in seq_along(tau)) {
              tau_mat <- tau[[s]]
              
              if (is.null(tau_mat) || is.null(dim(tau_mat)))
                next
              
              parent_map <- class_maps[[s]]
              child_map <- class_maps[[s + 1]]
              
              if (length(parent_map) != ncol(tau_mat))
                parent_map <- seq_len(ncol(tau_mat))
              
              if (length(child_map) != nrow(tau_mat))
                child_map <- seq_len(nrow(tau_mat))
              
              step_lab <- if (length(step_labels) >= s)
                step_labels[s]
              else
                paste0("L", s, " \u2192 L", s + 1)
              
              for (i in seq_len(nrow(tau_mat))) {
                for (j in seq_len(ncol(tau_mat))) {
                  out[[idx]] <- data.frame(
                    step = step_lab,
                    from = as.character(parent_map[j]),
                    to = as.character(child_map[i]),
                    prob = as.numeric(tau_mat[i, j]),
                    stringsAsFactors = FALSE
                  )
                  idx <- idx + 1
                }
              }
            }
            
            if (length(out) == 0)
              return(NULL)
            
            do.call(rbind, out)
          }
          
          .fill_noninv_tables <- function(lta_obj,
                                          latent_names,
                                          step_labels,
                                          factors,
                                          class_maps) {
            model_label <- "Non-invariant"
            
            # section header
            self$results$text3$setContent(
              paste0(
                "<div style='margin: 0 0 8px 0;'>",
                "<b>Non-invariant model results</b><br>",
                "Initial class probabilities, transition probabilities, and response probabilities are reported below.",
                "</div>"
              )
            )
            
            # PI
            pi_df <- .pi_to_long(lta_obj, latent_names, class_maps)
            if (!is.null(pi_df) && nrow(pi_df) > 0) {
              tab <- self$results$pi_noninv
              for (i in seq_len(nrow(pi_df))) {
                tab$addRow(
                  rowKey = paste0("pi_noninv_", i),
                  values = list(
                    time  = pi_df$time[i],
                    class = pi_df$class[i],
                    prob  = pi_df$prob[i]
                  )
                )
              }
            }
            # TAU
            tau_df <- .tau_to_long(lta_obj, step_labels, class_maps)
            if (!is.null(tau_df) && nrow(tau_df) > 0) {
              tab <- self$results$tau_noninv
              for (i in seq_len(nrow(tau_df))) {
                tab$addRow(
                  rowKey = paste0(
                    "tau_noninv_",
                    i, "_",
                    as.character(tau_df$step[i]), "_",
                    as.character(tau_df$from[i]), "_",
                    as.character(tau_df$to[i])
                  ),
                  values = list(
                    transition = as.character(tau_df$step[i]),
                    from       = .format_class_label(as.character(tau_df$from[i])),
                    to         = .format_class_label(as.character(tau_df$to[i])),
                    prob       = as.numeric(tau_df$prob[i])
                  )
                )
              }
            }
            # RHO (wide format with dynamic class columns)
            rho_long <- .rho_to_long(
              lta_obj,
              latent_names,
              factors,
              class_maps
            )
            if (!is.null(rho_long) && nrow(rho_long) > 0) {
              tab <- self$results$rho_noninv
              
              classes <- unique(rho_long$class)
              
              for (cls in classes) {
                col_name <- paste0("class_", gsub("\\s+", "", cls))
                tab$addColumn(
                  name = col_name,
                  title = cls,
                  type = "number"
                )
              }
              
              key_df <- unique(rho_long[, c("time", "item", "variable", "response")])
              key_df <- key_df[
                order(
                  key_df$time,
                  key_df$item,
                  suppressWarnings(as.numeric(key_df$response))
                ),
                , drop = FALSE
              ]
              
              for (cls in classes) {
                col_name <- paste0("class_", gsub("\\s+", "", cls))
                key_df[[col_name]] <- NA_real_
              }
              
              for (i in seq_len(nrow(rho_long))) {
                rr <- rho_long[i, ]
                
                hit <- which(
                  key_df$time     == rr$time &
                    key_df$item     == rr$item &
                    key_df$variable == rr$variable &
                    key_df$response == rr$response
                )
                
                if (length(hit) != 1)
                  next
                
                col_name <- paste0("class_", gsub("\\s+", "", rr$class))
                key_df[hit, col_name] <- rr$prob
              }
              
              for (i in seq_len(nrow(key_df))) {
                values <- list(
                  time     = key_df$time[i],
                  item     = key_df$item[i],
                  variable = key_df$variable[i],
                  response = key_df$response[i]
                )
                
                for (cls in classes) {
                  col_name <- paste0("class_", gsub("\\s+", "", cls))
                  values[[col_name]] <- key_df[[col_name]][i]
                }
                
                tab$addRow(
                  rowKey = paste0(
                    "rho_noninv_", i, "_",
                    key_df$time[i], "_",
                    key_df$item[i], "_",
                    key_df$response[i]
                  ),
                  values = values
                )
              }
            }
          }
          
          .fill_inv_tables <- function(lta_obj,
                                       latent_names,
                                       step_labels,
                                       factors,
                                       class_maps) {
            
            self$results$text4$setContent(
              paste0(
                "<div style='margin: 0 0 8px 0;'>",
                "<b>Measurement invariance model results</b><br>",
                "Initial class probabilities, transition probabilities, and response probabilities are reported below.",
                "</div>"
              )
            )
            
            # PI
            pi_df <- .pi_to_long(lta_obj, latent_names, class_maps)
            if (!is.null(pi_df) && nrow(pi_df) > 0) {
              tab <- self$results$pi_inv
              for (i in seq_len(nrow(pi_df))) {
                tab$addRow(
                  rowKey = paste0("pi_inv_", i),
                  values = list(
                    time  = pi_df$time[i],
                    class = pi_df$class[i],
                    prob  = pi_df$prob[i]
                  )
                )
              }
            }
            
            # TAU
            tau_df <- .tau_to_long(lta_obj, step_labels, class_maps)
            if (!is.null(tau_df) && nrow(tau_df) > 0) {
              tab <- self$results$tau_inv
              for (i in seq_len(nrow(tau_df))) {
                tab$addRow(
                  rowKey = paste0(
                    "tau_inv_",
                    i, "_",
                    as.character(tau_df$step[i]), "_",
                    as.character(tau_df$from[i]), "_",
                    as.character(tau_df$to[i])
                  ),
                  values = list(
                    transition = as.character(tau_df$step[i]),
                    from       = .format_class_label(as.character(tau_df$from[i])),
                    to         = .format_class_label(as.character(tau_df$to[i])),
                    prob       = as.numeric(tau_df$prob[i])
                  )
                )
              }
            }
            # RHO (wide + dynamic columns)
            rho_long <- .rho_to_long(
              lta_obj,
              latent_names,
              factors,
              class_maps
            )
            if (!is.null(rho_long) && nrow(rho_long) > 0) {
              tab <- self$results$rho_inv
              
              classes <- unique(rho_long$class)
              
              for (cls in classes) {
                col_name <- paste0("class_", gsub("\\s+", "", cls))
                tab$addColumn(
                  name  = col_name,
                  title = cls,
                  type  = "number"
                )
              }
              
              key_df <- unique(rho_long[, c("time", "item", "variable", "response")])
              key_df <- key_df[
                order(
                  key_df$time,
                  key_df$item,
                  suppressWarnings(as.numeric(key_df$response))
                ),
                , drop = FALSE
              ]
              
              for (cls in classes) {
                col_name <- paste0("class_", gsub("\\s+", "", cls))
                key_df[[col_name]] <- NA_real_
              }
              
              for (i in seq_len(nrow(rho_long))) {
                rr <- rho_long[i, ]
                
                hit <- which(
                  key_df$time     == rr$time &
                    key_df$item     == rr$item &
                    key_df$variable == rr$variable &
                    key_df$response == rr$response
                )
                
                if (length(hit) != 1)
                  next
                
                col_name <- paste0("class_", gsub("\\s+", "", rr$class))
                key_df[hit, col_name] <- rr$prob
              }
              
              for (i in seq_len(nrow(key_df))) {
                values <- list(
                  time     = key_df$time[i],
                  item     = key_df$item[i],
                  variable = key_df$variable[i],
                  response = key_df$response[i]
                )
                
                for (cls in classes) {
                  col_name <- paste0("class_", gsub("\\s+", "", cls))
                  values[[col_name]] <- key_df[[col_name]][i]
                }
                
                tab$addRow(
                  rowKey = paste0(
                    "rho_inv_", i, "_",
                    key_df$time[i], "_",
                    key_df$item[i], "_",
                    key_df$response[i]
                  ),
                  values = values
                )
              }
            }
          }
          
          
          # ---- helper: tau(list of matrices) -> stayer/mover summary
          # NOTE: stayer is a "rate" computed as mean(diag(tau)) = sum(diag)/K
          # so that mover = 1 - stayer is always valid (0~1)
          .tau_diag_summary <- function(lta_obj,
                                        step_labels,
                                        model_label,
                                        class_maps) {
            tau_long <- .tau_to_long(
              lta_obj,
              step_labels,
              class_maps
            )
            
            if (is.null(tau_long) || nrow(tau_long) == 0)
              return(NULL)
            
            steps <- unique(tau_long$step)
            out <- vector("list", length(steps))
            
            for (i in seq_along(steps)) {
              current <- tau_long[
                tau_long$step == steps[i] &
                  tau_long$from == tau_long$to,
                ,
                drop = FALSE
              ]
              
              if (nrow(current) == 0)
                next
              
              stayer_rate <- mean(current$prob, na.rm = TRUE)
              
              out[[i]] <- data.frame(
                model = model_label,
                transition = steps[i],
                stayer = stayer_rate,
                mover = 1 - stayer_rate,
                stringsAsFactors = FALSE
              )
            }
            
            out <- Filter(Negate(is.null), out)
            if (length(out) == 0)
              return(NULL)
            
            do.call(rbind, out)
          }
          
          # ---- helper: fit obj2 (non-invariant) / obj3 (invariant) cleanly
          .fit_obj2 <- function() {
            set.seed(1234)
            
            fit <- slca::slca(formula = form1)
            slca::estimate(fit, data = data)
          }
          
          .fit_obj3 <- function() {
            cons <- self$options$cons
            
            cons1 <- trimws(unlist(strsplit(cons, ",", fixed = TRUE)))
            cons1 <- cons1[nzchar(cons1)]
            
            if (length(cons1) == 0) {
              stop("At least one valid measurement-invariance constraint is required.")
            }
            
            set.seed(1234)
            
            fit <- slca::slca(
              formula = form1,
              constraints = cons1
            )
            
            slca::estimate(fit, data = data)
          }
          
          obj2 <- NULL
          obj3 <- NULL
          
          need_obj2 <- isTRUE(self$options$par2) ||
            isTRUE(self$options$fit1) ||
            isTRUE(self$options$plot1) ||
            isTRUE(self$options$plot3) ||
            isTRUE(self$options$plot4) ||
            isTRUE(self$options$tau) ||
            isTRUE(self$options$stayer)
          
          need_obj3 <- isTRUE(self$options$par3) ||
            isTRUE(self$options$fit1) ||
            isTRUE(self$options$plot2) ||
            isTRUE(self$options$plot3) ||
            isTRUE(self$options$plot4) ||
            isTRUE(self$options$tau) ||
            isTRUE(self$options$stayer)
          
          if (need_obj2)
            obj2 <- .fit_obj2()
          
          if (need_obj3)
            obj3 <- .fit_obj3()
          
          
          ntime_align <- length(latent_names)
          align_noninv <- NULL
          align_inv <- NULL
          maps_noninv <- NULL
          maps_inv <- NULL
          
          if (!is.null(obj2)) {
            align_noninv <- .align_class_profiles(
              obj2,
              ntime = ntime_align
            )
            
            maps_noninv <- if (!is.null(align_noninv)) {
              align_noninv$maps
            } else {
              lapply(
                .rho_matrices(obj2, ntime_align),
                function(x) seq_len(ncol(x))
              )
            }
          }
          
          if (!is.null(obj3)) {
            first_reference <- NULL
            if (!is.null(align_noninv))
              first_reference <- align_noninv$profiles[[1]]
            
            align_inv <- .align_class_profiles(
              obj3,
              ntime = ntime_align,
              first_reference = first_reference
            )
            
            maps_inv <- if (!is.null(align_inv)) {
              align_inv$maps
            } else {
              lapply(
                .rho_matrices(obj3, ntime_align),
                function(x) seq_len(ncol(x))
              )
            }
          }
          
          
          
          
          if (isTRUE(self$options$par2)) {
            
            .fill_noninv_tables(
              lta_obj = obj2,
              latent_names = latent_names,
              step_labels = step_labels,
              factors = factors,
              class_maps = maps_noninv
            )
          }
          
          if (isTRUE(self$options$par3)) {
            
            # if (!exists("obj3")) {
            #   obj3 <- .fit_obj3()
            # }
            
            .fill_inv_tables(
              lta_obj = obj3,
              latent_names = latent_names,
              step_labels = step_labels,
              factors = factors,
              class_maps = maps_inv
            )
          }
          
          if (isTRUE(self$options$fit1)) {
            # if (!exists("obj2")) {
            #   library(magrittr)
            #   set.seed(1234)
            #   obj2 <- slca::slca(formula = form1) %>%
            #     slca::estimate(data = data)
            # }
            # 
            # if (!exists("obj3")) {
            #   cons <- self$options$cons
            #   cons1 <- unlist(strsplit(cons, ","))
            #   
            #   library(magrittr)
            #   set.seed(1234)
            #   obj3 <- slca::slca(formula = form1, constraints = cons1) %>%
            #     slca::estimate(data = data)
            # }
            
            comp <- slca::compare(obj2, obj3, test = 'chisq')
            df <- as.data.frame(comp)
            
            table <- self$results$fit1
            for (name in rownames(df)) {
              table$addRow(
                rowKey = name,
                values = list(
                  df      = df[name, 1],
                  loglik  = df[name, 2],
                  aic     = df[name, 3],
                  bic     = df[name, 4],
                  gsq     = df[name, 5],
                  res     = df[name, 6],
                  p       = df[name, 7]
                )
              )
            }
          }
          
          # -------------------------------------------------
          # Transition Plot(s) (tau)
          # plot1 = non-invariant (obj2)  [forced]
          # plot2 = invariant (obj3)      [forced]
          # facet: L1→L2, L2→L3 ... auto from latent labels
          # -------------------------------------------------
          
          if (isTRUE(self$options$plot1)) {
            # 강제: obj2 (non-invariant)
            obj2_for_plot <- if (!is.null(obj2)) {
              obj2
            } else {
              .fit_obj2()
            }
            
            tau_long <- .tau_to_long(
              obj2_for_plot,
              step_labels,
              maps_noninv
            )
            if (!is.null(tau_long)) {
              image <- self$results$plot1
              image$setState(tau_long)
            }
          }
          
          if (isTRUE(self$options$plot2)) {
            # 강제: obj3 (invariant)
            obj3_for_plot <- if (!is.null(obj3)) obj3 else .fit_obj3()
            
            tau_long <- .tau_to_long(
              obj3_for_plot,
              step_labels,
              maps_inv
            )
            if (!is.null(tau_long)) {
              image <- self$results$plot2
              image$setState(tau_long)
            }
          }
          
          if (isTRUE(self$options$plot3)) {
            
            .tau_stability_long <- function(lta_obj,
                                            step_labels,
                                            model_label,
                                            class_maps) {
              tau_long <- .tau_to_long(
                lta_obj,
                step_labels,
                class_maps
              )
              
              if (is.null(tau_long) || nrow(tau_long) == 0)
                return(NULL)
              
              stability <- tau_long[
                tau_long$from == tau_long$to,
                ,
                drop = FALSE
              ]
              
              if (nrow(stability) == 0)
                return(NULL)
              
              stability$model <- model_label
              stability$class <- .format_class_label(stability$from)
              
              stability[, c(
                "model",
                "step",
                "class",
                "prob"
              ), drop = FALSE]
            }
            
            stability_data <- rbind(
              .tau_stability_long(
                obj2,
                step_labels,
                "Non-invariant (H1)",
                maps_noninv
              ),
              .tau_stability_long(
                obj3,
                step_labels,
                "Invariant (H0)",
                maps_inv
              )
            )
            
            if (!is.null(stability_data) && nrow(stability_data) > 0) {
              image <- self$results$plot3
              image$setState(stability_data)
            }
          }
          
          if (isTRUE(self$options$plot4)) {
            tau_noninv <- .tau_to_long(
              obj2,
              step_labels,
              maps_noninv
            )
            
            tau_inv <- .tau_to_long(
              obj3,
              step_labels,
              maps_inv
            )
            
            if (!is.null(tau_noninv) &&
                nrow(tau_noninv) > 0 &&
                !is.null(tau_inv) &&
                nrow(tau_inv) > 0) {
              
              tau_noninv <- tau_noninv[, c(
                "step",
                "from",
                "to",
                "prob"
              ), drop = FALSE]
              
              tau_inv <- tau_inv[, c(
                "step",
                "from",
                "to",
                "prob"
              ), drop = FALSE]
              
              names(tau_noninv)[names(tau_noninv) == "prob"] <- "prob_noninv"
              names(tau_inv)[names(tau_inv) == "prob"] <- "prob_inv"
              
              difference_data <- merge(
                tau_noninv,
                tau_inv,
                by = c("step", "from", "to"),
                all = FALSE,
                sort = FALSE
              )
              
              if (nrow(difference_data) > 0) {
                difference_data$difference <-
                  difference_data$prob_noninv -
                  difference_data$prob_inv
                
                difference_data$step <- factor(
                  difference_data$step,
                  levels = step_labels
                )
                
                difference_data$from_num <-
                  suppressWarnings(as.integer(difference_data$from))
                difference_data$to_num <-
                  suppressWarnings(as.integer(difference_data$to))
                
                difference_data <- difference_data[
                  order(
                    difference_data$step,
                    difference_data$to_num,
                    difference_data$from_num
                  ),
                  c(
                    "step",
                    "from",
                    "to",
                    "prob_noninv",
                    "prob_inv",
                    "difference"
                  ),
                  drop = FALSE
                ]
                
                rownames(difference_data) <- NULL
                
                image <- self$results$plot4
                image$setState(difference_data)
              }
            }
          }
          
          # -------------------------------------------------
          # ---- TAU table (new): obj2 + obj3 into one table
          # columns: model, transition, from, to, prob
          # -------------------------------------------------
          if (isTRUE(self$options$tau)) {
            
            .fill_tau_table <- function(lta_obj,
                                        model_label,
                                        step_labels,
                                        class_maps) {
              tau_long <- .tau_to_long(
                lta_obj,
                step_labels,
                class_maps
              )
              if (is.null(tau_long) || nrow(tau_long) == 0)
                return()
              
              table <- self$results$tau
              
              for (i in seq_len(nrow(tau_long))) {
                row <- tau_long[i, ]
                rowKey <- paste0(model_label, "_", as.character(row$step), "_", row$from, "_", row$to)
                
                table$addRow(
                  rowKey = rowKey,
                  values = list(
                    model      = model_label,
                    transition = as.character(row$step),
                    from       = as.character(row$from),
                    to         = as.character(row$to),
                    prob       = as.numeric(row$prob)
                  )
                )
              }
            }
            
            # 강제 추정/재사용 (필요할 때만)
            obj2_for_tau <- if (!is.null(obj2)) obj2 else .fit_obj2()
            obj3_for_tau <- if (!is.null(obj3)) obj3 else .fit_obj3()
            
            .fill_tau_table(
              obj2_for_tau,
              "Non-invariant (H1)",
              step_labels,
              maps_noninv
            )
            .fill_tau_table(
              obj3_for_tau,
              "Invariant (H0)",
              step_labels,
              maps_inv
            )
          }
          
          # -------------------------------------------------
          # Stayer / Mover summary (diag of TAU)
          # -------------------------------------------------
          if (isTRUE(self$options$stayer)) {
            
            obj2_for_stay <- if (!is.null(obj2)) obj2 else .fit_obj2()
            obj3_for_stay <- if (!is.null(obj3)) obj3 else .fit_obj3()
            
            df_stay <- rbind(
              .tau_diag_summary(
                obj2_for_stay,
                step_labels,
                "Non-invariant (H1)",
                maps_noninv
              ),
              .tau_diag_summary(
                obj3_for_stay,
                step_labels,
                "Invariant (H0)",
                maps_inv
              )
            )
            
            if (!is.null(df_stay) && nrow(df_stay) > 0) {
              tab <- self$results$stay
              
              for (i in seq_len(nrow(df_stay))) {
                tab$addRow(
                  rowKey = paste0(
                    df_stay$model[i],
                    "_",
                    df_stay$transition[i]
                  ),
                  values = list(
                    model      = df_stay$model[i],
                    transition = df_stay$transition[i],
                    stayer     = df_stay$stayer[i],
                    mover      = df_stay$mover[i]
                  )
                )
              }
              self$results$stay$setNote(
                "Note",
                paste(
                  "(1) Average stay probability: the unweighted mean of the diagonal",
                  "transition probabilities across latent classes;",
                  "(2) Average switching probability: 1 minus the average stay probability."
                )
              )
              
            }
          }
        }
        
        if (!is.null(self$options$covs) && length(self$options$covs) >= 1) {
          regform <- self$options$regform
          regform <- as.formula(regform)
          
          if (nfactors > 1) {
            lc_vars <- character()
            lc_counts <- integer()
            
            for (factor in factors) {
              label <- factor[["label"]]
              class_count <- as.integer(gsub(".*\\[(\\d+)\\].*", "\\1", label))
              lc_name <- sub("\\[.*?\\]", "", label)
              lc_vars <- c(lc_vars, lc_name)
              lc_counts <- c(lc_counts, class_count)
            }
            
            formulas <- list()
            
            for (factor in factors) {
              vars <- factor[["vars"]]
              label <- factor[["label"]]
              
              vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
              ind <- paste0(vars, collapse = '+')
              formula <- as.formula(paste0(label, ' ~ ', ind))
              
              formulas[[label]] <- formula
            }
            
            pf_class_count <- lc_counts[1]
            pf_formula <- as.formula(paste0("pf[", pf_class_count, "] ~ ", paste0(lc_vars, collapse = " + ")))
            
            new_form <- c(formulas, list(pf = pf_formula))
            
            cons <- self$options$cons
            if (cons == "") {
              cons1 <- lc_vars
            } else {
              cons1 <- unlist(strsplit(cons, ","))
            }
            
            set.seed(1234)
            
            lcpa_model <- tryCatch({
              
              fit <- slca::slca(
                formula = new_form,
                constraints = cons1
              )
              
              slca::estimate(fit, data = data)
              
            }, error = function(e) {
              message("error: ", e$message)
              return(NULL)
            })
            
            # regression using 3-step---
            
            if (!is.null(lcpa_model) && length(self$options$covs) >= 1) {
              regform <- self$options$regform
              regform <- as.formula(regform)
              
              # reg <- slca::regress(
              #   lcpa_model,
              #   regform,
              #   imputation = self$options$impu,
              #   method = self$options$method,
              #   data = data
              # )
              # 
              # # result---              
              # coef <- reg$coefficients
              # se <- as.vector(reg$std.err)
              # wald <- as.vector(coef / se)
              # pval <- stats::pnorm(abs(wald), 1, lower.tail = FALSE)
              # 
              # variable_names <- colnames(reg$coefficients)
              # class_info <- rep(rownames(reg$coefficients), each = length(variable_names))
              # 
              # reg.df <- data.frame(
              #   class = class_info[1:length(coef)],
              #   variable = variable_names,
              #   coef = as.vector(coef),
              #   std.err = se,
              #   wald = wald,
              #   p.value = pval
              # )
              
              # reference class----------- 
              target_latent <- "pf"
              nc <- pf_class_count
              
              lcpa_model_reg <- private$.reorderByReference(
                obj = lcpa_model,
                latentName = target_latent,
                ref = self$options$ref,
                nc = nc
              )
              
              reg <- slca::regress(
                lcpa_model_reg,
                regform,
                imputation = self$options$impu,
                method = self$options$method,
                data = data
              )
              
              # result: class × variable-------
              
              coef_mat <- as.matrix(reg$coefficients)
              se_mat   <- as.matrix(reg$std.err)
              
              variable_names <- colnames(coef_mat)
              display_classes <- private$.getDisplayClassLabels(self$options$ref, nc)
              
              if (is.null(variable_names))
                variable_names <- paste0("V", seq_len(ncol(coef_mat)))
              
              if (length(display_classes) != nrow(coef_mat)) {
                if (!is.null(rownames(coef_mat))) {
                  display_classes <- rownames(coef_mat)
                } else {
                  display_classes <- paste0("Class ", seq_len(nrow(coef_mat)))
                }
              }
              
              reg.df <- do.call(rbind, lapply(seq_len(nrow(coef_mat)), function(i) {
                coef_vec <- as.numeric(coef_mat[i, ])
                se_vec   <- as.numeric(se_mat[i, ])
                
                wald <- ifelse(
                  is.finite(se_vec) & abs(se_vec) > .Machine$double.eps,
                  coef_vec / se_vec,
                  NA_real_
                )
                
                pval <- 2 * stats::pnorm(abs(wald), lower.tail = FALSE)
                
                data.frame(
                  class = display_classes[i],
                  variable = variable_names,
                  coef = coef_vec,
                  std.err = se_vec,
                  wald = wald,
                  p.value = pval,
                  stringsAsFactors = FALSE
                )
              }))              
              
              table <- self$results$reg
              df <- as.data.frame(reg.df)
              for (name in rownames(df)) {
                table$addRow(
                  rowKey = name,
                  values = list(
                    cla   = df[name, 1],
                    va    = df[name, 2],
                    co    = df[name, 3],
                    se    = df[name, 4],
                    wald  = df[name, 5],
                    p     = df[name, 6]
                  )
                )
              }
              
              self$results$reg$setNote("Note", private$.getRefNoteText(self$options$ref, nc))
              
              # Covariate Effects Plot
              if (isTRUE(self$options$plot)) {
                plot_data <- reg.df
                image <- self$results$plot
                image$setState(plot_data)
              }
            }
          }
        }
        gc()
      },
      
      .plot = function(image,ggtheme, theme,...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        plot_data <- image$state
        
        plot_data$lower <- plot_data$coef - 1.96 * plot_data$std.err
        plot_data$upper <- plot_data$coef + 1.96 * plot_data$std.err
        
        plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x=variable, y=coef, color=class)) +
          ggplot2::geom_point(size=3, position=ggplot2::position_dodge(width=0.7)) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin=lower, ymax=upper),
                                 width=0.2, position=ggplot2::position_dodge(width=0.7)) +
          ggplot2::geom_hline(yintercept=0, linetype="dashed", color="gray50") +
          ggplot2::labs(title="",
                        x="Covariate", y="Coefficient (log-odds)") +
          ggplot2::coord_flip() +
          ggplot2::theme_minimal(base_size=13)
        
        
        print(plot)
        TRUE
      },
      
      .plot1 = function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        tau_long <- image$state
        tau_long$is_diag <- tau_long$from == tau_long$to
        
        p <- ggplot2::ggplot(
          tau_long,
          ggplot2::aes(x = from, y = to, fill = prob)
        ) +
          ggplot2::geom_tile(color = "grey85", linewidth = 0.5) +
          ggplot2::geom_text(
            ggplot2::aes(
              label = sprintf("%.3f", prob),
              fontface = ifelse(is_diag, "bold", "plain")
            ),
            size = 4.6
          ) +
          ggplot2::scale_fill_viridis_c(
            option = "C",
            limits = c(0, 1),
            name = expression(tau)
          ) +
          ggplot2::facet_wrap(~ step, nrow = 1) +
          ggplot2::labs(
            title = "Transition Probabilities (Non-invariant)",
            x = "Latent Class at Previous Time (Parent)",
            y = "Latent Class at Next Time (Child)"
            # ❌ Reading guide 문장 제거
          ) +
          ggplot2::coord_fixed() +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"),
            strip.text = ggplot2::element_text(face = "bold"),
            axis.title = ggplot2::element_text(face = "bold"),
            axis.text  = ggplot2::element_text(size = 12),
            panel.grid = ggplot2::element_blank(),
            legend.position = "right"
          )
        
        print(p)
        TRUE
      },
      
      
      .plot2 = function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        tau_long <- image$state
        tau_long$is_diag <- tau_long$from == tau_long$to
        
        p <- ggplot2::ggplot(
          tau_long,
          ggplot2::aes(x = from, y = to, fill = prob)
        ) +
          ggplot2::geom_tile(color = "grey85", linewidth = 0.5) +
          ggplot2::geom_text(
            ggplot2::aes(
              label = sprintf("%.3f", prob),
              fontface = ifelse(is_diag, "bold", "plain")
            ),
            size = 4.6
          ) +
          ggplot2::scale_fill_viridis_c(
            option = "C",
            limits = c(0, 1),
            name = expression(tau)
          ) +
          ggplot2::facet_wrap(~ step, nrow = 1) +
          ggplot2::labs(
            title = "Transition Probabilities (Invariant)",
            x = "Latent Class at Previous Time (Parent)",
            y = "Latent Class at Next Time (Child)"
            # ❌ Reading guide 문장 제거
          ) +
          ggplot2::coord_fixed() +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"),
            strip.text = ggplot2::element_text(face = "bold"),
            axis.title = ggplot2::element_text(face = "bold"),
            axis.text  = ggplot2::element_text(size = 12),
            panel.grid = ggplot2::element_blank(),
            legend.position = "right"
          )
        
        print(p)
        TRUE
      },
      
      .plot3 = function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        stability_data <- image$state
        
        stability_data$step <- factor(
          stability_data$step,
          levels = unique(stability_data$step)
        )
        
        stability_data$class <- factor(
          stability_data$class,
          levels = unique(stability_data$class)
        )
        
        stability_data$model <- factor(
          stability_data$model,
          levels = c(
            "Non-invariant (H1)",
            "Invariant (H0)"
          ),
          labels = c(
            "Non-invariant",
            "Invariant"
          )
        )
        
        class_levels <- levels(stability_data$class)
        
        stability_data$label_vjust <- ifelse(
          stability_data$class == class_levels[1],
          -0.9,
          1.5
        )
        
        dodge <- ggplot2::position_dodge(width = 0.08)
        
        p <- ggplot2::ggplot(
          stability_data,
          ggplot2::aes(
            x = step,
            y = prob,
            color = class,
            linetype = class,
            shape = class,
            group = class
          )
        ) +
          ggplot2::geom_line(
            linewidth = 1,
            position = dodge
          ) +
          ggplot2::geom_point(
            size = 3,
            position = dodge
          ) +
          ggplot2::geom_text(
            ggplot2::aes(
              label = sprintf("%.3f", prob),
              vjust = label_vjust
            ),
            position = dodge,
            show.legend = FALSE,
            size = 4
          ) +
          ggplot2::facet_wrap(
            ~ model,
            nrow = 1
          ) +
          ggplot2::scale_y_continuous(
            limits = c(0, 1),
            breaks = seq(0, 1, 0.2),
            expand = ggplot2::expansion(
              mult = c(0.02, 0.10)
            )
          ) +
          ggplot2::labs(
            title = "",
            x = "Transition",
            y = "Stability Probability",
            color = "Latent Class",
            linetype = "Latent Class",
            shape = "Latent Class"
          ) +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(
              face = "bold"
            ),
            strip.text = ggplot2::element_text(
              face = "bold"
            ),
            axis.title = ggplot2::element_text(
              face = "bold"
            ),
            axis.text = ggplot2::element_text(
              size = 12
            ),
            panel.grid.minor = ggplot2::element_blank(),
            legend.position = "right"
          )
        
        print(p)
        TRUE
      },
      
      .plot4 = function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        difference_data <- image$state
        
        difference_data$step <- factor(
          difference_data$step,
          levels = unique(difference_data$step)
        )
        
        difference_data$from <- factor(
          difference_data$from,
          levels = sort(unique(difference_data$from))
        )
        
        difference_data$to <- factor(
          difference_data$to,
          levels = sort(unique(difference_data$to))
        )
        
        limit <- max(
          abs(difference_data$difference),
          na.rm = TRUE
        )
        
        if (!is.finite(limit) || limit < 1e-8)
          limit <- 0.01
        
        p <- ggplot2::ggplot(
          difference_data,
          ggplot2::aes(
            x = from,
            y = to,
            fill = difference
          )
        ) +
          ggplot2::geom_tile(
            color = "grey85",
            linewidth = 0.5
          ) +
          ggplot2::geom_text(
            ggplot2::aes(
              label = sprintf("%+.3f", difference)
            ),
            size = 4.6
          ) +
          ggplot2::scale_fill_gradient2(
            low = "#3B4CC0",
            mid = "white",
            high = "#B40426",
            midpoint = 0,
            limits = c(-limit, limit),
            name = expression(Delta * tau)
          ) +
          ggplot2::facet_wrap(
            ~ step,
            nrow = 1
          ) +
          ggplot2::labs(
            title = "",
            subtitle = "Non-invariant − Invariant",
            x = "Latent Class at Previous Time (Parent)",
            y = "Latent Class at Next Time (Child)"
          ) +
          ggplot2::coord_fixed() +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::theme(
            plot.subtitle = ggplot2::element_text(
              face = "italic"
            ),
            strip.text = ggplot2::element_text(
              face = "bold"
            ),
            axis.title = ggplot2::element_text(
              face = "bold"
            ),
            axis.text = ggplot2::element_text(
              size = 12
            ),
            panel.grid = ggplot2::element_blank(),
            legend.position = "right"
          )
        
        print(p)
        TRUE
      }
      
    )
  )

# Example with R---

# library(slca)
# library(magrittr)
#
# #LCA---
# data = nlsy97
# set.seed(1234)
# obj <- slca(L1(2) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98) %>%
#   estimate(data = nlsy97)
# par<- slca::param(obj)
# par
# plot(obj)
# summary(obj)
# # Regression--
# set.seed(1234)
# obj0 %>% slca::regress(L1 ~ SEX, nlsy97)
#

# #####################
# data = nlsy97
# set.seed(1234)
# obj1 <- slca(L1(2) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98,
#              L2(2) ~ ESMK_03 + FSMK_03 + DSMK_03 + HSMK_03,
#              L1~L2) %>%
#   estimate(data = nlsy97)
# par1<- slca::param(obj1)
# par1
#
# # Regression--
# set.seed(1234)
# obj1 %>% slca::regress(L1 ~ SEX, nlsy97)

# #########################
# data = nlsy97
# set.seed(1234)
# obj2 <- slca(L1(2) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98,
#              L2(2) ~ ESMK_03 + FSMK_03 + DSMK_03 + HSMK_03,
#              L1~L2,constraints = c("L1", "L2")) %>%
#   estimate(data = nlsy97)
# par2<- slca::param(obj2)
# par2
# plot(obj2)
# # Regression--
# set.seed(1234)
# obj2 %>% slca::regress(L1 ~ SEX, nlsy97)
