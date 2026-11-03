# Internal helpers for Multilevel LPA only. No changes to existing analyses.
.mlpa_prepare <- function(data, vars, group, missing = "complete") {
    if (length(vars) < 2L || length(group) != 1L)
        stop("Select at least two continuous indicators and one group ID.")
    if (anyDuplicated(vars) || group %in% vars)
        stop("Use distinct indicators and a separate group ID.")
    if (!all(c(vars, group) %in% names(data))) stop("A selected variable is unavailable.")
    raw <- as.data.frame(data[, c(group, vars), drop = FALSE])
    if (!all(vapply(raw[vars], is.numeric, logical(1))))
        stop("All indicators must be numeric continuous variables.")
    x <- as.matrix(raw[vars])
    if (any(is.infinite(x))) stop("Indicators contain infinite values; correct them before analysis.")
    id <- raw[[group]]
    if (!(is.factor(id) || is.character(id) || is.numeric(id)))
        stop("Group IDs must be categorical or numeric identifiers.")
    if (is.numeric(id) && any(is.infinite(id))) stop("Group IDs contain infinite values.")
    idOK <- !is.na(id) & nzchar(trimws(as.character(id)))
    observed <- rowSums(!is.na(x))
    keep <- idOK & if (missing == "complete") observed == length(vars) else observed > 0L
    rows <- which(keep)
    raw <- raw[keep, , drop = FALSE]
    if (nrow(raw) < 4L) stop("Too few analyzable observations.")
    if (any(vapply(raw[vars], function(z) {
        z <- z[!is.na(z)]; length(z) < 2L || !is.finite(stats::sd(z)) || stats::sd(z) == 0
    }, logical(1)))) stop("Each indicator needs at least two observed values and nonzero variance.")
    sizes <- table(as.character(raw[[group]]))
    if (length(sizes) < 2L || all(sizes == 1L))
        stop("Multilevel LPA needs multiple groups and repeated observations within groups.")
    # Preserve original labels for display, but give the engine safe column names.
    names(raw) <- c("group_id", paste0("indicator_", seq_along(vars)))
    raw$group_id <- as.character(raw$group_id)
    list(data = raw, rows = rows, rowNames = rownames(data), nOriginal = nrow(data),
         nExcluded = sum(!keep), nGroups = length(sizes), sizes = as.integer(sizes),
         labels = vars, engineVars = names(raw)[-1L])
}

.mlpa_pairs <- function(k, h, compare = FALSE, models = "") {
    pairs <- data.frame(k = as.integer(k), h = as.integer(h))
    if (isTRUE(compare) && nzchar(trimws(models))) {
        tokens <- trimws(strsplit(models, ",", fixed = TRUE)[[1L]])
        if (any(!grepl("^[0-9]+\\s*/\\s*[0-9]+$", tokens)))
            stop("Enter comparison models as profiles/group classes, for example: 2/1, 3/2.")
        extra <- do.call(rbind, lapply(tokens, function(z) as.integer(trimws(strsplit(z, "/", fixed = TRUE)[[1L]]))))
        pairs <- unique(rbind(pairs, data.frame(k = extra[, 1L], h = extra[, 2L])))
    }
    if (anyNA(pairs) || any(pairs$k < 1L | pairs$k > 10L | pairs$h < 1L | pairs$h > 6L))
        stop("Use 1-10 individual profiles and 1-6 group classes.")
    if (any(pairs$k == 1L & pairs$h > 1L))
        stop("With one individual profile, only one group class is identifiable.")
    if (nrow(pairs) > 12L) stop("Compare at most 12 models per run.")
    pairs
}

.mlpa_estimate <- function(prepared, pairs, variance, missing, nStarts, maxIter, seed,
                           checkpoint = function(i, n) NULL) {
    fits <- vector("list", nrow(pairs)); warnings <- vector("list", nrow(pairs))
    errors <- rep("", nrow(pairs))
    for (i in seq_len(nrow(pairs))) {
        checkpoint(i, nrow(pairs))
        current <- tryCatch(withCallingHandlers({
            if (pairs$k[i] >= nrow(prepared$data)) stop("Profiles must be fewer than analyzed observations.")
            if (pairs$h[i] >= prepared$nGroups) stop("Group classes must be fewer than observed groups.")
            latents::multilpa(data = prepared$data, vars = prepared$engineVars,
                id = "group_id", n_profiles = pairs$k[i], n_group_classes = pairs$h[i],
                variance_model = variance, covariance_model = "diagonal",
                missing = if (missing == "complete") "error" else "fiml",
                n_starts = nStarts, max_iter = maxIter, seed = seed, cores = 1L)
        }, warning = function(w) {
            warnings[[i]] <<- unique(c(warnings[[i]], conditionMessage(w)))
            invokeRestart("muffleWarning")
        }), error = function(e) {
            errors[i] <<- conditionMessage(e)
            NULL
        })
        fits[i] <- list(current)
    }
    rows <- lapply(seq_len(nrow(pairs)), function(i) {
        f <- fits[[i]]
        r <- list(selected = if (i == 1L) "Yes" else "", profiles = pairs$k[i], groups = pairs$h[i],
            parameters = NA_integer_, loglik = NA_real_, aic = NA_real_, bicGroups = NA_real_,
            bicIndividuals = NA_real_, entropyIndividual = NA_real_, entropyGroup = NA_real_,
            replicated = "", status = if (is.null(f)) paste0("Failed: ", errors[i]) else "")
        if (!is.null(f)) {
            m <- latents::get_results(f, "model")
            en <- latents::get_results(f, "entropy")
            re <- latents::get_results(f, "replication")
            r$parameters <- m$n_parameters[1L]; r$loglik <- m$log_likelihood[1L]
            r$aic <- m$aic[1L]; r$bicGroups <- m$bic_groups[1L]; r$bicIndividuals <- m$bic_individual[1L]
            for (z in c("individuals", "groups")) {
                value <- en$relative_entropy[en$level == z]
                r[[if (z == "groups") "entropyGroup" else "entropyIndividual"]] <- if (length(value)) value[1L] else NA_real_
            }
            r$replicated <- paste0(re$n_best_replicated[1L], " / ", re$n_starts[1L])
            r$status <- if (!isTRUE(m$converged[1L])) "Not converged" else if (isTRUE(m$boundary[1L])) "Boundary solution" else if (re$n_best_replicated[1L] < 2L) "Best solution not replicated" else "Converged"
            if (length(warnings[[i]])) r$status <- paste0(r$status, "; warning")
        }
        as.data.frame(r, stringsAsFactors = FALSE)
    })
    list(fits = fits, comparison = do.call(rbind, rows), warnings = warnings, errors = errors)
}

.mlpa_extract <- function(fit, prepared) {
    classification <- latents::get_results(fit, "classification")
    ent <- latents::get_results(fit, "entropy")
    prof <- latents::get_results(fit, "profiles")
    comp <- latents::get_results(fit, "profile_probabilities")
    pp <- latents::get_results(fit, "posteriors", format = "wide")
    gp <- latents::get_results(fit, "group_posteriors", format = "wide")
    if (nrow(pp) != nrow(prepared$data) || !identical(as.integer(pp$row), seq_len(nrow(prepared$data))))
        stop("Posterior rows do not match the estimation data.")
    index <- match(as.character(pp$group), as.character(gp$group))
    if (anyNA(index)) stop("A group posterior could not be matched to its group ID.")
    prof$indicator <- prepared$labels[match(prof$indicator, prepared$engineVars)]
    if (anyNA(prof$indicator)) stop("An indicator label could not be matched.")
    groupProp <- unique(comp[, c("group_class", "group_class_probability")])
    starts <- latents::get_results(fit, "starts")
    list(classification = classification, entropy = ent, profiles = prof,
         composition = comp, groupProportions = groupProp, starts = starts,
         individualPosteriors = pp, groupPosteriors = gp, groupIndex = index)
}

.mlpa_output <- function(bundle, saveClasses, saveProbabilities) {
    t <- bundle$tables; prep <- bundle$prepared
    values <- list(); types <- character()
    add <- function(name, value, type) {
        out <- rep(NA_real_, prep$nOriginal); out[prep$rows] <- as.numeric(value)
        values[[name]] <<- out; types[name] <<- type
    }
    if (isTRUE(saveClasses)) {
        add("Individual profile", t$individualPosteriors$profile, "nominal")
        add("Group class", t$groupPosteriors$group_class[t$groupIndex], "nominal")
    }
    if (isTRUE(saveProbabilities)) {
        for (n in grep("^posterior_profile_", names(t$individualPosteriors), value = TRUE))
            add(paste0("P(profile ", sub("posterior_profile_", "", n), ")"), t$individualPosteriors[[n]], "continuous")
        for (n in grep("^posterior_group_class_", names(t$groupPosteriors), value = TRUE))
            add(paste0("P(group class ", sub("posterior_group_class_", "", n), ")"), t$groupPosteriors[[n]][t$groupIndex], "continuous")
    }
    list(values = values, types = types)
}
