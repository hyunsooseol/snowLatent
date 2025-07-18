
# This file is automatically generated, you probably don't want to edit this

glcaOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "glcaOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            vars = NULL,
            covs = NULL,
            group = NULL,
            nc = 2,
            fit = TRUE,
            mia = FALSE,
            mir = FALSE,
            cia = FALSE,
            cir = FALSE,
            marginal = FALSE,
            preval = FALSE,
            post = FALSE,
            item = FALSE,
            gamma = FALSE,
            co = FALSE,
            plot1 = FALSE,
            plot2 = FALSE,
            plot3 = FALSE,
            angle = 0,
            width = 500,
            height = 500,
            width1 = 500,
            height1 = 500, ...) {

            super$initialize(
                package="snowLatent",
                name="glca",
                requiresData=TRUE,
                ...)

            private$..vars <- jmvcore::OptionVariables$new(
                "vars",
                vars)
            private$..covs <- jmvcore::OptionVariables$new(
                "covs",
                covs,
                suggested=list(
                    "nominal",
                    "continuous"),
                permitted=list(
                    "factor",
                    "numeric"))
            private$..group <- jmvcore::OptionVariable$new(
                "group",
                group,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..nc <- jmvcore::OptionInteger$new(
                "nc",
                nc,
                min=2,
                default=2)
            private$..fit <- jmvcore::OptionBool$new(
                "fit",
                fit,
                default=TRUE)
            private$..mia <- jmvcore::OptionBool$new(
                "mia",
                mia,
                default=FALSE)
            private$..mir <- jmvcore::OptionBool$new(
                "mir",
                mir,
                default=FALSE)
            private$..cia <- jmvcore::OptionBool$new(
                "cia",
                cia,
                default=FALSE)
            private$..cir <- jmvcore::OptionBool$new(
                "cir",
                cir,
                default=FALSE)
            private$..marginal <- jmvcore::OptionBool$new(
                "marginal",
                marginal,
                default=FALSE)
            private$..preval <- jmvcore::OptionBool$new(
                "preval",
                preval,
                default=FALSE)
            private$..post <- jmvcore::OptionBool$new(
                "post",
                post,
                default=FALSE)
            private$..item <- jmvcore::OptionBool$new(
                "item",
                item,
                default=FALSE)
            private$..gamma <- jmvcore::OptionBool$new(
                "gamma",
                gamma,
                default=FALSE)
            private$..co <- jmvcore::OptionBool$new(
                "co",
                co,
                default=FALSE)
            private$..plot1 <- jmvcore::OptionBool$new(
                "plot1",
                plot1,
                default=FALSE)
            private$..plot2 <- jmvcore::OptionBool$new(
                "plot2",
                plot2,
                default=FALSE)
            private$..plot3 <- jmvcore::OptionBool$new(
                "plot3",
                plot3,
                default=FALSE)
            private$..angle <- jmvcore::OptionNumber$new(
                "angle",
                angle,
                min=0,
                max=90,
                default=0)
            private$..width <- jmvcore::OptionInteger$new(
                "width",
                width,
                default=500)
            private$..height <- jmvcore::OptionInteger$new(
                "height",
                height,
                default=500)
            private$..width1 <- jmvcore::OptionInteger$new(
                "width1",
                width1,
                default=500)
            private$..height1 <- jmvcore::OptionInteger$new(
                "height1",
                height1,
                default=500)

            self$.addOption(private$..vars)
            self$.addOption(private$..covs)
            self$.addOption(private$..group)
            self$.addOption(private$..nc)
            self$.addOption(private$..fit)
            self$.addOption(private$..mia)
            self$.addOption(private$..mir)
            self$.addOption(private$..cia)
            self$.addOption(private$..cir)
            self$.addOption(private$..marginal)
            self$.addOption(private$..preval)
            self$.addOption(private$..post)
            self$.addOption(private$..item)
            self$.addOption(private$..gamma)
            self$.addOption(private$..co)
            self$.addOption(private$..plot1)
            self$.addOption(private$..plot2)
            self$.addOption(private$..plot3)
            self$.addOption(private$..angle)
            self$.addOption(private$..width)
            self$.addOption(private$..height)
            self$.addOption(private$..width1)
            self$.addOption(private$..height1)
        }),
    active = list(
        vars = function() private$..vars$value,
        covs = function() private$..covs$value,
        group = function() private$..group$value,
        nc = function() private$..nc$value,
        fit = function() private$..fit$value,
        mia = function() private$..mia$value,
        mir = function() private$..mir$value,
        cia = function() private$..cia$value,
        cir = function() private$..cir$value,
        marginal = function() private$..marginal$value,
        preval = function() private$..preval$value,
        post = function() private$..post$value,
        item = function() private$..item$value,
        gamma = function() private$..gamma$value,
        co = function() private$..co$value,
        plot1 = function() private$..plot1$value,
        plot2 = function() private$..plot2$value,
        plot3 = function() private$..plot3$value,
        angle = function() private$..angle$value,
        width = function() private$..width$value,
        height = function() private$..height$value,
        width1 = function() private$..width1$value,
        height1 = function() private$..height1$value),
    private = list(
        ..vars = NA,
        ..covs = NA,
        ..group = NA,
        ..nc = NA,
        ..fit = NA,
        ..mia = NA,
        ..mir = NA,
        ..cia = NA,
        ..cir = NA,
        ..marginal = NA,
        ..preval = NA,
        ..post = NA,
        ..item = NA,
        ..gamma = NA,
        ..co = NA,
        ..plot1 = NA,
        ..plot2 = NA,
        ..plot3 = NA,
        ..angle = NA,
        ..width = NA,
        ..height = NA,
        ..width1 = NA,
        ..height1 = NA)
)

glcaResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "glcaResults",
    inherit = jmvcore::Group,
    active = list(
        instructions = function() private$.items[["instructions"]],
        text = function() private$.items[["text"]],
        fit = function() private$.items[["fit"]],
        mia = function() private$.items[["mia"]],
        mir = function() private$.items[["mir"]],
        cia = function() private$.items[["cia"]],
        cir = function() private$.items[["cir"]],
        marginal = function() private$.items[["marginal"]],
        preval = function() private$.items[["preval"]],
        plot1 = function() private$.items[["plot1"]],
        plot2 = function() private$.items[["plot2"]],
        plot3 = function() private$.items[["plot3"]],
        text3 = function() private$.items[["text3"]],
        text1 = function() private$.items[["text1"]],
        text4 = function() private$.items[["text4"]],
        text2 = function() private$.items[["text2"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Multiple Group LCA",
                refs="snowLatent")
            self$add(jmvcore::Html$new(
                options=options,
                name="instructions",
                title="Instructions",
                visible=TRUE))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text",
                title="Model information"))
            self$add(jmvcore::Table$new(
                options=options,
                name="fit",
                title="Model fit",
                rows=1,
                visible="(fit)",
                clearWith=list(
                    "vars",
                    "nc",
                    "covs",
                    "group"),
                refs="glca",
                columns=list(
                    list(
                        `name`="class", 
                        `title`="Class", 
                        `type`="number"),
                    list(
                        `name`="loglik", 
                        `title`="Log-likelihood", 
                        `type`="number"),
                    list(
                        `name`="AIC", 
                        `title`="AIC", 
                        `type`="number"),
                    list(
                        `name`="BIC", 
                        `title`="BIC", 
                        `type`="number"),
                    list(
                        `name`="entropy", 
                        `title`="Entropy", 
                        `type`="number"),
                    list(
                        `name`="df", 
                        `title`="df", 
                        `type`="integer"),
                    list(
                        `name`="Gsq", 
                        `title`="G\u00B2", 
                        `type`="number"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="mia",
                title="Absolute model fit for measurement invariance",
                visible="(mia)",
                refs="glca",
                clearWith=list(
                    "vars",
                    "covs",
                    "nc",
                    "group"),
                columns=list(
                    list(
                        `name`="name", 
                        `title`="Model", 
                        `type`="text", 
                        `content`="($key)"),
                    list(
                        `name`="loglik", 
                        `title`="Log-likelihood", 
                        `type`="number"),
                    list(
                        `name`="aic", 
                        `title`="AIC", 
                        `type`="number"),
                    list(
                        `name`="bic", 
                        `title`="BIC", 
                        `type`="number"),
                    list(
                        `name`="entropy", 
                        `title`="Entropy", 
                        `type`="number"),
                    list(
                        `name`="df", 
                        `title`="df", 
                        `type`="integer"),
                    list(
                        `name`="gsq", 
                        `title`="G\u00B2", 
                        `type`="number"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="mir",
                title="Relative model fit for measurement invariance",
                visible="(mir)",
                refs="glca",
                clearWith=list(
                    "vars",
                    "covs",
                    "nc",
                    "group"),
                columns=list(
                    list(
                        `name`="name", 
                        `title`="Model", 
                        `type`="text", 
                        `content`="($key)"),
                    list(
                        `name`="para", 
                        `title`="Parameter", 
                        `type`="number"),
                    list(
                        `name`="loglik", 
                        `title`="Log-likelihood", 
                        `type`="number"),
                    list(
                        `name`="df", 
                        `title`="df", 
                        `type`="integer"),
                    list(
                        `name`="dev", 
                        `title`="Deviance", 
                        `type`="number"),
                    list(
                        `name`="p", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="cia",
                title="Absolute model fit for the equality of coefficients",
                visible="(cia)",
                refs="glca",
                clearWith=list(
                    "vars",
                    "covs",
                    "nc",
                    "group"),
                columns=list(
                    list(
                        `name`="name", 
                        `title`="Model", 
                        `type`="text", 
                        `content`="($key)"),
                    list(
                        `name`="loglik", 
                        `title`="Log-likelihood", 
                        `type`="number"),
                    list(
                        `name`="aic", 
                        `title`="AIC", 
                        `type`="number"),
                    list(
                        `name`="bic", 
                        `title`="BIC", 
                        `type`="number"),
                    list(
                        `name`="entropy", 
                        `title`="Entropy", 
                        `type`="number"),
                    list(
                        `name`="df", 
                        `title`="df", 
                        `type`="integer"),
                    list(
                        `name`="gsq", 
                        `title`="G\u00B2", 
                        `type`="number"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="cir",
                title="Relative model fit for the equality of coefficients",
                visible="(cir)",
                refs="glca",
                clearWith=list(
                    "vars",
                    "covs",
                    "nc",
                    "group"),
                columns=list(
                    list(
                        `name`="name", 
                        `title`="Model", 
                        `type`="text", 
                        `content`="($key)"),
                    list(
                        `name`="para", 
                        `title`="Parameter", 
                        `type`="number"),
                    list(
                        `name`="loglik", 
                        `title`="Log-likelihood", 
                        `type`="number"),
                    list(
                        `name`="df", 
                        `title`="df", 
                        `type`="integer"),
                    list(
                        `name`="dev", 
                        `title`="Deviance", 
                        `type`="number"),
                    list(
                        `name`="p", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="marginal",
                title="Marginal prevalences for latent class",
                visible="(marginal)",
                refs="glca",
                clearWith=list(
                    "vars",
                    "covs",
                    "nc",
                    "group"),
                columns=list(
                    list(
                        `name`="name", 
                        `title`="", 
                        `type`="text", 
                        `content`="($key)"),
                    list(
                        `name`="value", 
                        `title`="Probability", 
                        `type`="number"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="preval",
                title="Class prevalences by group",
                refs="glca",
                visible="(preval)",
                clearWith=list(
                    "vars",
                    "nc",
                    "group",
                    "covs"),
                columns=list(
                    list(
                        `name`="name", 
                        `title`="", 
                        `type`="text", 
                        `content`="($key)"))))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot1",
                title="Profile plot",
                renderFun=".plot1",
                visible="(plot1)",
                refs="glca",
                requiresData=TRUE,
                clearWith=list(
                    "vars",
                    "nc",
                    "group",
                    "covs",
                    "width",
                    "height")))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot2",
                title="Item response probability by group(Measurement invariance=TRUE)",
                renderFun=".plot2",
                visible="(plot2)",
                refs="snowLatent",
                clearWith=list(
                    "vars",
                    "nc",
                    "group",
                    "covs",
                    "angle",
                    "width1",
                    "height1")))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot3",
                title="Item response probability by group(Measurement invariance=FALSE)",
                renderFun=".plot3",
                visible="(plot3)",
                refs="snowLatent",
                requiresData=TRUE,
                clearWith=list(
                    "vars",
                    "nc",
                    "group",
                    "covs",
                    "angle",
                    "width1",
                    "height1")))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text3",
                title="Logistic regression"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text1",
                title="Item response probability"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text4",
                title="Prevalence for level-1 class(gamma)"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text2",
                title="Posterior probability"))}))

glcaBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "glcaBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "snowLatent",
                name = "glca",
                version = c(1,0,0),
                options = options,
                results = glcaResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Multiple Group LCA
#'
#' 
#' @param data the data as a data frame
#' @param vars .
#' @param covs .
#' @param group .
#' @param nc .
#' @param fit .
#' @param mia .
#' @param mir .
#' @param cia .
#' @param cir .
#' @param marginal .
#' @param preval .
#' @param post .
#' @param item .
#' @param gamma .
#' @param co .
#' @param plot1 .
#' @param plot2 .
#' @param plot3 .
#' @param angle .
#' @param width .
#' @param height .
#' @param width1 .
#' @param height1 .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$fit} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$mia} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$mir} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$cia} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$cir} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$marginal} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$preval} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$plot1} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plot2} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plot3} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$text3} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$text1} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$text4} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$text2} \tab \tab \tab \tab \tab a preformatted \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$fit$asDF}
#'
#' \code{as.data.frame(results$fit)}
#'
#' @export
glca <- function(
    data,
    vars,
    covs,
    group,
    nc = 2,
    fit = TRUE,
    mia = FALSE,
    mir = FALSE,
    cia = FALSE,
    cir = FALSE,
    marginal = FALSE,
    preval = FALSE,
    post = FALSE,
    item = FALSE,
    gamma = FALSE,
    co = FALSE,
    plot1 = FALSE,
    plot2 = FALSE,
    plot3 = FALSE,
    angle = 0,
    width = 500,
    height = 500,
    width1 = 500,
    height1 = 500) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("glca requires jmvcore to be installed (restart may be required)")

    if ( ! missing(vars)) vars <- jmvcore::resolveQuo(jmvcore::enquo(vars))
    if ( ! missing(covs)) covs <- jmvcore::resolveQuo(jmvcore::enquo(covs))
    if ( ! missing(group)) group <- jmvcore::resolveQuo(jmvcore::enquo(group))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(vars), vars, NULL),
            `if`( ! missing(covs), covs, NULL),
            `if`( ! missing(group), group, NULL))

    for (v in group) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- glcaOptions$new(
        vars = vars,
        covs = covs,
        group = group,
        nc = nc,
        fit = fit,
        mia = mia,
        mir = mir,
        cia = cia,
        cir = cir,
        marginal = marginal,
        preval = preval,
        post = post,
        item = item,
        gamma = gamma,
        co = co,
        plot1 = plot1,
        plot2 = plot2,
        plot3 = plot3,
        angle = angle,
        width = width,
        height = height,
        width1 = width1,
        height1 = height1)

    analysis <- glcaClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

