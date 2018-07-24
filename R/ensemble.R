#' Create an ensemble of FitChoice models
#'
#' @param models A \code{list} of models, all of which are of class \code{FitChoice}.
#' @param compare.only Logical; whether to just produce a table comparing the models or
#' additionally combine them to make a new ensemble model.
#' @importFrom flipFormat Labels
#' @export
ChoiceEnsemble <- function(models,
                     compare.only = FALSE) {

    n.models <- length(models)
    if (n.models <= 1)
        stop("At least 2 models are required to create an ensemble.")

    # loose test that models are of the same class, not necessary to use same data
    underlying.class <- checkModelsClass(models)
    result <- list(underlying.class = underlying.class)

    if (!compare.only)   # make the ensemble
    {
        # stricter test that models use the same input data
        dat <- extractCommonData(models, underlying.class)

        result$subset <- models[[1]]$subset
        result$subset.description <- if (is.null(result$subset)) NULL else Labels(result$subset)
        result$n.questions.left.out <- models[[1]]$n.questions.left.out
        result$n.total <- models[[1]]$n.total
        result$n.respondents <- models[[1]]$n.respondents
        result$weights <- models[[1]]$weights
        result$weights.description <- models[[1]]$weights.description
        result$effective.sample.size <- models[[1]]$effective.sample.size
        result$n.questions <- models[[1]]$n.questions
        result$n.alternatives.per.task <- models[[1]]$n.alternatives.per.task

        params <- lapply(models, function(x) x$respondent.parameters)
        ensemble.paramaters <- Reduce("+", params) / n.models
        result$respondent.parameters <- ensemble.paramaters

        accuracy <- accuracyResults(dat, result, dat$n.questions.left.out)
        result$prediction.accuracies <- accuracy$prediction.accuracies
        result$in.sample.accuracy <- accuracy$in.sample.accuracy
        result$out.sample.accuracy <- accuracy$out.sample.accuracy
    }

    # list of items to extract
    statistics <- c("in.sample.accuracy", "out.sample.accuracy", "bic",
                    "log.likelihood", "n.classes", "algorithm", "time.taken")
    statistic.names <- c("In-sample accuracy", "Out-sample accuracy", "BIC",
                         "Log-likelihood", "Classes", "Algorithm", "Time taken (s)")

    # extract mutual statistics
    comparison <- data.frame(matrix(nrow = 0, ncol = length(statistics)))

    for (model in models)
        comparison <- rbind(comparison, model[statistics], stringsAsFactors = FALSE)

    colnames(comparison) <- statistic.names
    rownames(comparison) <- paste("Model", seq(nrow(comparison)))
    if (all(is.na(comparison$`Out-sample accuracy`)))
        comparison$`Out-sample accuracy` <- NULL

    result$comparison <- comparison
    result$ensemble <- !compare.only
    result$n.models <- n.models
    class(result) <- c("ChoiceEnsemble", class(result))
    return(result)
}

checkModelsClass <- function(models) {

    # extract mutual classes of models
    mutual.classes <- Reduce(intersect, lapply(models, class))

    if (length(intersect("FitChoice", mutual.classes)) != 1)
        stop("Ensemble requires all models to be FitChoice.")
}

extractCommonData <- function(models, underlying.class) {

    processed.data <- unique(lapply(models, function(x) x$processed.data))
    if (length(processed.data) != 1)
        stop("Models must have the same input data including weights, subset and questions left out,",
            " but they do not.")
    return(processed.data[[1]])
}


#' @importFrom flipFormat ComparisonTable FormatAsPercent
#' @export
print.ChoiceEnsemble <- function(x, ...) {

    if (x$ensemble)
    {
        title <- paste0("Choice Modeling : Ensemble of ", x$n.models, " models")

        subtitle <- footer <- ""
        # TODO subtitle and footer
        RespondentParametersTable(x$respondent.parameters, title, subtitle, footer)
    }
    else
    {
        tbl <- ComparisonTable(x$comparison,
                               order.values = FALSE,
                               title = paste("Comparison of", nrow(x$comparison), " Choice models"))
        tbl
    }
}


