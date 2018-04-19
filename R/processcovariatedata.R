#' Returns a model matrix for the fixed covariates in a choice model
#' @param formula model formula for the fixed (respondent-specific)
#'     covariates
#' @param data data.frame containing the covariate info for each
#'     respondent
#' @param stan.dat list formed from input data describing the choice
#'     model design and responses, to be used by stan
#' @param subset integer vector giving the subset of data to be used during fitting.
#' @importFrom stats model.matrix
#' @noRd
processCovariateData <- function(formula, data, stan.dat, subset)
{
    non.missing <- apply(data, 1, function(x) !any(is.na(x)))
    filter.subset <- CleanSubset(subset, nrow(data))
    subset <- filter.subset & non.missing

    data <- data[subset, , drop = FALSE]
    cdat <- model.matrix(formula, data)  # [, -1, drop = FALSE]

    if (nrow(cdat) != nrow(stan.dat$X.in))
        stop(gettextf("The length of the data in %s and %s do not match",
                      sQuote("experiment.data"), sQuote("cov.data")))

    stan.dat$P <- ncol(cdat)
    stan.dat$covariates <- cdat

    stan.dat$beta.names <- stan.dat$par.names
    g <- expand.grid(colnames(stan.dat$covariates), stan.dat$par.names, stringsAsFactors = FALSE)
    stan.dat$par.names <- paste(g$Var1, g$Var2, sep = "__")
    g <- expand.grid(colnames(stan.dat$covariates), stan.dat$all.names, stringsAsFactors = FALSE)
    stan.dat$all.names <- paste(g$Var1, g$Var2, sep = "__")

    stan.dat
}