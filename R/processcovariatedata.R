#' @importFrom stats model.matrix
processCovariateData <- function(formula, data, stan.dat, subset)
{
    non.missing <- apply(data, 1, function(x) !any(is.na(x)))
    filter.subset <- CleanSubset(subset, nrow(data))
    subset <- filter.subset & non.missing

    data <- data[subset, ]
    cdat <- model.matrix(formula, data)[, -1]

    if (nrow(cdat) != nrow(stan.dat$X.in))
        stop(gettextf("The length of the data in %s and %s do not match",
                      sQuote("experiment.data"), sQuote("cov.data")))
##:ess-bp-start::browser@nil:##
browser(expr=is.null(.ESSBP.[["@2@"]]));##:ess-bp-end:##

    covariates.out




}
