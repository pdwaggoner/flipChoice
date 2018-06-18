#' Additional processing of the data
#' (it is assummed that it contains covariates)
#' @param dat list formed from input data describing the choice
#'     model design, responses and other data
#' @noRd
processCovariateData <- function(dat, n.classes)
{
    covariates <- dat$covariates
    dat$n.covariates <- ncol(covariates)

    if (n.classes == 1) # fixed covariates
    {
        g <- expand.grid(colnames(covariates), dat$par.names, stringsAsFactors = FALSE)
        dat$par.names <- paste(g$Var1, g$Var2, sep = "__")
        g <- expand.grid(colnames(covariates), dat$all.names, stringsAsFactors = FALSE)
        dat$all.names <- paste(g$Var1, g$Var2, sep = "__")
    }

    for (i in 2:dat$n.covariates) # start at 2 to skip the intercept covariate
        if (!all(sort(unique(covariates[, i])) == c(0, 1)))
        {
            if (sd(covariates[, i]) == 0)
                stop("One of the covariates has no variation.")
            dat$covariates[, i] <- 0.5 * scale(covariates[, i])
        }
    dat
}
