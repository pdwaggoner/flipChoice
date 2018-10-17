#' Additional processing of the data
#' (it is assummed that it contains covariates)
#' @param dat list formed from input data describing the choice
#'     model design, responses and other data
#' @importFrom stats model.frame
#' @importFrom lme4 mkReTrms subbars findbars nobars
#' @noRd
processCovariateData <- function(dat, n.classes, cov.formula, cov.data)
{
    covariates <- dat$covariates
    dat$n.covariates <- NCOL(covariates)
    dat$Xmat <- model.matrix(lme4::nobars(cov.formula), cov.data)
    dat$V_fc <- ncol(dat$Xmat)
    bar.f <- findbars(cov.formula)
    if (!is.null(bar.f))
    {
        mf <- model.frame(subbars(cov.formula), data = cov.data)
        rt <- mkReTrms(bar.f,mf)

        dat$V_rc <- length(rt$Ztlist)
        dat$Zmat <- t(as.matrix(rt$Zt))
        if (dat$V_rc == 1L)
            dat$rc_dims <- array(nrow(rt$Ztlist[[1]]), dim = 1)
        else
            dat$rc_dims <- vapply(rt$Ztlist, nrow, 0L)

        dat$total_rc <- sum(dat$rc_dims)
    }else
    {
        ## choicemodelRC.stan not working if no random cov. in model
        dat$total_rc <- 0L
        dat$V_rc <- 0L
        dat$Zmat <- matrix(nrow = nrow(dat$covariates), ncol = 0L)
        dat$rc_dims <- integer(0)
    }

    ## * "par.names" contains the names of the mean parameters; i.e. the
    ##   parameters shown in the summary and print methods and parameter
    ##   statistics diagnostics (theta in stan)
    ## * for the no cov. and "fixed" cov models, the number of theta and sigma
    ##   parameters are the same and "par.names" can be used for both
    ## * for the "random" cov. model, there are less sigma parameters, so
    ##   depending on whether random effects are returned, sd.names/all.names
    ##   needs to be modified for the call to GetParameterStatistics
    ##
    ## par.names has length {#covar.}*({#altern. per question} + \sum_{i \in attr} {#levels_i-1})
    ## all.names had length {#covar.}*({#altern. per question} + \sum_{i \in attr} {#levels_i})
    ##
    ## "beta.names" contains the names for the respondent parameters (beta in stan model)
    ## beta.names has length ({#altern. per question} + \sum_{i \in attr} {#levels_i-1})
    ## beta.names is always same length and doesn't chg regardless of whether cov. are in model
    ## "all.beta.names" is "beta.names" plus the names of coefficients set to zero (the
    ##   first level of each attribute)
    ##
    ## Better names
    ## par.names: mean.par.names (theta.names)
    ## beta.names: respondent.par.names
    ## all.beta.names: unconstrained.respondent.par.names
    ## all.names: mean.and.sd.par.names (theta.and.sigma.names)
    if (n.classes == 1)  # choicemodelFC or choicemodelRC
    {   ## modify par.names to account for covariates
        ## cnames <- colnames(covariates)
        resp.par.names <- dat$par.names
        g <- expand.grid(colnames(dat$Xmat), resp.par.names, stringsAsFactors = FALSE)
        dat$par.names <- paste(g$Var1, g$Var2, sep = "__")
        if (!is.null(bar.f))
        {
            ## cnames <- c(resp.par.names, names(rt$flist))
            g <- expand.grid(names(rt$flist), resp.par.names, stringsAsFactors = FALSE)
            dat$sd.names <- c(resp.par.names, paste(g$Var1, g$Var2, sep = "__"))
        }else
            dat$sd.names <- resp.par.names

    }
    dat$covariates <- ScaleNumericCovariates(covariates)
    dat$Xmat <- ScaleNumericCovariates(dat$Xmat)
    dat$covariates <- as.matrix(dat$covariates)
    dat$cov.formula <- cov.formula
    dat
}

#' Scales numeric covariates to have a mean of zero and a standard deviation
#' of 0.5 as this is optimal for logit as recommended by Gelman in
# "Scaling regression inputs by dividing by two standard deviations (2008)"
#' @param covariates A matrix of covariates.
#' @export
ScaleNumericCovariates <- function(covariates)
{
    n.covariates <- ncol(covariates)
    idx <- which(!colnames(covariates) %in% "(Intercept)")
    for (i in idx) # start at 2 to skip the intercept covariate
        if (!all(sort(unique(covariates[, i])) %in% c(0, 1))) # is numeric
        {
            if (sd(covariates[, i]) == 0)
                stop("One of the covariates has no variation.")
            covariates[, i] <- 0.5 * scale(covariates[, i])
        }
    covariates
}
