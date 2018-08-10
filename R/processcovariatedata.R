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
        dat$total_rc <- 0L
        dat$V_rc <- 0L
        dat$Zmat <- matrix(nrow = nrow(dat$covariates), ncol = 0L)
        dat$rc_dims <- integer(0)
    }

    if (n.classes == 1) # fixed covariates
    {
        ## cnames <- colnames(covariates)
        cnames <- colnames(dat$Zmat)
        g <- expand.grid(cnames, dat$par.names, stringsAsFactors = FALSE)
        dat$par.names <- paste(g$Var1, g$Var2, sep = "__")
        g <- expand.grid(cnames, dat$all.names, stringsAsFactors = FALSE)
        dat$all.names <- paste(g$Var1, g$Var2, sep = "__")
    }
    dat$covariates <- ScaleNumericCovariates(covariates)
    dat$Xmat <- ScaleNumericCovariates(dat$Xmat)
    dat$covariates <- as.matrix(dat$covariates)
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
        if (!all(sort(unique(covariates[, i])) == c(0, 1))) # is numeric
        {
            if (sd(covariates[, i]) == 0)
                stop("One of the covariates has no variation.")
            covariates[, i] <- 0.5 * scale(covariates[, i])
        }
    covariates
}
