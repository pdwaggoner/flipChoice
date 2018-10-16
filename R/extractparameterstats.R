#' Extract Parameter Statistics from a Choice Model Fit
#'
#' If the Choice Model is from a Latent Class Analysis, a matrix of parameter
#' estimates, standard errors, t-statistics and p-values is returned.
#'
#' If the Choice Model is from Hierarchical Bayes, this function produces a
#' matrix sample statistics of the mean and standard deviation parameters for
#' the distribution from which individual coefficients are sampled.
#' @param fit An object of class \code{"FitChoice"} produced by a call
#'     to \code{FitChoiceModel} with \code{algorithm = "HB-Stan"}.
#' @return In the case of LCA, a matrix of parameter estimates, standard
#'     errors, t-statistics and p-values. For HB, a matrix containing a summary
#'     of the parameter samples from the MCMC results stored in \code{fit},
#'     including mean, standard error, effective sample size, and rhat for each
#'     mean and standard deviation parameter.
#' @importFrom rstan extract monitor
#' @family HB diagnostics
#' @seealso \code{\link[rstan]{monitor}}, \url{https://www.displayr.com/convergence-hb-maxdiff/},
#'     \code{\link{FitChoiceModel}} for an example, \code{\link{TracePlots}},
#' \code{\link{PlotPosteriorIntervals}}
#' @export
ExtractParameterStats <- function(fit)
{
    checkValidFit(fit, require.hb = FALSE)

    # has.grouped.covariate <- hasGroupedCovariate(fit)

    pars <- fit$param.names.list$stan.pars
    pars <- pars[pars != "log_likelihood"]

    if (!is.null(fit$algorithm) && fit$algorithm == "LCA")
        return(parameterStatisticsLCA(fit))

    is.multi.class <- fit$n.classes > 1
    if (is.multi.class)
    {
        if (fit$class.match.fail)
            stop("Parameter statistics are not available as classes from ",
                 "different chains could not be matched.")
        if ("class_weights" %in% fit$stan.fit@model_pars)
            par.names <- c("class_weights", par.names)
        else
            par.names <- c("covariates_beta", par.names)

    }

    ex <- rstan::extract(fit$stan.fit, pars = par.names,
                         permuted = FALSE, inc_warmup = FALSE)

    sample.stats <- suppressWarnings(rstan::monitor(ex, probs = c()))
    rownames(sample.stats) <- makeLabels(fit, TRUE)
    return(sample.stats)
}

#' Error if FitChoiceModel object does not have components necessary
#' for particular diagnostics
#' @param f FitChoiceModel object
#' @param require.hb Does \code{f} have to be fit with \code{algorithm = "HB-Stan"}
#' to work?
#' @return called for it's side-effect of erroring if the fit is not valid
#' @noRd
checkValidFit <- function(f, require.hb = TRUE)
{
    if (!inherits(f, "FitChoice"))
        stop("The selected output was not a choice model output. Please ",
             "select such an output before running this script.")

    if (require.hb && f$algorithm != "HB-Stan")
    {
        stop("The selected output was not a choice model output computed ",
             "using Hierarchical Bayes (Stan). Please select such an ",
             "output before running this script.")
    }else if (f$algorithm == "HB-Stan" && is.null(f$stan.fit))
        stop("This output is only available if FitChoiceModel is called with ",
             shQuote("hb.stanfit = TRUE"),
             ". Please refit the model with that argument set to 'TRUE'.")
}

makeLabels <- function(fit, add.weight.labels = FALSE)
{
    n.classes <- fit$n.classes
    par.names <- fit$param.names.list

    nms.sigma <- par.names$sd.pars
    nms <- par.names$mean.pars
##:ess-bp-start::browser@nil:##
browser(expr=is.null(.ESSBP.[["@4@"]]));##:ess-bp-end:##

    lbls <- c(rep(paste0(nms, " (Mean)"), each = n.classes),
              rep(paste0(nms.sigma, " (St. Dev.)"), each = n.classes))

    ## if (hasGroupedCovariate(fit))
    ##     lbls <- c(lbls, paste0(par.names$covariate.sd.pars, " (Covariate St. Dev.)"))

    if (n.classes > 1L)
        lbls <- paste0(lbls, rep(paste0(", Class ", 1:n.classes), 2 * length(nms)))
    if (add.weight.labels && n.classes > 1L)
    {
        if ("class_weights" %in% fit$stan.fit@sim$pars_oi)
            lbls <- c(paste0("Class ", 1:n.classes, " size"), lbls)
        else
            lbls <- c(paste0(par.names$covariates, " (Covariate Coefficient)"), lbls)
    }
    lbls
}

hasGroupedCovariate <- function(fit)
    fit$stan.fit@model_name == "choicemodelRCdiag"
