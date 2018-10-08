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

    if (!is.null(fit$algorithm) && fit$algorithm == "LCA")
        parameterStatisticsLCA(fit)
    else
    {
        is.multi.class <- fit$n.classes > 1

        if (is.multi.class && fit$class.match.fail)
            stop("Parameter statistics are not available as classes from ",
                 "different chains could not be matched.")

        if (is.multi.class)
        {
            if ("class_weights" %in% fit$stan.fit@model_pars)
                ex <- rstan::extract(fit$stan.fit,
                                     pars = c('class_weights', 'theta', 'sigma'),
                                     permuted = FALSE, inc_warmup = FALSE)
            else
                ex <- rstan::extract(fit$stan.fit,
                                     pars = c('covariates_beta', 'theta', 'sigma'),
                                     permuted = FALSE, inc_warmup = FALSE)
        }
        else
            ex <- rstan::extract(fit$stan.fit, pars = c('theta', 'sigma'),
                                 permuted = FALSE, inc_warmup = FALSE)
        sample.stats <- suppressWarnings(rstan::monitor(ex, probs = c()))
        rownames(sample.stats) <- makeLabels(fit, TRUE)
        sample.stats
    }
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
    nms.sigma <- if (!is.null(fit$reduced.respondent.parameters))
                    colnames(fit$reduced.respondent.parameters)
                 else
                    colnames(fit$respondent.parameters)
    nms <- fit$parameter.names

    lbls <- c(rep(paste0(nms, ' (Mean)'), each = n.classes),
              rep(paste0(nms.sigma, ' (St. Dev.)'), each = n.classes))

    if (n.classes > 1L)
        lbls <- paste0(lbls, rep(paste0(', Class ', 1:n.classes), 2 * length(nms)))
    if (add.weight.labels && n.classes > 1L)
    {
        if ('class_weights' %in% fit$stan.fit@sim$pars_oi)
            lbls <- c(paste0('Class ', 1:n.classes, ' size') , lbls)
        else
            lbls <- c(paste0(fit$covariate.names, " (Covariate Coefficient)") , lbls)
    }
    lbls
}
