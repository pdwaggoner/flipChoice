#' @importFrom flipU InterceptExceptions
hierarchicalBayesChoiceModel <- function(dat, n.iterations = 500, n.chains = 8,
                                         max.tree.depth = 10,
                                         adapt.delta = 0.8, seed = 123,
                                         keep.samples = FALSE, n.classes = 1,
                                         include.stanfit = TRUE,
                                         normal.covariance = "Full",
                                         show.stan.warnings = TRUE,
                                         beta.draws.to.keep = 0, ...)
{
    if (n.iterations <= 0)
        stop("The specified number of iterations must be greater than 0.")

    # allows Stan chains to run in parallel on multiprocessor machines
    options(mc.cores = parallel::detectCores())

    stan.dat <- createStanData(dat, n.classes, normal.covariance)

    has.covariates <- !is.null(dat$covariates)
    stan.model <- stanModel(n.classes, normal.covariance,
                            has.covariates,
                            has.covariates && dat$total_rc > 0)

    on.warnings <- GetStanWarningHandler(show.stan.warnings)
    on.error <- GetStanErrorHandler()

    keep.beta <- beta.draws.to.keep > 0

    InterceptExceptions(
        {
            stan.fit <- RunStanSampling(stan.dat, n.iterations, n.chains,
                                        max.tree.depth, adapt.delta, seed,
                                        stan.model, keep.beta, ...)
        }, warning.handler = on.warnings, error.handler = on.error)

    matched <- MatchChainClasses(stan.fit, n.chains, n.classes, stan.dat$V)
    stan.fit <- matched$stan.fit
    class.match.fail <- matched$match.fail

    param.names <- createNamesList(dat, keep.beta, stan.model)
    result <- list(param.names.list = param.names)

    result$reduced.respondent.parameters <- ComputeRespPars(stan.fit,
                                                    param.names$respondent.pars,
                                                    dat$subset,
                                                    dat$parameter.scales)
    result$respondent.parameters <- ComputeRespPars(stan.fit,
                                                param.names$respondent.pars,
                                                dat$subset,
                                                dat$parameter.scales,
                                    param.names$unconstrained.respondent.pars)
    result$class.match.fail <- class.match.fail

    if (!class.match.fail)
        result$parameter.statistics <- GetParameterStatistics(stan.fit,
                                                              param.names$mean.pars,
                                                              n.classes,
                                                              param.names$sd.pars)
    if (include.stanfit)
    {
        result$stan.fit <- if (keep.samples)
            stan.fit
        else
            ReduceStanFitSize(stan.fit)
        if (keep.beta)
            result$beta.draws <- ExtractBetaDraws(stan.fit,
                                                  beta.draws.to.keep)
    }




    n.hb.parameters <- numberOfHBParameters(stan.dat)
    result <- c(result, LogLikelihoodAndBIC(stan.fit, n.hb.parameters,
                                            stan.dat$R,
                                            dat$n.questions.left.out,
                                            dat$subset))
    result
}

#' @title RunStanSampling
#' @description Wrapper function for \code{rstan:stan} and
#' \code{rstan:sampling} to run Stan HB analysis.
#' @param stan.dat The data to be passed to Stan.
#' @param n.iterations The number of iterations in the analysis.
#' @param n.chains The number of chains in the analysis.
#' @param max.tree.depth Maximum tree depth setting. See Stan documentation.
#' @param adapt.delta Adapt delta setting. See Stan documentation.
#' @param seed Random seed.
#' @param stan.model Complied Stan model
#' @param keep.beta Whether retain the beta draws in the output.
#' @param ... Additional parameters to pass on to \code{rstan::stan} and
#' \code{rstan::sampling}.
#' @return A stanfit object.
#' @importFrom rstan stan sampling
#' @import Rcpp
#' @export
RunStanSampling <- function(stan.dat, n.iterations, n.chains,
                            max.tree.depth, adapt.delta,
                            seed, stan.model, keep.beta, ...)
{
    pars <- stanParameters(stan.dat, keep.beta, stan.model)
    init <- initialParameterValues(stan.dat)
    sampling(stan.model, data = stan.dat, chains = n.chains,
             pars = pars, iter = n.iterations, seed = seed,
             control = list(max_treedepth = max.tree.depth,
                            adapt_delta = adapt.delta),
             init = init, ...)
}

stanParameters <- function(stan.dat, keep.beta, stan.model)
{
    full.covariance <- is.null(stan.dat$U)
    multiple.classes <- !is.null(stan.dat$P)
    has.covariates <- !is.null(stan.dat$covariates)

    pars <- c("theta", "sigma")

    if (multiple.classes)
    {
        if (has.covariates)
            pars <- c(pars, "covariates_beta")
        else
            pars <- c(pars, "class_weights")
    }else if (stan.model@model_name == "choicemodelRCdiag")
        pars <- c("resp_fixed_coef", "sigma", "sig_rc",
                  "log_likelihood")
    if (keep.beta)
        pars <- c(pars, "beta")

    pars
}

initialParameterValues <- function(stan.dat)
{
    full.covariance <- is.null(stan.dat$U)
    multiple.classes <- !is.null(stan.dat$P)

    init <- function () structure(list(), .Names = character(0))
    if (full.covariance)
    {
        n.pars <- if (!is.null(stan.dat$K))
            stan.dat$K - 1
        else
            stan.dat$V

        if (multiple.classes)
        {
            n.classes <- stan.dat$P
            init <- function () {
                L_omega <- array(NA, dim = c(n.classes, n.pars, n.pars))
                for (i in 1:n.classes)
                    L_omega[i, , ] <- diag(n.pars)
                list(L_omega = L_omega)
            }
        }
        else
            init <- function () list(L_omega = diag(n.pars))
    }
    init
}

createStanData <- function(dat, n.classes, normal.covariance)
{
    if (dat$n.questions.left.out == 0)
    {
        Y.out <- as.array(1)
        dim.X.out <- dim(dat$X.in)
        dim.X.out[1] <- 1
        X.out <- array(data = 0, dim = dim.X.out)
        RS.out <- 1
    }
    else
    {
        Y.out <- dat$Y.out
        X.out <- dat$X.out
        RS.out <- dat$n.respondents * dat$n.questions.left.out
    }

    stan.dat <- list(C = dat$n.alternatives,
                     R = dat$n.respondents,
                     S = dat$n.questions.left.in,
                     S_out = dat$n.questions.left.out,
                     RS = nrow(dat$X.in),
                     RS_out = RS.out,
                     A = dat$n.attributes,
                     V = dat$n.parameters,
                     ## as.array necessary for one attribute, no alt. specific const case.
                     V_attribute = as.array(dat$n.attribute.parameters),
                     Y = dat$Y.in,
                     Y_out = Y.out,
                     X = dat$X.in,
                     X_out = X.out,
                     V_covariates = dat$n.covariates,
                     covariates = dat$covariates,
                     prior_mean = dat$prior.mean,
                     prior_sd = dat$prior.sd,
                     gamma_shape = dat$hb.sigma.prior.shape,
                     gamma_scale = dat$hb.sigma.prior.scale,
                     lkj_shape = dat$hb.lkj.prior.shape
    )

    if (n.classes > 1)
        stan.dat$P <- n.classes

    if (normal.covariance == "Diagonal")
        stan.dat$U <- dat$n.parameters
    else if (normal.covariance == "Spherical")
        stan.dat$U <- 1

    if (length(dat$covariates))
    {
        stan.dat$V_fc <- dat$V_fc
        stan.dat$Xmat <- dat$Xmat
        stan.dat$V_rc <- dat$V_rc
        stan.dat$Zmat <- dat$Zmat
        stan.dat$rc_dims <- dat$rc_dims
        stan.dat$total_rc <- dat$total_rc
    }

    stan.dat
}

#' @title ReduceStanFitSize
#' @description This function reduces the size of the stan.fit object to reduce the time
#' it takes to return it from the R server.
#' @param stan.fit A stanfit object.
#' @return A stanfit object with a reduced size.
#' @export
ReduceStanFitSize <- function(stan.fit)
{
    # Replace stanmodel with a dummy as stanmodel makes the output many times larger,
    # and is not required for diagnostic plots.
    dummy.stanmodel <- ""
    class(dummy.stanmodel) <- "stanmodel"
    stan.fit <- removeBeta(stan.fit)
    stan.fit@stanmodel <- dummy.stanmodel

    for (i in 1:stan.fit@sim$chains)
    {
        attr(stan.fit@sim$samples[[i]], "inits") <- NULL
        attr(stan.fit@sim$samples[[i]], "mean_pars") <- NULL
    }
    stan.fit@inits <- list()
    stan.fit@.MISC <- new.env()
    stan.fit
}

#' @title ComputeRespPars
#' @description Compute respondent parameters from a stanfit object.
#' @param stan.fit A stanfit object.
#' @param par.names Parameter names
#' @param subset Subset vector
#' @param parameter.scales Scale factors for numeric parameters.
#' @param all.names All parameter names, including those set to zero (excluded
#' from beta) due to dummy coding.
#' @return A matrix of respondent parameters
#' @importFrom rstan get_posterior_mean
#' @export
ComputeRespPars <- function(stan.fit, par.names, subset,
                            parameter.scales = NULL, all.names = NULL)
{
    n.chains <- stan.fit@sim$chains
    n.respondents <- sum(subset)
    resp.pars <- t(matrix(get_posterior_mean(stan.fit, pars = "beta"),
                          nrow = length(par.names)))
    if (n.chains > 1)
    {
        ind.start <- n.respondents * n.chains + 1
        ind.end <- n.respondents * (n.chains + 1)
        resp.pars <- resp.pars[ind.start:ind.end, ]
    }

    if (!is.null(parameter.scales))
        resp.pars <- t(t(resp.pars) / parameter.scales)

    if (!is.null(all.names))
    {
        n.all.names <- length(all.names)
        result <- matrix(NA, nrow = length(subset), ncol = n.all.names)
        for (i in 1:n.all.names)
        {
            if (all.names[i] %in% par.names)
                result[subset, i] <- resp.pars[, all.names[i] == par.names]
            else
                result[subset, i] <- 0
        }
        colnames(result) <- all.names
    }
    else
    {
        result <- matrix(NA, nrow = length(subset), ncol = length(par.names))
        result[subset, ] <- resp.pars
        colnames(result) <- par.names
    }
    result
}

stanModel <- function(n.classes, normal.covariance, has.covariates, has.grouped.cov)
{
    covariates.error.msg <- paste0("Covariates are not currently implemented ",
                                   "for the specified settings.")
    if (n.classes == 1)
    {
        if (normal.covariance == "Full")
        {
            if (has.covariates && !has.grouped.cov)
                stanmodels$choicemodelFC
            else if (has.covariates)
                stanmodels$choicemodelRC
            else
                stanmodels$choicemodel
        }
        else if (has.covariates)
            stop(covariates.error.msg, call. = FALSE)
        else
            stanmodels$diagonal
    }
    else
    {
        if (normal.covariance == "Full")
        {
            if (has.covariates && !has.grouped.cov)
                stanmodels$mixtureofnormalsC
            else if (has.covariates)
                stop(covariates.error.msg, call. = FALSE)
            else
                stanmodels$mixtureofnormals
        }
        else if (has.covariates)
            stop(covariates.error.msg, call. = FALSE)
        else
            stanmodels$diagonalmixture
    }
}

#' @title ExtractBetaDraws
#' @description This function extracts beta draws from a stanfit object.
#' @param stan.fit A stanfit object.
#' @param beta.draws.to.keep Maximum draws per respondent per parameter.
#' @return A 3D array of beta draws.
#' @importFrom rstan extract
#' @export
ExtractBetaDraws <- function(stan.fit, beta.draws.to.keep = 100)
{
    raw.betas <- extract(stan.fit, pars=c("beta"))$beta
    n.draws <- dim(raw.betas)[1]
    if (n.draws > beta.draws.to.keep)
    {
        ind <- round(seq.int(1L, n.draws, length.out = beta.draws.to.keep))
        raw.betas[ind, , ]
    }
    else
        raw.betas
}

#' @title IsRServer
#' @description This function indicates if it is being run on an R server.
#' @return TRUE if running on an R server. False otherwise.
#' @export
IsRServer <- function()
{
    node.name <- Sys.info()[["nodename"]]
    node.name == "reusdev" ||
        grepl("^reustest.*", node.name) ||
        grepl("^reusprod.*", node.name)
}

#' @title IsTestRServer
#' @description This function indicates if it is being run on the test R
#' server.
#' @return TRUE if running on the test R server. False otherwise.
#' @export
IsTestRServer <- function()
{
    node.name <- Sys.info()[["nodename"]]
    grepl("^reustest.*", node.name)
}

removeBeta <- function(stan.fit)
{
    nms <- stan.fit@sim$fnames_oi
    beta.nms <- nms[grepl("^beta", nms)]
    non.beta.nms <- nms[!grepl("^beta", nms)]
    stan.fit@sim$fnames_oi <- non.beta.nms
    stan.fit@sim$n_flatnames <- length(non.beta.nms)
    stan.fit@sim$pars_oi <- stan.fit@sim$pars_oi[stan.fit@sim$pars_oi != "beta"]
    stan.fit@sim$dims_oi$beta <- NULL
    for (i in 1:stan.fit@sim$chains)
        stan.fit@sim$samples[[i]][beta.nms] <- NULL
    stan.fit
}

#' @title GetStanWarningHandler
#' @description This function returns a function that handles Stan warnings.
#' @param show.stan.warnings Whether to return a function that shows
#' user-friendly Stan warnings.
#' @return A function that takes a warning object.
#' @export
GetStanWarningHandler <- function(show.stan.warnings)
{
    if (show.stan.warnings)
        onStanWarning
    else
        function(x) {}
}

#' @title GetStanErrorHandler
#' @description This function returns a function that handles Stan errors.
#' @return A function that takes an error object.
#' @export
GetStanErrorHandler <- function()
{
    function(error)
    {
        msg <- error$message
        if (grepl("missing value where", msg) ||
            grepl("unable to fork", msg))
        {
            stop("The R server has reached maximum capacity. ",
                 "Please rerun the calculation later or contact ",
                 "support@q-researchsoftware.com for assistance.")
        }
        else
            stop(msg)
    }
}

onStanWarning <- function(warn)
{
    msg <- warn$message
    support.msg <-
        if (grepl("divergent transitions after warmup", msg) ||
            grepl("Bayesian Fraction of Missing Information was low", msg))
            warning("Results may be inaccurate due to insufficient iterations. ",
                    "Rerun the analysis with more iterations. Please contact ",
                    "support@q-researchsoftware.com if increasing the number of ",
                    "iterations does not resolve this warning.", call. = FALSE)
    else if (grepl("Examine the pairs\\(\\) plot", msg))
        warning("Examine the Diagnostic plots to diagnose sampling problems",
                call. = FALSE)
    else if (grepl("exceeded the maximum treedepth", msg))
        warning("Results may be inaccurate as the maximum tree depth",
                " is too low. Rerun the analysis with a higher",
                " maximum tree depth. Please contact ",
                "support@q-researchsoftware.com if increasing the maximum ",
                "tree depth does not resolve this warning.")
    else
        warning(warn)
    message(msg)
    invisible()
}

#' @title GetParameterStatistics
#' @description This function returns a 2D array of the parameter statistics.
#' @param stan.fit A stanfit object.
#' @param parameter.names Names of the mean parameters.
#' @param n.classes The number of classes.
#' @param sigma.parameter.names Names of the variance parameters.
#' @return A matrix containing parameter summary statistics.
#' @importFrom rstan extract monitor
#' @export
GetParameterStatistics <- function(stan.fit, parameter.names, n.classes,
                                   sigma.parameter.names = parameter.names)
{
    if ("theta" %in% stan.fit@model_pars)
        pars <- c('theta', 'sigma')
    else  # model has grouped covariates; choicemodelRC.stan used
        pars <- c("resp_fixed_coef", "sigma", "sig_rc")

    ex <- extract(stan.fit, pars = pars, permuted = FALSE,
                  inc_warmup = FALSE)
    result <- suppressWarnings(monitor(ex, probs = c(), print = FALSE))
    lbls <- c(rep(paste0(parameter.names, ' (Mean)'), each = n.classes),
              rep(paste0(sigma.parameter.names, ' (St. Dev.)'),
                  each = n.classes))
    if (n.classes > 1)
        lbls <- paste0(lbls, rep(paste0(', Class ', 1:n.classes),
                                 2 * length(parameter.names)))
    row.names(result) <- lbls
    result
}

#' @title MatchChainClasses
#' @description This function attempts to match classes generated by different
#' chains in a stanfit object.
#' @param stan.fit A stanfit object.
#' @param n.chains The number of chains in the analysis.
#' @param n.classes The number of classes in the analysis.
#' @param n.variables The number of variables in the analysis.
#' @return A stanfit object with classes reordered if matching is successful.
#' Otherwise a warning is thrown and the original stanfit object is returned.
#' @export
MatchChainClasses <- function(stan.fit, n.chains, n.classes, n.variables)
{
    if (n.classes == 1 || n.chains == 1)
        result <- list(stan.fit = stan.fit, match.fail = FALSE)
    else
    {
        samples <- stan.fit@sim$samples
        means <- computeThetaMeans(samples, n.classes, n.variables)
        match.fail <- FALSE
        for (i in 2:n.chains)
        {
            mapping <- rep(NA, n.classes)
            for (j in 1:n.classes)
            {
                norms <- rep(NA, n.classes)
                for (k in 1:n.classes)
                    norms[k] <- parameterDistance(means[, j, 1], means[, k, i])
                ind <- which.min(norms)
                if (norms[ind] > 0.5)
                {
                    match.fail <- TRUE
                    break
                }
                mapping[j] <- ind
            }
            if (!match.fail && length(unique(mapping)) < length(mapping))
                match.fail <- TRUE
            if (match.fail)
                break

            samples[[i]] <- permuteClasses(samples[[i]], mapping, n.variables)
        }

        if (!match.fail)
            stan.fit@sim$samples <- samples
        else
            warning("Classes could not be matched between chains. ",
                    "Parameter statistics will not be available. ",
                    "It is recommended that you either run the model with one chain, ",
                    "or increase the number of iterations.")

        result <- list(stan.fit = stan.fit, match.fail = match.fail)
    }
    result
}

permuteClasses <- function(chain.samples, mapping, n.variables)
{
    n.classes <- length(mapping)
    result <- chain.samples
    for (j in 1:n.classes)
    {
        for (k in 1:n.variables)
        {
            theta.previous <- paste0("theta[", mapping[j], ",", k, "]")
            theta.new <- paste0("theta[", j, ",", k, "]")
            result[[theta.new]] <- chain.samples[[theta.previous]]

            sigma.previous <- paste0("sigma[", mapping[j], ",", k, "]")
            sigma.new <- paste0("sigma[", j, ",", k, "]")
            result[[sigma.new]] <- chain.samples[[sigma.previous]]
        }
    }
    result
}

# 2 * |p1-p2|^2/(|p1|^2 + |p2|^2) where |*| is the Euclidean norm.
parameterDistance <- function(p1, p2)
{
    2 * sum((p1 - p2) * (p1 - p2)) / (sum(p1 * p1) + sum(p2 * p2))
}

computeThetaMeans <- function(samples, n.classes, n.variables)
{
    n.chains <- length(samples)
    means <- array(NA, dim = c(n.variables, n.classes, n.chains))
    for (i in 1:n.chains)
    {
        for (j in 1:n.classes)
        {
            for (k in 1:n.variables)
            {
                parameter.name <- paste0("theta[", j, ",", k, "]")
                parameter.samples <- samples[[i]][[parameter.name]]
                means[k, j, i] <- mean(parameter.samples)
            }
        }
    }
    means
}

pkgCxxFlags <- function()
{
    if (IsRServer())
        cat("CXXFLAGS=-Ofast -mtune=native -march=native -Wno-unused-variable -Wno-unused-function")
    else
        cat("")
}

#' @title LogLikelihoodAndBIC
#' @description This function extracts the log-likelihood from a stanfit object
#' and computes the BIC.
#' @param stan.fit A stanfit object.
#' @param n.parameters The number of HB model parameters.
#' @param sample.size The sample size of the analysis.
#' @param n.questions.left.out The number of questions left out.
#' @param subset The subset used on the data.
#' @return A list containing the log likelihood and BIC.
#' @importFrom rstan get_posterior_mean
#' @export
LogLikelihoodAndBIC <- function(stan.fit, n.parameters, sample.size,
                                n.questions.left.out, subset)
{
    # If there are multiple chains, get_posterior_mean returns a vector of
    # length n.chains + 1, where the last value in the vector is the average
    # over all chains. We define ind to index this value regardless of the
    # number of chains.
    ind <- if (stan.fit@sim$chains == 1)
        1
    else
        stan.fit@sim$chains + 1

    log.likelihood <- get_posterior_mean(stan.fit,
                                         pars = "log_likelihood")[ind]
    rlh <- rep(NA, length(subset))
    rlh[subset] <- get_posterior_mean(stan.fit, pars = "rlh")[, ind]

    result <- list(log.likelihood = log.likelihood,
                   rlh = rlh,
                   bic = log(sample.size) * n.parameters - 2 * log.likelihood)
    if (n.questions.left.out > 0)
    {
        result$rlh.out <- rep(NA, length(subset))
        result$rlh.out[subset] <- get_posterior_mean(stan.fit,
                                                     pars = "rlh_out")[, ind]
        result$log.likelihood.out <- get_posterior_mean(stan.fit,
                                            pars = "log_likelihood_out")[ind]
    }
    result
}

# The number of HB parameters is:
#
# (<number of mean parameters> + <number of covariance parameters>) *
#     <number of classes> + <number of class parameters>
#
# The number of covariance parameters depends on how the covariance is
# constrained:
#
# Full covariance: n(diagonal + triangle) = n.coef * (n.coef + 1) / 2
# Diagonal covariance: n(diagonal) = n.coef
# Spherical covariance: n(same values on diagonal) = 1
#
# where n.coef is the number of mean parameters.
# The number of class parameters is n.classes - 1
numberOfHBParameters <- function(stan.dat)
{
    n.coef <- stan.dat$V
    if (is.null(stan.dat$P)) # 1 class
    {
        if (is.null(stan.dat$U)) # full covariance
            n.coef + n.coef * (n.coef + 1) / 2
        else
            n.coef + stan.dat$U
    }
    else # multi-class
    {
        n.classes <- stan.dat$P
        if (is.null(stan.dat$U)) # full covariance
            n.classes * (n.coef + n.coef * (n.coef + 1) / 2) + n.classes - 1
        else
            n.classes * (n.coef + stan.dat$U) + n.classes - 1
    }
}

#' Creates a list of all model parameter names for use
#' in print methods and diagnostic functions
#' @return A list with components
#' \itemize{
#' \item respondent.pars - names for the (constrained) respondent parameters/coefficients
#' \item unconstrained.respondent.pars - names for the unconstrained respondent
#' parameters/coefficients
#' \item stan.pars - names for the parameters used in the stan code; useful for extracting
#' samples, using diagnostics, etc. when working with the stan.fit object
#' \code mean.pars - names for the (population) mean parameters (theta in the stan code) for
#' the respondent parameters
#' \item covariates - names for the covariates in the model (i.e. the
#'       terms in \code{cov.formula})
#' \item sd.pars - names for the standard deviation (sigma) parameters in the
#' model. Equal to \code{mean.pars} unless grouped covariates are included in the model
#' }
#' @noRd
createNamesList <- function(stan.dat, keep.beta, stan.model)
{
    if (is.null(stan.dat$sd.names))
        stan.dat$sd.names <- stan.dat$par.names
    list(respondent.pars = stan.dat$beta.names,
         unconstrained.respondent.pars = stan.dat$all.beta.names,
         stan.pars = stanParameters(stan.dat, keep.beta, stan.model),
         mean.pars = stan.dat$par.names,
         covariates = colnames(stan.dat$covariates),
         sd.pars = stan.dat$sd.names)
}
