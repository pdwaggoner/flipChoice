#' @title FitChoiceModel
#' @description Fit a choice-based conjoint model using methods such
#'     as Hierarchical Bayes
#' @param design A design object produced by ChoiceModelDesign
#' @param experiment.data A data.frame from an Experiment question
#' @param cho.lines A character vector of lines from a CHO file.
#' @param cho.file The file path to a cho file.
#' @param design.variables A list of variables of columns from a Sawtooth
#'     design file (dual file format) or a JMP design file.
#' @param design.file The file path to a Sawtooth design file (dual
#'     file format) or a JMP design file.
#' @param attribute.levels A list of attribute levels (with attribute names as
#'     names) or a character matrix with the first row containing attribute
#'     names and subsequent rows containing attribute levels.
#' @param attribute.levels.file The file path to an Excel file
#'     containing the level names of each attribute.
#' @param cov.formula An optional \code{\link{formula}} for any fixed
#'     (respondent-specific) covariates to be included in the model.
#'     When only 1 class is specified, covariates are applied to the
#'     mean parameter theta (fixed covariates). When more than 1 class
#'     is specified, covariates are applied to the class weight
#'     parameters.
#' @param cov.data An optional \code{\link{data.frame}} containing the
#'     variables present in \code{cov.formula}.
#' @param choices A data.frame of choices made by respondents for each
#'     question.
#' @param tasks A data.frame of IDs of tasks presented to the
#'     respondents.
#' @param questions Deprecated. Replaced by tasks.
#' @param simulated.priors A 2-column matrix whose columns correspond
#'     to the mean and standard deviations of the parameters; or a
#'     character matrix with attribute levels and corresponding mean
#'     and sd columns after each attribute level column.
#' @param simulated.priors.from.design Whether simulated priors from
#'     the design object are to be used.
#' @param simulated.sample.size The number of simulated respondents to
#'     generate.
#' @param synthetic.priors Deprecated. see simulated.priors.
#' @param synthetic.priors.from.design Deprecated. See
#'     simulated.priors.from.design.
#' @param synthetic.sample.size Deprecated. See simulated.sample.size.
#' @param n.classes The number of latent classes.
#' @param subset An optional vector specifying a subset of
#'     observations to be used in the fitting process.
#' @param weights An optional vector of sampling or frequency weights.
#' @param missing How missing data is to be treated in the
#'     regression. Options: \code{"Error if missing data"},
#'     \code{"Exclude cases with missing data"}, and
#'     \code{"Use partial data"}.
#' @param seed Random seed.
#' @param tasks.left.out Number of questions to leave out for
#'     cross-validation.
#' @param algorithm Either "HB-Stan" for Hierarchical Bayes or "LCA"
#'     for latent class analysis.
#' @param lc.tolerance The tolerance used for defining convergence in
#'     latent class analysis.
#' @param initial.parameters Specify initial parameters intead of
#'     starting at random in latent class analysis. The initial
#'     parameters need to be supplied as list consisting of a matrix
#'     called class.parameters whose columns are the parameters of the
#'     classes, and a vector called class.sizes containing the class
#'     size parameters.
#' @param normal.covariance The form of the covariance matrix for
#'     Hierarchical Bayes. Can be 'Full, 'Spherical', 'Diagonal'.
#' @param hb.iterations The number of iterations in Hierarchical
#'     Bayes.
#' @param hb.chains The number of chains in Hierarchical Bayes.
#' @param hb.max.tree.depth
#'     http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
#' @param hb.adapt.delta
#'     http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#' @param hb.keep.samples Whether to keep the samples of all the
#'     parameters in the output.
#' @param hb.stanfit Whether to include the stanfit property.
#' @param hb.prior.mean The mean for the priors of the mean parameters
#'     theta_raw. This is either passed as a scalar which applies to
#'     all parameters or a numeric vector with each element
#'     corresponding to a variable or attribute. If hb.prior.mean is
#'     nonzero for a categorical attribute, the attribute is treated
#'     as ordered categorical and hb.prior.mean controls the offsets
#'     from the base attribute.
#' @param hb.prior.sd The standard deviations for the priors of the
#'     mean parameters theta_raw. This is passed as a numeric vector
#'     with each element corresponding to an attribute, or a
#'     scalar. If hb.prior.mean is nonzero for a categorical
#'     attribute, the attribute is treated as ordered categorical and
#'     hb.prior.sd controls the standard deviations of the offsets
#'     from the base attribute.
#' @param hb.sigma.prior.shape Postive real number; the shape
#'     hyperparameter for the gamma priors used for the scale
#'     parameters of the respondent coefficients covariance matrix.
#' @param hb.sigma.prior.scale Postive real number; the rate
#'     hyperparameter for the gamma priors used for the scale
#'     parameters of the respondent coefficients covariance matrix.
#' @param hb.lkj.prior.shape Real number greater than one; the shape
#'     hyperparameter for the LKJ prior used for the correlation matrix
#'     of the respondent coefficients distribution. A value of one gives
#' equal probability weight to all possible correlation matrices. Larger values
#' favour less correlation (draws closer to the identity matrix).
#' @param hb.warnings Whether to show warnings from Stan.
#' @param hb.beta.draws.to.keep Maximum number of beta draws per
#'     respondent to return in beta.draws.
#' @param include.choice.parameters Whether to include
#'     alternative-specific parameters.
#' @param respondent.ids If a cho file is supplied, this is the vector
#'     of the respondent IDs to use.
#' @param ... Additional parameters to pass on to \code{rstan::stan}
#'     and \code{rstan::sampling}.
#' @return A list with the following components:
#' \itemize{
#'     \item \code{respondent.parameters} A matrix containing the parameters
#'     of each respondent.
#'     \item \code{reduced.respondent.parameters} A matrix containing the
#'     parameters of each respondent, excluding the constrained parameters.
#'     \item \code{simulated.respondent.parameters} If simulated priors are
#'     used, this is a matrix containing the simulated parameters of
#'     each respondent, excluding the constrained parameters.
#'     \item \code{parameter.statistics} A matrix
#'     containing parameter statistics such as effective sample size
#'     and Rhat (HB-Stan) or standard errors and p-values (LCA).
#'     \item \code{stan.fit} The stanfit object from the
#'     analysis (only for the HB-Stan algorithm).
#'     \item \code{beta.draws} A 3D array containing
#'     sampling draws of beta for each respondent (only for the HB-Stan
#'      algorithm).
#' \item \code{param.names.list} A list containing names for various model parameters
#' as follows:
#' \itemize{
#' \item respondent.pars - names for the (constrained) respondent parameters/coefficients
#' \item unconstrained.respondent.pars - names for the unconstrained respondent
#' parameters/coefficients
#' \item stan.pars - names for the parameters used in the stan code; useful for extracting
#' samples, using diagnostics, etc. when working with the stan.fit object
#' \code{mean.pars} - names for the (population) mean parameters (theta in the stan code) for
#' the respondent parameters
#' \item covariates - names for the covariates in the model (i.e. the
#'       terms in \code{cov.formula})
#' \item sd.pars - names for the standard deviation (sigma) parameters in the
#' model. Equal to \code{mean.pars} unless grouped covariates are included in the model
#' }
#'
##     \item \code{parameter.names} Character vector of parameter names
##     for the mean parameters in the model.
#'     \item \code{in.sample.accuracy} The in-sample prediction accuracy.
#'     \item \code{out.sample.accuracy} The out-of-sample prediction
#'     accuracy.
#'     \item \code{prediction.accuracies} A vector of
#'     prediction accuracies for each respondent.
#'     \item \code{algorithm} The type of algorithm used.
#'     \item \code{n.questions.left.out} The number of questions left out
#'     for out-of-sample testing.
#'     \item \code{n.classes} The number of classes.
#'     \item \code{n.respondents} The number of respondents.
#'     \item \code{n.questions} The number of questions
#'     per respondent.
#'     \item \code{n.alternatives} The number of alternatives per question.
#'     \item \code{n.attributes} The number of attributes.
#'     \item \code{n.parameters} The number of parameters in the analysis.
#'     \item \code{time.taken} The time taken to run the analysis.
#'     \item \code{log.likelihood} The log likelihood.
#'     \item \code{bic} The Bayesian Information Criterion.
#'     \item \code{coef} A vector/matrix of parameter estimates (LCA only).
#' }
#' @examples
#' \dontrun{
#' data(eggs, package = "flipChoice")
#' fit <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 100,
#'                       hb.chains = 1, tasks.left.out = 2)
#' print(fit)
#' ExtractParameterStats(fit)
#' PlotPosteriorIntervals(fit)
#' TracePlots(fit)
#'
#' choices <- eggs.data[, 1:8]
#' questions <- data.frame(t(matrix(1:3040, nrow = 8)))
#' hb.prior.mean <- c(0, 0, 1, 1, 2, 1, 1, 2, 4, 0, 0, 0, -1, -3, -5, -9)
#' fit.with.prior <- FitChoiceModel(
#'     design.file = "http://wiki.q-researchsoftware.com/images/3/35/Eggs_design_with_levels.xlsx",
#'     choices = choices, questions = questions,
#'     hb.iterations = 100, hb.chains = 1,
#'     tasks.left.out = 2, hb.prior.mean = hb.prior.mean)
#' print(fit.with.prior)
#' }
#' @importFrom flipFormat Labels
#' @importFrom stats model.matrix.default
#' @importFrom lme4 nobars
#' @export
#'
FitChoiceModel <- function(design = NULL, experiment.data = NULL,
                           cho.lines = NULL, cho.file = NULL,
                           design.variables = NULL,
                           design.file = NULL,
                           attribute.levels = NULL,
                           attribute.levels.file = NULL,
                           cov.formula = NULL, cov.data = NULL,
                           choices = NULL, questions = NULL,
                           tasks = NULL,
                           simulated.priors = NULL,
                           simulated.priors.from.design = FALSE,
                           simulated.sample.size = 300,
                           synthetic.priors = NULL,
                           synthetic.priors.from.design = NULL,
                           synthetic.sample.size = NULL,
                           n.classes = 1,
                           subset = NULL, weights = NULL,
                           missing = "Use partial data", seed = 123,
                           tasks.left.out = 0, algorithm = "HB-Stan",
                           lc.tolerance = 0.0001,
                           initial.parameters = NULL,
                           normal.covariance = "Full",
                           hb.iterations = 100, hb.chains = 8,
                           hb.max.tree.depth = 10, hb.adapt.delta = 0.8,
                           hb.keep.samples = FALSE, hb.stanfit = TRUE,
                           hb.prior.mean = 0, hb.prior.sd = 5,
                           hb.sigma.prior.shape = 1.39435729464721,
                           hb.sigma.prior.scale = 0.39435729464721,
                           hb.lkj.prior.shape = 4,
                           hb.warnings = TRUE, hb.beta.draws.to.keep = 0,
                           include.choice.parameters = TRUE,
                           respondent.ids = NULL, ...)
{
    if (algorithm == "HB-Stan" && !is.null(weights))
        stop("Weights are not able to be applied for Hierarchical Bayes.")

    if (!is.null(questions) && is.null(tasks))
        tasks <- questions

    if (any(hb.prior.sd <= 0))
        stop("All prior standard deviations must be greater than 0.")

    if (!is.null(synthetic.priors))
        simulated.priors <- synthetic.priors
    if (!is.null(synthetic.priors.from.design))
        simulated.priors.from.design <- synthetic.priors.from.design
    if (!is.null(synthetic.sample.size))
        simulated.sample.size <- synthetic.sample.size

    if (simulated.priors.from.design)
    {
        if (!is.null(design))
        {
            if (!is.null(design$prior))
                simulated.priors <- design$prior
            else
                simulated.priors <- 0 # mean = 0 and sd = 0
        }
        else
            warning("Simulated data was not used as no design was supplied.")
    }

    if (simulated.sample.size <= 0)
        stop("The specified simulated sample size must greater than 0.")

    start.time <- proc.time()

    covariates <- if (!is.null(cov.formula))
        model.matrix(lme4::nobars(cov.formula), cov.data)  # model.matrix.default(cov.formula, cov.data)
    else
        NULL

    dat <- if (!is.null(design) && (!is.null(simulated.priors) ||
                                    (!is.null(choices) && !is.null(tasks))))
        processDesignObject(design, choices, tasks, subset, weights,
                            tasks.left.out, seed, hb.prior.mean, hb.prior.sd,
                            include.choice.parameters, missing, covariates,
                            simulated.priors, simulated.sample.size)
    else if (!is.null(experiment.data))
        processExperimentData(experiment.data, subset, weights, tasks.left.out,
                              seed, hb.prior.mean, hb.prior.sd, missing,
                              covariates, simulated.priors,
                              simulated.sample.size)
    else if (!is.null(cho.lines) && !is.null(attribute.levels))
        processChoVariables(cho.lines, attribute.levels, subset, weights,
                            tasks.left.out, seed, hb.prior.mean, hb.prior.sd,
                            include.choice.parameters, respondent.ids, missing,
                            covariates, simulated.priors,
                            simulated.sample.size)
    else if (!is.null(cho.file) && !is.null(attribute.levels.file))
        processChoFile(cho.file, attribute.levels.file, subset, weights,
                       tasks.left.out, seed, hb.prior.mean, hb.prior.sd,
                       include.choice.parameters, respondent.ids, missing,
                       covariates, simulated.priors, simulated.sample.size)
    else if (!is.null(design.variables) && (!is.null(simulated.priors) ||
                                            (!is.null(choices) && !is.null(tasks))))
        processDesignVariables(design.variables, attribute.levels, choices,
                               tasks, subset, weights, tasks.left.out, seed,
                               hb.prior.mean, hb.prior.sd,
                               include.choice.parameters, missing, covariates,
                               simulated.priors, simulated.sample.size)
    else if (!is.null(design.file) && (!is.null(simulated.priors) ||
                                       (!is.null(choices) && !is.null(tasks))))
        processDesignFile(design.file, attribute.levels.file, choices, tasks,
                          subset, weights, tasks.left.out, seed, hb.prior.mean,
                          hb.prior.sd, include.choice.parameters, missing,
                          covariates, simulated.priors, simulated.sample.size)
    else
        stop("Insufficient data was supplied.")

    if (!is.null(dat$covariates))
        dat <- processCovariateData(dat, n.classes, cov.formula, cov.data)

    dat$hb.sigma.prior.shape <- hb.sigma.prior.shape
    dat$hb.sigma.prior.scale <- hb.sigma.prior.scale
    dat$hb.lkj.prior.shape <- hb.lkj.prior.shape


    if (algorithm == "HB-Stan")
    {
        result <- hierarchicalBayesChoiceModel(dat, hb.iterations, hb.chains,
                                               hb.max.tree.depth,
                                               hb.adapt.delta,
                                               seed, hb.keep.samples,
                                               n.classes,
                                               hb.stanfit, normal.covariance,
                                               hb.warnings,
                                               hb.beta.draws.to.keep, ...)
    }
    else
    {
        result <- latentClassChoiceModel(dat, n.classes, seed,
                                         initial.parameters, lc.tolerance)
    }

    end.time <- proc.time()

    simulated.resp.pars <- dat$simulated.respondent.parameters
    if (!is.null(simulated.resp.pars))
        colnames(simulated.resp.pars) <- colnames(result$reduced.respondent.parameters)

    result <- accuracyResults(dat, result, tasks.left.out)
    result$processed.data <- dat
    result$algorithm <- algorithm
    result$n.questions.left.out <- tasks.left.out
    result$n.classes <- n.classes
    result$subset <- subset
    result$subset.description <- if (is.null(subset)) NULL else Labels(subset)
    result$weights <- weights
    result$weights.description <- if (is.null(weights)) NULL else Labels(weights)
    result$n.respondents <- dat$n.respondents
    result$n.questions <- dat$n.questions
    result$n.alternatives <- dat$n.alternatives
    result$n.attributes <- dat$n.attributes
    result$n.parameters <- dat$n.parameters
    result$n.total <- length(dat$subset)
    result$simulated.respondent.parameters <- simulated.resp.pars
    result$synthetic.respondent.parameters <- simulated.resp.pars # deprecated
    result$time.taken <- (end.time - start.time)[3]
    class(result) <- "FitChoice"
    result
}

accuracyResults <- function(dat, result, n.questions.left.out)
{
    n.respondents <- length(dat$n.questions.left.in)
    resp.pars <- result$reduced.respondent.parameters[dat$subset, ]

    n.rs <- dim(dat$X.in)[1]
    n.alternatives <- dim(dat$X.in)[2]

    in.sample.accuracies <- rep(NA, n.respondents)
    rs <- 1
    for (i in 1:n.respondents)
    {
        pars <- resp.pars[i, ]
        n.questions <- dat$n.questions.left.in[i]
        score <- rep(NA, n.questions)
        for (j in 1:n.questions)
        {
            u <- rep(NA, n.alternatives)
            for (k in 1:n.alternatives)
                u[k] <- sum(pars * dat$X.in[rs, k, ])
            score[j] <- if(which.max(u) == dat$Y.in[rs]) 1 else 0
            rs <- rs + 1
        }
        in.sample.accuracies[i] <- mean(score)
    }

    w <- dat$weights
    result$in.sample.accuracy <- sum(in.sample.accuracies * w) / sum(w)

    if (n.questions.left.out > 0)
    {
        out.sample.accuracies <- rep(NA, n.respondents)
        rs <- 1
        for (i in 1:n.respondents)
        {
            pars <- resp.pars[i, ]
            score <- rep(NA, n.questions.left.out)
            for (j in 1:n.questions.left.out)
            {
                u <- rep(NA, n.alternatives)
                for (k in 1:n.alternatives)
                    u[k] <- sum(pars * dat$X.out[rs, k, ])
                score[j] <- if(which.max(u) == dat$Y.out[rs]) 1 else 0
                rs <- rs + 1
            }
            out.sample.accuracies[i] <- mean(score)
        }
        result$prediction.accuracies <- rep(NA, length(dat$subset))
        result$prediction.accuracies[dat$subset] <- out.sample.accuracies
        result$out.sample.accuracy <- sum(out.sample.accuracies * w) / sum(w)
    }
    else
    {
        result$prediction.accuracies <- rep(NA, length(dat$subset))
        result$prediction.accuracies[dat$subset] <- in.sample.accuracies
        result$out.sample.accuracy <- NA
    }
    result
}

predict.FitChoice <- function(object, data,  n.reps = 10000, ...)
{
    if (missing(data))
        data <- object$processed.data
    n.respondents <- length(data$n.questions.left.in)
    #    resp.pars <- result$reduced.respondent.parameters[dat$subset, ]

    n.rs <- dim(data$X.in)[1]  # n.q*n.resp
    n.alternatives <- dim(data$X.in)[2]
    is.q.const <- length(unique(data$n.questions.left.in)) == 1L
    if (!is.q.const)
        stop("Number of questions per respondent needs to be constant")

    n.questions <- data$n.questions.left.in[1L]
    resp.pars <- extract(object$stan.fit, pars = "beta")[[1L]]

    ## in.sample.accuracies <- rep(NA, n.respondents)
    y.pred <- matrix(nrow = n.respondents, ncol = n.questions)
    y.rep <- matrix(nrow = n.reps, ncol = n.rs)
    if ("theta" %in% stan.fit@model_pars)
        pars <- "theta"
    else
        pars <- c("resp_fixed_coef", "resp_rand_eff")


    mean.par.samps <- extract(object$stan.fit, pars = pars, permuted = TRUE,
                              inc_warmup = FALSE)[[1L]]
    mean.new.samps <- lapply(mean.par.samps, function(x) apply(x, 2:3, sample,
                                                               replace = TRUE, size = n.reps))
    ## new.dat <- processCovariateData(object$processed.data,
    ##                                 object$n.classes,
    ##                                 object$processed.data$cov.formula,
    ##                                 data$cov.data)
    ## design matrices for the covariates are already formed since we
    ##   fit the model to all respondents and only hold out a subset of
    ##   questions for each respondent

    rs <- 1
    for (i in 1:n.respondents)
    {
        pars <- resp.pars[, i, ]
        pars <- apply(pars, 2, sample, size = n.reps, replace = TRUE)
        ## n.questions <- data$n.questions.left.in[i]
        ## score <- rep(NA, n.questions)
        for (j in 1:n.questions)
        {
            ## u <- rep(NA, n.alternatives)
            lp <- tcrossprod(pars, data$X.in[rs, , ])
            probs <- t(apply(lp, 1, flipChoice:::softmax))
            y.preds <- apply(probs, 1, which.max)
            y.pred[i, j] <- which.max(table(y.preds))
            ## for (k in 1:n.alternatives)
            ##     u[k] <- sum(pars * dat$X.in[rs, k, ])

            ## score[j] <- if(which.max(u) == dat$Y.in[rs]) 1 else 0
            y.rep[, rs] <- y.preds
            rs <- rs + 1
        }
        ## in.sample.accuracies[i] <- mean(score)
    }

    ## w <- dat$weights
    ## result$in.sample.accuracy <- sum(in.sample.accuracies * w) / sum(w)

    ## if (n.questions.left.out > 0)
    ## {
    ##     out.sample.accuracies <- rep(NA, n.respondents)
    ##     rs <- 1
    ##     for (i in 1:n.respondents)
    ##     {
    ##         pars <- resp.pars[i, ]
    ##         score <- rep(NA, n.questions.left.out)
    ##         for (j in 1:n.questions.left.out)
    ##         {
    ##             u <- rep(NA, n.alternatives)
    ##             for (k in 1:n.alternatives)
    ##                 u[k] <- sum(pars * dat$X.out[rs, k, ])
    ##             score[j] <- if(which.max(u) == dat$Y.out[rs]) 1 else 0
    ##             rs <- rs + 1
    ##         }
    ##         out.sample.accuracies[i] <- mean(score)
    ##     }
    ##     result$prediction.accuracies <- rep(NA, length(dat$subset))
    ##     result$prediction.accuracies[dat$subset] <- out.sample.accuracies
    ##     result$out.sample.accuracy <- sum(out.sample.accuracies * w) / sum(w)
    ## }
    ## else
    ## {
    ##     result$prediction.accuracies <- rep(NA, length(dat$subset))
    ##     result$prediction.accuracies[dat$subset] <- in.sample.accuracies
    ##     result$out.sample.accuracy <- NA
    ## }
    ## result
    list(y.rep = y.rep, y.pred = y.pred)
}

computeAccuracy <- function(object, data, ...)
{
    y.pred.in <- predict(object, data, ...)
    n.resp <- length(data$n.questions.left.in)
    y.in <- matrix(data$Y.in, nrow = n.resp, byrow = TRUE)
    in.correct <- y.in == y.pred.in$y.pred

    y.pred.out <- predict(object,
                          data = list(Y.in = data$Y.out, X.in = data$X.out,
                                      n.questions.left.in = rep(data$n.questions.left.out, n.resp)),
                          ...)

    y.out <- matrix(data$Y.out, nrow = n.resp, byrow = TRUE)
    out.correct <- y.out == y.pred.out$y.pred
    list(y.pred.in, y.pred.out, in.acc = mean(in.correct), out.acc = mean(out.correct))
}


#' @title RespondentParameters
#' @description The parameters for each respondent.
#' @param object A \code{FitChoice} or \code{FitMaxDiff} object.
#' @export
RespondentParameters <- function(object)
{
    as.data.frame(object$respondent.parameters)
}

#' @title RespondentParametersTable
#' @description Produces a formattable table with histograms of respondent parameters.
#' @param resp.pars A matrix of respondent parameters
#' @param title Table title.
#' @param subtitle Table subtitle.
#' @param footer Table footer.
#' @importFrom flipFormat FormatAsReal
#' @importFrom stats sd
#' @export
RespondentParametersTable <- function(resp.pars, title, subtitle, footer)
{
    bin.max <- max(ceiling(max(resp.pars, na.rm = TRUE)), -floor(min(resp.pars, na.rm = TRUE)))
    bin.min <- -bin.max

    n.parameters <- ncol(resp.pars)
    stats.table <- matrix(NA, nrow = n.parameters, ncol = 2)
    for (i in 1:n.parameters)
    {
        stats.table[i, 1] <- FormatAsReal(mean(resp.pars[, i], na.rm = TRUE), decimals = 1)
        stats.table[i, 2] <- FormatAsReal(sd(resp.pars[, i], na.rm = TRUE), decimals = 1)
    }
    colnames(stats.table) <- c("Mean", "Standard Deviation")

    bin.size <- (bin.max - bin.min) / 50

    footer <- paste0(footer, "column width: ", FormatAsReal(bin.size, decimals = 2), "; ")

    HistTable(resp.pars, title = title, subtitle = subtitle, footer = footer,
              bin.size = bin.size, bin.min = bin.min, bin.max = bin.max, hist.width = 300,
              hist.height = 20, color.negative = TRUE, show.tooltips = FALSE,
              histogram.column.name = "Respondent Coefficients", stats.table)
}

#' @title print.FitChoice
#' @description Produces a string mentioning the parameters with the lowest
#' effective sample size and highest Rhat.
#' @param parameter.statistics Matrix containing parameter statistics from
#' a summary of a stan.fit object.
#' @param parameter.names Names of the parameters.
#' @param n.classes The number of classes.
#' @return A string containing information about parameter statistics.
#' @export
ParameterStatisticsInfo <- function(parameter.statistics, parameter.names,
                                    n.classes)
{
    n.rows <- nrow(parameter.statistics)
    theta.statistics <- parameter.statistics[1:(n.rows / 2), ]
    theta.n.eff.ind <- which.min(theta.statistics[, 4])
    theta.n.eff <- FormatAsReal(theta.statistics[theta.n.eff.ind, 4],
                                decimals = 1)
    theta.rhat.ind <- which.max(theta.statistics[, 5])
    theta.rhat <- FormatAsReal(theta.statistics[theta.rhat.ind, 5],
                               decimals = 1)

    sigma.statistics <- parameter.statistics[(n.rows / 2 + 1):n.rows, ]
    sigma.n.eff.ind <- which.min(sigma.statistics[, 4])
    sigma.n.eff <- FormatAsReal(sigma.statistics[sigma.n.eff.ind, 4],
                                decimals = 1)
    sigma.rhat.ind <- which.max(sigma.statistics[, 5])
    sigma.rhat <- FormatAsReal(sigma.statistics[sigma.rhat.ind, 5],
                               decimals = 1)

    if (n.classes > 1)
        nms <- rep(paste0(rep(parameter.names, each = n.classes), ", Class ",
                          1:n.classes), 2)
    else
        nms <- rep(parameter.names, 2)

    result <- ""
    if (length(theta.rhat.ind) == 0)
        result <- paste0(result, "lowest effective sample size (Mean): ",
                         theta.n.eff, " at ", nms[theta.n.eff.ind],
                         "; Rhat (Mean) not available; ")
    else if (theta.n.eff.ind == theta.rhat.ind)
        result <- paste0(result, "lowest effective sample size (Mean): ",
                         theta.n.eff, " and highest Rhat (Mean): ",
                         theta.rhat, " at ", nms[theta.n.eff.ind],
                         "; ")
    else
        result <- paste0(result, "lowest effective sample size (Mean): ",
                         theta.n.eff, " at ", nms[theta.n.eff.ind],
                         "; ", "highest Rhat (Mean): ", theta.rhat, " at ",
                         nms[theta.rhat.ind], "; ")

    if (length(sigma.rhat.ind) == 0)
        result <- paste0(result, "lowest effective sample size (St. Dev.): ",
                         sigma.n.eff, " at ", nms[sigma.n.eff.ind],
                         "; Rhat (St. Dev.) not available; ")
    else if (sigma.n.eff.ind == sigma.rhat.ind)
        result <- paste0(result, "lowest effective sample size (St. Dev.): ",
                         sigma.n.eff, " and highest Rhat (St. Dev.): ",
                         sigma.rhat, " at ", nms[sigma.n.eff.ind],
                         "; ")
    else
        result <- paste0(result, "lowest effective sample size (St. Dev.): ",
                         sigma.n.eff, " at ", nms[sigma.n.eff.ind],
                         "; ", "highest Rhat (St. Dev.): ", sigma.rhat, " at ",
                         nms[sigma.rhat.ind], "; ")
    result
}

#' @title print.FitChoice
#' @description Print a FitChoice object
#' @param x FitMaxDiff object.
#' @param ... further arguments passed to or from other methods.
#' @importFrom flipFormat HistTable FormatAsPercent SampleDescription FormatAsReal
#' @importFrom flipTime FormatPeriod
#' @export
#' @method print FitChoice
print.FitChoice <- function(x, ...)
{
    title <- if (x$algorithm == "HB-Stan")
    {
        if (x$n.classes > 1)
            paste0("Choice Model: ", x$n.classes, "-class Hierarchical Bayes")
        else
            "Choice Model: Hierarchical Bayes"
    }
    else
        paste0("Choice Model: ", x$n.classes, "-class Latent Class Analysis")

    footer <- choiceModelFooter(x)
    footer <- paste0(footer, "number of classes: ", x$n.classes, "; ")
    footer <- paste0(footer, "mean RLH: ",
                     FormatAsReal(mean(x$rlh), decimals = 2), "; ")
    if (x$n.questions.left.out > 0)
        footer <- paste0(footer, "mean holdout RLH: ",
                         FormatAsReal(mean(x$rlh.out), decimals = 2), "; ")
    footer <- paste0(footer, "log-likelihood: ",
                     FormatAsReal(x$log.likelihood, decimals = 0), "; ")
    if (x$n.questions.left.out > 0)
        footer <- paste0(footer, "holdout log-likelihood: ",
                         FormatAsReal(x$log.likelihood.out, decimals = 0),
                         "; ")

    footer <- paste0(footer, "BIC: ", FormatAsReal(x$bic, decimals = 0), "; ")

    if (!is.null(x$class.match.fail)) # HB-Stan only
    {
        if (x$class.match.fail)
            footer <- paste0(footer, "parameter statistics not available; ")
        else
        {
            info <- ParameterStatisticsInfo(x$parameter.statistics,
                                            colnames(x$reduced.respondent.parameters),
                                            x$n.classes)
            footer <- paste0(footer, info)
        }
    }
    if (IsTestRServer())
        footer <- paste0(footer, "time taken to run analysis: [hidden for tests]; ")
    else
        footer <- paste0(footer, "time taken to run analysis: ",
                         FormatPeriod(x$time.taken), "; ")

    subtitle <- choiceModelSubtitle(x)

    RespondentParametersTable(x$respondent.parameters, title, subtitle, footer)
}

#' @importFrom flipFormat ExtractChartData
#' @export
flipFormat::ExtractChartData

#' @export
ExtractChartData.FitChoice <- function(x)
{
    return(x$respondent.parameters)
}

#' @importFrom flipFormat FormatAsPercent
choiceModelSubtitle <- function(x)
{
    subtitle <- if (!is.na(x$out.sample.accuracy))
        paste0("Prediction accuracy (leave-", x$n.questions.left.out , "-out cross-validation): ",
               FormatAsPercent(x$out.sample.accuracy, decimals = 1))
    else
        paste0("Prediction accuracy (in-sample): ",
               FormatAsPercent(x$in.sample.accuracy, decimals = 1))
}

#' @importFrom flipFormat SampleDescription
choiceModelFooter <- function(x) {

    n.subset <- if (is.null(x$subset)) x$n.respondents else sum(x$subset)
    footer <- SampleDescription(n.total = x$n.total, n.subset = n.subset,
                                n.estimation = n.subset,
                                subset.label = x$subset.description,
                                weighted = !is.null(x$weights),
                                weight.label = x$weights.description,
                                missing = FALSE)
    footer <- paste0(footer, " ")

    footer <- paste0(footer, "number of questions: ", x$n.questions, "; ")
    if (x$n.questions.left.out > 0)
    {
        footer <- paste0(footer, "questions used in estimation: ", x$n.questions - x$n.questions.left.out, "; ")
        footer <- paste0(footer, "questions left out: ", x$n.questions.left.out, "; ")
    }
    footer <- paste0(footer, "choices per question: ", x$n.alternatives, "; ")
    footer <- paste0(footer, "number of attributes: ", x$n.attributes, "; ")
    footer <- paste0(footer, "number of parameters: ", x$n.parameters, "; ")
    if (!is.na(x$out.sample.accuracy))
        footer <- paste0(footer, " in-sample accuracy: ",
                         FormatAsPercent(x$in.sample.accuracy, decimals = 1),
                         "; ")
    return(footer)
}
