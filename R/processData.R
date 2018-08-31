#' @param data list with components
#' \itemize{
#' \item \code{Y} - a vector of respondent choices
#' \item \code{X} - an array of containing the attribute levels shown to each respondent for each questions
#' \item \code{respondent.ids} - list with the same length \code{Y}
#' \item \code{non.missing} - a vector containing the indices of any missing observations
#' \item \code{type} - character; one of "cho", "experiment", "dualfile"
#' \item \code {n.questions} - number of questions shown to each respondent
#' \item \code {n.alternatives}
#' }
#' @noRd
processData <- function(
                        data,
                        subset,
                        weights,
                        n.questions.left.out, seed,
                        input.prior.mean,
                        input.prior.sd,
                        include.choice.parameters,
                        respondent.ids, missing,
                        covariates,
                        simulated.priors,
                        simulated.sample.size)
{
    with(data, {
        ## check for simulated data and warn
        is.data.simulated <- !is.null(simulated.priors)
        if (is.data.simulated)
        {
            simulatedDataWarnings(subset, weights, covariates)
            subset <- NULL
            weights <- NULL
            covariates <- NULL
        }

        ## error for missing data if needed
        if (missing == "Error if missing data" &&
            ((!is.null(subset) && any(is.na(subset))) ||
             (!is.null(weights) && any(is.na(weights)))) ||
            (!is.null(covariates) && any(is.na(covariates))))
            MissingDataFail();

        ## get parameter names
        n.attribute.parameters <- unlist(lapply(attribute.levels, length)) - 1
        n.parameters <-  sum(n.attribute.parameters)
        par.names <- parameterNamesFromAttributes(attribute.levels)
        all.names <- allNamesFromAttributes(attribute.levels)


        ## check prior params
        checkPriorParameters(input.prior.mean, input.prior.sd, n.alternatives,
                             n.attributes, n.parameters, include.choice.parameters)

        ## check questions left out
        checkNumberOfQuestionsLeftOut(n.questions, n.questions.left.out)
        checkNumberOfQuestionsLeftOut(max(sapply(respondent.indices, length)),
                                      n.questions.left.out)

        ## handle missing, filters, subset, weights
        filter.subset <- CleanSubset(subset, n.respondents)
        subset <- filter.subset & non.missing

        if (sum(filter.subset) == 0)
            stop("All respondents have been filtered out.")
        else if (sum(subset) == 0)
            NoData()

        weights <- prepareWeights(weights, subset)
        rs.subset <- unlist(respondent.indices[subset])
        n.respondents <- sum(rs.subset)

        ## split data into training and test sets
        split.data <- crossValidationSplit(X, Y, n.questions.left.out, seed,
                                           respondent.indices)

         ## process prior means and sd
         prior.mean <- processInputPrior(input.prior.mean, n.parameters,
                                    n.attributes, n.attribute.parameters)
         prior.sd <- processInputPrior(input.prior.sd, n.parameters, n.attributes,
                                    n.attribute.parameters)


            list(n.questions = n.questions.common,
             n.questions.left.out = n.questions.left.out,
             n.alternatives = n.alternatives,
             n.attributes = n.attributes,
             n.respondents = n.respondents,
             n.parameters = n.parameters,
             n.attribute.parameters = n.attribute.parameters,
             par.names = par.names,
             all.names = all.names,
             ## beta.names = par.names,
             ## all.beta.names = all.names,
             X.in = split.data$X.in,
             Y.in = split.data$Y.in,
             X.out = split.data$X.out,
             Y.out = split.data$Y.out,
             n.questions.left.in = split.data$n.questions.left.in,
             subset = subset,
             weights = weights,
             covariates = covariates,
             parameter.scales = rep(1, n.parameters),
             prior.mean = prior.mean,
             prior.sd = prior.sd,
             simulated.respondent.parameters = simulated.respondent.parameters)
        })
}

#' Assumes dummy coding, merely drops first level from each
#' factor and calls \code}allNamesFromAttributes}
#' @noRd
parameterNamesFromAttributes <- function(attribute.levels)
    allNamesFromAttributes(lapply(attribute.levels, `[`, -1L))
## {
##     n.attributes <- length(attribute.levels)
##     n.attribute.parameters <- unlist(lapply(attribute.levels, length)) - 1
##     n.parameters <- sum(n.attribute.parameters)
##     attribute.names <- names(attribute.levels)
##     result <- rep("", n.parameters)
##     ind <- 1
##     for (i in 1:n.attributes)
##     {
##         for (j in 1:n.attribute.parameters[i])
##         {
##             result[ind] <- paste0(attribute.names[i], ": ",
##                                   attribute.levels[[i]][j + 1])
##             ind <- ind + 1
##         }
##     }
##     result
## }

allNamesFromAttributes <- function(attribute.levels)
{
    n.attributes <- length(attribute.levels)
    n.attribute.levels <- vapply(attribute.levels, length, 0L)
    attribute.names <- names(attribute.levels)
    result <- character(sum(n.attribute.levels) + sum(n.attribute.levels == 0L))
    ind <- 1
    for (i in 1:n.attributes)
    {
        if (n.attribute.levels == 0L)
        {
            result[ind] <- attribute.names[i]
            ind <- ind + 1
        }else
            for (j in 1:n.attribute.levels[i])
            {
                result[ind] <- paste0(attribute.names[i], ": ",
                                      attribute.levels[[i]][j])
                ind <- ind + 1
            }
    }
    result
}

checkPriorParameters <- function(input.prior.mean, input.prior.sd,
                                 n.alternatives, n.attributes, n.parameters,
                                 include.choice.parameters = FALSE)
{
    if (include.choice.parameters)
    {
        n.attributes <- n.attributes + 1
        n.parameters <- n.parameters + n.alternatives - 1
    }
    if (!is.numeric(input.prior.mean) ||
        (length(input.prior.mean) != n.parameters &&
         length(input.prior.mean) != n.attributes &&
         length(input.prior.mean) != 1))
        stop("The supplied parameter hb.prior.mean is inappropriate. ",
                    "Based on the input data this needs to be a numeric ",
                    "vector of length 1, ", n.attributes,
                    " (number of attributes) or ",
                    n.parameters, " (number of parameters).")
    if (!is.numeric(input.prior.sd) ||
        (length(input.prior.sd) != n.parameters &&
         length(input.prior.sd) != n.attributes &&
         length(input.prior.sd) != 1))
        stop("The supplied parameter hb.prior.sd is inappropriate. ",
                    "Based on the input data this needs to be a numeric ",
                    "vector of length 1, ", n.attributes,
                    " (number of attributes) or ",
                    n.parameters, " (number of parameters).")
}

crossValidationSplit <- function(X, Y, n.questions.left.out, seed,
                                 respondent.indices)
{
    n.respondents <- length(respondent.indices)
    n.questions.left.in <- rep(NA, n.respondents)
    if (n.questions.left.out > 0)
    {
        n.alternatives <- dim(X)[2]
        n.parameters <- dim(X)[3]
        n.in <- nrow(X) - n.questions.left.out * n.respondents
        X.in <- array(dim = c(n.in, n.alternatives, n.parameters))
        Y.in <- rep(NA, n.in)
        X.out <- array(dim = c(n.respondents * n.questions.left.out,
                               n.alternatives, n.parameters))
        Y.out <- rep(NA, n.respondents * n.questions.left.out)

        set.seed(seed)
        rs <- 0
        rs.in <- 0
        for (r in 1:n.respondents)
        {
            n.questions <- length(respondent.indices[[r]])
            n.questions.left.in[r] <- n.questions - n.questions.left.out
            ind.left.out <- sample(n.questions, n.questions.left.out)
            ind.left.in <- setdiff(1:n.questions, ind.left.out)
            ind.left.out.new <- (r - 1) * n.questions.left.out +
                                1:n.questions.left.out
            ind.left.in.new <- rs.in + 1:n.questions.left.in[r]
            X.in[ind.left.in.new, , ] <- X[rs + ind.left.in, , ]
            Y.in[ind.left.in.new] <- Y[rs + ind.left.in]
            X.out[ind.left.out.new, , ] <- X[rs + ind.left.out, , ]
            Y.out[ind.left.out.new] <- Y[rs + ind.left.out]
            rs <- rs + n.questions
            rs.in <- rs.in + n.questions.left.in[r]
        }
    }
    else
    {
        X.in <- X
        Y.in <- Y
        X.out <- NULL
        Y.out <- NULL
        n.questions.left.in <- sapply(respondent.indices, length)
    }
    list(X.in = X.in, X.out = X.out, Y.in = Y.in, Y.out = Y.out,
         n.questions.left.in = n.questions.left.in)
}

#' @importFrom flipData CalibrateWeight CleanWeights
prepareWeights <- function(weights, subset)
{
    if (!is.null(weights))
    {
        weights <- CleanWeights(weights)
        weights <- weights[subset]
        CalibrateWeight(weights)
    }
    else
        rep(1, sum(subset))
}

processInputPrior <- function(prior.par, n.parameters, n.attributes,
                              n.attribute.parameters, parameter.scales = NULL)
{
    if (is.null(parameter.scales))
        parameter.scales <- rep(1, n.parameters)
    result <- rep(NA, n.parameters)
    if (length(prior.par) == 1)
        result <- rep(prior.par, n.parameters)
    else if (length(prior.par) == n.attributes)
    {
        result <- rep(NA, n.parameters)
        for (i in 1:n.attributes)
        {
            if (n.attribute.parameters[i] == 1) # numeric
            {
                ind <- sum(n.attribute.parameters[1:i])
                result[ind] <- prior.par[i] * parameter.scales[ind]
            }
            else # categorical
            {
                ind.end <- sum(n.attribute.parameters[1:i])
                ind.start <- ind.end - n.attribute.parameters[i] + 1
                for (j in ind.start:ind.end)
                    result[j] <- prior.par[i]
            }
        }
    }
    else # length(prior.par) == n.parameters
        result <- prior.par * parameter.scales

    result
}
