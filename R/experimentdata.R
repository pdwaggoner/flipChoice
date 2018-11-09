#' @importFrom flipData CleanSubset NoData
processExperimentData <- function(experiment.data, subset, weights,
                                  n.questions.left.out, seed, input.prior.mean,
                                  input.prior.sd, missing, covariates,
                                  simulated.priors, simulated.sample.size)
{
    nms <- names(experiment.data)
    choice.name <- nms[1]
    n.questions <- sum(nms == choice.name)
    n.alternatives <- length(levels(experiment.data[[1]]))
    n.qc <- n.questions * n.alternatives
    n.attributes <- (length(nms) - n.questions) / n.qc
    if (round(n.attributes) != n.attributes)
        stop("The number of parameters in the Experiment question is invalid.")
    checkNumberOfQuestionsLeftOut(n.questions, n.questions.left.out)

    is.data.simulated <- !is.null(simulated.priors)
    if (is.data.simulated)
    {
        simulatedDataWarnings(subset, weights, covariates)
        subset <- NULL
        weights <- NULL
        covariates <- NULL
    }

    if (missing == "Error if missing data")
        errorIfMissingDataFoundExperiment(experiment.data, subset, weights,
                                          covariates, n.questions,
                                          is.data.simulated)

    non.missing.table <- nonMissingTableForExperiment(experiment.data, subset,
                                                      weights, covariates,
                                                      n.questions,
                                                      n.alternatives,
                                                      n.attributes, missing,
                                                      is.data.simulated)
    non.missing <- nonMissingRespondents(non.missing.table,
                                         n.questions.left.out, missing,
                                         n.questions)

    filter.subset <- CleanSubset(subset, nrow(experiment.data))
    subset <- filter.subset & non.missing

    if (sum(filter.subset) == 0)
        stop("All respondents have been filtered out.")
    else if (sum(subset) == 0)
        NoData()

    weights <- prepareWeights(weights, subset)
    experiment.data <- experiment.data[subset, ]
    non.missing.table <- non.missing.table[subset, , drop = FALSE]
    if (!is.null(covariates))
        covariates <- covariates[subset, ]
    n.respondents <- nrow(experiment.data)
    Y <- extractChoices(experiment.data, non.missing.table)
    attribute.data <- experiment.data[, -1:-n.questions]
    names(attribute.data) <- nms[-1:-n.questions]
    attribute.data <- completeLevels(attribute.data)
    n.attribute.parameters <- nAttributeParameters(attribute.data,
                                                   n.attributes, n.questions,
                                                   n.alternatives)
    n.parameters <- sum(n.attribute.parameters)
    par.names <- parameterNames(attribute.data, n.attributes, n.questions,
                                n.alternatives, n.parameters)
    all.names <- allNames(attribute.data, n.attributes, n.questions,
                          n.alternatives, n.parameters)
    attribute.levels <- attributeLevels(attribute.data, n.attributes,
                                        n.questions, n.alternatives,
                                        n.parameters)

    checkPriorParameters(input.prior.mean, input.prior.sd, n.alternatives,
                         n.attributes, n.parameters)

    x.list <- createDesignMatrix(attribute.data, n.attributes, n.questions,
                                 n.alternatives, n.parameters,
                                 n.attribute.parameters, input.prior.mean,
                                 non.missing.table)
    X <- x.list$X
    parameter.scales <- x.list$parameter.scales
    parameter.range <- calcRange(attribute.data)
    prior.mean <- processInputPrior(input.prior.mean, n.parameters,
                                    n.attributes, n.attribute.parameters,
                                    parameter.scales)
    prior.sd <- processInputPrior(input.prior.sd, n.parameters,
                                  n.attributes, n.attribute.parameters,
                                  parameter.scales)

    respondent.indices <- constructRespondentIndices(non.missing.table)

    if (is.data.simulated)
    {
        n.respondents <- simulated.sample.size
        subset <- rep(TRUE, n.respondents)
        weights <- rep(1, n.respondents)
        covariates <- NULL
        sampled.output <- sampleFromX(X, respondent.indices,
                                      n.respondents, seed)
        X <- sampled.output$X
        respondent.indices <- sampled.output$respondent.indices
        attribute.names <- unique(names(attribute.data))
        output <- generateSimulatedChoices(X, respondent.indices,
                                           simulated.priors, seed,
                                           n.alternatives,
                                           n.attribute.parameters,
                                           attribute.names,
                                           parameter.scales)
        Y <- output$choices
        simulated.respondent.parameters <- output$respondent.parameters
    }
    else
        simulated.respondent.parameters <- NULL

    split.data <- crossValidationSplit(X, Y, n.questions.left.out, seed,
                                       respondent.indices)
    none.alternatives <- which(apply(split.data$X.in, 2, function(x) nrow(unique(x)) == 1))

    list(n.questions = n.questions,
         n.questions.left.out = n.questions.left.out,
         n.alternatives = n.alternatives,
         n.attributes = n.attributes,
         n.respondents = n.respondents,
         n.parameters = n.parameters,
         n.attribute.parameters = n.attribute.parameters,
         par.names = par.names,
         all.names = all.names,
         beta.names = par.names,
         all.beta.names = all.names,
         X.in = split.data$X.in,
         Y.in = split.data$Y.in,
         X.out = split.data$X.out,
         Y.out = split.data$Y.out,
         left.out = split.data$left.out,
         n.questions.left.in = split.data$n.questions.left.in,
         subset = subset,
         weights = weights,
         covariates = covariates,
         parameter.scales = parameter.scales,
         parameter.range = parameter.range,
         prior.mean = prior.mean,
         prior.sd = prior.sd,
         simulated.respondent.parameters = simulated.respondent.parameters,
         attribute.levels = attribute.levels,
         none.alternatives = none.alternatives)
}

extractChoices <- function(experiment.data, non.missing.table)
{
    n.questions <- ncol(non.missing.table)
    result <- c(t(sapply(experiment.data[, 1:n.questions],
                         function(x) as.numeric(x))))
    result <- result[c(t(non.missing.table))]
}

# Ensure that each factor contains a complete set of levels
completeLevels <- function(attribute.data)
{
    nms <- names(attribute.data)
    unique.names <- unique(nms)
    for (nm in unique.names)
    {
        att <- attribute.data[nms == nm]
        if (is.factor(att[[1]]))
        {
            complete.levels <- unique(unlist(lapply(att, levels)))
            ind <- which(nms == nm)
            for (i in ind)
            {
                att.q <- attribute.data[[i]]
                lvls <- levels(att.q)
                map <- rep(NA, length(lvls))

                for (j in 1:length(lvls))
                    map[j] <- which(lvls[j] == complete.levels)
                attribute.data[[i]] <- factor(complete.levels[map[att.q]],
                                              levels = complete.levels)
            }
        }
    }
    attribute.data
}

createDesignMatrix <- function(attribute.data, n.attributes, n.questions,
                               n.alternatives, n.parameters,
                               n.attribute.parameters,
                               input.prior.mean, non.missing.table)
{
    n.rs <- sum(non.missing.table)
    n.qc <- n.questions * n.alternatives
    n.respondents <- nrow(attribute.data)
    meansAndSDs <- getParameterMeanAndSD(attribute.data, n.attributes,
                                            n.questions, n.alternatives)
    parameter.scales <- rep(1, n.parameters)

    rs <- 1
    X <- array(dim = c(n.rs, n.alternatives, n.parameters))
    for (r in 1:n.respondents)
    {
        for (q in 1:n.questions)
        {
            if (non.missing.table[r, q])
            {
                p <- 1
                for (i in 1:n.attributes)
                {
                    is.ordered <- length(input.prior.mean) == n.attributes &&
                                  input.prior.mean[i] != 0
                    for (j in 1:n.alternatives)
                    {
                        ind <- n.qc * (i - 1) + n.alternatives * (q - 1) + j
                        v <- attribute.data[[ind]][r]
                        if (is.factor(v))
                        {
                            n.v <- length(levels(v)) - 1
                            int.v <- as.numeric(v)
                            X[rs, j, p:(p + n.v - 1)] <- 0
                            if (int.v > 1)
                            {
                                if (is.ordered)
                                    X[rs, j, p:(p + int.v - 2)] <- 1
                                else
                                    X[rs, j, p + int.v - 2] <- 1
                            }
                        }
                        else
                        {
                            mn <- meansAndSDs$means[i]
                            std <- meansAndSDs$sds[i]
                            # Divide by 2 * SD as recommended by Gelman in
                            # "Scaling regression inputs by dividing by two
                            # standard deviations (2008)"
                            X[rs, j, p] <- 0.5 * (v - mn) / std

                            if (q == 1 && j == 1)
                                parameter.scales[p] <- 2 * std
                        }
                    }
                    if (is.factor(v))
                        p <- p + n.v
                    else
                        p <- p + 1
                }
                rs <- rs + 1
            }
        }
    }
    list(X = X, parameter.scales = parameter.scales)
}

nAttributeParameters <- function(attribute.data, n.attributes, n.questions,
                                 n.alternatives)
{
    result <- rep(NA, n.attributes)
    for (i in 1:n.attributes)
    {
        v <- attribute.data[[n.questions * n.alternatives * (i - 1) + 1]]
        if (is.factor(v))
            result[i] <- length(levels(v)) - 1
        else
            result[i] <- 1
    }
    result
}

parameterNames <- function(attribute.data, n.attributes, n.questions,
                           n.alternatives, n.parameters)
{
    nms <- names(attribute.data)
    result <- rep("", n.parameters)
    ind <- 1
    for (i in 1:n.attributes)
    {
        col <- n.questions * n.alternatives * (i - 1) + 1
        v <- attribute.data[[col]]
        if (is.factor(v))
        {
            lvls <- levels(v)
            for (j in 2:length(lvls))
                result[ind + j - 2] <- paste0(nms[col], ": ", lvls[j])
            ind <- ind + length(lvls) - 1
        }
        else
        {
            result[ind] <- nms[col]
            ind <- ind + 1
        }
    }
    result
}

# Includes the names of parameters left out
allNames <- function(attribute.data, n.attributes, n.questions,
                     n.alternatives, n.parameters)
{
    nms <- names(attribute.data)
    result <- rep("", n.parameters)
    ind <- 1
    for (i in 1:n.attributes)
    {
        col <- n.questions * n.alternatives * (i - 1) + 1
        v <- attribute.data[[col]]
        if (is.factor(v))
        {
            lvls <- levels(v)
            for (j in 1:length(lvls))
                result[ind + j - 1] <- paste0(nms[col], ": ", lvls[j])
            ind <- ind + length(lvls)
        }
        else
        {
            result[ind] <- nms[col]
            ind <- ind + 1
        }
    }
    result
}

# Creates a list of attribute levels. If an attribute is numeric, an empty
# character vector is provided for that attribute.
attributeLevels <- function(attribute.data, n.attributes, n.questions,
                            n.alternatives, n.parameters)
{
    nms <- names(attribute.data)
    result <- list()
    for (i in 1:n.attributes)
    {
        col <- n.questions * n.alternatives * (i - 1) + 1
        v <- attribute.data[[col]]
        result[[nms[col]]] <- if (is.factor(v))
            levels(v)
        else
            character(0)
    }
    result
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

getParameterMeanAndSD <- function(attribute.data, n.attributes, n.questions,
                                  n.alternatives)
{
    n.qc <- n.questions * n.alternatives

    means <- rep(NA, n.attributes)
    sds <- rep(NA, n.attributes)
    for (i in 1:n.attributes)
    {
        v <- attribute.data[[n.qc * (i - 1) + 1]]
        if (!is.factor(v))
        {
            ind.start <- n.qc * (i - 1) + 1
            ind.end <- n.qc * i
            values <- as.matrix(attribute.data[ind.start:(n.qc * i)])
            means[i] <-  mean(values)
            sds[i] <- sd(values)
        }
    }
    list(means = means, sds = sds)
}

#' @importFrom flipData MissingDataFail
errorIfMissingDataFoundExperiment <- function(experiment.data, subset, weights,
                                              covariates, n.questions,
                                              is.data.simulated)
{
    if (is.data.simulated)
        experiment.data <- experiment.data[, -1:-n.questions]

    if (any(is.na(experiment.data)) ||
        (!is.null(subset) && any(is.na(subset))) ||
        (!is.null(weights) && any(is.na(weights))) ||
        (!is.null(covariates) && any(is.na(covariates))))
        MissingDataFail();
}

#' Returns which respondents are considered missing based on missing data
#' settings.
#' @param non.missing.table A logical matrix of respondents x questions
#'     indicating which ones are not missing.
#' @param n.questions.left.out The number of questions to leave out.
#' @param missing The missing data setting.
#' @param n.questions The number of questions per respondent.
nonMissingRespondents <- function(non.missing.table, n.questions.left.out,
                                  missing, n.questions)
{
    n.respondents <- nrow(non.missing.table)
    if (missing == "Error if missing data")
        rep(TRUE, n.respondents)
    else if (missing == "Use partial data")
        rowSums(non.missing.table) > n.questions.left.out
    else if (missing == "Exclude cases with missing data")
        apply(non.missing.table, 1, all)
}

nonMissingTableForExperiment <- function(experiment.data, subset, weights,
                                         covariates, n.questions,
                                         n.alternatives, n.attributes, missing,
                                         is.data.simulated)
{
    result <- matrix(TRUE, nrow = nrow(experiment.data), ncol = n.questions)
    if (missing == "Use partial data")
    {
        if (!is.data.simulated)
            for (i in 1:n.questions)
                result[, i] <- result[, i] & !is.na(experiment.data[[i]])

        for (i in 1:n.attributes)
        {
            for (j in 1:n.questions)
            {
                ind <- n.questions + (i - 1) * n.questions * n.alternatives +
                       (j - 1) * n.alternatives + (1:n.alternatives)
                not.missing <- !is.na(rowSums(sapply(experiment.data[, ind],
                                                     as.numeric)))
                result[, j] <- result[, j] & not.missing
            }
        }
    }
    else
    {
        if (is.data.simulated)
            experiment.data <- experiment.data[, -1:-n.questions]
        result <- result & !is.na(rowSums(sapply(experiment.data, as.numeric)))
    }

    if (!is.null(subset))
        result <- result & !is.na(subset)
    if (!is.null(weights))
        result <- result & !is.na(weights)
    if (!is.null(covariates))
        result <- result & !is.na(rowSums(covariates))
    result
}
