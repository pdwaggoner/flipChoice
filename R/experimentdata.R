#' @importFrom flipData CalibrateWeight CleanSubset CleanWeights
processExperimentData <- function(experiment.data, subset, weights,
                                  n.questions.left.out, seed,
                                  input.prior.mean,
                                  input.prior.sd)
{
    non.missing <- !is.na(rowSums(sapply(experiment.data, as.numeric)))

    # Tidying weights and subset
    filter.subset <- CleanSubset(subset, nrow(experiment.data))
    subset <- filter.subset & non.missing
    weights <- prepareWeights(weights, subset)

    nms <- names(experiment.data)
    choice.name <- nms[1]
    n.questions <- sum(nms == choice.name)
    n.questions.left.in <- n.questions - n.questions.left.out
    n.choices <- length(levels(experiment.data[[1]]))
    n.qc <- n.questions * n.choices
    n.attributes <- (length(nms) - n.questions) / n.qc
    if (round(n.attributes) != n.attributes)
        stop("The number of variables in the Experiment question is invalid.")
    experiment.data <- experiment.data[subset, ]
    n.respondents <- nrow(experiment.data)
    Y <- sapply(experiment.data[, 1:n.questions], function(x) as.numeric(x))
    attribute.data <- experiment.data[, -1:-n.questions]
    names(attribute.data) <- nms[-1:-n.questions]
    attribute.data <- completeLevels(attribute.data)
    n.attribute.variables <- nAttributeVariables(attribute.data, n.attributes, n.questions, n.choices)
    n.variables <- sum(n.attribute.variables)
    n.raw.variables <- sum(sapply(n.attribute.variables - 1, function(x) max(x, 1)))
    var.names <- variableNames(attribute.data, n.attributes, n.questions, n.choices, n.variables)

    checkPriorParameters(input.prior.mean, input.prior.sd, n.attributes)

    X.list <- createDesignMatrix(attribute.data, n.attributes, n.questions,
                                 n.choices, n.variables, input.prior.mean)
    variable.scales <- X.list$variable.scales
    prior.mean <- processInputPrior(input.prior.mean, n.raw.variables,
                                    n.attributes, n.attribute.variables,
                                    variable.scales)
    prior.sd <- processInputPrior(input.prior.sd, n.raw.variables,
                                  n.attributes, n.attribute.variables,
                                  variable.scales)
    split.data <- crossValidationSplit(X.list$X, Y, n.questions.left.out, seed)

    result <- list(n.questions = n.questions,
                   n.questions.left.in = n.questions.left.in,
                   n.questions.left.out = n.questions.left.out,
                   n.choices = n.choices,
                   n.attributes = n.attributes,
                   n.respondents = n.respondents,
                   n.variables = n.variables,
                   n.raw.variables = n.raw.variables,
                   n.attribute.variables = n.attribute.variables,
                   var.names = var.names,
                   X.in = split.data$X.in,
                   Y.in = split.data$Y.in,
                   X.out = split.data$X.out,
                   Y.out = split.data$Y.out,
                   subset = subset,
                   weights = weights,
                   variable.scales = variable.scales,
                   prior.mean = prior.mean,
                   prior.sd = prior.sd)
    result
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
            ind <- (1:length(attribute.data))[nms == nm]
            for (i in ind)
            {
                att.q <- attribute.data[[i]]
                lvls <- levels(att.q)
                map <- rep(NA, length(lvls))
                for (j in 1:length(lvls))
                    map[j] <- (1:length(complete.levels))[lvls[j] == complete.levels]
                attribute.data[[i]] <- factor(complete.levels[map[att.q]], levels = complete.levels)
            }
        }
    }
    attribute.data
}

createDesignMatrix <- function(attribute.data, n.attributes, n.questions,
                               n.choices, n.variables, input.prior.mean)
{
    n.qc <- n.questions * n.choices
    n.respondents <- nrow(attribute.data)
    variable.scales <- rep(1, n.variables)

    c <- 1
    X <- array(dim = c(n.respondents, n.questions, n.choices, n.variables))
    for (i in 1:n.attributes)
    {
        for (q in 1:n.questions)
        {
            for (j in 1:n.choices)
            {
                v <- attribute.data[[n.qc * (i - 1) + n.choices * (q - 1) + j]]
                if (is.factor(v))
                {
                    is.ordered <- (length(input.prior.mean) == 1 &&
                                   input.prior.mean != 0) ||
                                  (length(input.prior.mean) > 1 &&
                                   input.prior.mean[i] != 0)
                    if (is.ordered)
                    {
                        n.v <- length(levels(v))
                        numeric.v <- as.numeric(v)
                        X[, q, j, c:(c + n.v - 1)] <- 0
                        for (r in 1:n.respondents)
                            X[r, q, j, c:(numeric.v[r] - 1 + c)] <- 1
                    }
                    else
                    {
                        n.v <- length(levels(v))
                        numeric.v <- as.numeric(v)
                        X[, q, j, c:(c + n.v - 1)] <- 0
                        for (r in 1:n.respondents)
                            X[r, q, j, numeric.v[r] - 1 + c] <- 1
                    }
                }
                else
                {
                    scaled.v <- 0.5 * scale(v)
                    X[, q, j, c] <- scaled.v
                    # this is 2 times the standard deviation of v
                    variable.scales[c] <- 2 * attr(scaled.v, "scaled:scale")
                }
            }
        }
        if (is.factor(v))
            c <- c + n.v
        else
            c <- c + 1
    }
    list(X = X, variable.scales = variable.scales)
}

nAttributeVariables <- function(attribute.data, n.attributes, n.questions, n.choices)
{
    result <- rep(NA, n.attributes)
    for (i in 1:n.attributes)
    {
        v <- attribute.data[[n.questions * n.choices * (i - 1) + 1]]
        if (is.factor(v))
            result[i] <- length(levels(v))
        else
            result[i] <- 1
    }
    result
}

variableNames <- function(attribute.data, n.attributes, n.questions, n.choices, n.variables)
{
    nms <- names(attribute.data)
    result <- rep("", n.variables)
    ind <- 1
    for (i in 1:n.attributes)
    {
        col <- n.questions * n.choices * (i - 1) + 1
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

#' @title LeftOutQuestions
#' @description Randomly select questions to leave out for each respondent.
#' @param n.respondents Number of respondents.
#' @param n.questions Number of questions per respondent.
#' @param n.questions.left.out Number of questions per respondent to leave out.
#' @param seed Random seed.
#' @return A logical matrix (\code{n.questions} by \code{n.respondents}) indicating
#' which questions are left out.
#' @export
LeftOutQuestions <- function(n.respondents, n.questions, n.questions.left.out, seed)
{
    set.seed(seed)
    sapply(rep(n.questions, n.respondents), function(x) (1:n.questions) %in% sample(x, n.questions.left.out))
}

processInputPrior <- function(prior.par, n.raw.variables, n.attributes,
                              n.attribute.variables, variable.scales)
{
    result <- rep(NA, n.raw.variables)

    if (length(prior.par) == 1)
        result <- rep(prior.par, n.raw.variables)
    else
    {
        n.attribute.raw.variables <- pmax(n.attribute.variables - 1, 1)

        result <- rep(NA, n.raw.variables)
        for (i in 1:n.attributes)
        {
            if (n.attribute.variables[i] == 1) # numeric
            {
                index.all <- sum(n.attribute.variables[1:i])
                index.raw <- sum(n.attribute.raw.variables[1:i])
                result[index.raw] <- prior.par[i] * variable.scales[index.all]
            }
            else # categorical
            {
                index.raw.end <- sum(n.attribute.raw.variables[1:i])
                index.raw.start <- index.raw.end - n.attribute.raw.variables[i] + 1
                for (j in index.raw.start:index.raw.end)
                    result[j] <- prior.par[i]
            }
        }
    }
    result
}

crossValidationSplit <- function(X, Y, n.questions.left.out, seed)
{
    if (n.questions.left.out > 0)
    {
        n.respondents <- dim(X)[1]
        n.questions <- dim(X)[2]
        n.choices <- dim(X)[3]
        n.variables <- dim(X)[4]
        n.questions.left.in <- n.questions - n.questions.left.out
        left.out <- LeftOutQuestions(n.respondents, n.questions, n.questions.left.out, seed)
        X.in <- array(dim = c(n.respondents, n.questions.left.in, n.choices, n.variables))
        Y.in <- matrix(NA, nrow = n.respondents, ncol = n.questions.left.in)
        X.out <- array(dim = c(n.respondents, n.questions.left.out, n.choices, n.variables))
        Y.out <- matrix(NA, nrow = n.respondents, ncol = n.questions.left.out)
        for (r in 1:n.respondents)
        {
            X.in[r, , , ] <- X[r, !left.out[, r], , ]
            Y.in[r, ] <- Y[r, !left.out[, r]]
            X.out[r, , , ] <- X[r, left.out[, r], , ]
            Y.out[r, ] <- Y[r, left.out[, r]]
        }
    }
    else
    {
        X.in <- X
        Y.in <- Y
        X.out <- NULL
        Y.out <- NULL
    }
    list(X.in = X.in, X.out = X.out, Y.in = Y.in, Y.out = Y.out)
}

checkPriorParameters <- function(input.prior.mean, input.prior.sd,
                                 n.attributes)
{
    if (!is.numeric(input.prior.mean) ||
        (length(input.prior.mean) != n.attributes && length(input.prior.mean) != 1))
        stop(paste0("The supplied parameter hb.prior.mean is inappropriate."))
    if (!is.numeric(input.prior.sd) ||
        (length(input.prior.sd) != n.attributes && length(input.prior.sd) != 1))
        stop(paste0("The supplied parameter hb.prior.sd is inappropriate."))
}

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