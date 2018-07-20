latentClassChoiceModel <- function(dat, n.classes = 1, seed = 123,
                                   initial.parameters = NULL, tol = 0.0001)
{
    ind.levels <- respondentIndicies(dat$n.questions.left.in)
    n.levels <- length(ind.levels)
    n.alternatives <- dim(dat$X.in)[2]
    n.parameters <- dim(dat$X.in)[3]

    X <- transformDataForLCA(dat$X.in, dat$Y.in)
    weights <- dat$weights
    repeated.weights <- RepeatWeights(weights, ind.levels)
    resp.questions.to.levels <- RespondentQuestionsToLevels(ind.levels)

    pars <- if (!is.null(initial.parameters))
        initial.parameters
    else
    {
        class.memberships <- RandomClassMemberships(n.levels, n.classes, seed)
        inferParameters(class.memberships, X, repeated.weights,
                        resp.questions.to.levels, n.alternatives, n.parameters)
    }

    previous.log.likelihood <- -Inf

    repeat
    {
        # Expectation
        post.prob <- posteriorProbabilities(pars, X, ind.levels, n.classes,
                                            n.alternatives, n.parameters)
        # Maximisation
        pars <- inferParameters(post.prob, X, repeated.weights,
                                resp.questions.to.levels, n.alternatives,
                                n.parameters)

        log.likelihood <- logLikelihood(pars, X, weights, ind.levels,
                                        n.classes, n.alternatives,
                                        n.parameters)

        if (log.likelihood - previous.log.likelihood < tol)
            break
        else
            previous.log.likelihood <- log.likelihood
    }

    if (!is.null(dat$subset))
    {
        resp.post.probs <- matrix(NA, length(dat$subset), n.classes)
        resp.post.probs[dat$subset, ] <- post.prob
    }
    else
        resp.post.probs <- post.prob

    sorted <- sortClasses(pars, resp.post.probs)
    pars <- sorted$pars
    resp.post.probs <- sorted$resp.post.probs

    par.stats <- parameterStatisticsLCA(pars, X, ind.levels, weights,
                                        n.classes, n.alternatives,
                                        n.parameters, dat$par.names,
                                        dat$parameter.scales)

    # Descale class parameters
    pars$class.parameters <- pars$class.parameters / dat$parameter.scales

    result <- list()
    result$log.likelihood <- log.likelihood
    result$posterior.probabilities <- resp.post.probs
    result$effective.sample.size <- ess <- sum(weights)
    n.lca.parameters <- n.classes * n.parameters + n.classes - 1
    result$bic <- -2 * log.likelihood + log(ess) * n.lca.parameters
    result$reduced.respondent.parameters <- computeRespParsLCA(resp.post.probs,
                                                        pars$class.parameters,
                                                        dat$par.names)

    result$respondent.parameters <- computeRespParsLCA(resp.post.probs,
                                                       pars$class.parameters,
                                                       dat$par.names,
                                                       dat$all.names)
    result$parameter.statistics <- par.stats
    result$class.parameters <- pars$class.parameters
    result$coef <- createCoefOutput(pars, dat$par.names, dat$all.names)
    class(result) <- "FitChoice"
    result
}

# Infer parameters given class memberships
inferParameters <- function(class.memberships, X, weights,
                            resp.questions.to.levels,
                            n.alternatives, n.parameters)
{
    n.classes <- ncol(class.memberships)
    result <- list(class.parameters = matrix(NA, nrow = n.parameters,
                                             ncol = n.classes))
    # Class parameters
    for (c in 1:n.classes)
    {
        w <- class.memberships[resp.questions.to.levels, c] * weights
        solution <- choiceLogit(X, w, n.alternatives, n.parameters)
        p <- solution$par
        p[p > 100] = 100
        p[p < -100] = -100
        result$class.parameters[, c] <- p
    }

    # Class sizes
    result$class.sizes <- colMeans(class.memberships)

    result
}

#' @title RandomClassMemberships
#' @description Randomly assign n individuals to classes.
#' @param n The number of individuals to be assigned classes.
#' @param n.classes The number of classes.
#' @param seed The random seed.
#' @return A matrix of size n.classes x n containing 1s and 0s where 1
#'     indicates membership.
#' @export
RandomClassMemberships <- function(n, n.classes, seed = 123)
{
    set.seed(seed)
    memberships <- vector("integer", n)
    if (n < n.classes)
        stop("The number of individuals may not be less than the number of classes.")

    # Ensures that each class has at least one member
    memberships <- c(1:n.classes, sample(1:n.classes, n - n.classes,
                                         replace = T))[sample(n, n)]

    res <- matrix(0, n, n.classes)
    for (i in 1:n)
        res[i, memberships[i]] <- 1
    res
}

# Calculate posterior probabilities of class membership given parameters
posteriorProbabilities <- function(pars, X, ind.levels, n.classes,
                                   n.alternatives, n.parameters)
{
    log.densities <- logDensities(pars$class.parameters, X, ind.levels,
                                  n.classes, n.alternatives, n.parameters)
    PosteriorProbsFromDensities(log.densities, log(pars$class.sizes),
                                length(ind.levels), n.classes)
}

#' @title PosteriorProbsFromDensities
#' @description Compute posterior probabilities from log densities.
#' @param log.densities Log densities for each respondent question.
#' @param log.class.weights Log of the class weights.
#' @param n.levels The number of levels (respondents).
#' @param n.classes The number of classes.
#' @return A matrix containing posterior probabilities of class membership
#'     for each respondent.
#' @export
PosteriorProbsFromDensities <- function(log.densities, log.class.weights,
                                        n.levels, n.classes)
{
    repsondents.log.densities <- vector("numeric", n.levels)
    for (l in 1:n.levels)
        repsondents.log.densities[l] <- logSumExp(log.class.weights +
                                                      log.densities[l, ])

    exp(t(matrix(rep(log.class.weights, n.levels), n.classes)) + log.densities
        - t(matrix(rep(repsondents.log.densities, each = n.classes), n.classes)))
}

logLikelihood <- function(pars, X, weights, ind.levels, n.classes,
                          n.alternatives, n.parameters)
{
    log.class.weights <- log(pars$class.sizes)
    log.densities <- logDensities(pars$class.parameters, X, ind.levels,
                                  n.classes, n.alternatives, n.parameters)
    n.levels <- length(ind.levels)
    res <- 0
    for (l in 1:n.levels)
        res <- res + logSumExp(log.class.weights + log.densities[l, ]) *
               weights[l]
    res
}

logLikelihoodForHessian <- function(p, X, weights, ind.levels,
                                    n.classes, n.alternatives,
                                    n.parameters)
{
    pars <- parameterVectorToList(p, n.classes, n.parameters)
    logLikelihood(pars, X, weights, ind.levels,
                  n.classes, n.alternatives,
                  n.parameters)
}

parameterListToVector <- function(parameter.list)
{
    class.parameters <- parameter.list$class.parameters
    class.sizes <- parameter.list$class.sizes
    n.parameters <- nrow(class.parameters)
    n.classes <- ncol(class.parameters)
    if (n.classes == 1)
        class.parameters[, 1]
    else
    {
        result <- rep(NA, n.parameters * n.classes + n.classes - 1)
        result[1:(n.classes - 1)] <- log(class.sizes[2:n.classes] /
                                             class.sizes[1])
        start.ind <- n.classes
        for (i in 1:n.classes)
        {
            end.ind <- start.ind + n.parameters - 1
            result[start.ind:end.ind] <- class.parameters[, i]
            start.ind <- end.ind + 1
        }
        result
    }
}

parameterVectorToList <- function(p, n.classes, n.parameters)
{
    result <- list()
    result$class.parameters <- matrix(NA, nrow = n.parameters,
                                      ncol = n.classes)
    if (n.classes == 1)
    {
        result$class.sizes <- 1
        result$class.parameters <- matrix(p, nrow = n.parameters, ncol = 1)
    }
    else
    {
        exp.pars <- exp(p[1:(n.classes - 1)])
        total <- 1 + sum(exp.pars)
        result$class.sizes <- c(1, exp.pars) / total
        start.ind <- n.classes
        for (i in 1:n.classes)
        {
            end.ind <- start.ind + n.parameters - 1
            result$class.parameters[, i] <- p[start.ind:end.ind]
            start.ind <- end.ind + 1
        }
    }
    result
}

logDensities <- function(class.pars, X, ind.levels, n.classes,
                         n.alternatives, n.parameters)
{
    n.resp.questions <- nrow(X)
    log.shares <- matrix(NA, n.resp.questions, n.classes)
    for (c in 1:n.classes)
        log.shares[, c] <- logDensitiesChoice(class.pars[, c], X,
                                              rep(1, n.resp.questions),
                                              n.alternatives, n.parameters)
    n.levels <- length(ind.levels)
    result <- matrix(NA, n.levels, n.classes)
    for (l in 1:n.levels)
        result[l, ] <- colSums(log.shares[ind.levels[[l]], , drop = FALSE])
    result
}

respondentIndicies <- function(n.resp.questions)
{
    n.respondents <- length(n.resp.questions)
    result <- list()
    last.index <- 0
    for (i in 1:n.respondents)
    {
        result[[i]] <- (last.index + 1):(last.index + n.resp.questions[i])
        last.index <- last.index + n.resp.questions[i]
    }
    result
}

#' @importFrom stats optim
choiceLogit <- function(X, weights, n.alternatives, n.parameters)
{
    init.b <- seq(.01, .02, length.out = n.parameters)
    optim(init.b,
          logDensityChoice,
          gr = gradientChoice,
          X = X,
          weights = weights,
          n_alternatives = n.alternatives,
          n_parameters = n.parameters,
          method =  "BFGS",
          control = list(fnscale  = -1, maxit = 1000, trace = 0),
          hessian = FALSE)
}

# Transforms the array X to a matrix by combining the alternative and
# parameter dimensions and moves the chosen alternative to the front.
transformDataForLCA <- function(X, Y)
{
    n.respondent.questions <- dim(X)[1]
    n.alternatives <- dim(X)[2]
    n.parameters <- dim(X)[3]
    result <- matrix(NA, nrow = n.respondent.questions,
                     ncol = n.alternatives * n.parameters)
    for (i in 1:n.respondent.questions)
    {
        chosen <- X[i, Y[i], ]
        X[i, 2:n.alternatives, ] <- X[i, -Y[i], ]
        X[i, 1, ] <- chosen
        result[i, ] <- c(t(X[i, , ]))
    }
    result
}

#' @title RepeatWeights
#' @description Repeat weights for each respondent question.
#' @param weights A vector of respondent weights.
#' @param respondent.indices A list of question indices for each respondent.
#' @return A vector of repeated weights.
#' @export
RepeatWeights <- function(weights, respondent.indices)
{
    result <- rep(NA, sum(sapply(respondent.indices, length)))
    n.respondents <- length(respondent.indices)
    start.ind <- 1
    for (i in 1:n.respondents)
    {
        end.ind <- start.ind + length(respondent.indices[[i]]) - 1
        result[start.ind:end.ind] <- weights[i]
        start.ind <- end.ind + 1
    }
    result
}

#' @title RespQuestionsToLevels
#' @description Maps respondent question indices to levels.
#' @param ind.levels A list of question indices for each level.
#' @return A vector of levels.
#' @export
RespondentQuestionsToLevels <- function(ind.levels)
{
    n.levels <- length(ind.levels)
    result <- rep(NA, sum(sapply(ind.levels, length)))
    for (i in 1:n.levels)
        result[ind.levels[[i]]] <- i
    result
}

#' @importFrom pracma hessian
standardErrorsForLCA <- function(p, X, ind.levels, weights, n.classes,
                                 n.alternatives, n.parameters)
{
    hess <- hessian(logLikelihoodForHessian, p, X = X,
                    weights = weights,
                    ind.levels = ind.levels, n.classes = n.classes,
                    n.alternatives = n.alternatives,
                    n.parameters = n.parameters)
    sqrt(diag(solve(-hess)))
}

gradientLCA <- function(pars, X, ind.levels, weights, n.alternatives)
{
    class.parameters <- pars$class.parameters
    class.sizes <- pars$class.sizes
    n.parameters <- nrow(class.parameters)
    n.classes <- length(class.sizes)
    n.levels <- length(ind.levels)

    result <- rep(0, n.parameters * n.classes)
    exp.discriminants <- computeExpDiscriminants(class.parameters, X,
                                                 ind.levels, n.classes,
                                                 n.alternatives)
    for (i in 1:n.levels)
    {
        n.questions <- length(ind.levels[[i]])
        inverse.density <- computeInverseDensity(exp.discriminants,
                                                 class.sizes, i, n.questions)
        n.questions <- length(ind.levels[[i]])
        ind <- 1
        for (k in 1:n.classes)
        {
            for (p in 1:n.parameters)
            {
                prod.dens <- computeProductDensity(exp.discriminants, i, k,
                                                   n.questions)
                share.deriv <- computeShareDerivative(X[ind.levels[[i]], ],
                                                  exp.discriminants, i, k, p,
                                                  n.parameters)
                result[ind] <- result[ind] + inverse.density * weights[i] *
                               class.sizes[k] * prod.dens * share.deriv
                ind <- ind + 1
            }
        }
    }
    result
}

computeExpDiscriminants <- function(class.parameters, X, ind.levels, n.classes,
                                    n.alternatives)
{
    n.levels <- length(ind.levels)
    n.parameters <- nrow(class.parameters)
    max.n.questions <- max(sapply(ind.levels, length))
    result <- array(dim = c(n.levels, n.classes, max.n.questions, n.alternatives))
    for (i in 1:n.levels)
    {
        n.questions <- length(ind.levels[[i]])
        for (k in 1:n.classes)
        {
            for (q in 1:n.questions)
            {
                ind <- 1
                for (j in 1:n.alternatives)
                {
                    discriminant <- 0
                    for (p in 1:n.parameters)
                    {
                         discriminant <- discriminant +
                                         X[ind.levels[[i]][q], ind] *
                                         class.parameters[p, k]
                         ind <- ind + 1
                    }
                    result[i, k, q, j] <- exp(discriminant)
                }
            }
        }
    }
    result
}

computeProductDensity <- function(exp.discriminants, level.index,
                                  class.index, n.questions)
{
    result <- 1
    for (q in 1:n.questions)
        result <- result * exp.discriminants[level.index, class.index, q, 1] /
                  sum(exp.discriminants[level.index, class.index, q, ])
    result
}

computeInverseDensity <- function(exp.discriminants, class.sizes, level.index,
                                  n.questions)
{
    n.classes <- length(class.sizes)
    result <- 0
    for (k in 1:n.classes)
        result <- result + class.sizes[k] *
            computeProductDensity(exp.discriminants, level.index, k,
                                  n.questions)
    1 / result
}

computeShareDerivative <- function(X, exp.discriminants, level.index,
                                   class.index, parameter.index, n.parameters)
{
    n.questions <- nrow(X)
    n.alternatives <- dim(exp.discriminants)[4]
    result <- 0
    for (q in 1:n.questions)
    {
        sum.exp <- 0
        sum.x.exp <- 0
        ind <- parameter.index
        for (j in 1:n.alternatives)
        {
            sum.exp <- sum.exp + exp.discriminants[level.index, class.index,
                                                   q, j]
            sum.x.exp <- sum.x.exp + X[q, ind] *
                         exp.discriminants[level.index, class.index, q, j]
            ind <- ind + n.parameters
        }
        result <- result + X[q, parameter.index] - sum.x.exp / sum.exp
    }
    result
}

sortClasses <- function(pars, resp.post.probs)
{
    reordering <- rank(-pars$class.sizes)
    pars$class.sizes[reordering] <- pars$class.sizes
    resp.post.probs[, reordering] <- resp.post.probs
    n.classes <- length(pars$class.sizes)
    class.parameters <- pars$class.parameters
    for (i in 1:n.classes)
        class.parameters[, reordering[i]] <- pars$class.parameters[, i]
    pars$class.parameters <- class.parameters
    list(pars = pars, resp.post.probs = resp.post.probs)
}

computeRespParsLCA <- function(resp.post.probs, class.parameters,
                               par.names, all.names = NULL)
{
    if (is.null(all.names))
        all.names <- par.names

    n.classes <- ncol(class.parameters)
    n.respondents <- nrow(resp.post.probs)
    n.all.parameters <- length(all.names)
    all.class.parameters <- matrix(0, nrow = n.all.parameters,
                                   ncol = n.classes)
    ind <- 1
    for (i in 1:n.all.parameters)
    {
        if (all.names[i] %in% par.names)
        {
            all.class.parameters[i, ] <- class.parameters[ind, ]
            ind <- ind + 1
        }
    }

    result <- matrix(0, nrow = n.respondents, ncol = n.all.parameters)
    for (i in 1:n.classes)
        result <- result + resp.post.probs[, i] %*% t(all.class.parameters[, i])
    colnames(result) <- all.names

    result
}

#' @importFrom stats pt
parameterStatisticsLCA <- function(pars, X, ind.levels, weights,
                                   n.classes, n.alternatives, n.parameters,
                                   parameter.names, parameter.scales)
{
    p <- parameterListToVector(pars)
    std.errors <- standardErrorsForLCA(p, X, ind.levels, weights,
                         n.classes, n.alternatives,
                         n.parameters)
    t.stats <- p / std.errors
    p.values <- 2*pt(abs(t.stats), nrow(X) - length(p), lower = FALSE)
    m <- matrix(NA, nrow = length(p), ncol = 4)
    m[, 1] <- p
    m[, 2] <- std.errors
    m[, 3] <- t.stats
    m[, 4] <- p.values

    # Reorder parameters so that parameters can be compared between classes
    # and descale parameters and standard errors
    result <- matrix(NA, nrow = length(p), ncol = 4)
    result[1:(n.classes - 1), ] <- m[1:(n.classes - 1), ]
    ind <- n.classes
    for (i in 1:n.parameters)
    {
        for (j in 1:n.classes)
        {
            old.ind <- n.classes - 1 + (j - 1) * n.parameters + i
            result[ind, ] <- m[old.ind, ]
            result[ind, 1:2] <- result[ind, 1:2] / parameter.scales[i]
            ind <- ind + 1
        }
    }

    colnames(result) <- c("Coefficient", "Standard Error", "t-Statistic",
                          "p-Value")
    if (n.classes == 1)
        rownames(result) <- parameter.names
    else
        rownames(result) <- c(paste0("Class ", 2:n.classes, " size parameter"),
                              paste0(rep(parameter.names, each = n.classes),
                                     ", Class ",
                                     rep(1:n.classes, n.parameters)))
    result
}

#' @importFrom flipFormat FormatAsPercent
createCoefOutput <- function(pars, par.names, all.names)
{
    class.parameters <- pars$class.parameters
    class.sizes <- pars$class.sizes
    n.classes <- length(pars$class.sizes)
    n.all.parameters <- length(all.names)

    if (n.classes == 1)
    {
        result <- rep(NA, n.all.parameters)
        ind <- 1
        for (i in 1:n.all.parameters)
        {
            if (all.names[i] %in% par.names)
            {
                result[i] <- class.parameters[ind]
                ind <- ind + 1
            }
        }
        rownames(result) <- all.names
    }
    else
    {
        result <- matrix(0, nrow = n.all.parameters, ncol = n.classes)
        ind <- 1
        for (i in 1:n.all.parameters)
        {
            if (all.names[i] %in% par.names)
            {
                result[i, ] <- class.parameters[ind, ]
                ind <- ind + 1
            }
        }
        rownames(result) <- all.names
        colnames(result) <- paste0("Class ", 1:n.classes, " (",
                                   FormatAsPercent(class.sizes, decimals = 1), ")")
    }
    result
}
