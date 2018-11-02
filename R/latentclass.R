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
    colnames(resp.post.probs) <- paste0('Class ', 1:n.classes)

    lca.data <- list(pars = pars, compressed.X = compressNumericMatrix(X),
                     dim.X = dim(X), ind.levels = ind.levels,
                     weights = weights, parameter.names = dat$par.names,
                     parameter.scales = dat$parameter.scales)

    # Descale class parameters
    pars$class.parameters <- pars$class.parameters / dat$parameter.scales

    result <- list()

    result$log.likelihood <- log.likelihood
    result$rlh <- rootLikelihood(pars, X, ind.levels, n.classes,
                                            n.alternatives, n.parameters)
    result$mean.rlh <- exp(log.likelihood / sum(repeated.weights))
    n.questions.out <- dat$n.questions.left.out
    if (n.questions.out > 0)
    {
        X.out <- transformDataForLCA(dat$X.out, dat$Y.out)
        ind.levels.out <- lapply(0:(dat$n.respondents - 1), function(x)
                                 x * n.questions.out + 1:n.questions.out)
        log.likelihood.out <- logLikelihood(pars, X.out, weights,
                                            ind.levels.out, n.classes,
                                            n.alternatives, n.parameters)
        result$log.likelihood.out <- log.likelihood.out
        result$rlh.out <- rootLikelihood(pars, X.out, ind.levels.out,
                                         n.classes, n.alternatives,
                                         n.parameters)
        result$mean.rlh.out <- exp(log.likelihood.out /
                                       (sum(weights) * n.questions.out))
    }
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
    result$class.parameters <- pars$class.parameters
    result$class.sizes <- pars$class.sizes
    result$coef <- createCoefOutput(pars, dat$par.names, dat$all.names)
    result$lca.data <- lca.data
    result
}

# Infer parameters given class memberships
inferParameters <- function(class.memberships, X, repeated.weights,
                            resp.questions.to.levels,
                            n.alternatives, n.parameters)
{
    n.classes <- ncol(class.memberships)
    result <- list(class.parameters = matrix(NA, nrow = n.parameters,
                                             ncol = n.classes))
    # Class parameters
    for (c in 1:n.classes)
    {
        w <- class.memberships[resp.questions.to.levels, c] * repeated.weights
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
        repsondents.log.densities[l] <- LogSumExp(log.class.weights +
                                                      log.densities[l, ])
    res <- exp(t(matrix(rep(log.class.weights, n.levels), n.classes)) + log.densities
        - t(matrix(rep(repsondents.log.densities, each = n.classes), n.classes)))
    res
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
        res <- res + LogSumExp(log.class.weights + log.densities[l, ]) *
               weights[l]
    res
}

rootLikelihood <- function(pars, X, ind.levels, n.classes, n.alternatives,
                           n.parameters)
{
    log.class.weights <- log(pars$class.sizes)
    log.densities <- logDensities(pars$class.parameters, X, ind.levels,
                                  n.classes, n.alternatives, n.parameters)
    n.levels <- length(ind.levels)
    res <- rep(NA, n.levels)
    for (l in 1:n.levels)
        res[l] <- exp(LogSumExp(log.class.weights + log.densities[l, ]) / length(ind.levels[[l]]))
    res
}

logLikelihoodForHessian <- function(p, X, ind.levels, weights,
                                    n.classes, n.alternatives,
                                    n.parameters)
{
    pars <- parameterVectorToList(p, n.classes, n.parameters)
    res <- logLikelihood(pars, X, weights, ind.levels,
                  n.classes, n.alternatives,
                  n.parameters)
    res
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

#' @title RespondentQuestionsToLevels
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
    output <- optim(p, logLikelihoodForHessian, gr = gradientLCA, X = X,
                    ind.levels = ind.levels, weights = weights,
                    n.classes = n.classes, n.alternatives = n.alternatives,
                    n.parameters = n.parameters, method =  "BFGS",
                    control = list(fnscale  = -1, maxit = 0, trace = 0),
                    hessian = TRUE)
    h <- output$hessian
    sqrt(diag(solve(-h)))
}

gradientLCA <- function(p, X, ind.levels, weights, n.classes, n.alternatives, n.parameters)
{
    pars <- parameterVectorToList(p, n.classes, n.parameters)
    class.parameters <- pars$class.parameters
    class.sizes <- pars$class.sizes
    n.parameters <- nrow(class.parameters)
    n.classes <- length(class.sizes)
    n.levels <- length(ind.levels)
    parameter.ind <- n.classes:length(p)

    result <- rep(0, n.parameters * n.classes + n.classes - 1)

    if (n.classes > 1)
        size.pars <- log(class.sizes[2:n.classes] / class.sizes[1])

    for (i in 1:n.levels)
    {
        n.questions <- length(ind.levels[[i]])
        resp.X <- X[ind.levels[[i]], ]

        product.densities <- rep(NA, n.classes)
        gradient.inc <- rep(NA, n.classes * n.parameters)

        ind <- 1

        for (k in 1:n.classes)
        {
            exp.discriminants <- computeExpDiscriminants(resp.X,
                                                         class.parameters[, k],
                                                         n.alternatives)

            product.densities[k] <- computeProductDensity(exp.discriminants)

            for (j in 1:n.parameters)
            {
                share.deriv <- computeShareDerivative(resp.X,
                                                exp.discriminants, j,
                                                      n.parameters)
                gradient.inc[ind] <- class.sizes[k] * product.densities[k] *
                                     share.deriv
                ind <- ind + 1
            }
        }

        inverse.density <- computeInverseDensity(product.densities,
                                                 class.sizes)
        result[parameter.ind] <- result[parameter.ind] +
                                 weights[i] * inverse.density * gradient.inc

        # Class sizes
        if (n.classes > 1)
            for (k in 1:(n.classes - 1))
                result[k] <- result[k] + inverse.density * weights[i] *
                    computeSizeDerivative(product.densities, size.pars, k + 1)
    }
    result
}

computeProductDensity <- function(exp.discriminants)
{
    prod(exp.discriminants[, 1] / rowSums(exp.discriminants))
}

computeInverseDensity <- function(product.densities, class.sizes)
{
    1 / sum(class.sizes * product.densities)
}

computeSizeDerivative <- function(product.densities, size.pars, class.index)
{
    exp.pars <- exp(c(0, size.pars))
    sum.exp.pars <- sum(exp.pars)
    n.classes <- length(exp.pars)
    sgn <- rep(-1, n.classes)
    sgn[class.index] <- 1
    combined.densities <- sum(sgn * product.densities)
    exp.pars[class.index] * (sum.exp.pars - exp.pars[class.index]) *
        combined.densities / (sum.exp.pars ^ 2)
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

#' @title parameterStatisticsLCA
#' @description Produces a table of parameter statistics for LCA.
#' @param obj A FitChoice object from running LCA.
#' @return A table of parameter statistics (coefficients, standard errors,
#'     t-statistics and p-values).
#' @importFrom stats pt
#' @noRd
parameterStatisticsLCA <- function(obj)
{
    if (is.null(obj$lca.data))
        stop("A latent class analysis output is required.")

    pars <- obj$lca.data$pars

    X <- decompressNumericMatrix(obj$lca.data$compressed.X,
                                 dim = obj$lca.data$dim.X)
    ind.levels <- obj$lca.data$ind.levels
    weights <- obj$lca.data$weights
    parameter.names <- obj$lca.data$parameter.names
    parameter.scales <- obj$lca.data$parameter.scales
    n.classes <- obj$n.classes
    n.alternatives <- obj$n.alternatives
    n.parameters <- obj$n.parameters

    p <- parameterListToVector(pars)
    std.errors <- standardErrorsForLCA(p, X, ind.levels, weights,
                         n.classes, n.alternatives, n.parameters)
    t.stats <- p / std.errors
    p.values <- 2*pt(abs(t.stats), nrow(X) - length(p), lower.tail = FALSE)
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
        names(result) <- all.names
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

#' @title Memberships
#' @description Produces a vector of respondent class memberships based on
#'     the maximum posterior probability.
#' @param fit A FitChoice object from running LCA.
#' @return A vector containing respondent class memberships.
#' @export
Memberships <- function(fit)
{
    UseMethod("Memberships")
}

#' @export
Memberships.FitChoice <- function(fit)
{
    pp <- fit$posterior.probabilities
    .fun <- function(x)
    {
        if (any(is.na(x)))
            NA
        else
            match(max(x), x)[1]
    }
    apply(pp, 1, .fun)
}

#' @title ExtractClassParameters
#' @description Produces a matrix of parameters from LCA.
#' @param fit A FitChoice object from running LCA.
#' @return A matrix of parameters where each column corresponds to a class.
#'     If there is only one class, this is a vector of parameters.
#' @export
ExtractClassParameters <- function(fit)
{
    UseMethod("ExtractClassParameters")
}

#' @export
ExtractClassParameters.FitChoice <- function(fit)
{
    if (is.null(fit$lca.data))
        stop("The selected output was not from a Latent Class Analysis ",
             "Choice Model. Please select such an output before running this ",
             "script.")
    fit$coef
}

compressNumericMatrix <- function(mat)
{
    memCompress(as.character(mat), type = "gzip")
}

decompressNumericMatrix <- function(compressed.mat, dim)
{
    char.vector <- strsplit(memDecompress(compressed.mat, type = "gzip",
                                          asChar = TRUE), "\n")[[1]]
    matrix(as.numeric(char.vector), nrow = dim[1], ncol = dim[2])
}
