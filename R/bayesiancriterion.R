# Based on C.M. Gotwalt, B.A. Jones, D.M. Steinberg. Fast Computation of
# Designs Robust to Parameter Uncertainty for Nonlinear Settings.
# Technometrics, 51(1):88-95, 2009.

simplexWeight <- function(p)
{
    p * (7 - p) / (2 * (p + 1) ^ 2 * (p + 2))
}

midpointWeight <- function(p)
{
    2 * (p - 1) ^ 2 / (p * (p + 1) ^ 2 * (p + 2))
}

simplexVertices <- function(p)
{
    vertices <- matrix(NA, nrow = p, ncol = p + 1)
    for (j in 1:(p + 1))
    {
        for (i in 1:p)
        {
            if (j < i)
                vertices[i, j] <- 0
            else if (j == i)
                vertices[i, j] <- sqrt((p + 1) * (p - i + 1) / (p * (p - i + 2)))
            else # j > i
                vertices[i, j] <- -sqrt((p + 1) / ((p - i + 1) * p * (p - i + 2)))
        }
    }
    vertices
}

computeMidpoints <- function(vertices)
{
    p <- nrow(vertices)
    midpoints <- matrix(NA, nrow = p, ncol = 0.5 * p * (p + 1))
    counter <- 1
    for (i in 1:p)
    {
        for (j in (i + 1):(p + 1))
        {
            midpoints[, counter] <- 0.5 * (vertices[, i] + vertices[, j])
            counter <- counter + 1
        }
    }
    if (dim(midpoints) == c(1, 1) && midpoints == 0)
        midpoints
    else
        t(t(midpoints) / sqrt(colSums(midpoints ^ 2)))
}

extendedSimplexWeights <- function(p)
{
    c(rep(simplexWeight(p), 2 * p + 2),
      rep(midpointWeight(p), p * (p + 1)))
}

extendedSimplexAbscissas <- function(p)
{
    vertices <- simplexVertices(p)
    midpoints <- computeMidpoints(vertices)
    cbind(vertices, -vertices, midpoints, -midpoints)
}

radialWeightZero <- function(p)
{
    8 / ((p + 2) * (p + 4))
}

radialWeights <- function(p)
{
    c(p * (p + 2) / ((p + 4) * (2 - sqrt(2 * p + 8)) ^ 2),
      p * (p + 2) / ((p + 4) * (2 + sqrt(2 * p + 8)) ^ 2))
}

radialAbscissas <- function(p)
{
    c(sqrt(p + 4 - sqrt(2 * p + 8)),
      sqrt(p + 4 + sqrt(2 * p + 8)))
}

#' @importFrom pracma randortho
computeQuadratureValues <- function(mean.vector, sd.vector, n.rotations, seed)
{
    nonzero.variation <- sd.vector > 0
    p <- sum(nonzero.variation)

    radial.abscissas <- radialAbscissas(p)
    extended.simplex.abscissas <- extendedSimplexAbscissas(p)
    n.extended.simplex <- ncol(extended.simplex.abscissas)

    set.seed(seed)
    abscissas.vectors <- vector(2 * n.rotations * n.extended.simplex, mode = "list")
    ind <- 1
    for (i in 1:2)
        for (j in 1:n.rotations)
        {
            q <- randortho(p)
            for (k in 1:n.extended.simplex)
            {
                v <- mean.vector
                v[nonzero.variation] <- mean.vector[nonzero.variation] +
                                        sd.vector[nonzero.variation] *
                                        (radial.abscissas[i] *
                                       (q %*% extended.simplex.abscissas[, k]))
                abscissas.vectors[[ind]] <- v
                ind <- ind + 1
            }
        }

    list(radial.weight.zero = radialWeightZero(p),
         radial.weights = radialWeights(p),
         extended.simplex.weights = extendedSimplexWeights(p),
         abscissas.vectors = abscissas.vectors,
         n.rotations = n.rotations,
         n.extended.simplex = n.extended.simplex)
}

bayesianCriterion <- function(design, prior, n.questions,
                              alternatives.per.question)
{
    n.rotations <- prior$n.rotations
    n.extended.simplex <- prior$n.extended.simplex
    result <- prior$radial.weight.zero *
              dPCriterion(design, prior$mean, n.questions,
                          alternatives.per.question)
    ind <- 1
    for (i in 1:2)
        for (j in 1:n.rotations)
            for (k in 1:n.extended.simplex)
            {
                if (is.infinite(result))
                    return(-Inf)
                result <- result + prior$radial.weights[i] *
                          prior$extended.simplex.weights[k] *
                          dPCriterion(design, prior$abscissas.vectors[[ind]],
                                      n.questions,
                                      alternatives.per.question) / n.rotations
                ind <- ind + 1
            }
    result
}

bayesianError <- function(design, prior, n.questions,
                          alternatives.per.question)
{
    K <- length(prior$mean)
    exp(bayesianCriterion(design, prior, n.questions,
                          alternatives.per.question)) ^ (-1 / K)
}

quadraturePartialInfoMatrices <- function(design, prior, n.questions, question,
                                          alternatives.per.question)
{
    n.rotations <- prior$n.rotations
    n.extended.simplex <- prior$n.extended.simplex
    p <- length(prior$mean)
    radius.zero <- dPPartialInfoMatrix(design, prior$mean, n.questions,
                                       question, alternatives.per.question)
    abscissas <- vector(2 * n.rotations * n.extended.simplex, mode = "list")
    ind <- 1
    for (i in 1:2)
        for (j in 1:n.rotations)
            for (k in 1:n.extended.simplex)
            {
                abscissas[[ind]] <- dPPartialInfoMatrix(design,
                                          prior$abscissas.vectors[[ind]],
                                          n.questions, question,
                                          alternatives.per.question)
                ind <- ind + 1
            }
    list(radius.zero = radius.zero,
         abscissas = abscissas)
}

bayesianCriterionShortcut <- function(question.design, prior,
                                      partial.info.matrices,
                                      alternatives.per.question)
{
    prior.mean <- prior$mean
    n.rotations <- prior$n.rotations
    n.extended.simplex <- prior$n.extended.simplex
    result <- prior$radial.weight.zero *
              dPCriterionShortcut(question.design, prior.mean,
                                  partial.info.matrices$radius.zero,
                                  alternatives.per.question)
    ind <- 1
    for (i in 1:2)
        for (j in 1:n.rotations)
            for (k in 1:n.extended.simplex)
            {
                if (is.infinite(result))
                    return(-Inf)
                result <- result + prior$radial.weights[i] *
                    prior$extended.simplex.weights[k] *
                    dPCriterionShortcut(question.design,
                                    prior$abscissas.vectors[[ind]],
                                    partial.info.matrices$abscissas[[ind]],
                                    alternatives.per.question) /
                                    n.rotations
                ind <- ind + 1
            }
    result
}

monteCarloBayesianCriterion <- function(design, prior, n.questions,
                                        alternatives.per.question, n.draws, seed)
{
    n.parameters <- nrow(prior)
    means <- prior[, 1]
    sds <- prior[, 2]
    set.seed(seed)
    draws <- matrix(rnorm(n.draws * n.parameters) * sds + means,
                    ncol = n.draws)

    criterions <- rep(NA, n.draws)
    for (i in 1:n.draws)
    {
        criterions[i] <- dPCriterion(design, draws[, i], n.questions,
                                     alternatives.per.question)
    }
    mean(criterions)
}

monteCarloBayesianError <- function(design, prior, n.questions,
                                        alternatives.per.question, n.draws, seed)
{
    n.parameters <- nrow(prior)
    means <- prior[, 1]
    sds <- prior[, 2]
    K <- nrow(prior)
    set.seed(seed)
    draws <- matrix(rnorm(n.draws * n.parameters) * sds + means,
                    ncol = n.draws)

    errors <- rep(NA, n.draws)
    for (i in 1:n.draws)
    {
        errors[i] <- exp(dPCriterion(design, draws[, i], n.questions,
                                     alternatives.per.question))^ (-1 / K)
    }
    errors
}

quadratureBayesianCriterion <- function(design, prior, n.questions,
                                        alternatives.per.question, n.rotations,
                                        seed)
{
    quadrature.values <- computeQuadratureValues(prior[, 1], prior[, 2],
                                                 n.rotations, seed)
    prior <- c(list(mean = prior[, 1]), quadrature.values)
    bayesianCriterion(design, prior, n.questions,
                      alternatives.per.question)
}

dBError <- function(design, prior, n.questions, alternatives.per.question,
                    n.rotations, seed)
{
    quadrature.values <- computeQuadratureValues(prior[, 1], prior[, 2],
                                                 n.rotations, seed)
    prior <- c(list(mean = prior[, 1]), quadrature.values)
    bayesianError(design, prior, n.questions, alternatives.per.question)
}
