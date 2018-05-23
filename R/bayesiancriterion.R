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
computeQuadratureValues <- function(p, n.rotations, seed, mean.vector, sd)
{
    radial.abscissas <- radialAbscissas(p)
    extended.simplex.abscissas <- extendedSimplexAbscissas(p)
    n.extended.simplex <- ncol(extended.simplex.abscissas)

    set.seed(seed)
    abscissas.vectors <- array(dim = c(2, n.rotations, n.extended.simplex, p))
    for (i in 1:2)
        for (j in 1:n.rotations)
        {
            q <- randortho(p)
            for (k in 1:n.extended.simplex)
                abscissas.vectors[i, j, k, ] <- mean.vector + sd *
                                            (radial.abscissas[i] *
                                            (q %*%
                                            extended.simplex.abscissas[, k]))
        }

    list(radial.weight.zero = radialWeightZero(p),
         radial.weights = radialWeights(p),
         extended.simplex.weights = extendedSimplexWeights(p),
         abscissas.vectors = abscissas.vectors)
}

bayesianCriterion <- function(design, prior, n.questions,
                              alternatives.per.question)
{
    dims <- dim(prior$abscissas.vectors)
    n.rotations <- dims[2]
    n.extended.simplex <- dims[3]
    result <- prior$radial.weight.zero *
              dPCriterion(design, prior$mean, n.questions,
                          alternatives.per.question)
    for (i in 1:2)
        for (j in 1:n.rotations)
            for (k in 1:n.extended.simplex)
            {
                if (is.infinite(result))
                    return(-Inf)
                result <- result + prior$radial.weights[i] *
                          prior$extended.simplex.weights[k] *
                          dPCriterion(design, prior$abscissas.vectors[i, j, k, ],
                                      n.questions,
                                      alternatives.per.question) / n.rotations
            }
    result
}

quadraturePartialInfoMatrices <- function(design, prior, n.questions, question,
                                          alternatives.per.question)
{
    dims <- dim(prior$abscissas.vectors)
    n.rotations <- dims[2]
    n.extended.simplex <- dims[3]
    p <- length(prior$mean)
    radius.zero <- dPPartialInfoMatrix(design, prior$mean, n.questions,
                                       question, alternatives.per.question)
    abscissas <- array(dim = c(2, n.rotations, n.extended.simplex, p, p))
    for (i in 1:2)
        for (j in 1:n.rotations)
            for (k in 1:n.extended.simplex)
                abscissas[i, j, k, , ] <- dPPartialInfoMatrix(design,
                                          prior$abscissas.vectors[i, j, k, ],
                                          n.questions, question,
                                          alternatives.per.question)
    list(radius.zero = radius.zero,
         abscissas = abscissas)
}

bayesianCriterionShortcut <- function(question.design, prior,
                                      partial.info.matrices,
                                      alternatives.per.question)
{
    prior.mean <- prior$mean
    dims <- dim(prior$abscissas.vectors)
    n.rotations <- dims[2]
    n.extended.simplex <- dims[3]
    result <- prior$radial.weight.zero *
              dPCriterionShortcut(question.design, prior.mean,
                                  partial.info.matrices$radius.zero,
                                  alternatives.per.question)
    for (i in 1:2)
        for (j in 1:n.rotations)
            for (k in 1:n.extended.simplex)
            {
                if (is.infinite(result))
                    return(-Inf)
                result <- result + prior$radial.weights[i] *
                    prior$extended.simplex.weights[k] *
                    dPCriterionShortcut(question.design,
                                    prior$abscissas.vectors[i, j, k, ],
                                    partial.info.matrices$abscissas[i, j, k, , ],
                                    alternatives.per.question) /
                                    n.rotations
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

quadratureBayesianCriterion <- function(design, prior, n.questions,
                                        alternatives.per.question, n.rotations,
                                        seed)
{
    quadrature.values <- computeQuadratureValues(nrow(prior), n.rotations,
                                                 seed, prior[, 1], prior[, 2])
    prior <- c(list(mean = prior[, 1]), quadrature.values)
    bayesianCriterion(design, prior, n.questions,
                      alternatives.per.question)
}
