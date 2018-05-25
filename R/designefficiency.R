# Simple method as produced by AlgDesign package
#' @importFrom stats model.matrix
dScore <- function(design)
{
    attribute.columns <- data.frame(design[, c(-1, -2, -3)])
    attribute.columns <- data.frame(lapply(attribute.columns, as.factor))
    X <- model.matrix( ~ ., data = data.frame(attribute.columns))
    d.score <- det(crossprod(X)) ^ (1 / ncol(X)) / nrow(X)
    return(d.score)
}

#' Compute the D-error of an unlabeled design (according to Huber and Zwerina 1996)
#' @param design.matrix is a matrix for an unlabeled choice design in the long format, meaning that
#'   each row describes one of the alternatives in one of the choice tasks. Complete data for
#'   each task is spread across several rows. The columns are:
#'   - Column 1 indicates the version number for each profile
#   - Column 2 indicates the task number for each profile
#   - Column 3 indicates the alternative number
#   - Columns 4 and up each correspond to an attribute, with the entries in the columns indicating
#     the level of the attribute (beginning at level 1).
#' @param attribute.levels is a vector of numbers indicating how many levels are in each attribute. The order should
#   correspond to the order of columns in the design.
#' @param effects is a boolean parameter indicating whether or not the error should be computed based on
#   effects coding (TRUE) or dummy coding (FALSE).
#' @param prior is a vector of prior parameters for the attribute levels. Keeping prior = NULL uses a flat prior
#' @param n.rotations The number of random rotations performed when computing
#'     the Bayesian criterion/error when the prior mean and variance are supplied for
#'     partial profiles.
#' @param seed Integer; random seed to be used by the algorithms.
#' @references See https://faculty.fuqua.duke.edu/~jch8/bio/Papers/Huber%20Zwerina%201996%20Marketing%20Research.pdf
#' @export
DError <- function(design.matrix, attribute.levels, effects = TRUE,
                            prior = NULL, n.rotations = 10, seed = 123)
{
    if (!is.matrix(design.matrix))
        stop("The input design.matrix needs to be a matrix.")
    K <- sum(attribute.levels - 1) # Total number of parameters
    J <- max(design.matrix[, 3]) # Number of alts per task
    N <- nrow(design.matrix) / J # Number of tasks

    # Generate a coded version of the design using dummy coding or effects coding
    des.att <- design.matrix[, 3:ncol(design.matrix)] # Part of the design matrix containing the attributes
    coded.design <- encodeDesign(des.att, effects = effects)

    if (is.null(prior))
        d0Criterion(coded.design, N, J, FALSE) ^ (-1 / K)
    else if (is.vector(prior) && is.numeric(prior))
        dPCriterion(coded.design, prior, N, J, FALSE) ^ (-1 / K)
    else
        dBError(coded.design, prior, N, J, n.rotations, seed)
}

dPCriterion <- function(coded.design, prior, n.questions,
                        alternatives.per.question, compute.log = TRUE)
{
    # Generate choice probabilities of each alternative
    choice.probs <- logitChoiceProbs(coded.design, prior,
                                     alternatives.per.question,
                                     n.questions)

    xbars <- vector("numeric")
    for (s in 1L:n.questions)
    {
        question.indices <- (s - 1) * alternatives.per.question +
                            (1:alternatives.per.question)
        sums <- colSums(coded.design[question.indices, ] *
                        choice.probs[question.indices])
        xbars <- rbind(xbars, repRow(sums, alternatives.per.question))
    }
    Z <- as.matrix(coded.design - xbars)
    info.matrix <- crossprod(Z, choice.probs * Z)   # t(Z) %*% P %*% Z
    det.info.matrix <- max(det(info.matrix), 0)
    if (compute.log)
        log(det.info.matrix)
    else
        det.info.matrix
}

d0Criterion <- function(coded.design, n.questions, alternatives.per.question,
                        compute.log = TRUE)
{
    n.parameters <- ncol(coded.design)
    info.matrix <- matrix(0, nrow = n.parameters, ncol = n.parameters)

    for (s in 1:n.questions)
    {
        question.indices <- (s - 1) * alternatives.per.question +
                            (1:alternatives.per.question)
        xs <- coded.design[question.indices, ]

        info.matrix <- info.matrix + crossprod(xs) - tcrossprod(colSums(xs)) /
                       alternatives.per.question
    }
    info.matrix <- info.matrix / alternatives.per.question
    det.info.matrix <- max(det(info.matrix), 0)
    if (compute.log)
        log(det.info.matrix)
    else
        det.info.matrix
}

logsumexp <- function(x)
{
   xmax <- which.max(x)
   log1p(sum(exp(x[-xmax]-x[xmax])))+x[xmax]
}

softmax <- function(x)
    exp(x - logsumexp(x))

logitChoiceProbs <- function(coded.matrix, prior, number.alternatives, number.tasks)
{
    if (ncol(coded.matrix) != length(prior))
        stop("Number of columns in coded.matrix does not match the number of prior parameters")

    if (nrow(coded.matrix) != number.alternatives * number.tasks)
        stop("Number of rows in design does not match the number of alternatives and tasks")

    lp <- drop(coded.matrix%*%prior)

    unname(unlist(tapply(lp, rep(seq_len(number.tasks), each = number.alternatives),
                  softmax, simplify = FALSE)))
}


repRow = function(x, n) {
    # Returns a matrix with n rows where each row is a copy of x
    matrix(rep(x, each = n), nrow = n)
}

# Produce an encoded matrix without intercept
encodeDesign <- function(design, effects = TRUE) {

    old.contrasts <- options("contrasts")
    if (!"data.frame" %in% class(design))
    {
        design <- data.frame(design)
        design[colnames(design)] <- lapply(design[colnames(design)], factor)
    }

    if (effects)
        options(contrasts = c("contr.sum", "contr.poly"))
    else
        options(contrasts = c("contr.treatment", "contr.poly"))

    dummy.matrix <- model.matrix( ~ ., data = design)
    options(contrasts = old.contrasts[[1]])
    return(dummy.matrix[, -1])
}

# This should be merged into encodeDesign
encodeDesignFast <- function(design, levels.per.attribute)
{
    n.attributes <- length(levels.per.attribute)
    n.parameters <- sum(levels.per.attribute)
    n.rows <- nrow(design)
    design.vector <- as.vector(design)
    offset <- rep(1:n.rows, n.attributes) +
        rep(cumsum(c(0, levels.per.attribute[-n.attributes]) * n.rows),
                  each = n.rows)
    encoded.vector <- rep(0, n.rows * n.parameters)
    encoded.vector[(design.vector - 1) * n.rows + offset] <- 1
    encoded.vector <- encoded.vector[-offset]
    matrix(encoded.vector, nrow = n.rows)
}

decodeDesign <- function(design, levels.per.attribute)
{
    n.rows <- nrow(design)
    n.attributes <- length(levels.per.attribute)
    cumulative.levels <- cumsum(c(0, levels.per.attribute - 1))
    result <- matrix(NA, nrow = n.rows, ncol = n.attributes)
    for (i in 1:n.attributes)
    {
        ind.start <- cumulative.levels[i] + 1
        ind.end <- cumulative.levels[i + 1]
        for (j in 1:n.rows)
        {
            lvl <- which(design[j, ind.start:ind.end] == 1)
            if (length(lvl) == 0)
                result[j, i] <- 1
            else
                result[j, i] <- lvl + 1
        }
    }
    result
}

