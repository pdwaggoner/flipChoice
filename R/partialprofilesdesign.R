#' Construct an Optimal Partial Profile Design for Discrete Choice Experiments
#'
#' Uses the integrated and extensive integrated algorithms as described in
#' D. P. Cuervo, R. Kessels, P. Goos and K. Sorensen (2016).
#'
#' @param levels.per.attribute A \emph{named} vector containing the
#'     number of levels for each attribute with names giving the
#'     attribute names.  See the Details.
#' @param prior Number vector or two-column matrix containing prior
#'     values for the model coefficients.  The vector length or number
#'     of rows in the matrix must correspond to the number of
#'     attributes/attribute levels specified in
#'     \code{levels.per.attribute}.  If \code{NULL}, the prior for the
#'     coefficients is assumed to be identically zero.  In the matrix
#'     case, the first column is taken as the prior mean for the
#'     coefficients and the second is taken to be the prior variances.
#'     If only one column is present, the prior for the coefficients
#'     is assumed to be centered at those values.
#' @param alternatives.per.question Numeric value indicating the
#'     number of profiles to show per question.
#' @param n.questions Numeric value specifying the total number of
#'     questions/tasks to be performed by each respondent.
#' @param n.constant.attributes The number of attributes to keep constant.
#' @param extensive Logical; whether to used the extensive algorithm instead of
#'     the integrated algorithm for partial profiles.
#' @param seed Integer value specifying the random seed to use for the
#'     algorithm.
#' @param n.rotations The number of random rotations performed when computing
#'     the Bayesian criterion when the prior mean and variance are supplied.
#' @return \itemize{
#'     \item \code{design} - A numeric matrix which contains an efficient
#'     design.
#'     \item \code{d.criterion} - Numeric value of the D_p criterion of the
#'     design as described in the paper.
#'     \item \code{const.attr.list} - List of numeric vectors
#'     of indices of the constant attributes in each question.
#' }
#' @references D. P. Cuervo, R. Kessels, P. Goos and K. Sorensen (2016).
#' An integrated algorithm for the optimal design of stated choice experiments
#' with partial profiles. Transportation Research Part B 93.
#' \url{http://antor.uantwerpen.be/wordpress/wp-content/papercite-data/pdf/
#' palhazicuervo2016integrated.pdf}
#' @examples
#' \dontrun{
#' result <- partialProfilesDesign(levels.per.attribute = c(2, 2, 2),
#'                                 prior = NULL,
#'                                 alternatives.per.question = 2,
#'                                 n.questions = 6,
#'                                 n.constant.attributes = 1,
#'                                 seed = 123)
#' result$design
#' }
partialProfilesDesign <- function(levels.per.attribute, prior = NULL,
                                  alternatives.per.question, n.questions,
                                  n.constant.attributes, extensive = FALSE,
                                  seed = 123, n.rotations = 10)
{
    n.attributes <- length(levels.per.attribute)
    if (is.null(names(levels.per.attribute)))
        names(levels.per.attribute) <- paste0("Att", 1:n.attributes)
    if (n.constant.attributes < 0 || n.constant.attributes >= n.attributes)
        stop("The number of constant attributes is invalid. It needs to be a ",
             "whole number from 0 to number of attributes - 1.")
    if (n.constant.attributes == 0 && extensive)
    {
        warning("The extensive algorithm cannot be applied when there are no ",
                "constant attributes. The integrated algorithm will be used ",
                "instead.")
        extensive <- FALSE
    }
    if (!is.null(prior))
    {
        if (!is.vector(prior))
        {
            quadrature.values <- computeQuadratureValues(nrow(prior), n.rotations,
                                                        seed, prior[, 1], prior[, 2])
            prior <- c(list(mean = prior[, 1]), quadrature.values)
        }
        else if (all(prior == 0))
            prior <- NULL
    }

    output <- partialProfilesRandomDesign(levels.per.attribute,
                                          alternatives.per.question,
                                          const.attr.list, n.questions,
                                          n.constant.attributes, seed)
    design <- output$design
    const.attr.list <- output$const.attr.list
    # pt <- proc.time()
    result <- if (extensive)
        extensiveAlgorithm(design, const.attr.list, prior, n.questions,
                           alternatives.per.question, levels.per.attribute)
    else
        integratedAlgorithm(design, const.attr.list, prior, n.questions,
                            alternatives.per.question, levels.per.attribute)
    # print(proc.time() - pt)
    result$design <- decorateDesign(result$design, n.questions,
                                    alternatives.per.question,
                                    levels.per.attribute)
    result
}

# See Algorithm 1 of Cuervo et al. (2016)
integratedAlgorithm <- function(design, const.attr.list, prior,
                                n.questions, alternatives.per.question,
                                levels.per.attribute)
{
    start.indices <- cumsum(c(0, levels.per.attribute - 1)) + 1
    d.zero <- computeDCriterion(design, prior, n.questions,
                                alternatives.per.question,
                                levels.per.attribute, start.indices)
    repeat
    {

        design.zero <- design
        for (question in 1:n.questions)
        {
            partial.info.matrix <- constructPartialInfoMatrix(design, prior,
                                                              n.questions,
                                                              question,
                                                    alternatives.per.question)
            question.ind <- (question - 1) * alternatives.per.question +
                            (1:alternatives.per.question)
            question.design <- design[question.ind, ]
            other.questions <- setdiff(1:nrow(design), question.ind)
            reduced.design <- design[other.questions, ]
            is.complete <- checkDesignHasCompleteLevels(reduced.design,
                                                        start.indices)
            missing.levels <- if (!is.complete)
                findMissingLevels(reduced.design, start.indices)
            else
                NULL
            output <- improveConstantAttributes(question.design,
                                                prior, const.attr.list[[question]],
                                                levels.per.attribute,
                                                alternatives.per.question,
                                                n.questions, start.indices,
                                                partial.info.matrix,
                                                is.complete, missing.levels)
            question.design <- output$question.design
            const.attr.list[[question]] <- output$const.attr

            question.design <- improveVaryingAttributes(question.design, prior,
                                               const.attr.list[[question]],
                                               levels.per.attribute,
                                               NULL,
                                               alternatives.per.question,
                                               n.questions, start.indices,
                                               partial.info.matrix,
                                               is.complete, missing.levels)
            design[question.ind, ] <- question.design
        }
        d.new <- computeDCriterionShortcut(question.design, prior,
                                           alternatives.per.question,
                                           start.indices, partial.info.matrix,
                                           is.complete, missing.levels)
        if (d.new <= d.zero)
            break
        else
            d.zero <- d.new
    }

    list(design = design.zero,
         d.criterion = d.zero,
         const.attr.list = const.attr.list)
}

# See Algorithm 2 of Cuervo et al. (2016)
improveVaryingAttributes <- function(question.design, prior,
                                     const.attr, levels.per.attribute,
                                     attributes.to.consider = NULL,
                                     alternatives.per.question,
                                     n.questions, start.indices,
                                     partial.info.matrix, is.complete,
                                     missing.levels)
{
    flag <- is.null(attributes.to.consider)
    n.attributes <- length(levels.per.attribute)
    if (is.null(attributes.to.consider))
        attributes.to.consider <-  1:n.attributes
    repeat
    {
        d.zero <- computeDCriterionShortcut(question.design, prior,
                                            alternatives.per.question,
                                            start.indices, partial.info.matrix,
                                            is.complete, missing.levels)
        question.design.zero <- question.design

        for (j in 1:alternatives.per.question)
        {
            for (f in attributes.to.consider)
            {
                if (!(f %in% const.attr))
                {
                    d.star <- computeDCriterionShortcut(question.design, prior,
                                                    alternatives.per.question,
                                                    start.indices,
                                                    partial.info.matrix,
                                                    is.complete,
                                                    missing.levels)

                    l.star <- getLevel(question.design, j, f, levels.per.attribute,
                                       start.indices)

                    for (l in 1:levels.per.attribute[f])
                    {
                        question.design <- setLevel(question.design, j, f, l,
                                           levels.per.attribute, start.indices)

                        d.new <- computeDCriterionShortcut(question.design,
                                                    prior,
                                                    alternatives.per.question,
                                                    start.indices,
                                                    partial.info.matrix,
                                                    is.complete,
                                                    missing.levels)
                        if (d.new > d.star)
                        {
                            d.star <- d.new
                            l.star <- l
                        }
                    }
                    question.design <- setLevel(question.design, j, f, l.star,
                                       levels.per.attribute, start.indices)
                }
            }
        }
        d.new <- computeDCriterionShortcut(question.design, prior,
                                           alternatives.per.question,
                                           start.indices,
                                           partial.info.matrix,
                                           is.complete, missing.levels)
        if (d.new <= d.zero)
            break
    }
    question.design.zero
}

# See Algorithm 3 of Cuervo et al. (2016)
improveConstantAttributes <- function(question.design, prior,
                                      const.attr,
                                      levels.per.attribute,
                                      alternatives.per.question,
                                      n.questions, start.indices,
                                      partial.info.matrix,
                                      is.complete, missing.levels)
{
    n.attributes <- length(levels.per.attribute)
    d.star <- computeDCriterionShortcut(question.design, prior,
                                        alternatives.per.question,
                                        start.indices,
                                        partial.info.matrix,
                                        is.complete, missing.levels)
    original.const.attr <- const.attr
    for (c.i in original.const.attr)
    {
        c.star <- c.i
        const.attr <- setdiff(const.attr, c.i)
        question.design <- improveVaryingAttributes(question.design, prior,
                                           const.attr,
                                           levels.per.attribute, c.i,
                                           alternatives.per.question,
                                           n.questions, start.indices,
                                           partial.info.matrix,
                                           is.complete, missing.levels)
        for (f in 1:n.attributes)
        {
            if (!(f %in% const.attr))
            {
                candidate.design <- question.design
                candidate.design <- setLevelAllRows(candidate.design, f, 1,
                                                    levels.per.attribute,
                                                    start.indices)

                d.new <- computeDCriterionShortcut(candidate.design, prior,
                                                   alternatives.per.question,
                                                   start.indices,
                                                   partial.info.matrix,
                                                   is.complete, missing.levels)
                if (d.new > d.star)
                {
                    d.star <- d.new
                    c.star <- f
                }
            }
        }
        const.attr <- sort(union(const.attr, c.star))
        question.design <- setLevelAllRows(question.design, c.star, 1,
                                           levels.per.attribute,
                                           start.indices)
    }
    list(const.attr = const.attr, question.design = question.design)
}

# See Algorithm 4 of Cuervo et al. (2016)
extensiveAlgorithm <- function(design, const.attr.list, prior,
                               n.questions, alternatives.per.question,
                               levels.per.attribute)
{
    start.indices <- cumsum(c(0, levels.per.attribute - 1)) + 1
    n.attributes <- length(levels.per.attribute)
    repeat
    {
        design.zero <- design
        d.zero <- computeDCriterion(design.zero, prior, n.questions,
                                    alternatives.per.question,
                                    levels.per.attribute, start.indices)
        for (question in 1:n.questions)
        {
            for (c.i in const.attr.list[[question]])
            {
                design.star <- design
                const.attr.list.star <- const.attr.list
                d.star <- computeDCriterion(design.star, prior, n.questions,
                                            alternatives.per.question,
                                            levels.per.attribute,
                                            start.indices)
                for (f in 1:n.attributes)
                {
                    if (!(f %in% const.attr.list[[question]]))
                    {
                        output <- extensiveAlgorithmInner(design, prior,
                                                  question, c.i, f,
                                                  const.attr.list,
                                                  alternatives.per.question,
                                                  levels.per.attribute,
                                                  n.questions,
                                                  start.indices)
                        d.dash <- computeDCriterion(output$design, prior,
                                                    n.questions,
                                                    alternatives.per.question,
                                                    levels.per.attribute,
                                                    start.indices)
                        if (d.dash > d.star)
                        {
                            design.star <- output$design
                            const.attr.list.star <- output$const.attr.list
                            d.star <- d.dash
                        }
                    }
                }
                design <- design.star
                const.attr.list <- const.attr.list.star
            }
        }
        if (d.star <= d.zero)
            break
    }
    list(design = design.zero,
         d.criterion = d.zero,
         const.attr.list = const.attr.list)
}

extensiveAlgorithmInner <- function(design.dash, prior, question, c.i,
                                    f, const.attr.list,
                                    alternatives.per.question,
                                    levels.per.attribute, n.questions,
                                    start.indices)
{
    const.attr.list.dash <- const.attr.list
    const.attr <- const.attr.list.dash[[question]]
    const.attr <- setdiff(const.attr, c.i)

    partial.info.matrix <- constructPartialInfoMatrix(design.dash, prior,
                                                      n.questions,
                                                      question,
                                                  alternatives.per.question)
    question.ind <- (question - 1) * alternatives.per.question +
                    (1:alternatives.per.question)
    question.design <- design.dash[question.ind, ]
    other.questions <- setdiff(1:nrow(design.dash), question.ind)
    reduced.design <- design.dash[other.questions, ]
    is.complete <- checkDesignHasCompleteLevels(reduced.design, start.indices)
    missing.levels <- if (!is.complete)
        findMissingLevels(reduced.design, start.indices)
    else
        NULL

    question.design <- improveVaryingAttributes(question.design, prior,
                                                const.attr,
                                                levels.per.attribute,
                                                c.i, alternatives.per.question,
                                                n.questions, start.indices,
                                                partial.info.matrix,
                                                is.complete, missing.levels)
    question.design <- setLevelAllRows(question.design, f, 1,
                                       levels.per.attribute, start.indices)
    design.dash[question.ind, ] <- question.design

    const.attr <- sort(union(const.attr, c.i))
    const.attr.list.dash[[question]] <- const.attr

    integratedAlgorithm(design.dash, const.attr.list.dash, prior, n.questions,
                        alternatives.per.question, levels.per.attribute)
}

computeDCriterion <- function(design, prior, n.questions,
                              alternatives.per.question,
                              levels.per.attribute,
                              start.indices)
{
    is.complete <- checkDesignHasCompleteLevels(design, start.indices)
    if (is.complete)
    {
        if (is.null(prior))
            d0Criterion(design, n.questions, alternatives.per.question)
        else if (is.numeric(prior))
            dPCriterion(design, prior, n.questions, alternatives.per.question)
        else
            bayesianCriterion(design, prior, n.questions,
                              alternatives.per.question)
    }
    else
        -Inf
}

computeDCriterionShortcut <- function(question.design, prior,
                                      alternatives.per.question,
                                      start.indices, partial.info.matrix,
                                      is.complete = FALSE,
                                      missing.levels = NULL)
{
    if (is.complete || checkDesignHasMissingLevels(question.design,
                                                   start.indices,
                                                   missing.levels))
    {
        if (is.null(prior))
            d0CriterionShortcut(question.design, partial.info.matrix,
                                alternatives.per.question)
        else if (is.numeric(prior))
            dPCriterionShortcut(question.design, prior, partial.info.matrix,
                                alternatives.per.question)
        else
            bayesianCriterionShortcut(question.design, prior,
                                      partial.info.matrix,
                                      alternatives.per.question)
    }
    else
        -Inf
}

constructPartialInfoMatrix <- function(design, prior, n.questions, question,
                                       alternatives.per.question)
{
    if (is.null(prior))
        d0PartialInfoMatrix(design, n.questions, question,
                            alternatives.per.question)
    else if (is.numeric(prior))
    {
        dPPartialInfoMatrix(design, prior, n.questions, question,
                            alternatives.per.question)
    }
    else
    {
        quadraturePartialInfoMatrices(design, prior, n.questions, question,
                                      alternatives.per.question)
    }
}

checkDesignHasCompleteLevels <- function(design, start.indices)
{
    if (any(colSums(design) == 0))
        FALSE
    else
    {
        n.attributes <- length(start.indices) - 1
        n.rows <- nrow(design)
        for (i in 1:n.attributes)
        {
            if (sum(design[, start.indices[i]:(start.indices[i + 1] - 1)]) == n.rows)
                return(FALSE)
        }
        TRUE
    }
}

checkDesignHasMissingLevels <- function(design, start.indices, missing.levels)
{
    for (i in 1:length(missing.levels))
    {
        if (!is.null(missing.levels[[i]]))
        {
            attribute.design <- design[, start.indices[i]:
                                        (start.indices[i + 1] - 1),
                                       drop = FALSE]
            attribute.design <- cbind(rowSums(attribute.design) == 0,
                                      attribute.design)
            if (!all(missing.levels[[i]] %in% which(colSums(attribute.design) > 0)))
                return(FALSE)
        }
    }
    TRUE
}

findMissingLevels <- function(design, start.indices)
{
    n.attributes <- length(start.indices) - 1
    result <- list()
    for (i in 1:n.attributes)
    {
        attribute.design <- design[, start.indices[i]:
                                    (start.indices[i + 1] - 1), drop = FALSE]
        attribute.design <- cbind(rowSums(attribute.design) == 0,
                                  attribute.design)
        missing.levels <- which(colSums(attribute.design) == 0)
        if (length(missing.levels) > 0)
            result[[i]] <- missing.levels
    }
    result
}

initializeConstantAttributesList <- function(n.questions, n.attributes,
                                             n.constant.attributes,
                                             seed = 123)
{
    set.seed(seed)
    lapply(as.list(rep(NA, n.questions)),
           function(x) sample(n.attributes, n.constant.attributes))
}

partialProfilesRandomDesign <- function(levels.per.attribute,
                                        alternatives.per.question,
                                        const.attr.list,
                                        n.questions,
                                        n.constant.attributes,
                                        seed = 123)
{
    start.indices <- cumsum(c(0, levels.per.attribute - 1)) + 1
    n.attributes <- length(levels.per.attribute)
    n.attempts <- 0
    repeat
    {
        const.attr.list <- initializeConstantAttributesList(n.questions,
                                                            n.attributes,
                                                    n.constant.attributes,
                                                            seed)
        design <- partialProfilesRandomDesignRaw(levels.per.attribute,
                                                 alternatives.per.question,
                                                 const.attr.list, seed)
        design <- encodeDesignFast(design, levels.per.attribute)
        is.complete <- checkDesignHasCompleteLevels(design, start.indices)
        if (is.complete)
        {
            criterion <- computeDCriterion(design, NULL, n.questions,
                                           alternatives.per.question,
                                           levels.per.attribute,
                                           start.indices)
            if (exp(criterion) > .Machine$double.eps)
                break
        }
        if (n.attempts > 1000)
            inputNotSensibleError()
        set.seed(seed)
        seed <- sample(1000, 1)
        n.attempts <- n.attempts + 1
    }
    list(design = design, const.attr.list = const.attr.list)
}

# Raw partial profiles random design, which may be invalid.
partialProfilesRandomDesignRaw <- function(levels.per.attribute,
                                           alternatives.per.question,
                                           const.attr.list, seed)
{
    set.seed(seed)
    n.questions <- length(const.attr.list)
    n.attributes <- length(levels.per.attribute)
    result <- matrix(NA, nrow = n.questions * alternatives.per.question,
                     ncol = length(levels.per.attribute))
    for (question in 1:n.questions)
    {
        ind <- (question - 1) * alternatives.per.question +
            (1:alternatives.per.question)
        for (i in 1:n.attributes)
        {
            if (i %in% const.attr.list[[question]])
                result[ind, i] <- 1
            else
                result[ind, i] <- sample(levels.per.attribute[[i]],
                                         alternatives.per.question,
                                         replace = TRUE)
        }
    }
    result
}

decorateDesign <- function(design, n.questions, alternatives.per.question,
                           levels.per.attribute)
{
    design <- decodeDesign(design, levels.per.attribute)
    design <- addAttributeNames(design, levels.per.attribute)
    design <- addQuestionAndAlternativeColumns(design, n.questions,
                                               alternatives.per.question)
    design
}

addQuestionAndAlternativeColumns <- function(design, n.questions,
                                             alternatives.per.question)
{
    design <- cbind(rep(1:n.questions, each = alternatives.per.question),
                    rep(1:alternatives.per.question, n.questions), design)
    colnames(design)[1] <- "Question"
    colnames(design)[2] <- "Alternative"
    design
}

addAttributeNames <- function(design, levels.per.attribute)
{
    if (length(levels.per.attribute) == 1)
        colnames(design) <- list(names(levels.per.attribute))
    else
        colnames(design) <- names(levels.per.attribute)
    design
}
