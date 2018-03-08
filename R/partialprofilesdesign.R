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
#' @param seed Integer value specifying the random seed to use for the
#'     algorithm.
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
                                  seed = 123)
{
    n.attributes <- length(levels.per.attribute)
    if (is.null(names(levels.per.attribute)))
        names(levels.per.attribute) <- paste0("Att", 1:n.attributes)
    if (n.constant.attributes < 0 || n.constant.attributes >= n.attributes)
        stop("The number of constant attributes is invalid. It needs to be a ",
             "whole number from 0 to number of attributes - 1.")
    const.attr.list <- initializeConstantAttributesList(n.questions,
                                                              n.attributes,
                                                      n.constant.attributes,
                                                              seed)
    design <- partialProfilesRandomDesign(levels.per.attribute,
                                          alternatives.per.question,
                                          const.attr.list,
                                          n.questions, seed)
    result <- if (extensive)
        extensiveAlgorithm(design, const.attr.list, prior, n.questions,
                           alternatives.per.question, levels.per.attribute)
    else
        integratedAlgorithm(design, const.attr.list, prior, n.questions,
                            alternatives.per.question, levels.per.attribute)

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
    repeat
    {
        d.zero <- computeDCriterion(design, prior, n.questions,
                                    alternatives.per.question,
                                    levels.per.attribute)
        design.zero <- design
        for (question in 1:n.questions)
        {
            output <- improveConstantAttributes(design, prior,
                                            question,
                                            const.attr.list[[question]],
                                            levels.per.attribute,
                                            alternatives.per.question,
                                            n.questions)
            design <- output$design
            const.attr.list[[question]] <- output$const.attr

            design <- improveVaryingAttributes(design, prior, question,
                                           const.attr.list[[question]],
                                           levels.per.attribute,
                                           NULL,
                                           alternatives.per.question,
                                           n.questions)
        }
        d.new <- computeDCriterion(design, prior, n.questions,
                                   alternatives.per.question,
                                   levels.per.attribute)
        if (d.new <= d.zero)
            break
    }

    list(design = design.zero,
         d.criterion = d.zero,
         const.attr.list = const.attr.list)
}

# See Algorithm 2 of Cuervo et al. (2016)
improveVaryingAttributes <- function(design, prior, question, const.attr,
                                     levels.per.attribute,
                                     attributes.to.consider = NULL,
                                     alternatives.per.question,
                                     n.questions)
{
    n.attributes <- length(levels.per.attribute)
    if (is.null(attributes.to.consider))
        attributes.to.consider <-  1:n.attributes

    repeat
    {
        d.zero <- computeDCriterion(design, prior, n.questions,
                                    alternatives.per.question,
                                    levels.per.attribute)
        for (j in 1:alternatives.per.question)
        {
            for (f in attributes.to.consider)
            {
                if (!(f %in% const.attr))
                {
                    d.star <- computeDCriterion(design, prior, n.questions,
                                                alternatives.per.question,
                                                levels.per.attribute)
                    ind <- (question - 1) * alternatives.per.question + j
                    l.star <- design[ind, f]
                    for (l in 1:levels.per.attribute[f])
                    {
                        design[ind, f] <- l
                        d.new <- computeDCriterion(design, prior, n.questions,
                                                   alternatives.per.question,
                                                   levels.per.attribute)
                        if (d.new > d.star)
                        {
                            d.star <- d.new
                            l.star <- l
                        }
                    }
                    design[ind, f] <- l.star
                }
            }
        }
        d.new <- computeDCriterion(design, prior, n.questions,
                                   alternatives.per.question,
                                   levels.per.attribute)
        if (d.new <= d.zero)
            break
    }
    design
}

# See Algorithm 3 of Cuervo et al. (2016)
improveConstantAttributes <- function(design, prior, question,
                                      const.attr,
                                      levels.per.attribute,
                                      alternatives.per.question,
                                      n.questions)
{
    n.attributes <- length(levels.per.attribute)
    d.star <- computeDCriterion(design, prior, n.questions,
                                alternatives.per.question,
                                levels.per.attribute)
    original.const.attr <- const.attr
    ind <- (question - 1) * alternatives.per.question +
            (1:alternatives.per.question)
    for (c.i in original.const.attr)
    {
        c.star <- c.i
        const.attr <- setdiff(const.attr, c.i)
        design <- improveVaryingAttributes(design, prior, question,
                                           const.attr,
                                           levels.per.attribute, c.i,
                                           alternatives.per.question,
                                           n.questions)
        for (f in 1:n.attributes)
        {
            if (!(f %in% const.attr))
            {
                candidate.design <- design
                candidate.design[ind, f] <- 1
                d.new <- computeDCriterion(candidate.design, prior,
                                           n.questions,
                                           alternatives.per.question,
                                           levels.per.attribute)
                if (d.new > d.star)
                {
                    d.star <- d.new
                    c.star <- f
                }
            }
        }
        const.attr <- sort(union(const.attr, c.star))
        design[ind, c.star] <- 1
    }
    list(const.attr = const.attr, design = design)
}

# See Algorithm 4 of Cuervo et al. (2016)
extensiveAlgorithm <- function(design, const.attr.list, prior,
                                n.questions, alternatives.per.question,
                                levels.per.attribute)
{
    n.attributes <- length(levels.per.attribute)
    repeat
    {
        d.zero <- computeDCriterion(design, prior, n.questions,
                                    alternatives.per.question,
                                    levels.per.attribute)
        design.zero <- design
        for (question in 1:n.questions)
        {
            for (c.i in const.attr.list[[question]])
            {
                design.star <- design
                const.attr.list.star <- const.attr.list

                for (f in 1:n.attributes)
                {
                    if (!(f %in% const.attr.list[[question]]))
                    {
                        output <- extensiveAlgorithmInner(design, design.star,
                                                    prior, question, c.i, f,
                                                    const.attr.list,
                                                    const.attr.list.star,
                                                    alternatives.per.question,
                                                    levels.per.attribute,
                                                    n.questions)
                        design.star <- output$design.star
                        const.attr.list.star <- output$const.attr.list.star
                    }
                }
                design <- design.star
                const.attr.list <- const.attr.list.star
            }
        }
        d.new <- computeDCriterion(design, prior, n.questions,
                                   alternatives.per.question,
                                   levels.per.attribute)
        print(d.new)
        if (d.new <= d.zero)
            break
    }
    list(design = design.zero,
         d.criterion = d.zero,
         const.attr.list = const.attr.list)
}

extensiveAlgorithmInner <- function(design, design.star, prior, question, c.i, f,
                                    const.attr.list, const.attr.list.star,
                                    alternatives.per.question,
                                    levels.per.attribute, n.questions)
{
    design.dash <- design
    const.attr.list.dash <- const.attr.list
    const.attr <- const.attr.list.dash[[question]]
    const.attr <- setdiff(const.attr, c.i)
    design.dash <- improveVaryingAttributes(design.dash, prior, question,
                                            const.attr, levels.per.attribute,
                                            c.i, alternatives.per.question,
                                            n.questions)
    const.attr <- sort(union(const.attr, c.i))
    const.attr.list.dash[[question]] <- const.attr
    ind <- (question - 1) * alternatives.per.question + (1:alternatives.per.question)
    design.dash[ind, f] <- 1
    output <- integratedAlgorithm(design.dash, const.attr.list.dash, prior,
                                  n.questions, alternatives.per.question,
                                  levels.per.attribute)
    design.dash <- output$design
    const.attr.list.dash <- output$const.attr.list

    d.dash <- computeDCriterion(design.dash, prior, n.questions,
                                alternatives.per.question,
                                levels.per.attribute)
    d.star <- computeDCriterion(design.star, prior, n.questions,
                                alternatives.per.question,
                                levels.per.attribute)
    if (d.dash > d.star)
    {
        design.star <- design.dash
        const.attr.list.star <- const.attr.list.dash
    }
    list(design.star = design.star,
         const.attr.list.star = const.attr.list.star)
}

computeDCriterion <- function(design, prior, n.questions,
                              alternatives.per.question,
                              levels.per.attribute)
{
    is.complete <- checkDesignHasCompleteLevels(design, levels.per.attribute)
    if (is.complete)
    {
        coded.design <- encode.design(design, effects = FALSE)
        if (is.null(prior))
            d0Criterion(coded.design, n.questions,
                        alternatives.per.question)
        else
        {
            # to be done
        }
    }
    else
        -Inf
}

checkDesignHasCompleteLevels <- function(design, levels.per.attribute)
{
    is.complete <- TRUE
    n.attributes <- ncol(design)
    for (i in 1:n.attributes)
    {
        if (length(unique(design[, i])) < levels.per.attribute[i])
        {
            is.complete <- FALSE
            break
        }
    }
    is.complete
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
                                        n.questions, seed = 123)
{
    repeat
    {
        design <- partialProfilesRandomDesignRaw(levels.per.attribute,
                                                 alternatives.per.question,
                                                 const.attr.list,
                                                 seed)
        is.complete <- checkDesignHasCompleteLevels(design,
                                            levels.per.attribute)
        if (is.complete)
        {
            criterion <- computeDCriterion(design, NULL, n.questions,
                                           alternatives.per.question,
                                           levels.per.attribute)
            if (criterion > 0)
                break
        }
    }
    design
}

# Raw partial profiles random design, which may be invalid.
partialProfilesRandomDesignRaw <- function(levels.per.attribute,
                                           alternatives.per.question,
                                           const.attr.list,
                                           seed = 123)
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
