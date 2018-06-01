# Alternative-specific designs either random or Federov, based on a subsample of the enumeration.
#' @importFrom AlgDesign optFederov gen.factorial
alternativeSpecificDesign <- function(design.algorithm = "Alternative specific - Federov",
                                      attribute.levels,
                                      n.questions,
                                      n.versions = 1,
                                      prohibitions = NULL,
                                      iterations = 100,        # ignored if random
                                      max.subsample = 1e8,     # ignored if random
                                      seed = 12345) {

    set.seed(seed)

    alternatives.per.question <- length(attribute.levels)
    n.attributes.per.alternative <- sapply(attribute.levels, length)
    n.levels.per.alternative <- sapply(sapply(attribute.levels, unlist), length)
    n.levels.per.attribute <- sapply(attribute.levels, function(x) sapply(x, length))
    levels.per.attribute <- unlist(n.levels.per.attribute)

    if (design.algorithm == "Alternative specific - Random")
    {
        design <- randomAlternativeSpecific(levels.per.attribute, n.questions * n.versions)
    }
    else
    {
        n.enumerated.questions <- prod(levels.per.attribute)
        #print(paste(n.enumerated.questions, "rows in full factorial design."))
        n.sample <- min(max.subsample, n.enumerated.questions)

        # Generate a pool of n.sample questions, from which to create the optimal design.
        # If we need (almost) the whole sample or the enumeration is small enough, it is faster to
        # create a pool of the full enumeration. Else generate the pool of questions randomly.
        if (max.subsample > 0.5 * n.enumerated.questions || n.enumerated.questions < 1e7)
        {
            asymmetric.full.factorial <- gen.factorial(levels.per.attribute, factors = "all")
            factorial.sample <- asymmetric.full.factorial[sample(n.enumerated.questions, n.sample), ]
        }
        else
        {
            factorial.sample <- randomAlternativeSpecific(levels.per.attribute, n.sample)
        }
        # Do not add versions variable, since each version must contain the same number of rows and
        # this is not necessarily the case, especially when the design contains many attributes.
        design <- fedDesign(factorial.sample, n.questions * n.versions, iterations)$design
    }

    if (nrow(design) < sum(levels.per.attribute))
        warning("Number of questions in the design is less than the total number of levels.",
                " It is preferable to more levels or versions, or fewer attributes.")

    # Recalculate d.error with version blocking
    versions.col <- as.factor(rep(seq(n.versions), each = n.questions))
    d.error <- fedDesign(cbind(design, versions.col), nrow(design))$D

    # Standardize format with ChoiceModelDesign
    design <- splitAlternativesByRow(design, alternatives.per.question, n.attributes.per.alternative)
    colnames(design) <- names(unlist(n.levels.per.attribute))
    design <- cbind(#Version = versions.col,
                    #Task = rep(seq(n.questions * n.versions), each = alternatives.per.question),
                    Question = rep(rep(seq(n.questions), each = alternatives.per.question), n.versions),
                    Alternative = rep(seq(alternatives.per.question), length.out = nrow(design)),
                    design)

    result <- list(design = data.matrix(design), d.error = 1 / d.error)

    return(result)
}


# Wrapper for optFederov with error handling
fedDesign <- function(factorial.sample, n.rows, iterations = 100) {
    tryCatch ({
        fed <- optFederov( ~ ., factorial.sample,
                           n.rows,
                           maxIteration = iterations,
                           nRepeats = 1)
        }, error = function(e) list(D = Inf))
}

# Generate a random design
randomAlternativeSpecific <- function(levels.per.attribute, n) {

    design <- matrix(0, nrow = n, ncol = length(levels.per.attribute))
    dups <- rep(TRUE, n)

    while (any(dups))
    {
        for (i in which(dups))
            design[i, ] <- sapply(levels.per.attribute, sample, 1)
        dups <- duplicated(design)
    }

    design <- data.frame(design)
    design[] <- lapply(design, factor)
    design
}

# Convert from one row per question to one row per alternative per question.
# Attriibutes not applicable to an alternative are set to NA.
splitAlternativesByRow <- function(design, alternatives.per.question, n.attributes.per.alternative) {

    # Replicate each row by alternatives.per.question
    design <- design[rep(rownames(design), each = alternatives.per.question), ]

    cols.per.alternative <- unlist(mapply(rep, seq(alternatives.per.question), n.attributes.per.alternative))
    for (i in seq(nrow(design)))
        design[i, ((i - 1) %% alternatives.per.question) + 1 != cols.per.alternative] <- NA

    return(design)
}







