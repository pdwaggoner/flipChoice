#' Choice modeling experimental design
#'
#' Creates choice model experimental designs according to a given algorithm.
#'
#' @param design.algorithm The algorithm used to create the
#'     design. One of \code{"Random"}, \code{"Shortcut"},
#'     \code{"Balanced overlap"}, \code{"Complete enumeration"},
#'     \code{"Efficient"}, \code{Partial profiles},
#'     \code{"Alternative specific - Random"} and
#'     \code{"Alternative specific - Federov"}.
#' @param attribute.levels \code{\link{list}} of \code{\link{vector}}s
#'     containing the labels of levels for each attribute, with names
#'     corresponding to the attribute labels; \emph{or} a character
#'     matrix with first row containing attribute names and subsequent
#'     rows containing attribute levels.
#' @param prior Matrix containing prior values for the model
#'     coefficients; only used for \code{design.algorithm ==
#'     "Efficient"} and \code{design.algorithm == "Partial profiles"};
#'     see Details.
#' @param n.questions Integer; the number of questions asked to each
#'     respondent.
#' @param n.versions Integer; the number of versions of the survey to
#'     create.
#' @param alternatives.per.question Integer; the number of alternative
#'     products shown in each question. Ignored if
#'     \code{"labeled.alternatives"} is TRUE.
#' @param prohibitions Character \code{\link{matrix}} where each row
#'     is a prohibited alternative consisting of the levels of each
#'     attribute. If a level is \code{""} or is \code{"All"} then all
#'     levels of that attribute in combination with the other
#'     specified attribute levels are prohibited.
#' @param none.alternatives Integer; the number of 'None' in all
#'     questions. Not required if \code{none.positions} is supplied.
#' @param none.positions Integer \code{\link{vector}} specifying the indices
#'     of the 'None' alternatives in each question. If not specified, 'None'
#'     will be the last alternative(s). A comma-delimited string of integers
#'     may be supplied instead of Integer \code{\link{vector}}.
#' @param labeled.alternatives Logical; whether the first attribute
#'     labels the alternatives.
#' @param n.constant.attributes Integer; the number of attributes to keep
#'     constant for partial profiles.
#' @param extensive Logical; whether to used the extensive algorithm instead of
#'     the integrated algorithm for partial profiles.
#' @param n.rotations The number of random rotations performed when computing
#'     the Bayesian criterion/error when the prior mean and variance are supplied for
#'     partial profiles.
#' @param max.subsample The maximum number of questions from a fully enumerated design
#'     to consider when \code{design.algorithm == "Alternative specific - Federov"}.
#' @param output One of \code{"Labeled design"} or \code{"Inputs"}.
#' @param seed Integer; random seed to be used by the algorithms.
#' @return A list with components
#' \itemize{
#' \item \code{design} - a numeric matrix with number of rows equal to number of
#' versions times number of questions times number of alternatives per question,
#' with columns for version number, question number, task number, alternative number,
#' and numeric level for each attribute.
#' \item \code{design.with.none} - as per \code{design} except one additional row per none alternative
#' is added to each question with \code{NA} for all attribute levels.
#' \item \code{labeled.design} - as per \code{design.with.none} except indices of levels are
#' replaced by their labels.
#' \item \code{design.algorithm} - as per input.
#' \item \code{attribute.levels} - as per input.
#' \item \code{prohibitions} - as per input.
#' \item \code{n.questions} - as per input.
#' \item \code{n.versions} - as per input.
#' \item \code{alternatives.per.question} - as per input.
#' \item \code{none.alternatives} - as per input.
#' \item \code{labeled.alternatives} - as per input.
#' \item \code{output} - as per input.
#' \item \code{db.error} - the Db-error of \code{design}.
#' \item \code{d.error} - the D-error of \code{design}.
#' \item \code{d.criterion} - the D-optimality criterion when the partial
#' profiles algorithm is used.
#' \item \code{model.matrix} - the model matrix of dummy coded variables for each alternative
#' in every choice set.
#' \item \code{balances.and.overlaps} a list with components
#'     \itemize{
#'     \item\code{singles} a \code{list} of the counts of each level per attribute.
#'     \item\code{pairs} a \code{list} of the counts of pairwise occurences of levels
#'     for each pair of attributes.
#'     \item\code{overlaps} a \code{vector} of the percentage of questions that include
#'     one or more duplicated level per attribute.
#'     }
#' }
#'
#' @details If \code{prior} is supplied and \code{design.algorithm ==
#'     "Efficient"}, the number of coefficients must correspond
#'     to the number of attributes/attribute levels specified in
#'     \code{attribute.levels}.  If \code{prior} is \code{NULL}, the prior for the
#'     coefficients is assumed to be identically zero.  If the supplied matrix
#'     contains two columns, the first column is taken as the prior
#'     mean for the coefficients and the second is taken to be the
#'     prior variances.  If only one column is present, the prior for
#'     the coefficients is assumed to be centered at those values.
#' @examples
#' x <- CreateExperiment(c(3, 5, 7, 10), 20)
#' ChoiceModelDesign("Random", x$attribute.levels, n.questions = 30,
#'     alternatives.per.question = 4, prohibitions = x$prohibitions)
#' @importFrom utils getFromNamespace modifyList
#' @importFrom flipU ConvertCommaSeparatedStringToVector
#' @export
ChoiceModelDesign <- function(design.algorithm = c("Random", "Shortcut",
                                                   "Balanced overlap",
                                                   "Complete enumeration",
                                                   "Efficient",
                                                   "Partial profiles",
                                                   "Alternative specific - Random",
                                                   "Alternative specific - Federov"),
                              attribute.levels = NULL,
                              prior = NULL,
                              n.questions,
                              n.versions = 1,
                              alternatives.per.question = 0,
                              prohibitions = NULL,
                              none.alternatives = 0,
                              none.positions = NULL,
                              labeled.alternatives = FALSE,
                              n.constant.attributes = 0,
                              extensive = FALSE,
                              n.rotations = 10,
                              max.subsample = 1e8,
                              output = "Labeled design",
                              seed = 54123) {


    alt.specific <- grepl("Alternative specific", design.algorithm)

    if (!alt.specific)
    {
        ## Map the design.algorithm to the function
        design.algorithm <- match.arg(design.algorithm)
        function.name <- sub("^([A-Z])", "\\L\\1", design.algorithm, perl = TRUE)
        function.name <- gsub(" ([[:alpha:]])", "\\U\\1", function.name, perl = TRUE)
        design.function <- getFromNamespace(paste0(function.name, "Design"),
                                            ns = "flipChoice")

        if (is.list(attribute.levels))
        {
            if (is.null(names(attribute.levels)))
                names(attribute.levels) <- paste("Attribute", seq(length(attribute.levels)))
            levels.per.attribute <- sapply(attribute.levels, length)
            names(levels.per.attribute) <- names(attribute.levels)
        }
        else if (is.character(attribute.levels))
        {
            parsed.data <- parsePastedData(attribute.levels, n.sim = 10, coding = "D",
                                           labeled.alternatives)
            levels.per.attribute <- parsed.data[["lvls"]]
            attribute.levels <- parsed.data[["attribute.list"]]
            if (is.null(prior))
                prior <- parsed.data[["prior"]]
        }
        else
            stop("Input must be either a list of vectors containing the labels of levels for each ",
                 "attribute, with names corresponding to the attribute labels; or a character ",
                 "matrix with first row containing attribute names and subsequent rows containing attribute levels.")
    }
    else     # alternative specific designs
    {
        levels.per.attribute <- unlist(sapply(attribute.levels, function(x) sapply(x, length)))
        specific.attribute.levels <- attribute.levels
        attribute.levels <- unlist(attribute.levels, recursive = FALSE)
        labeled.alternatives <- TRUE
    }

    if ((!is.null(prior) || !all(prior == 0)) &&
        !(design.algorithm %in% c("Efficient", "Partial profiles")))
        warning("Prior data will be ignored as it can only be used with ",
                "algorithms 'Efficient' or 'Partial profiles'.")

    if(any(levels.per.attribute < 2))
        stop("All attributes must have at least 2 levels.")

    # If labeled.alternatives then alternatives.per.question is calculated and not supplied
    if (labeled.alternatives)
    {
        if (!missing(alternatives.per.question) && alternatives.per.question != 0 &&
            alternatives.per.question != length(attribute.levels[[1]]))
            warning("Since ", sQuote("labeled.alternatives"), " is TRUE, the number ",
                    "of alternatives per question will be taken from the number of levels ",
                    "of the first attribute in ", sQuote("attribute.levels"), "; ignoring ",
                    sQuote("alternatives.per.question"))
        alternatives.per.question <- if (alt.specific)
            length(specific.attribute.levels)
        else
            length(attribute.levels[[1]])
    }

    if (is.character(none.positions))
    {
        none.positions <- as.numeric(ConvertCommaSeparatedStringToVector(none.positions))
        if (length(none.positions) == 0)
            none.positions <- NULL
    }
    if (none.alternatives != 0 && !is.null(none.positions) && length(none.positions) != none.alternatives)
        stop("Number of none alternatives is inconsistent with the number of positions of none alternatives.")
    if (!is.null(none.positions))
    {
        none.alternatives <- length(none.positions)
        if(min(none.positions) < 1 || max(none.positions) > n.questions + none.alternatives)
            stop("Position of none of alternatives are inconsistent with the number of questions.")
    }
    if (none.alternatives != 0 && is.null(none.positions))
        none.positions <- seq(alternatives.per.question + 1, alternatives.per.question + none.alternatives)

    if (n.questions * n.versions <= sum(levels.per.attribute - 1))
        stop("There are insufficient questions or versions in your design to fit a model. Increase the",
             " number of questions * versions to more than ", sum(levels.per.attribute - 1), ".")

    # Check if prohibitions are valid for the algorithm
    algorithms.without.prohibitions <- c("Efficient", "Shortcut", "Partial profiles", "Alternative specific - Random",
                                         "Alternative specific - Federov")
    if (!is.null(prohibitions) && length(prohibitions) > 0 && design.algorithm %in% algorithms.without.prohibitions)
        warning(gettextf("Prohibitions are not yet implemented for algorithm %s and will be ignored.",
                    sQuote(design.algorithm)))

    # Convert prohibitions from labels to numeric and factors
    prohibitions <- encodeProhibitions(prohibitions, attribute.levels)
    integer.prohibitions <- data.frame(lapply(prohibitions, as.integer))

    if (alt.specific)
    {
        a.s <- alternativeSpecificDesign(design.algorithm = design.algorithm,
                                          attribute.levels = specific.attribute.levels,
                                          n.questions = n.questions,
                                          n.versions = n.versions,
                                          max.subsample = max.subsample,
                                          seed = seed)
        design <- a.s$design
    }
    else
    {
        # Call the algorithm to create the design
        # Design algorithms     - use only unlabeled levels (i.e. integer level indices)
        #                       - simply multiply questions per respondent by n.versions
        #                       - ignore None alternatives, these are added later
        args <- list(levels.per.attribute = levels.per.attribute,
                           prior = prior,
                           n.questions = n.questions * n.versions,
                           alternatives.per.question = alternatives.per.question,
                           prohibitions = integer.prohibitions,
                           labeled.alternatives = labeled.alternatives,
                           n.constant.attributes = n.constant.attributes,
                           extensive = extensive,
                           n.rotations = n.rotations,
                           seed = seed)

        f <- formals(design.function)
        args <- modifyList(as.list(f), args[names(args) %in% names(f)])

        design <- do.call(design.function, args)
    }

    result <- list(design.algorithm = design.algorithm,
                   attribute.levels = attribute.levels,
                   prohibitions = prohibitions,
                   n.questions = n.questions,
                   n.versions = n.versions,
                   alternatives.per.question = alternatives.per.question,
                   none.alternatives = none.alternatives,
                   output = output)

    if (design.algorithm == "Efficient")
    {
        code <- ifelse(any(design$model.matrix == -1), "E", "D")
        cfun <- if (code == "E") "contr.sum" else "contr.treatment"
        name.idx <- -(1:2)
        df <- as.data.frame(apply(design$design[, name.idx], 2, as.factor))
        cfun <- as.list(rep(cfun, ncol(df)))
        names(cfun) <- names(df)
        form <- as.formula(paste0("~`",
                                  paste(colnames(design$design)[name.idx],
                                        collapse = "`+`", sep = ""), "`"))
        result$model.matrix <- model.matrix(form,
                                            df, contrasts.arg = cfun)[, -1]
        result$db.error <- design$error
        design <- design$design
    }
    else if (design.algorithm == "Partial profiles")
    {
        if (n.constant.attributes > 0)
        {
            constants.array <- do.call(rbind, design$const.attr.list)
            constants.by.version <- split.data.frame(constants.array, rep(seq(n.versions), each = n.questions))
            max.constant.att.by.version <- lapply(constants.by.version, function(x) max(table(x)))
            min.appearances <- n.questions - max(unlist(max.constant.att.by.version))
            if (min.appearances < 3)
                warning("One or more of the attributes is shown only ",
                    min.appearances, " time(s) within a version.",
                    " It is preferable that each alternative should appear 3 times.")
            design.constant.na <- design$design
            for (i in seq(n.questions * n.versions))
            {
                row.start <- (i - 1) * alternatives.per.question + 1
                row.end <- row.start +  alternatives.per.question - 1
                design.constant.na[row.start:row.end, design$const.attr.list[[i]] + 2] <- NA

            }
            design.constant.na <- addTaskNumber(design.constant.na, n.versions, n.questions, alternatives.per.question)
            result$design.constant.na <- addVersions(design.constant.na, n.versions)
        }

        result$d.criterion <- design$d.criterion
        result$const.attr.list <- design$const.attr.list
        design <- design$design
    }

    ## Add designs and diagnostics
    design <- addTaskNumber(design, n.versions, n.questions, alternatives.per.question)
    result$design <- addVersions(design, n.versions)
    result$design.with.none <- addNoneAlternatives(result$design, none.positions,
                                                   alternatives.per.question)
    if (alt.specific)
    {
        result$d.error <- a.s$d.error
    }
    else
        result$d.error <- DError(result$design,
                                 sapply(result$attribute.levels, length),
                                 effects = FALSE, prior = prior,
                                 n.rotations = n.rotations, seed = seed)

    result$labeled.design <- labelDesign(result$design.with.none, attribute.levels)
    result$labeled.alternatives <- labeled.alternatives
    result$balances.and.overlaps <- balancesAndOverlaps(result)

    if (!alt.specific)
    {
        ml.model <- mlogitModel(result)
        if (is.null(ml.model))
            warning("Standard errors cannot be calculated. The design does not sufficiently explore",
                    " the combinations of levels. To fix this, increase the number of questions or versions.")
        else
            result$standard.errors <- summary(ml.model)$CoefTable[, 1:2]
    }

    class(result) <- c(class(result), "ChoiceModelDesign")
    return(result)
}

## Additional columns added to every design output returned by our algorithms
.non.attr.col.names <- c("Version", "Task", "Question", "Alternative")
.n.non.attr.col <- length(.non.attr.col.names)

checkDesignColNames <- function(design.matrix)
{
    for (name in .non.attr.col.names)
        if (!name %in% colnames(design.matrix))
            stop("The input design needs to contain a column called '", name,
                 "'.")
}

#' @export
#' @method print ChoiceModelDesign
#' @noRd
print.ChoiceModelDesign <- function(x, ...) {

    # Output a table with attributes along the columns and levels along the rows
    if (x$output == "Inputs")
    {
        max.levels <- max(sapply(x$attribute.levels, length))
        levels.table <- sapply(x$attribute.levels, function (z) c(z, rep("", max.levels - length(z))))
        rownames(levels.table) <- paste("Level", seq.int(max.levels))
        print(list(levels.table = levels.table, prohibitions = x$prohibitions))
        return()
    }

    print(x$labeled.design)
}

######################### HELPER FUNCTIONS ###########################

# Convert prohibitions from labels to indices (numeric levels)
# and expand "" or "All" to all levels of the attribute.
encodeProhibitions <- function(prohibitions, attribute.levels) {

    if (is.null(prohibitions) || length(prohibitions) == 0)
        return(data.frame())

    prohibitions[prohibitions == ""] <- "All"
    prohibitions <- data.frame(prohibitions)
    if (nrow(prohibitions) == 0)
        return(prohibitions)

    if (ncol(prohibitions) != length(attribute.levels))
        stop("Each prohibition must include a level for each attribute (possibly including 'All').")

    for (i in 1:length(attribute.levels))
    {
        # set levels, standardize rownames and find rows with "All"
        prohibitions[, i] <- factor(prohibitions[, i], levels = c(attribute.levels[[i]], "All"))
        if (any(is.na(prohibitions[, i])))
            stop("Prohibition number(s) ", paste(which(is.na(prohibitions[, i])), collapse = ", "),
                 " contains level(s) that are invalid for attribute ", names(attribute.levels)[i])

        rownames(prohibitions) <- seq(nrow(prohibitions))
        rows.with.all <- prohibitions[, i] == "All"

        if (any(rows.with.all))
        {
            # duplicate rows with "All" then fill with levels
            expanded.rows <- rep(rownames(prohibitions),
                                                  (length(attribute.levels[[i]]) - 1) * rows.with.all + 1)
            prohibitions <- prohibitions[expanded.rows, ]
            prohibitions[prohibitions[, i] == "All", i] <- attribute.levels[[i]]
        }
    }
    rownames(prohibitions) <- seq(nrow(prohibitions))
    colnames(prohibitions) <- names(attribute.levels)
    return(prohibitions)
}

# Convert an unlabeled design into a labeled design
labelDesign <- function(unlabeled.design, attribute.levels) {
    labeled.design <- lapply(seq_along(attribute.levels),
                             function(i) factor(unlabeled.design[, i + .n.non.attr.col],
                                                levels = seq(length(attribute.levels[[i]])),
                                                labels = attribute.levels[[i]]))
    labeled.design <- as.data.frame(labeled.design)
    labeled.design <- cbind(unlabeled.design[, seq_len(.n.non.attr.col)], labeled.design)
    colnames(labeled.design) = c(.non.attr.col.names,
                                 names(attribute.levels))
    return(labeled.design)
}

# Compute one and two-way level balances and overlaps
balancesAndOverlaps <- function(cmd) {

    design <- if (is.null(cmd$design.constant.na)) cmd$design else cmd$design.constant.na
    singles <- singleLevelBalances(design)
    pairs <- pairLevelBalances(design)

    # label the levels
    singles <- labelSingleBalanceLevels(singles, cmd$attribute.levels)
    pairs <- labelPairBalanceLevels(pairs, cmd$attribute.levels)

    # flatten pairwise list of list and remove unused and all zero
    pairs <- unlist(pairs, recursive = FALSE)
    if (!is.null(pairs))
        pairs <- pairs[!is.na(pairs)]
    pairs <- pairs[lapply(pairs, sum) != 0]

    overlaps <- countOverlaps(design, cmd$alternatives.per.question,
                              sapply(cmd$attribute.levels, length))

    # do not calculate diagnostic stats if any constant attributes or alternative specific
    if (!is.null(cmd$design.constant.na) || grepl("Alternative specific", cmd$design.algorithm))
        return(list(overlaps = overlaps, singles = singles, pairs = pairs))

    # calculate the balance for each version and across all versions
    version.balances.singles <- matrix(0, nrow = cmd$n.versions, ncol = length(singles))
    version.balances.pairs <- matrix(0, nrow = cmd$n.versions, ncol = length(pairs))

    for (version in seq(cmd$n.versions))
    {
        version.start <- 1 + ((version - 1) * cmd$n.questions * cmd$alternatives.per.question)
        version.end <- version.start + cmd$n.questions * cmd$alternatives.per.question - 1
        version.design <- design[version.start:version.end, ]

        singles.version <- singleLevelBalances(version.design)
        pairs.version <- pairLevelBalances(version.design)
        if (!is.null(pairs))
        {
            pairs.version <- unlist(pairs.version, recursive = FALSE)
            if (!is.null(pairs.version))
                pairs.version <- pairs.version[!is.na(pairs.version)]
        }

        if (version == 1)
        {
            worst.balance.singles.version <- sapply(singles.version, worst.balance)
            if (!is.null(pairs))
                worst.balance.pairs.version <- sapply(pairs.version, worst.balance)
        }

        version.balances.singles[version, ] <- sapply(singles.version, balance.stat)
        if (!is.null(pairs))
            version.balances.pairs[version, ] <- sapply(pairs.version, balance.stat)
    }

    mean.version.balance <- mean(1 - (colMeans(version.balances.singles) / worst.balance.singles.version))
    across.version.balance <- mean(1 - (sapply(singles, balance.stat) / (cmd$n.versions * worst.balance.singles.version)))
    mean.version.pairwise.balance <- across.version.pairwise.balance <- NULL
    if (!is.null(pairs))
    {
        mean.version.pairwise.balance <- mean(1 - (colMeans(version.balances.pairs) / worst.balance.pairs.version))
        across.version.pairwise.balance <- mean(1 - (sapply(pairs, balance.stat) / (cmd$n.versions * worst.balance.pairs.version)))
    }

    return(list(overlaps = overlaps,
                mean.version.balance = mean.version.balance,
                mean.version.pairwise.balance = mean.version.pairwise.balance,
                across.version.balance = across.version.balance,
                across.version.pairwise.balance = across.version.pairwise.balance,
                singles = singles,
                pairs = pairs))
}

balance.stat <- function(x) {
    return(sum(abs(x - mean(x))))
}

# Balance if only one value is non-zero
worst.balance <- function(x) {
    return(2 * (sum(x) - mean(x)))
}

singleLevelBalances <- function(design) {
    columns <- design[, (.n.non.attr.col + 1L):ncol(design), drop = FALSE]
    attribute.names <- colnames(columns)
    columns <- split(columns, rep(seq_len(NCOL(columns)),
                                  each = NROW(columns)))
    singles <- lapply(columns, table, useNA = "ifany")
    names(singles) <- attribute.names
    return(singles)
}

pairLevelBalances <- function(design) {
    n.attributes <- ncol(design) - .n.non.attr.col
    if (n.attributes == 1)
        return(NULL)
    pairs <- replicate(n.attributes, rep(list(NA), n.attributes), simplify = FALSE)
    for (i in seq_len(n.attributes - 1))
        for (j in (i + 1):n.attributes) {
            pairs[[i]][[j]] <- table(design[, i + .n.non.attr.col], design[, j + .n.non.attr.col])
            names(pairs[[i]])[[j]] <- paste0(colnames(design)[i + .n.non.attr.col], "/",
                                             colnames(design)[j + .n.non.attr.col])
        }
    return(pairs)
}

labelSingleBalanceLevels <- function(singles, attribute.levels)
{
    return(mapply(function(x, y)
    {
        nms <- y[as.integer(names(x))]
        if (length(nms) == 1 + length(y))
            nms[length(nms)] <- "Not shown"
        names(x) <- nms
        x
    }, singles, attribute.levels, SIMPLIFY = FALSE))
}

labelPairBalanceLevels <- function(pairs, attribute.levels) {
    if (is.null(pairs))
        return(NULL)
    n.attributes <- length(attribute.levels)
    for (i in 1:(n.attributes - 1))
        for (j in (i + 1):n.attributes) {
            rownames(pairs[[i]][[j]]) <- attribute.levels[[i]][as.integer(rownames(pairs[[i]][[j]]))]
            colnames(pairs[[i]][[j]]) <- attribute.levels[[j]][as.integer(colnames(pairs[[i]][[j]]))]
        }
    return(pairs)
}

#' @importFrom flipFormat FormatAsPercent
countOverlaps <- function(design, alternatives.per.question, levels.per.attribute) {
    # table of counts for each level by question, listed for each attribute
    design <- design[, !colnames(design) %in% .non.attr.col.names, drop = FALSE]
    columns <- split(design, col(design))
    question <- rep(seq(NROW(design) / alternatives.per.question), each = alternatives.per.question)
    overlaps <- lapply(columns, table, question)

    # duplicated levels
    overlaps <- lapply(overlaps, ">=", 2)
    # overlaps for questions (rows) by attribute (cols)
    overlaps <- sapply(overlaps, function(x) apply(x, 2, any))
    prop.qn.overlap.by.attr <- FormatAsPercent(colSums(overlaps) / nrow(overlaps), decimals = 2)
    names(prop.qn.overlap.by.attr) <- paste0(colnames(design), " (", levels.per.attribute, ")")

    return(prop.qn.overlap.by.attr)
}

flattenDesign <- function(design) {
    n.qns <- dim(design)[1]
    n.alts <- dim(design)[2]
    flattened <- matrix(design, nrow = n.qns * n.alts)
    flattened <- cbind(rep(seq(n.qns), n.alts), rep(seq(n.alts), each = n.qns), flattened)
    flattened <- flattened[order(as.numeric(flattened[, 1])), ]
    colnames(flattened) <- c("Question", "Alternative", dimnames(design)[[3]])
    return(flattened)
}

addNoneAlternatives <- function(design, none.positions, alternatives.per.question) {

    if (is.null(none.positions))
        return(design)
    none.alternatives <- length(none.positions)

    if (is.data.frame(design))
    {
        stop("what is this")
        n.questions <- design[["Version"]][nrow(design)]
        new.rows <- matrix(nrow = n.questions*none.alternatives, ncol = ncol(design))
        new.rows[, "Version"] <- rep(seq_len(n.questions), each = none.alternatives)
        new.rows[, "Question"] <- rep((alternatives.per.question+1):(alternatives.per.question+none.alternatives),
                             times = n.questions)
        colnames(new.rows) <- colnames(design)
        out <- rbind(design, new.rows)
        out <- out[order(out$question, out$alternative), ]
        return(out)
    }
    n <- nrow(design)
    new.n <- n * (alternatives.per.question + none.alternatives) / alternatives.per.question
    design.with.none <- matrix(NA, nrow = new.n, ncol = ncol(design))

    # copy existing alternatives
    question.rows <- rep(!(seq(alternatives.per.question + none.alternatives) %in% none.positions),
                         new.n / (alternatives.per.question + none.alternatives))
    design.with.none[question.rows, ] <- design

    colnames(design.with.none) <- colnames(design)
    n.versions <- design[NROW(design), "Version"]
    design.with.none[, "Version"] <- rep(seq.int(n.versions), each = NROW(design.with.none) / n.versions)
    n.questions <- n / alternatives.per.question / n.versions
    design.with.none[, "Task"] <- rep(seq.int(n.versions*n.questions),
                                 each = alternatives.per.question + none.alternatives)
    design.with.none[, "Question"] <- rep.int(rep(seq.int(n.questions),
                                     each = alternatives.per.question + none.alternatives), n.versions)
    design.with.none[, "Alternative"] <- rep.int(seq(alternatives.per.question + none.alternatives),
                                 n / alternatives.per.question)
    return(design.with.none)
}

addTaskNumber <- function(design, n.v, n.q, apq)
    cbind(Task = rep(seq.int(n.v*n.q), each = apq), design)

addVersions <- function(design, n.versions) {
    rows.per.version <- NROW(design) / n.versions
    design <- cbind(Version = rep(seq.int(n.versions), each = rows.per.version), design)
    ## Need to update Question column to repeat for each version
    design[, "Question"] <- rep(design[seq_len(rows.per.version), "Question"], n.versions)
    return(design)
}


# randomly choose responses to a ChoiceModelDesign
randomChoices <- function(cmd, respondents = 300) {
    n.alts <- cmd$alternatives.per.question
    n.qns <- respondents * cmd$n.questions
    chosen.alternatives <- replicate(n.qns, sample(seq(n.alts), 1))
    chosen.indices <- chosen.alternatives + seq(n.qns) * n.alts - n.alts
    chosen <- rep(FALSE, n.alts * n.qns)
    chosen[chosen.indices] <- TRUE
    return(chosen)
}

# Fit a design and choices with mlogit package.
# Assumes 300 repsondents.
#' @importFrom mlogit mlogit.data mlogit
#' @importFrom stats as.formula
mlogitModel <- function(cmd, choices = NULL) {
    if (is.null(choices))
        choices <- randomChoices(cmd)

    labeled <- as.data.frame(labelDesign(cmd$design, cmd$attribute.levels))
    labeled <- labeled[, !colnames(labeled) %in% c("Version", "Task")]
    labeled <- labeled[rep_len(seq_len(nrow(labeled)), length.out = length(choices)), ]

    labeled$Choice <- choices
    mlogit.df <- mlogit.data(labeled, choice = "Choice", shape = "long",
                             varying = which(!colnames(labeled) %in% .non.attr.col.names),
                             alt.var = "Alternative", id.var = "Question", drop.index = TRUE)

    form <- paste("Choice ~ ", paste0("`", colnames(mlogit.df)[1:ncol(mlogit.df) - 1],
                                      "`", collapse = "+"), "| -1")
    ml.model <- tryCatch(mlogit(as.formula(form), data = mlogit.df), error = function(e) {NULL})
    return(ml.model)
}

inputNotSensibleError <- function(has.constant.attributes)
{
    if (has.constant.attributes)
        stop("The specified design does not sufficiently explore the ",
             "combinations of levels. To fix this, increase the number of ",
             "questions or versions or reduce the number of constant ",
             "attributes.")
    else
        stop("The specified design does not sufficiently explore the ",
             "combinations of levels. To fix this, increase the number of ",
             "questions or versions.")
}
