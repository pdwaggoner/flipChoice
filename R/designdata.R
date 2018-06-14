processDesignObject <- function(design.object, choices, questions, subset,
                                weights, n.questions.left.out, seed,
                                input.prior.mean, input.prior.sd,
                                include.choice.parameters, missing,
                                covariates, synthetic.priors,
                                synthetic.sample.size)
{
    processDesign(design.object$design.with.none,
                  design.object$attribute.levels,
                  choices, questions, subset, weights, n.questions.left.out,
                  seed, input.prior.mean, input.prior.sd,
                  include.choice.parameters, missing, covariates,
                  synthetic.priors, synthetic.sample.size)
}

processDesignFile <- function(design.file, attribute.levels.file, choices,
                              questions, subset, weights, n.questions.left.out,
                              seed, input.prior.mean, input.prior.sd,
                              include.choice.parameters, missing, covariates,
                              synthetic.priors, synthetic.sample.size)
{
    output <- readDesignFile(design.file, attribute.levels.file)
    processDesign(output$design, output$attribute.levels, choices, questions,
                  subset, weights, n.questions.left.out, seed,
                  input.prior.mean, input.prior.sd, include.choice.parameters,
                  missing, covariates, synthetic.priors, synthetic.sample.size)
}

#' @importFrom flipData CleanSubset NoData
processDesign <- function(design, attribute.levels, choices, questions, subset,
                          weights, n.questions.left.out, seed, input.prior.mean,
                          input.prior.sd, include.choice.parameters, missing,
                          covariates, synthetic.priors, synthetic.sample.size)
{
    checkDesignColNames(design)

    design.attributes <- design[, !colnames(design) %in% .non.attr.col.names]
    n.attributes <- ncol(design.attributes)
    if (n.attributes != length(attribute.levels))
        stop("The number of attributes in the design file is inconsistent ",
             "with the number of attributes in the attribute levels file.")

    n.questions <- max(design[, "Question"])
    n.alternatives <- max(design[, "Alternative"])

    checkNumberOfQuestionsLeftOut(n.questions, n.questions.left.out)

    if (!is.null(choices) && !is.null(questions))
    {
        choices <- data.frame(sapply(choices, as.numeric))
        questions <- data.frame(sapply(questions, as.numeric))

        if (missing == "Error if missing data")
            errorIfMissingDataFound(choices, questions, subset, weights,
                                    covariates, missing)

        non.missing.table <- nonMissingTable(choices, questions, subset, weights,
                                             covariates, missing)
        non.missing <- nonMissingRespondents(non.missing.table,
                                             n.questions.left.out, missing,
                                             n.questions)

        filter.subset <- CleanSubset(subset, nrow(choices))
        subset <- filter.subset & non.missing

        if (sum(filter.subset) == 0)
            stop("All respondents have been filtered out.")
        else if (sum(subset) == 0)
            NoData()

        n.respondents <- sum(subset)
        weights <- prepareWeights(weights, subset)
        choices <- choices[subset, ]
        questions <- questions[subset, ]
        non.missing.table <- non.missing.table[subset, ]
        if (!is.null(covariates))
            covariates <- covariates[subset, ]
    }
    else if (!is.null(synthetic.priors))
    {
        questions <- generateSyntheticTasks(synthetic.sample.size, design)
        n.respondents <- synthetic.sample.size
        non.missing.table <- matrix(TRUE, nrow = n.respondents,
                                    ncol = n.questions)
        if (!is.null(subset))
        {
            warning("Filters have been ignored as respondent choices and ",
                    "tasks were not supplied.")
        }
        subset <- rep(TRUE, n.respondents)
        if (!is.null(weights))
        {
            warning("Weights have been ignored as respondent choices and ",
                    "tasks were not supplied.")
        }
        weights <- rep(1, n.respondents)
        if (!is.null(covariates))
        {
            warning("Covariates have been ignored as respondent choices and ",
                    "tasks were not supplied.")
            covariates <- NULL
        }
    }
    else
        stop("Insufficient choice data was supplied.")

    # A "None of these" option is left out from the design
    add.none.of.these <- n.alternatives == length(unique(design[, "Alternative"])) + 1
    # A "None of these" option is included in the design
    none.of.these.included <- any(rowSums(design.attributes) == 0)
    has.none.of.these <- add.none.of.these || none.of.these.included

    n.attribute.parameters <- unlist(lapply(attribute.levels, length)) - 1
    n.parameters <-  sum(n.attribute.parameters)
    par.names <- parameterNamesFromAttributes(attribute.levels)
    all.names <- allNamesFromAttributes(attribute.levels)

    checkPriorParameters(input.prior.mean, input.prior.sd, n.alternatives,
                         n.attributes, n.parameters, include.choice.parameters)
    ordered.attributes <- orderedAttributes(input.prior.mean, n.attributes,
                                            n.parameters)
    if (any(ordered.attributes) && has.none.of.these)
        stop('Ordered attributes cannot be specified when the "None of these"',
             ' alternative is present in the analysis.')

    n.rs <- sum(non.missing.table)
    X <- array(data = 0, dim = c(n.rs, n.alternatives, n.parameters))

    rs <- 1
    for (i in 1:n.respondents)
    {
        for (j in 1:n.questions)
        {
            if (non.missing.table[i, j])
            {
                question.number <- questions[i, j]
                ind <- which(design[, "Task"] == question.number)[1]
                for (k in 1:n.alternatives)
                {
                    if (has.none.of.these && k == n.alternatives)
                        X[rs, k, ] <- fillXNoneOfThese(n.parameters,
                                                       n.attributes,
                                                       n.attribute.parameters)
                    else
                    {
                        question.design <- c(t(design.attributes[ind, ]))
                        X[rs, k, ] <- fillXAttributes(n.parameters,
                                                      n.attributes,
                                                      n.attribute.parameters,
                                                      ordered.attributes,
                                                      question.design)
                        ind <- ind + 1
                    }
                }
                rs <- rs + 1
            }
        }
    }

    if (include.choice.parameters)
    {
        output <- addChoiceParameters(X, n.attributes, n.parameters,
                                      n.attribute.parameters, n.alternatives,
                                      par.names, all.names, has.none.of.these)
        X <- output$X
        n.attributes <- output$n.attributes
        n.parameters <- output$n.parameters
        n.attribute.parameters <- output$n.attribute.parameters
        par.names <- output$par.names
        all.names <- output$all.names
    }

    respondent.indices <- constructRespondentIndices(non.missing.table)

    Y <- if (!is.null(synthetic.priors))
        generateSyntheticChoices(X, respondent.indices, synthetic.priors, seed)
    else
        c(t(as.matrix(choices)))[c(t(non.missing.table))]

    split.data <- crossValidationSplit(X, Y, n.questions.left.out, seed,
                                       respondent.indices)

    prior.mean <- processInputPrior(input.prior.mean, n.parameters,
                                    n.attributes, n.attribute.parameters)
    prior.sd <- processInputPrior(input.prior.sd, n.parameters,
                                  n.attributes, n.attribute.parameters)

    list(n.questions = n.questions,
         n.questions.left.out = n.questions.left.out,
         n.alternatives = n.alternatives,
         n.attributes = n.attributes,
         n.respondents = n.respondents,
         n.parameters = n.parameters,
         n.attribute.parameters = n.attribute.parameters,
         par.names = par.names,
         all.names = all.names,
         beta.names = par.names,
         all.beta.names = all.names,
         X.in = split.data$X.in,
         Y.in = split.data$Y.in,
         X.out = split.data$X.out,
         Y.out = split.data$Y.out,
         n.questions.left.in = split.data$n.questions.left.in,
         subset = subset,
         weights = weights,
         covariates = covariates,
         parameter.scales = rep(1, n.parameters),
         prior.mean = prior.mean,
         prior.sd = prior.sd)
}

readDesignFile <- function(design.file, attribute.levels.file)
{
    design <- readExcelFile(design.file)
    n.attributes <- length(design) - 3
    design.is.numeric <- all(sapply(design[4:length(design)], function(x) {
            is.numeric(x) || !any(is.na(suppressWarnings(as.numeric(x))))
        }))
    if (design.is.numeric && (is.null(attribute.levels.file) ||
                              attribute.levels.file == ""))
        stop("A file containing attribute levels is required.")
    else if (!design.is.numeric && !is.null(attribute.levels.file) &&
                                    attribute.levels.file != "")
        warning("The supplied attribute levels will be ignored as levels",
            " are present in the design file.")

    if (design.is.numeric)
    {
        # we apply as.numeric because the design may still contain strings of
        # numbers, e.g. "12"
        design <- data.frame(sapply(design, as.numeric))
        attribute.levels <- processAttributeLevelsFile(attribute.levels.file)
    }
    else
    {
        nms <- names(design[-1:-3])
        attribute.levels <- list()
        for (i in 1:n.attributes)
        {
            attribute.factor <- as.factor(design[[i + 3]])
            attribute.levels[[nms[i]]] <- levels(attribute.factor)
            design[[i + 3]] <- as.numeric(attribute.factor)
        }
    }
    design <- as.matrix(design)

    # Add a "Question" column
    n.rows <- nrow(design)
    question <- rep(NA, n.rows)
    question[1] <- 1
    for (i in 2:n.rows)
    {
        if (design[i, 1] == design[i - 1, 1]) # Version
        {
            question[i] <- if (design[i, 2] == design[i - 1, 2]) # Task
                question[i - 1]
            else
                question[i - 1] + 1
        }
        else
            question[i] <- 1
    }
    design <- cbind(design[, 1:2], question, design[, -1:-2])

    colnames(design)[1:4] <- c("Version", "Task", "Question", "Alternative")
    list(design = design, attribute.levels = attribute.levels)
}

generateSyntheticTasks <- function(synthetic.sample.size, design)
{
    design.versions <- design[, "Version"]
    design.tasks <- design[, "Task"]
    n.versions <- max(design.versions)
    n.questions <- max(design[, "Question"])
    versions <- sample(n.versions, synthetic.sample.size, replace = TRUE)

    result <- matrix(NA, nrow = synthetic.sample.size, ncol = n.questions)

    for (i in 1:synthetic.sample.size)
    {
        ind <- design.versions == versions[i]
        result[i, ] <- unique(design.tasks[ind])
    }
    result
}

#' @importFrom flipData MissingDataFail
nonMissingTable <- function(choices, questions, subset, weights, covariates,
                            missing)
{
    n.respondents <- nrow(choices)
    n.questions <- ncol(choices)

    if (missing == "Use partial data")
    {
        non.missing.table <- !is.na(choices) & !is.na(questions)
        missing.ind <- rep(FALSE, n.respondents)
    }
    else
    {
        non.missing.table <- matrix(TRUE, nrow = n.respondents,
                                    ncol = n.questions)
        missing.ind <- is.na(rowSums(choices)) | is.na(rowSums(questions))
    }
    if (!is.null(subset))
        missing.ind <- missing.ind | is.na(subset)
    if (!is.null(weights))
        missing.ind <- missing.ind | is.na(weights)
    if (!is.null(covariates))
        missing.ind <- missing.ind | is.na(rowSums(covariates))

    non.missing.table[missing.ind, ] <- FALSE
    non.missing.table
}

# Values for the "None of these" choice
fillXNoneOfThese <- function(n.parameters, n.attributes, n.attribute.parameters)
{
    rep(0, n.parameters)
}

# Reads Excel file given local path or URL
#' @importFrom readxl read_excel
#' @importFrom httr GET write_disk
readExcelFile <- function(file.path)
{
    ext <- if (grepl("\\.xls$", file.path))
        ".xls"
    else if (grepl("\\.xlsx$", file.path))
        ".xlsx"
    else
        stop("File name does not end in .xls or .xlsx")

    if (file.exists(file.path)) # local
        read_excel(file.path)
    else # URL
    {
        GET(file.path, write_disk(temp.file <- tempfile(fileext = ext)))
        read_excel(temp.file)
    }
}

orderedAttributes <- function(input.prior.mean, n.attributes, n.parameters)
{
    if (length(input.prior.mean) == 1 ||
        length(input.prior.mean) == n.parameters)
        rep(FALSE, n.attributes)
    else
        input.prior.mean != 0
}

#' @importFrom flipData MissingDataFail
errorIfMissingDataFound <- function(choices, questions, subset, weights,
                                    covariates, missing)
{
    if (any(is.na(choices)) || any(is.na(questions)) ||
        (!is.null(subset) && any(is.na(subset))) ||
        (!is.null(weights) && any(is.na(weights)))
        (!is.null(covariates) && any(is.na(covariates))))
        MissingDataFail();
}

constructRespondentIndices <- function(non.missing.table)
{
    n.respondents <- nrow(non.missing.table)
    result <- vector(n.respondents, mode = "list")
    start.ind <- NA
    end.ind <- 0
    for (i in 1:n.respondents)
    {
        start.ind <- end.ind + 1
        end.ind <- start.ind + sum(non.missing.table[i, ]) - 1
        result[[i]] <- start.ind:end.ind
    }
    result
}

checkNumberOfQuestionsLeftOut <- function(n.questions, n.questions.left.out)
{
    if (n.questions <= n.questions.left.out)
        stop("The number of questions left out needs to be less than the ",
             "number of questions per respondent (", n.questions ,").")
}
