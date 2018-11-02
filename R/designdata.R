processDesignObject <- function(design.object, choices, tasks, subset,
                                weights, n.questions.left.out, seed,
                                input.prior.mean, input.prior.sd,
                                include.choice.parameters, missing,
                                covariates, simulated.priors,
                                simulated.sample.size)
{
    if (!is.null(design.object$design.algorithm) &&
        grepl("^Alternative specific.*", design.object$design.algorithm))
        stop("An alternative specific design was supplied but modeling ",
             "this type of design has not yet been implemented.")

    processDesign(design.object$design.with.none,
                  design.object$attribute.levels,
                  choices, tasks, subset, weights, n.questions.left.out,
                  seed, input.prior.mean, input.prior.sd,
                  include.choice.parameters, missing, covariates,
                  simulated.priors, simulated.sample.size)
}

processDesignFile <- function(design.file, attribute.levels.file, choices,
                              tasks, subset, weights, n.questions.left.out,
                              seed, input.prior.mean, input.prior.sd,
                              include.choice.parameters, missing, covariates,
                              simulated.priors, simulated.sample.size)
{
    output <- readDesignFile(design.file, attribute.levels.file)
    processDesign(output$design, output$attribute.levels, choices, tasks,
                  subset, weights, n.questions.left.out, seed,
                  input.prior.mean, input.prior.sd, include.choice.parameters,
                  missing, covariates, simulated.priors, simulated.sample.size)
}

processDesignVariables <- function(design.variables, attribute.levels, choices,
                                   tasks, subset, weights,
                                   n.questions.left.out, seed,
                                   input.prior.mean, input.prior.sd,
                                   include.choice.parameters, missing,
                                   covariates, simulated.priors,
                                   simulated.sample.size)
{
    if (length(unique(sapply(design.variables, length))) > 1)
        stop("The variables supplied for the design have ",
             "differing lengths. Please ensure that they are from the ",
             "same data set or have the same length.")

    design <- data.frame(design.variables)

    is.labeled <- any(sapply(design[, -1:-3], function(x) {
        if (is.factor(x))
            any(sapply(levels(x), function(x) {
                any(is.na(suppressWarnings(as.numeric(x))))
            }))
        else
            FALSE
    }))

    if (is.labeled && (is.null(attribute.levels) || max(dim(attribute.levels)) == 0))
    {
        attribute.levels <- sapply(design[-1:-3], levels)
        names(attribute.levels) <- sapply(design[-1:-3],
                                          function(x) attr(x, "label"))
    }
    else if (is.matrix(attribute.levels) && is.character(attribute.levels))
        attribute.levels <- parseAttributeLevelsMatrix(attribute.levels)

    design <- convertDesign(as.matrix(sapply(design, as.numeric)))

    processDesign(design, attribute.levels, choices, tasks,
                  subset, weights, n.questions.left.out, seed,
                  input.prior.mean, input.prior.sd, include.choice.parameters,
                  missing, covariates, simulated.priors, simulated.sample.size)
}

#' @importFrom flipData CleanSubset NoData
processDesign <- function(design, attribute.levels, choices, tasks, subset,
                          weights, n.questions.left.out, seed, input.prior.mean,
                          input.prior.sd, include.choice.parameters, missing,
                          covariates, simulated.priors, simulated.sample.size)
{
    checkDesignColNames(design)

    design.attributes <- design[, !colnames(design) %in% .non.attr.col.names, drop = FALSE]
    n.attributes <- ncol(design.attributes)
    if (n.attributes != length(attribute.levels))
        stop("The number of attributes in the design is inconsistent ",
             "with the number of attributes in the attribute levels")

    n.questions <- max(design[, "Question"])

    checkNumberOfQuestionsLeftOut(n.questions, n.questions.left.out)

    if (!is.null(choices) && !is.null(tasks))
    {
        if (is.list(choices) && !is.data.frame(choices) &&
            length(unique(sapply(choices, length))) > 1)
            stop("The variables supplied for respondent choices have ",
                 "differing lengths. Please ensure that they are from the ",
                 "same data set or have the same length.")

        if (is.list(tasks) && !is.data.frame(tasks) &&
            length(unique(sapply(tasks, length))) > 1)
            stop("The variables supplied for respondent tasks have ",
                 "differing lengths. Please ensure that they are from the ",
                 "same data set or have the same length.")

        choices <- if (is.matrix(choices))
            data.frame(choices)
        else
        {
            data.frame(sapply(choices, function(x) {
                as.numeric(as.character(x))
            }))
        }

        tasks <- if (is.matrix(tasks))
            data.frame(tasks)
        else
            data.frame(sapply(tasks, as.numeric))

        if (nrow(choices) != nrow(tasks))
            stop("The respondent choices and tasks variables have differing ",
                 "lengths. Please ensure that they are from the same data ",
                 "set or have the same length.")

        if (ncol(choices) != ncol(tasks))
            stop("The respondent choices and tasks have differing numbers of ",
                 "variables. Please ensure that the choices and tasks ",
                 "variables have been correctly supplied.")

        n.alternatives <- getNumberOfAlternatives(choices)

        if (missing == "Error if missing data")
            errorIfMissingDataFound(choices, tasks, subset, weights,
                                    covariates, missing)

        non.missing.table <- nonMissingTable(choices, tasks, subset, weights,
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
        tasks <- tasks[subset, ]
        non.missing.table <- non.missing.table[subset, ]
        if (!is.null(covariates))
            covariates <- covariates[subset, ]
    }
    else if (!is.null(simulated.priors))
    {
        n.alternatives <- max(design[, "Alternative"])
        tasks <- generateSimulatedTasks(simulated.sample.size, design,
                                            seed)
        n.respondents <- simulated.sample.size
        non.missing.table <- matrix(TRUE, nrow = n.respondents,
                                    ncol = n.questions)
        simulatedDataWarnings(subset, weights, covariates)
        subset <- rep(TRUE, n.respondents)
        weights <- rep(1, n.respondents)
        covariates <- NULL
    }
    else
        stop("Insufficient choice data was supplied.")

    # A "None of these" option is left out from the design, assummed to be last
    if (n.alternatives == length(unique(design[, "Alternative"])) + 1)
    {
        is.none.alternative <- rep(FALSE, n.alternatives)
        is.none.alternative[n.alternatives] <- TRUE
    }
    else
    {
        # "None of these" options are included in the design
        first.question.attr <- design.attributes[1:n.alternatives, 1]
        is.none.alternative <- first.question.attr == 0 |
                               is.na(first.question.attr)
    }

    n.attribute.parameters <- unlist(lapply(attribute.levels, length)) - 1
    n.parameters <-  sum(n.attribute.parameters)
    par.names <- parameterNamesFromAttributes(attribute.levels)
    all.names <- allNamesFromAttributes(attribute.levels)
    attribute.names <- names(attribute.levels)

    checkPriorParameters(input.prior.mean, input.prior.sd, n.alternatives,
                         n.attributes, n.parameters, include.choice.parameters)
    ordered.attributes <- orderedAttributes(input.prior.mean, n.attributes,
                                            n.parameters)
    if (any(ordered.attributes) && any(is.none.alternative))
        stop('Ordered attributes cannot be specified when a "None of these"',
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
                question.number <- tasks[i, j]
                ind <- which(design[, "Task"] == question.number)[1]
                for (k in 1:n.alternatives)
                {
                    if (is.none.alternative[k])
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
                    }
                    ind <- ind + 1
                }
                rs <- rs + 1
            }
        }
    }

    if (include.choice.parameters)
    {
        output <- addChoiceParameters(X, n.attributes, n.parameters,
                                      n.attribute.parameters, n.alternatives,
                                      par.names, all.names,
                                      is.none.alternative)
        X <- output$X
        n.attributes <- output$n.attributes
        n.parameters <- output$n.parameters
        n.attribute.parameters <- output$n.attribute.parameters
        par.names <- output$par.names
        all.names <- output$all.names
        attribute.names <- c("Alternative", attribute.names)
    }

    respondent.indices <- constructRespondentIndices(non.missing.table)

    if (!is.null(simulated.priors))
    {

        output <- generateSimulatedChoices(X, respondent.indices,
                                           simulated.priors, seed,
                                           n.alternatives,
                                           n.attribute.parameters,
                                           attribute.names)
        Y <- output$choices
        simulated.respondent.parameters <- output$respondent.parameters
    }
    else
    {
        Y <- c(t(as.matrix(choices)))[c(t(non.missing.table))]
        simulated.respondent.parameters <- NULL
    }

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
         prior.sd = prior.sd,
         simulated.respondent.parameters = simulated.respondent.parameters)
}

readDesignFile <- function(design.file, attribute.levels.file)
{
    design <- readExcelFile(design.file)
    n.attributes <- length(design) - 3
    design.is.numeric <- all(sapply(design[-1:-3], function(x) {
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
    design <- convertDesign(as.matrix(design))

    list(design = design, attribute.levels = attribute.levels)
}

# Takes in a data frame from a design file and outputs a design matrix
# with Version, Task, Question, Alternative and attribute columns.
convertDesign <- function(design.data)
{
    n.rows <- nrow(design.data)

    result <- matrix(NA, nrow = n.rows, ncol = ncol(design.data) + 1)
    result[, 1] <- design.data[, 1]
    if (any(diff(design.data[, 2]) < 0)) # Question provided
    {
        question <- design.data[, 2]
        task <- rep(NA, n.rows)
        task[1] <- 1
        for (i in 2:n.rows)
        {
            task[i] <- if (question[i] == question[i - 1])
                task[i - 1]
            else
                task[i - 1] + 1
        }
        result <- cbind(design.data[, 1], task, design.data[, -1])
    }
    else # Task provided
    {
        vers <- design.data[, 1]
        task <- design.data[, 2]
        question <- rep(NA, n.rows)
        question[1] <- 1
        for (i in 2:n.rows)
        {
            if (vers[i] == vers[i - 1])
            {
                question[i] <- if (task[i] == task[i - 1])
                    question[i - 1]
                else
                    question[i - 1] + 1
            }
            else
                question[i] <- 1
        }
        result <- cbind(design.data[, 1:2], question, design.data[, -1:-2])
    }
    colnames(result)[1:4] <- c("Version", "Task", "Question", "Alternative")
    result
}

generateSimulatedTasks <- function(simulated.sample.size, design, seed)
{
    design.versions <- design[, "Version"]
    design.tasks <- design[, "Task"]
    n.versions <- max(design.versions)
    n.questions <- max(design[, "Question"])
    set.seed(seed)
    versions <- sample(n.versions, simulated.sample.size, replace = TRUE)

    result <- matrix(NA, nrow = simulated.sample.size, ncol = n.questions)

    for (i in 1:simulated.sample.size)
    {
        ind <- design.versions == versions[i]
        result[i, ] <- unique(design.tasks[ind])
    }
    result
}

#' @importFrom flipData MissingDataFail
nonMissingTable <- function(choices, tasks, subset, weights, covariates,
                            missing)
{
    n.respondents <- nrow(choices)
    n.questions <- ncol(choices)

    if (missing == "Use partial data")
    {
        non.missing.table <- !is.na(choices) & !is.na(tasks)
        missing.ind <- rep(FALSE, n.respondents)
    }
    else
    {
        non.missing.table <- matrix(TRUE, nrow = n.respondents,
                                    ncol = n.questions)
        missing.ind <- is.na(rowSums(choices)) | is.na(rowSums(tasks))
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
#' @importFrom flipU InterceptExceptions
readExcelFile <- function(file.path)
{
    get.file.error <- function(unused)
    {
        stop("The file in the link ", file.path , " could not be found. ",
             "Please check that it exists. Note that local file paths are not ",
             "currently supported in Q and Displayr.")
    }

    excel.file.error <- function(unused)
    {
        stop("The file could not be opened. Please check that it is an Excel ",
             ".xls or .xlsx file.")
    }

    ext <- if (grepl("\\.xlsx", file.path))
        ".xlsx"
    else if (grepl("\\.xls", file.path))
        ".xls"
    else
        excel.file.error(NULL)

    if (file.exists(file.path)) # local
        InterceptExceptions(read_excel(file.path),
                            error.handler = excel.file.error)
    else # URL
    {
        InterceptExceptions(outcome <- GET(file.path, write_disk(temp.file <- tempfile(fileext = ext))),
                            error.handler = get.file.error)
        if (outcome$status_code >= 400)
            stop("The file in the link ", file.path, " could not be ",
                 "downloaded. Please manually check that it exists by ",
                 "pasting the link in the browser.")
        InterceptExceptions(read_excel(temp.file),
                            error.handler = excel.file.error)
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
errorIfMissingDataFound <- function(choices, tasks, subset, weights,
                                    covariates, missing)
{
    if (any(is.na(choices)) || any(is.na(tasks)) ||
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

getNumberOfAlternatives <- function(choices)
{
    first.choices.column <- choices[[1]]
    if (is.numeric(first.choices.column))
        sum(!is.na(unique(first.choices.column)))
    else
        length(levels(first.choices.column))
}

simulatedDataWarnings <- function(subset, weights, covariates)
{
    if (!is.null(subset))
        warning("Filters have been ignored as simulated data has been ",
                "generated.")
    if (!is.null(weights))
        warning("Weights have been ignored as simulated data has been",
                "generated.")
    if (!is.null(covariates))
        warning("Covariates have been ignored as simulated data has been",
                "generated.")
}
