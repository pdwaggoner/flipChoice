#' @title SawtoothDualCsv
#' @description Creates the design and response .csv files to be used as input
#'     to Sawtooth CBC HB (dual csv format). Also outputs a covariates .csv
#'     file if covariates are supplied. Remember to select "Code variables
#'     using dummy coding" in the Sawtooth settings.
#' @param design A choice model design object.
#' @param respondent.data Repondent data frame containing version, tasks,
#'     choices.
#' @param covariates.data Covariates data frame to be exported to a .csv file.
#' @param subset A filter variable for respondents.
#' @param dual.response.none Whether to use the "dual response none" data,
#'     if it is present.
#' @param dual.response.none.as.none Whether to treat "dual response none" data
#'     as a "none" option.
#' @param design.file Name of the design csv file to export.
#' @param design.out.file Name of the out-of-sample design csv file to
#'      export. This is to be used as an input into SawtoothAccuracy.
#' @param respondent.file Name of the response csv file to export.
#' @param respondent.out.file Name of the out-of-sample response csv file
#'      to export. This is to be used as an input into SawtoothAccuracy.
#' @param covariates.file Name of the covariates csv file to export.
#' @param include.choice.parameters Whether to include choice parameters to
#'      the design.
#' @param n.questions.left.out Number of questions to leave out. If questions
#'      are left out then the "out" file names need to be supplied.
#' @param seed Random seed.
#' @return Nothing is returned, but .csv files are written to the working
#'      directory.
#' @importFrom utils write.csv
#' @export
SawtoothDualCsv <- function(design, respondent.data, covariates.data = NULL,
                            subset = NULL, dual.response.none = FALSE,
                            dual.response.none.as.none = FALSE,
                            design.file, design.out.file = NULL,
                            respondent.file, respondent.out.file = NULL,
                            covariates.file = NULL,
                            include.choice.parameters = TRUE,
                            n.questions.left.out = 0, seed = 123)
{
    # Respondent and covariates file
    nms <- names(respondent.data)
    respondent.data$caseid <- 1:nrow(respondent.data)
    if (!is.null(covariates.data))
        covariates.data <- cbind(caseid = 1:nrow(respondent.data), covariates.data)

    second.response <- NULL
    if (dual.response.none)
    {
        is.dual.response.none <- grepl("^dual.response.none\\d+$", nms)
        second.response.yes <- respondent.data[, is.dual.response.none] == "Yes"
        second.response <- matrix(2 - as.numeric(second.response.yes),
                                  ncol = sum(is.dual.response.none))
    }

    if (!is.null(subset))
    {
        respondent.data <- respondent.data[subset, ]
        covariates.data <- covariates.data[subset, ]
        second.response <- second.response[subset, ]
    }

    n.respondents <- nrow(respondent.data)
    choices <- respondent.data[, grepl("^choice\\d+$", nms)]
    n.questions <- ncol(choices)
    n.alternatives <- max(choices)

    if (dual.response.none && dual.response.none.as.none)
    {
        for (i in 1:n.respondents)
        {
            for (j in 1:n.questions)
            {
                if (second.response[i, j] == 2)
                    choices[i, j] <- n.alternatives + 1
            }
        }
    }

    if (n.questions.left.out > 0)
    {
        left.out <- questionsLeftOut(respondent.data, n.questions.left.out,
                                     seed)
        n.questions.left.in <- n.questions - n.questions.left.out
        choices.in <- matrix(NA, nrow = n.respondents,
                             ncol = n.questions.left.in)
        choices.out <- matrix(NA, nrow = n.respondents,
                              ncol = n.questions.left.out)
        for (i in 1:n.respondents)
        {
            choices.in[i, ] <- unlist(choices[i, !left.out[i, ]])
            choices.out[i, ] <- unlist(choices[i, left.out[i, ]])
        }

        if (dual.response.none && !dual.response.none.as.none)
        {
            second.response.in <- matrix(NA, nrow = n.respondents,
                                         ncol = n.questions.left.in)
            second.response.out <- matrix(NA, nrow = n.respondents,
                                         ncol = n.questions.left.out)
            for (i in 1:n.respondents)
            {
                second.response.in[i, ] <- second.response[i, !left.out[i, ]]
                second.response.out[i, ] <- second.response[i, left.out[i, ]]
            }
        }

        sawtooth.resp.data.in <- data.frame(Caseid = respondent.data$caseid,
                                            Version = 1:n.respondents)
        sawtooth.resp.data.out <- data.frame(Caseid = respondent.data$caseid,
                                             Version = 1:n.respondents)
        for (i in 1:n.questions.left.in)
        {
            nm <- paste0("choice", i)
            sawtooth.resp.data.in[[nm]] <- choices.in[, i]
            if (dual.response.none && !dual.response.none.as.none)
            {
                nm <- paste0("choice", i, "none")
                sawtooth.resp.data.in[[nm]] <- second.response.in[, i]
            }
        }

        for (i in 1:n.questions.left.out)
        {
            nm <- paste0("choice", i)
            sawtooth.resp.data.out[[nm]] <- choices.out[, i]
            if (dual.response.none && !dual.response.none.as.none)
            {
                nm <- paste0("choice", i, "none")
                sawtooth.resp.data.out[[nm]] <- second.response.out[, i]
            }
        }

        write.csv(x = sawtooth.resp.data.in, file = respondent.file,
                  row.names = FALSE)
        write.csv(x = sawtooth.resp.data.out, file = respondent.out.file,
                  row.names = FALSE)
    }
    else
    {
        sawtooth.resp.data <- data.frame(Caseid = respondent.data$caseid,
                                         Version = respondent.data$version)
        for (i in 1:n.questions)
        {
            nm <- paste0("choice", i)
            sawtooth.resp.data[[nm]] <- choices[, i]
            if (dual.response.none && !dual.response.none.as.none)
            {
                nm <- paste0("choice", i, "none")
                sawtooth.resp.data[[nm]] <- second.response[, i]
            }
        }

        write.csv(x = sawtooth.resp.data, file = respondent.file,
                  row.names = FALSE)
    }

    if (!is.null(covariates.data))
    {
        covariates.data <- covariates.data[!sapply(covariates.data,
                                                   is.character)]
        covariates.data <- data.frame(sapply(covariates.data, as.numeric))
        write.csv(x = covariates.data, file = covariates.file,
                  row.names = FALSE)
    }

    # Design
    des <- data.frame(design$design)
    att.names <- setdiff(names(des),
                         c("Version", "Task", "Question", "Alternative"))
    if (n.questions.left.out > 0)
    {
        n.attributes <- length(att.names)
        n.alternatives <- max(des$Alternative)

        des.in <- matrix(NA, nrow = n.respondents * n.questions.left.in * n.alternatives,
                         ncol = ncol(des))
        des.out <- matrix(NA, nrow = n.respondents * n.questions.left.out * n.alternatives,
                         ncol = ncol(des))

        for (i in 1:n.respondents)
        {
            left.in.indices <- rep((which(!left.out[i, ]) - 1) * n.alternatives, each = n.alternatives) +
                                rep(1:n.alternatives, n.questions.left.in)
            left.out.indices <- rep((which(left.out[i, ]) - 1) * n.alternatives, each = n.alternatives) +
                                 rep(1:n.alternatives, n.questions.left.out)
            ind <- which(des$Version == respondent.data$version[i])

            new.ind <- (i - 1) * n.questions.left.in * n.alternatives +
                       (1:(n.questions.left.in * n.alternatives))
            des.in[new.ind, ] <- as.matrix(des[ind[left.in.indices], ])

            new.ind <- (i - 1) * n.questions.left.out * n.alternatives +
                       (1:(n.questions.left.out * n.alternatives))
            des.out[new.ind, ] <- as.matrix(des[ind[left.out.indices], ])
        }

        colnames(des.in) <- names(des)
        colnames(des.out) <- names(des)

        des.in <- data.frame(des.in)
        des.out <- data.frame(des.out)

        sawtooth.design.in <- data.frame(Version = rep(1:n.respondents,
                                            each = n.questions.left.in * n.alternatives),
                             Task = rep(rep(1:n.questions.left.in, each = n.alternatives),
                                                n.respondents),
                             Concept = rep(rep(1:n.alternatives),
                                           n.respondents * n.questions.left.in))
        sawtooth.design.out <- data.frame(Version = rep(1:n.respondents,
                                            each = n.questions.left.out * n.alternatives),
                             Task = rep(rep(1:n.questions.left.out, each = n.alternatives),
                                                n.respondents),
                             Concept = rep(rep(1:n.alternatives),
                                           n.respondents * n.questions.left.out))

        if (include.choice.parameters)
        {
            sawtooth.design.in$Alternative <- sawtooth.design.in$Concept
            sawtooth.design.out$Alternative <- sawtooth.design.out$Concept
        }

        for (nm in att.names)
        {
            sawtooth.design.in[[nm]] <- des.in[[nm]]
            sawtooth.design.out[[nm]] <- des.out[[nm]]
        }

        if (dual.response.none && dual.response.none.as.none)
        {
            sawtooth.design.in <- addNoneAlternative(sawtooth.design.in)
            sawtooth.design.out <- addNoneAlternative(sawtooth.design.out)
        }

        write.csv(x = sawtooth.design.in, file = design.file, row.names = FALSE)
        write.csv(x = sawtooth.design.out, file = design.out.file, row.names = FALSE)
    }
    else
    {
        sawtooth.design <- data.frame(Version = des$Version,
                                      Task = des$Question,
                                      Concept = des$Alternative)
        if (include.choice.parameters)
            sawtooth.design$Alternative <- des$Alternative

        for (nm in att.names)
            sawtooth.design[[nm]] <- des[[nm]]

        if (dual.response.none && dual.response.none.as.none)
            sawtooth.design <- addNoneAlternative(sawtooth.design)

        write.csv(x = sawtooth.design, file = design.file, row.names = FALSE)
    }
}

#' @title SawtoothAccuracy
#' @description Computes the accuracies given the design, response and
#'     utilities files.
#' @param design.file The csv design file generated by SawtoothDualCsv. For
#'     out-of-sample predictions, use the file from design.out.file.
#' @param respondent.file The csv response file generated by SawtoothDualCsv. For
#'     out-of-sample predictions, use the file from respondent.out.file.
#' @param utilities.file The csv file of respondent utilities from Sawtooth
#'     CBC HB.
#' @param dual.response.none Whether the data contains dual response none.
#' @return If the design does not contain dual-response none, then a vector of
#'     prediction accuracies of the respondents is returned. Otherwise, a list
#'     is returned with prediction accuracies of just the second (none)
#'     response and the prediction accuracies of the overall response.
#' @importFrom utils read.csv
#' @export
SawtoothAccuracy <- function(design.file, respondent.file, utilities.file,
                             dual.response.none = FALSE)
{
    design <- read.csv(design.file)
    responses <- read.csv(respondent.file)
    utilities <- read.csv(utilities.file)
    n.respondents <- nrow(responses)
    n.alternatives <- max(design$Concept)
    accuracies <- rep(NA, n.respondents)

    n.att.lvls <- unname(sapply(design[, -1:-3, drop = FALSE], max))
    has.none.response <- sum(n.att.lvls) == ncol(utilities) - 3 && !dual.response.none
    has.second.response <- sum(n.att.lvls) == ncol(utilities) - 3 && dual.response.none

    param.offset <- if (has.none.response)
        rep(c(0, cumsum(n.att.lvls[-length(n.att.lvls)])),
                            each = n.alternatives - 1)
    else
        rep(c(0, cumsum(n.att.lvls[-length(n.att.lvls)])),
                            each = n.alternatives)

    if (has.second.response)
        second.response.accuracies <- rep(NA, n.respondents)
    if (has.none.response || has.second.response)
    {
        non.none.score <- 0
        non.none.count <- 0
    }

    for (i in 1:n.respondents)
    {
        id <- responses$Caseid[i]
        u <- unlist(utilities[utilities[[1]] == id, -1:-2])

        v <- responses$Version[i]
        version.design <- design[design$Version == v, ]
        n.questions <- max(version.design$Task)
        tally <- 0

        if (has.second.response)
        {
            second.response.tally <- 0
            for (j in 1:n.questions)
            {
                question.design <- version.design[version.design$Task == j, ]
                s <- rowSums(matrix(u[as.matrix(question.design[, -1:-3]) +
                                          param.offset], nrow = n.alternatives))
                if (max(s) < u[length(u)])
                {
                    if (responses[i, 2 * j + 2] == 2)
                    {
                        tally <- tally + 1
                        second.response.tally <- second.response.tally + 1
                    }
                }
                else
                {
                    if (responses[i, 2 * j + 2] == 1)
                    {
                        second.response.tally <- second.response.tally + 1
                        if (which.max(s) == responses[i, 2 * j + 1])
                            tally <- tally + 1
                    }
                }
                if (responses[i, 2 * j + 2] == 1)
                {
                    non.none.count <- non.none.count + 1
                    if (which.max(s) == responses[i, 2 * j + 1])
                        non.none.score <- non.none.score + 1
                }
            }
            accuracies[i] <- tally / n.questions
            second.response.accuracies[i] <- second.response.tally / n.questions
        }
        else if (has.none.response)
        {
            for (j in 1:n.questions)
            {
                question.design <- version.design[version.design$Task == j, ]
                question.design <- question.design[1:(n.alternatives - 1), ]
                s <- rowSums(matrix(u[as.matrix(question.design[, -1:-3]) +
                                          param.offset], nrow = n.alternatives - 1))
                if (max(s) > u[length(u)] && which.max(s) == responses[i, j + 2])
                    tally <- tally + 1
                else if (responses[i, j + 2] == n.alternatives)
                    tally <- tally + 1
                if (responses[i, j + 2] < n.alternatives)
                {
                    non.none.count <- non.none.count + 1
                    if (which.max(s) == responses[i, j + 2])
                        non.none.score <- non.none.score + 1
                }
            }
            accuracies[i] <- tally / n.questions
        }
        else
        {
            for (j in 1:n.questions)
            {
                question.design <- version.design[version.design$Task == j, ]
                s <- rowSums(matrix(u[as.matrix(question.design[, -1:-3, drop = FALSE]) +
                                          param.offset], nrow = n.alternatives))
                if (which.max(s) == responses[i, j + 2])
                    tally <- tally + 1
            }
            accuracies[i] <- tally / n.questions
        }
    }

    if (has.second.response)
        list(accuracies = accuracies,
             second.response.accuracies = second.response.accuracies,
             non.none.accuracy = non.none.score / non.none.count)
    else if (has.none.response)
        list(accuracies = accuracies,
             non.none.accuracy = non.none.score / non.none.count)
    else
        accuracies
}

questionsLeftOut <- function(respondent.data, n.questions.left.out, seed)
{
    set.seed(seed)
    n.respondents <- nrow(respondent.data)
    nms <- names(respondent.data)
    choice.names <- nms[grepl("^choice\\d+$", nms)]
    n.questions <- length(choice.names)
    left.out <- matrix(FALSE, nrow = n.respondents, ncol = n.questions)
    for (i in 1:n.respondents)
        left.out[i, sample(n.questions, n.questions.left.out)] <- TRUE
    left.out
}

addNoneAlternative <- function(design)
{
    n.alternatives <- max(design$Concept)
    n.tasks <- nrow(design) / n.alternatives
    n.rows.new <- (n.alternatives + 1) * n.tasks
    ind <- rep(TRUE, n.rows.new)
    ind[(1:n.tasks) * (n.alternatives + 1)] <- FALSE
    new.version <- rep(NA, n.rows.new)
    new.version[ind] <- design$Version
    new.version[!ind] <- design$Version[(1:n.tasks) * n.alternatives]
    new.task <- rep(NA, n.rows.new)
    new.task[ind] <- design$Task
    new.task[!ind] <- design$Task[(1:n.tasks) * n.alternatives]
    result <- data.frame(Version = new.version,
                         Task = new.task,
                         Concept = rep(1:(n.alternatives + 1), n.tasks))
    att.names <- setdiff(names(design), c("Version", "Task", "Concept"))
    for (nm in att.names)
    {
        new.att <- rep(0, n.rows.new)
        new.att[ind] <- design[[nm]]
        result[[nm]] <- new.att
    }
    result
}
