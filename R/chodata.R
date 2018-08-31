#' @importFrom flipData CleanSubset NoData MissingDataFail
processChoFile <- function(cho.file, attribute.levels.file,
                           subset, weights, n.questions.left.out, seed,
                           input.prior.mean, input.prior.sd,
                           include.choice.parameters, respondent.ids, missing,
                           covariates, simulated.priors, simulated.sample.size)
{
    is.data.simulated <- !is.null(simulated.priors)
    if (is.data.simulated)
    {
        simulatedDataWarnings(subset, weights, covariates)
        subset <- NULL
        weights <- NULL
        covariates <- NULL
    }

    if (missing == "Error if missing data" &&
        ((!is.null(subset) && any(is.na(subset))) ||
        (!is.null(weights) && any(is.na(weights)))) ||
        (!is.null(covariates) && any(is.na(covariates))))
        MissingDataFail();

    raw.lines <- readLines(cho.file)
    attribute.levels <- processAttributeLevelsFile(attribute.levels.file)

    raw.num <- lapply(strsplit(raw.lines, " "), as.numeric)
    n.attributes <- raw.num[[1]][3]
    n.questions.common <- raw.num[[1]][4]
    n.alternatives <- raw.num[[3]][1]
    if (raw.num[[1]][5] == 1)
    {
        n.alternatives <- n.alternatives + 1
        is.none.alternative <- rep(FALSE, n.alternatives)
        is.none.alternative[n.alternatives] <- TRUE
    }
    else
        is.none.alternative <- rep(FALSE, n.alternatives)

    n.raw <- length(raw.num)

    n.attributes <- length(attribute.levels)
    n.attribute.parameters <- unlist(lapply(attribute.levels, length)) - 1
    n.parameters <-  sum(n.attribute.parameters)
    par.names <- parameterNamesFromAttributes(attribute.levels)
    all.names <- allNamesFromAttributes(attribute.levels)
    attribute.names <- names(attribute.levels)

    checkPriorParameters(input.prior.mean, input.prior.sd, n.alternatives,
                         n.attributes, n.parameters, include.choice.parameters)

    ordered.attributes <- orderedAttributes(input.prior.mean, n.attributes,
                                            n.parameters)

    file.respondent.ids <- rep(NA, n.raw)
    X <- array(data = 0, dim = c(n.raw, n.alternatives, n.parameters))
    Y <- rep(NA, n.raw)
    file.respondent.indices <- vector("list", n.raw)
    respondent.has.missing <- rep(FALSE, n.raw)

    row.i <- 0
    rs <- 0
    respondent.i <- 0
    while (row.i < n.raw)
    {
        row.i <- row.i + 1 # first respondent row
        respondent.i <- respondent.i + 1
        file.respondent.ids[respondent.i] <- raw.num[[row.i]][1]
        n.questions <- raw.num[[row.i]][4]
        if (!is.na(n.questions.common) && n.questions != n.questions.common)
            n.questions.common <- NA
        row.i <- row.i + 1 # second respondent row
        rs.initial <- rs
        for (j in 1:n.questions)
        {
            row.i <- row.i + 1 # question format row
            question.X <- matrix(NA, nrow = n.alternatives,
                                 ncol = n.parameters)
            for (k in 1:n.alternatives)
            {
                if (is.none.alternative[k])
                    question.X[k, ] <- fillXNoneOfThese(n.parameters,
                                                     n.attributes,
                                                     n.attribute.parameters)
                else
                {
                    row.i <- row.i + 1 # attributes row
                    question.X[k, ] <- fillXAttributes(n.parameters,
                                                    n.attributes,
                                                    n.attribute.parameters,
                                                    ordered.attributes,
                                                    raw.num[[row.i]])
                }
            }
            row.i <- row.i + 1 # choice row
            if (is.data.simulated)
            {
                rs <- rs + 1
                X[rs, , ] <- question.X
            }
            else
            {
                question.Y <- raw.num[[row.i]][1]
                if (question.Y > 0)
                {
                    rs <- rs + 1
                    Y[rs] <- question.Y
                    X[rs, , ] <- question.X
                }
                else if (missing == "Error if missing data")
                    MissingDataFail()
                else
                    respondent.has.missing[respondent.i] <- TRUE
            }
        }
        if (rs.initial < rs)
            file.respondent.indices[[respondent.i]] <- (rs.initial + 1):rs
        else
            file.respondent.indices[[respondent.i]] <- numeric(0)
    }

    n.rs <- rs
    n.file.respondents <- respondent.i
    X <- X[1:n.rs, , ]
    Y <- Y[1:n.rs]
    file.respondent.ids <- file.respondent.ids[1:n.file.respondents]
    file.respondent.indices <- file.respondent.indices[1:n.file.respondents]
    respondent.has.missing <- respondent.has.missing[1:n.file.respondents]

    # Reconcile cho file with supplied respondent IDs
    if (!is.null(respondent.ids))
    {
        reordering <- reconcileRespondentIDs(respondent.ids, file.respondent.ids)
        respondent.has.missing <- respondent.has.missing[reordering]
        n.respondents <- length(respondent.ids)
        ind.list <- vector("list", n.respondents)
        for (i in 1:n.respondents)
            ind.list[[i]] <- file.respondent.indices[[reordering[i]]]
        ind <- unlist(ind.list)
        X <- X[ind, , ]
        Y <- Y[ind]
        cs <- c(0, cumsum(sapply(ind.list, length)))
        respondent.indices <- vector("list", n.respondents)
        for (i in 1:n.respondents)
            if (cs[i + 1] - cs[i] > 0)
                respondent.indices[[i]] <- (cs[i] + 1):cs[i + 1]
            else
                respondent.indices[[i]] <- numeric(0)
    }
    else
    {
        n.respondents <- n.file.respondents
        respondent.indices <- file.respondent.indices
    }

    if (include.choice.parameters)
    {
        output <- addChoiceParameters(X, n.attributes, n.parameters,
                                      n.attribute.parameters, n.alternatives,
                                      par.names, all.names, is.none.alternative)
        X <- output$X
        n.attributes <- output$n.attributes
        n.parameters <- output$n.parameters
        n.attribute.parameters <- output$n.attribute.parameters
        par.names <- output$par.names
        all.names <- output$all.names
        attribute.names <- c("Alternative", attribute.names)
    }

    checkNumberOfQuestionsLeftOut(max(sapply(respondent.indices, length)),
                                  n.questions.left.out)

    non.missing <- nonMissingRespondentsCho(respondent.indices,
                                            respondent.has.missing, subset,
                                            weights, covariates,
                                            n.questions.left.out, missing)

    ## filter.subset <- CleanSubset(subset, n.respondents)
    ## subset <- filter.subset & non.missing

    ## if (sum(filter.subset) == 0)
    ##     stop("All respondents have been filtered out.")
    ## else if (sum(subset) == 0)
    ##     NoData()

    weights <- prepareWeights(weights, subset)
    rs.subset <- unlist(respondent.indices[subset])
    X <- X[rs.subset, , ]
    Y <- Y[rs.subset]
    respondent.indices <- respondent.indices[subset]
    if (!is.null(covariates))
        covariates <- covariates[subset, ]
    n.respondents <- sum(subset)

    if (is.data.simulated)
    {
        n.respondents <- simulated.sample.size
        subset <- rep(TRUE, n.respondents)
        weights <- rep(1, n.respondents)
        covariates <- NULL
        sampled.output <- sampleFromX(X, respondent.indices,
                                      n.respondents, seed)
        X <- sampled.output$X
        respondent.indices <- sampled.output$respondent.indices
        output <- generateSimulatedChoices(X, respondent.indices,
                                           simulated.priors, seed,
                                           n.alternatives,
                                           n.attribute.parameters,
                                           attribute.names)
        Y <- output$choices
        simulated.respondent.parameters <- output$respondent.parameters
    }
    else
        simulated.respondent.parameters <- NULL

    ## split.data <- crossValidationSplit(X, Y, n.questions.left.out, seed,
    ##                                    respondent.indices)

    ## prior.mean <- processInputPrior(input.prior.mean, n.parameters,
    ##                                 n.attributes, n.attribute.parameters)
    ## prior.sd <- processInputPrior(input.prior.sd, n.parameters, n.attributes,
    ##                               n.attribute.parameters)

      list(n.questions = n.questions.common,
           n.questions.left.out = n.questions.left.out,
           n.alternatives = n.alternatives,
           n.attributes = n.attributes,
           n.respondents = n.respondents,
           n.parameters = n.parameters,
           n.attribute.parameters = n.attribute.parameters,
           par.names = par.names,
           all.names = all.names,
           ## beta.names = par.names,
           ## all.beta.names = all.names,
           ## X.in = split.data$X.in,
           ## Y.in = split.data$Y.in,
           ## X.out = split.data$X.out,
           ## Y.out = split.data$Y.out,
           Y = Y,
           X = X,
           n.questions.left.in = split.data$n.questions.left.in,
           subset = subset,
           weights = weights,
           covariates = covariates,
           parameter.scales = rep(1, n.parameters),
           prior.mean = prior.mean,
           prior.sd = prior.sd,
           simulated.respondent.parameters = simulated.respondent.parameters)
  }

  processAttributeLevelsFile <- function(attribute.levels.file)
  {
      raw.attribute.levels <- readExcelFile(attribute.levels.file)
      n.attributes <- length(raw.attribute.levels)
      nms <- names(raw.attribute.levels)
      attribute.levels <- list()
      for (i in 1:n.attributes)
      {
          not.na <- !is.na(raw.attribute.levels[[i]])
          attribute.levels[[nms[i]]] <- raw.attribute.levels[[i]][not.na]
      }
      attribute.levels
  }


  addChoiceParameters <- function(X, n.attributes, n.parameters,
                                  n.attribute.parameters, n.alternatives,
                                  par.names, all.names, is.none.alternative)
  {
      X <- addChoiceParametersX(X)
      n.attributes <- n.attributes + 1
      n.parameters <- n.parameters + n.alternatives - 1
      n.attribute.parameters <- c(n.alternatives - 1, n.attribute.parameters)
      alt.labels <- createAlternativeLabels(n.alternatives, is.none.alternative)
      par.names <- c(alt.labels[-1], par.names)
      all.names <- c(alt.labels, all.names)
      list(X = X, n.attributes = n.attributes, n.parameters = n.parameters,
           n.attribute.parameters = n.attribute.parameters,
           par.names = par.names, all.names = all.names)
  }

  addChoiceParametersX <- function(X)
  {
      dim.X <- dim(X)
      n.alternatives <- dim.X[2]
      dim.new.X <- dim.X
      dim.new.X[3] <- dim.X[3] + n.alternatives - 1
      new.X <- array(data = 0, dim = dim.new.X)
      new.X[, , n.alternatives:dim.new.X[3]] <- X
      for (i in 1:(n.alternatives - 1))
          new.X[, i + 1, i] <- 1
      new.X
  }

  createAlternativeLabels <- function(n.alternatives, is.none.alternative)
  {
      result <- character(n.alternatives)
      for (i in 1:n.alternatives)
      {
          if (is.none.alternative[i])
              result[i] <- paste0("Alternative: ", i, " (none of these)")
          else
              result[i] <- paste0("Alternative: ", i)
      }
      result
  }

  fillXAttributes <- function(n.parameters, n.attributes, n.attribute.parameters,
                              ordered.attributes, question.design)
  {
      result <- rep(0, n.parameters)
      parameter.index <- 0
      for (l in 1:n.attributes)
      {
          if (ordered.attributes[l])
          {
              if (question.design[l] > 1)
              {
                  start.ind <- parameter.index + 1
                  end.ind <- parameter.index + question.design[l] - 1
                  result[start.ind:end.ind] <- 1
              }
          }
          else
          {
              if (question.design[l] > 1)
                  result[parameter.index + question.design[l] - 1] <- 1
          }

          parameter.index <- parameter.index + n.attribute.parameters[l]
      }
      result
  }

  reconcileRespondentIDs <- function(respondent.ids, file.respondent.ids)
  {
      n.respondents <- length(respondent.ids)
      reordering <- rep(NA, n.respondents)
      for (i in 1:n.respondents)
      {
          id <- respondent.ids[i]
          ind <- which(file.respondent.ids == id)
          if (length(ind) == 0)
              stop("Respondent ", id, " not found in the .cho file.")
          else if (length(ind) > 1)
              stop("Respondent ", id, " has duplicate entries in the .cho file.")
          else
              reordering[i] <- ind
      }
      if (length(setdiff(file.respondent.ids, respondent.ids)) > 0)
          warning("Respondents in the .cho file that do not appear in the ",
                  "supplied respondent IDs have been omitted.")
      reordering
  }

  nonMissingRespondentsCho <- function(respondent.indices,
                                       respondent.has.missing, subset, weights,
                                       covariates, n.questions.left.out, missing)
  {
      result <- sapply(respondent.indices, length) > n.questions.left.out
      if (missing == "Exclude cases with missing data")
        result <- result & !respondent.has.missing
    if (!is.null(subset))
        result <- result & !is.na(subset)
    if (!is.null(weights))
        result <- result & !is.na(weights)
    if (!is.null(covariates))
        result <- result & !is.na(rowSums(covariates))
    result
}
