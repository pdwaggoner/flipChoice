#' @importFrom stats rmultinom
generateSimulatedChoices <- function(X, respondent.indices, simulated.priors,
                                     seed, n.alternatives,
                                     n.attribute.parameters,
                                     attribute.names,
                                     parameter.scales = NULL)
{
    set.seed(seed)
    n.respondents <- length(respondent.indices)
    n.parameters <- dim(X)[3]
    prior <- processSimulatedPriors(simulated.priors, n.parameters,
                                    n.alternatives, n.attribute.parameters,
                                    attribute.names)
    respondent.parameters <- t(matrix(rnorm(n.respondents * n.parameters,
                                            prior$mean, prior$sd),
                                      nrow = n.parameters))
    if (!is.null(parameter.scales))
        respondent.parameters <- t(t(respondent.parameters) * parameter.scales)

    choices <- rep(NA, dim(X)[1])
    for (i in 1:n.respondents)
    {
        n.questions <- length(respondent.indices[[i]])
        for (j in 1:n.questions)
        {
            ind <- respondent.indices[[i]][j]
            discriminants <- X[ind, , ] %*% respondent.parameters[i, ]
            probs <- exp(discriminants) / sum(exp(discriminants))
            choices[ind] <- which(rmultinom(1, 1, probs) == 1)
        }
    }
    list(choices = choices,
         respondent.parameters = respondent.parameters)
}

processSimulatedPriors <- function(simulated.priors, n.parameters,
                                   n.alternatives, n.attribute.parameters,
                                   attribute.names)
{
    error.msg <- paste0("The format of the priors for generating simulated ",
                        "choices does not match the supplied data. Please ",
                        "refer to the documentation on how to format the",
                        "simulated priors.")
    n.pars.without.alts <- n.parameters - n.alternatives + 1
    prior.zeros <- rep(0, n.alternatives - 1)

    if (is.vector(simulated.priors))
    {
        if (length(simulated.priors) == 1 && simulated.priors == 0) # no priors in design
        {
            prior.mean <- rep(0, n.parameters)
            prior.sd <- rep(0, n.parameters)
            warning("The supplied design does not contain priors. The ",
                    "prior mean and standard deviations have been ",
                    "assummed to be zero.")
        }
        else if (all(simulated.priors == 0) &&
                 (length(simulated.priors) == n.parameters ||
                  length(simulated.priors) == n.pars.without.alts)) # vector of 0s
        {
            prior.mean <- rep(0, n.parameters)
            prior.sd <- rep(0, n.parameters)
            warning("The prior mean and standard deviations have been ",
                    "assummed to be zero.")
        }
        else if (length(simulated.priors) == n.parameters)
        {
            prior.mean <- simulated.priors
            prior.sd <- rep(0, n.parameters)
            warning("The prior standard deviations have been ",
                    "assummed to be zero.")
        }
        else if (length(simulated.priors) == n.pars.without.alts)
        {
            prior.mean <- c(prior.zeros, simulated.priors)
            prior.sd <- rep(0, n.parameters)
            warning("The prior standard deviations have been ",
                    "assummed to be zero.")
        }
        else
            stop(error.msg)
    }
    else if (is.matrix(simulated.priors))
    {
        if (max(dim(simulated.priors)) == 0) # no entered prior
        {
            warning("No prior for simulated data was entered. ",
                    "The prior mean and standard deviations have been ",
                    "assummed to be zero.")
            prior.mean <- rep(0, n.parameters)
            prior.sd <- rep(0, n.parameters)
        }
        else if (is.character(simulated.priors)) # Pasted data
        {
            parsed.data <- parsePastedData(simulated.priors, n.sim = 10,
                                          coding = "D")
            prior <- fillInPriors(parsed.data, n.attribute.parameters,
                                 attribute.names, n.parameters)
            prior.mean <- prior[, 1]
            prior.sd <- prior[, 2]
        }
        else if (is.numeric(simulated.priors) && ncol(simulated.priors) == 2)
        {
            if (nrow(simulated.priors) == n.parameters)
            {
                prior.mean <- simulated.priors[, 1]
                prior.sd <- simulated.priors[, 2]
            }
            else if (nrow(simulated.priors) == n.pars.without.alts)
            {
                prior.mean <- c(prior.zeros, simulated.priors[, 1])
                prior.sd <- c(prior.zeros, simulated.priors[, 2])
            }
            else
                stop(error.msg)
        }
        else
            stop(error.msg)
    }
    else
        stop(error.msg)

    list(mean = prior.mean, sd = prior.sd)
}

fillInPriors <- function(pasted.data, n.attribute.parameters, attribute.names,
                         n.parameters)
{
    pasted.priors <- pasted.data$prior
    attribute.list <- pasted.data$attribute.list
    n.prior.attributes <- length(attribute.list)

    if (is.vector(pasted.priors))
        pasted.priors <- cbind(pasted.priors, rep(0, length(pasted.priors)))

    attr.ind <- attributeIndicies(n.attribute.parameters)
    prior.ind <- priorAttributeIndices(attribute.list)
    prior.attr.names <- names(attribute.list)

    result <- matrix(0, nrow = n.parameters, ncol = 2)
    for (i in 1:n.prior.attributes)
    {
        ind <- which(prior.attr.names[i] == attribute.names)
        if (length(ind) == 1)
            result[attr.ind[[ind]], ] <- pasted.priors[prior.ind[[i]], ]
    }

    unused.prior.names <- setdiff(prior.attr.names, attribute.names)
    if (length(unused.prior.names) > 0)
        warning("The following attribute(s) were supplied in the priors but ",
                "could not be matched to the design: ",
                paste0(unused.prior.names, collapse = ", "))

    missing.attr.names <- setdiff(attribute.names, prior.attr.names)
    if (length(missing.attr.names) > 0)
        warning("The following attribute(s) were missing from the priors and ",
                "are assumed to have means and standard deviations of 0: ",
                paste0(missing.attr.names, collapse = ", "))
    result
}

attributeIndicies <- function(n.attribute.parameters)
{
    n.attributes <- length(n.attribute.parameters)
    end.ind <- 0
    result <- list()
    for (i in 1:n.attributes)
    {
        start.ind <- end.ind + 1
        end.ind <- end.ind + n.attribute.parameters[i]
        result[[i]] <- start.ind:end.ind
    }
    result
}

priorAttributeIndices <- function(attribute.list)
{
    n.prior.attr <- length(attribute.list)
    n.prior.attr.parameters <- pmax(sapply(attribute.list, length) - 1, 1)
    result <- list()
    end.ind <- 0
    for (i in 1:n.prior.attr)
    {
        start.ind <- end.ind + 1
        end.ind <- end.ind + n.prior.attr.parameters[i]
        result[[i]] <- start.ind:end.ind
    }
    result
}

sampleFromX <- function(X, respondent.indices, sample.size, seed)
{
    set.seed(seed)
    n.respondents <- length(respondent.indices)
    sample.ind <- sample(n.respondents, sample.size, replace = TRUE)
    sample.X <- X[unlist(respondent.indices[sample.ind]), , ]
    sample.respondent.indices <- list()
    end.ind <- 0
    for (i in 1:sample.size)
    {
        start.ind <- end.ind + 1
        end.ind <- end.ind + length(respondent.indices[[sample.ind[i]]])
        sample.respondent.indices[[i]] <- start.ind:end.ind
    }
    list(X = sample.X, respondent.indices = sample.respondent.indices)
}
