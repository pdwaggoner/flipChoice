#' @importFrom stats rmultinom
generateSyntheticChoices <- function(X, respondent.indices, synthetic.priors,
                                     seed, n.alternatives,
                                     parameter.scales = NULL)
{
    set.seed(seed)
    n.respondents <- length(respondent.indices)
    n.parameters <- dim(X)[3]
    prior <- processSyntheticPriors(synthetic.priors, n.parameters,
                                    n.alternatives)
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

processSyntheticPriors <- function(synthetic.priors, n.parameters,
                                   n.alternatives)
{
    error.msg <- paste0("The format of the priors for generating synthetic ",
                        "choices does not match the supplied data. Please ",
                        "refer to the documentation on how to format the",
                        "synthetic priors.")
    n.pars.without.alts <- n.parameters - n.alternatives + 1
    prior.zeros <- rep(0, n.alternatives - 1)

    if (is.matrix(synthetic.priors) &&
        is.character(synthetic.priors)) # Pasted data
    {
        parsed.data <- parsePastedData(synthetic.priors, n.sim = 10,
                                       coding = "D")
        prior <- parsed.data[["prior"]]
        if (!is.matrix(prior) ||
            (nrow(prior) != n.parameters &&
             nrow(prior) != n.pars.without.alts))
            stop(error.msg)
        else
        {
            if (nrow(prior) == n.parameters)
            {
                prior.mean <- prior[, 1]
                prior.sd <- prior[, 2]
            }
            else # let alternative parameters have priors of zero
            {
                prior.mean <- c(prior.zeros, prior[, 1])
                prior.sd <- c(prior.zeros, prior[, 2])
            }
        }
    }
    else if (is.matrix(synthetic.priors) && is.numeric(synthetic.priors) &&
             ncol(synthetic.priors) == 2 &&
             (nrow(synthetic.priors) == n.parameters ||
              nrow(synthetic.priors) == n.pars.without.alts)) # 2-column matrix
    {
        if (nrow(synthetic.priors) == n.parameters)
        {
            prior.mean <- synthetic.priors[, 1]
            prior.sd <- synthetic.priors[, 2]
        }
        else # let alternative parameters have priors of zero
        {
            prior.mean <- c(prior.zeros, synthetic.priors[, 1])
            prior.sd <- c(prior.zeros, synthetic.priors[, 2])
        }
    }
    else if (synthetic.priors == 0) # assume mean and sd to be zero
    {
        prior.mean <- rep(0, n.parameters)
        prior.sd <- rep(0, n.parameters)
    }
    else
        stop(error.msg)

    list(mean = prior.mean, sd = prior.sd)
}
