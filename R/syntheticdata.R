#' @importFrom stats rmultinom
generateSyntheticChoices <- function(X, respondent.indices, synthetic.priors,
                                     seed)
{
    set.seed(seed)
    n.respondents <- length(respondent.indices)
    n.parameters <- dim(X)[3]
    prior <- processSyntheticPriors(synthetic.priors, n.parameters)
    respondent.parameters <- t(matrix(rnorm(n.respondents * n.parameters,
                                            prior$mean, prior$sd),
                                      nrow = n.parameters))
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

processSyntheticPriors <- function(synthetic.priors, n.parameters)
{
    error.msg <- paste0("The priors for generating synthetic choices do not ",
                        "match the supplied data. Please refer to the ",
                        "documentation for the valid formats of the ",
                        "synthetic priors.")
    if (is.matrix(synthetic.priors) && is.character(synthetic.priors))
    {
        parsed.data <- parsePastedData(synthetic.priors, n.sim = 10,
                                       coding = "D")
        prior <- parsed.data[["prior"]]
        if (!is.matrix(prior) || nrow(prior) != n.parameters)
            stop(error.msg)
        else
        {
            prior.mean <- prior[, 1]
            prior.sd <- prior[, 2]
        }
    }
    else if (is.matrix(synthetic.priors) && is.numeric(synthetic.priors) &&
             ncol(synthetic.priors) == 2 &&
             nrow(synthetic.priors) == n.parameters)
    {
        prior.mean <- synthetic.priors[, 1]
        prior.sd <- synthetic.priors[, 2]
    }
    else
        stop(error.msg)

    list(mean = prior.mean, sd = prior.sd)
}
