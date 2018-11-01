
accuracyResults <- function(dat, result, n.questions.left.out)
{
    n.respondents <- length(dat$n.questions.left.in)
    resp.pars <- result$reduced.respondent.parameters[dat$subset, ]

    n.rs <- dim(dat$X.in)[1]
    n.alternatives <- dim(dat$X.in)[2]

    in.sample.accuracies <- rep(NA, n.respondents)
    rs <- 1
    for (i in 1:n.respondents)
    {
        pars <- resp.pars[i, ]
        n.questions <- dat$n.questions.left.in[i]
        score <- rep(NA, n.questions)
        for (j in 1:n.questions)
        {
            u <- rep(NA, n.alternatives)
            for (k in 1:n.alternatives)
                u[k] <- sum(pars * dat$X.in[rs, k, ])
            score[j] <- if(which.max(u) == dat$Y.in[rs]) 1 else 0
            rs <- rs + 1
        }
        in.sample.accuracies[i] <- mean(score)
    }

    w <- dat$weights
    result$in.sample.accuracy <- sum(in.sample.accuracies * w) / sum(w)

    if (n.questions.left.out > 0)
    {
        out.sample.accuracies <- rep(NA, n.respondents)
        rs <- 1
        for (i in 1:n.respondents)
        {
            pars <- resp.pars[i, ]
            score <- rep(NA, n.questions.left.out)
            for (j in 1:n.questions.left.out)
            {
                u <- rep(NA, n.alternatives)
                for (k in 1:n.alternatives)
                    u[k] <- sum(pars * dat$X.out[rs, k, ])
                score[j] <- if(which.max(u) == dat$Y.out[rs]) 1 else 0
                rs <- rs + 1
            }
            out.sample.accuracies[i] <- mean(score)
        }
        result$prediction.accuracies <- rep(NA, length(dat$subset))
        result$prediction.accuracies[dat$subset] <- out.sample.accuracies
        result$out.sample.accuracy <- sum(out.sample.accuracies * w) / sum(w)
    }
    else
    {
        result$prediction.accuracies <- rep(NA, length(dat$subset))
        result$prediction.accuracies[dat$subset] <- in.sample.accuracies
        result$out.sample.accuracy <- NA
    }
    result
}

computeAccuracy <- function(object, data, ...)
{
    y.pred.in <- predict(object, data, ...)
    n.resp <- length(data$n.questions.left.in)
    y.in <- matrix(data$Y.in, nrow = n.resp, byrow = TRUE)
    in.correct <- y.in == y.pred.in$y.pred

    y.pred.out <- predict(object,
                          data = list(Y.in = data$Y.out, X.in = data$X.out,
                                      n.questions.left.in = rep(data$n.questions.left.out, n.resp)),
                          ...)

    y.out <- matrix(data$Y.out, nrow = n.resp, byrow = TRUE)
    out.correct <- y.out == y.pred.out$y.pred
    list(y.pred.in, y.pred.out, in.acc = mean(in.correct), out.acc = mean(out.correct))
}


#' Predict choice probabilities for choice model scenarios
#'
#' Given a fit from \code{\link{FitChoiceModel}}, and a new scenario
#' of alternatives, predict choice probabilites for each respondent
#' for each alternative.
#' @param object A \code{FitChoice} object output from
#'     \code{\link{FitChoiceModel}}.
#' @param scenario A list containing the scenario/alternatives to use
#'     to make a prediction of respondent choices. Each element of the
#' list is one alternative to use for the prediction. The elements
#' are named character vectors of the form
#' \code{c("attribute1" = "level1", "attribute2" = "level3", ...)}.
#' See the examples.
#' @param rule Character string, currently, one of
#'     \code{"logit respondent"}, \code{"logit draw"},
#'     \code{"first choice draw"}, or \code{"first choice respondent"}
#'     specifying how to compute the choice probabilities. See the
#'     details. Defaults to \code{"logit respondent"}.
#' @param scale A numeric vector with length equal to \code{number of
#'     respondents}, which will be used to weight/multiply the
#'     utilities for each respondent. Note, \code{offset} is applied
#'     before \code{scale}.
#' @param offset A numeric matrix with dimension \code{number of
#'     respondents x number of alternatives}, containing offsets to be
#'     applied to each utility. Note, \code{offset} is applied before
#'     \code{scale}.
#' @param ...currently ignored
#' @return A \code{num. respondents x num. alternatives} matrix of
#'     estimated choice probabilites
#' @details For a choice model fit using Hierarchical Bayes
#'     (\code{algorithm = "HB-Stan"} in \code{\link{FitChoiceModel}}),
#'     the various \code{rule}s have the following implementations:
#' \itemize{
#' \item \code{"logit respondent"} - the default, for each respondent, utilities
#' are averaged over HB iterations and the prediction probabilities are obtained by
#' applying the softmax function to these average utilites.
#' \item \code{"logit draw"} - Similar to \code{"logit respondent"} except the
#' softmax function is used first on each HB iteration for each respondent and then
#' the mean is computed over HB iterations for each respondent to obtain the prediction.
#' \item \code{"first choice draw"} - For every HB iteration, the respondent's choice
#' based on the computed utilities is tallied, the prediction is the frequency each
#' alternative is chosen over HB iterations.
#' \item \code{"first choice respondent"} - the utilities for each respondent are
#' averaged over HB iterations and their choice is computed based on this average
#' utility. Thus, the output matrix will contain one \code{1} in each row with all
#' other entries \code{0}.
#' }
#' @examples
#' data(fast.food, package = "flipChoice")
#' data(fast.food.design, package = "flipChoice")
#' choices <- fast.food[, grepl("^choice", colnames(fast.food))]
#' tasks <- fast.food[, grepl("^task", colnames(fast.food))]
#' fit <- FitChoiceModel(design = fast.food.design,
#'                       choices = choices, tasks = tasks,
#'                       hb.stanfit = TRUE, hb.warnings = FALSE,
#'                       hb.iterations = 20, hb.beta.draws.to.keep = 10)
#' scenario <- list("Alt1" = c("Price per person" = "$15",
#'                             "Cuisine" = "Pizza",
#'                             "Delivery time" =  "30 min",
#'                             "Average review rating (out of 5 stars)" = "4"),
#'                   "Alt2" = c("Price per person" = "$20",
#'                             "Cuisine" = "Hamburgers",
#'                             "Delivery time" =  "40 min",
#'                             "Average review rating (out of 5 stars)" = "4.7"))
#' head(predict(fit, scenario, rule = "logit respondent"))
#' \dontrun{
#' al <- list(Att1 = 1:2, Att2 = 1:2, Att3 = 1:3)
#' des.none <- ChoiceModelDesign(design.algorithm = "Partial profiles",
#'                               attribute.levels = al,
#'                               n.questions = 12,
#'                               n.versions = 2,
#'                               alternatives.per.question = 2,
#'                               none.alternatives = 2,
#'                               none.positions = 2:3,
#'                               seed = 1)
#' fit.none <- FitChoiceModel(design = des.none,
#'                       hb.iterations = 15, hb.chains = 2,
#'                       hb.warnings = FALSE,
#'                       simulated.priors.from.design = TRUE,
#'                       simulated.sample.size = 100)
#'
#' scen.none <- list("Alt. 1" = c("Att1" = 1, "Att2" = 2, "Att3" = 3),
#'                   "Alt. 2" = c("Att1" = 2, "Att2" = 2, "Att3" = 1),
#'                   "None" = c("Alternative" = "2 (none of these)"))
#' out <- predict(fit.none, scen.none)
#' }
#' @importFrom stats predict
#' @export
predict.FitChoice <- function(object,
                              scenario,
                              rule = c("logit respondent", "logit draw",
                                       "first choice draw", "first choice respondent"),
                              scale = NULL,
                              offset = NULL,
                              ...)
{
    rule <- match.arg(rule)

    is.lca <- object$algorithm != "HB-Stan"
    if (is.lca)
        stop("Prediction with choice models fit using LCA is not currently implemented.")

    ## form attribute list by parsing fit parameter labels
    ## breaks if attr. names or labels have a ": "!!
    ## fix for DS-2056
    par.names <- object$param.names.list$unconstrained.respondent.pars
    attr.list <- makeAttributeList(par.names)


    checkScenario(scenario, par.names, attr.list)

    scenario.par.names <- scenarioToParameterNames(scenario)

    if(rule == "first choice draw" || rule == "logit draw")
    {
        if (is.null(object$beta.draw))
            stop("Rule ", sQuote(rule), " is not available since ",
                 deparse(substitute(object)), " does not contain ",
                 "respondent parameter values for every iteration.",
                 " Either refit the model with ", sQuote("hb.beta.draws.to.keep"),
                 " set to a non-zero value or change the rule to ", dQuote("logit respondent"),
                 " or ", dQuote("first choice respondent"), ".")
        betas <- object$beta.draws
    }else
        betas <- array(object$reduced.respondent.parameters,
                       dim = c(1, dim(object$reduced.respondent.parameters)))
    dimnames(betas)[[3]] <- object$param.names.list$respondent.pars

    utilities <- calcUtilities(betas, scenario.par.names, scale, offset)
    out <- calcPrediction(utilities, rule)
    colnames(out) <- names(scenario)
    return(out)

}

calcPrediction <- function(utilities, rule)
{
    return(switch(tolower(rule),
                  "first choice draw" = calcFCDprobs(utilities),
                  "first choice respondent" = calcFCRprobs(utilities),
                  "logit draw" = calcLDprobs(utilities),
                  "logit respondent" = calcLRprobs(utilities),
                  stop("Invalid rule. ", sQuote(rule), " must be one of ",
                       paste(eval(formals(predict.FitChoice)$rule), ".",
                             collapse = ", "))))
}

#' Probabilities for "first choice draw" rule
#' @param u An array of utilities of dimension
#' \code{num. iterations x num. respondents x num. alternatives}.
#' @return A \code{num. respondents x num. alternatives} matrix
#' of choice probabilites
#' @noRd
calcFCDprobs <- function(u)
{
    n.alt <- dim(u)[3]
    n.iter <- dim(u)[1]
    choices <- apply(u, 1:2, which.max)
    return(t(apply(choices, 2, tabulate, nbins = n.alt))/n.iter)
}

#' probabilities for "first choice respondent" rule
#' @param u An array of utilities of dimension
#' \code{num. iterations x num. respondents x num. alternatives}.
#' @return A \code{num. respondents x num. alternatives} matrix
#' of choice probabilites
#' @noRd
calcFCRprobs <- function(u)
{
    u <- apply(u, 2:3, mean)
    out <- 0*u
    idx <- apply(u, 1, which.max)
    out[cbind(seq_len(nrow(out)), idx)] <- 1
    return(out)
}

#' probabilities for "logit draw" rule
#' @param u An array of utilities of dimension
#' \code{num. iterations x num. respondents x num. alternatives}.
#' @return A \code{num. respondents x num. alternatives} matrix
#' of choice probabilites
#' @noRd
calcLDprobs <- function(u)
{
    probs <- apply(u, 1:2, softmax)
    return(t(apply(probs, c(1, 3), mean)))
}

#' probabilities for "logit respondent draw" rule
#' @param u An array of utilities of dimension
#' \code{num. iterations x num. respondents x num. alternatives}.
#' @return A \code{num. respondents x num. alternatives} matrix
#' of choice probabilites
#' @noRd
calcLRprobs <- function(u)
{
    u <- apply(u, 2:3, mean)
    return(t(apply(u, 1, softmax)))
}


calcUtilities <- function(betas, par.names.list, scale, offset)
{
    n.resp <- dim(betas)[2L]
    n.alt <- length(par.names.list)

    if (!is.null(scale) && (!is.numeric(scale) || length(scale) != n.resp))
        stop(sQuote("scale"), " must have length ", n.resp, ".")

    if (!is.null(offset) && (!is.numeric(offset) || nrow(offset) != n.resp
        || ncol(offset) != n.alt))
        stop(sQuote("offset"), " must be a numeric matrix with ", n.resp, " rows and ",
             n.alt, " columns.")

    out <- array(dim = c(dim(betas)[1L], n.resp, n.alt))
    for (i in seq_len(n.alt))
        out[, , i] <- calcAlternativeUtilities(betas, par.names.list[[i]], scale, offset)

    if (!is.null(scale) || !is.null(offset))
        out <- applyOffsetAndScale(out, scale, offset)

    return(out)
}

applyOffsetAndScale <- function(u, scale, offset)
{
    n.resp <- dim(u)[2L]
    n.alt <- dim(u)[3L]
    if (is.null(offset))
        offset <- matrix(0, n.resp, n.alt)
    if (is.null(scale))
        scale <- rep.int(1L, n.resp)
    for (i in seq_len(dim(u)[1L]))
        u[i, , ] <- offset + u[i, , ]*scale
    return(u)
}

calcAlternativeUtilities <- function(betas, par.names, scale, offset)
{
    return(apply(betas[, , dimnames(betas)[[3]] %in% par.names, drop = FALSE],
                 1:2, sum))
}

makeAttributeList <- function(par.names)
{
    attr.list <- strsplit(par.names, ": ")
    attributes <- vapply(attr.list, `[`, "", 1L)
    attr.names <- unique(attributes)
    n.levels <- vapply(attr.names, function(a) sum(attributes == a), 0L)
    has.asc <- attr.names[1] == "Alternative"
    level.names <- vapply(attr.list, `[`, "", 2L)
    attr.list <- vector("list", length(n.levels))
    names(attr.list) <- attr.names
    cs <- c(0, cumsum(n.levels))
    for (i in seq_along(attr.list))
        attr.list[[i]] <- level.names[(cs[i]+1):cs[i+1]]

    return(attr.list)
}

scenarioToParameterNames <- function(scenario)
{
    lapply(scenario,
           function(alternative) paste(names(alternative), alternative, sep = ": "))
}
## predict.FitChoice <- function(object, data,  n.reps = 10000, ...)
## {
##     if (missing(data))
##         data <- object$processed.data
##     n.respondents <- length(data$n.questions.left.in)
##     #    resp.pars <- result$reduced.respondent.parameters[dat$subset, ]

##     n.rs <- dim(data$X.in)[1]  # n.q*n.resp
##     n.alternatives <- dim(data$X.in)[2]
##     n.questions <- data$n.questions.left.in[1L]
##     is.q.const <- length(unique(data$n.questions.left.in)) == 1L
##     if (!is.q.const)
##         stop("Number of questions per respondent needs to be constant")

##     stan.fit <- object$stan.fit
##     resp.pars <- extract(stan.fit, pars = "beta")[[1L]]

##     ## in.sample.accuracies <- rep(NA, n.respondents)
##     y.pred <- matrix(nrow = n.respondents, ncol = n.questions)
##     y.rep <- matrix(nrow = n.reps, ncol = n.rs)
##     if ("theta" %in% stan.fit@model_pars)
##         pars <- "theta"
##     else
##         pars <- c("resp_fixed_coef", "resp_rand_eff")


##     mean.par.samps <- extract(stan.fit, pars = pars, permuted = TRUE,
##                               inc_warmup = FALSE)[[1L]]
##     mean.new.samps <- lapply(mean.par.samps, function(x) apply(x, 2:3, sample,
##                                                                replace = TRUE, size = n.reps))
##     ## new.dat <- processCovariateData(object$processed.data,
##     ##                                 object$n.classes,
##     ##                                 object$processed.data$cov.formula,
##     ##                                 data$cov.data)
##     ## design matrices for the covariates are already formed since we
##     ##   fit the model to all respondents and only hold out a subset of
##     ##   questions for each respondent

##     rs <- 1
##     for (i in 1:n.respondents)
##     {
##         pars <- resp.pars[, i, ]
##         pars <- apply(pars, 2, sample, size = n.reps, replace = TRUE)
##         ## n.questions <- data$n.questions.left.in[i]
##         ## score <- rep(NA, n.questions)
##         for (j in 1:n.questions)
##         {
##             ## u <- rep(NA, n.alternatives)
##             lp <- tcrossprod(pars, data$X.in[rs, , ])
##             probs <- t(apply(lp, 1, flipChoice::softmax))
##             y.preds <- apply(probs, 1, which.max)
##             y.pred[i, j] <- which.max(table(y.preds))
##             ## for (k in 1:n.alternatives)
##             ##     u[k] <- sum(pars * dat$X.in[rs, k, ])

##             ## score[j] <- if(which.max(u) == dat$Y.in[rs]) 1 else 0
##             y.rep[, rs] <- y.preds
##             rs <- rs + 1
##         }
##         ## in.sample.accuracies[i] <- mean(score)
##     }

##     ## w <- dat$weights
##     ## result$in.sample.accuracy <- sum(in.sample.accuracies * w) / sum(w)

##     ## if (n.questions.left.out > 0)
##     ## {
##     ##     out.sample.accuracies <- rep(NA, n.respondents)
##     ##     rs <- 1
##     ##     for (i in 1:n.respondents)
##     ##     {
##     ##         pars <- resp.pars[i, ]
##     ##         score <- rep(NA, n.questions.left.out)
##     ##         for (j in 1:n.questions.left.out)
##     ##         {
##     ##             u <- rep(NA, n.alternatives)
##     ##             for (k in 1:n.alternatives)
##     ##                 u[k] <- sum(pars * dat$X.out[rs, k, ])
##     ##             score[j] <- if(which.max(u) == dat$Y.out[rs]) 1 else 0
##     ##             rs <- rs + 1
##     ##         }
##     ##         out.sample.accuracies[i] <- mean(score)
##     ##     }
##     ##     result$prediction.accuracies <- rep(NA, length(dat$subset))
##     ##     result$prediction.accuracies[dat$subset] <- out.sample.accuracies
##     ##     result$out.sample.accuracy <- sum(out.sample.accuracies * w) / sum(w)
##     ## }
##     ## else
##     ## {
##     ##     result$prediction.accuracies <- rep(NA, length(dat$subset))
##     ##     result$prediction.accuracies[dat$subset] <- in.sample.accuracies
##     ##     result$out.sample.accuracy <- NA
##     ## }
##     ## result
##     list(y.rep = y.rep, y.pred = y.pred)
## }


checkScenario <- function(scenario, par.names, attr.list)
{
    error.msgs <- lapply(scenario, checkValidAlternative, par.names, attr.list)
    bad.idx <- which(error.msgs != "")
    if (length(bad.idx))
    {
        scen.alt.names <- names(scenario)
        if (length(idx <- which(scen.alt.names == "")))
            scen.alt.names[idx] <- paste0("Alternative", idx)
        error.msg <- paste(paste0("Invalid scenario:\n", scen.alt.names[bad.idx], " - ",
                                  error.msgs[bad.idx]), collapse = "\n")
        stop(error.msg, call. = FALSE)
    }
    ## for (i in which(bad != 0))
    ## {
    ##     if (bad[i] == 1)
    ##     {  # attribute names bad
    ##         alt.name <- scen.alt.names[i]
    ##         valid.attr <- paste(alt.names, collapse = ", ")
    ##         if (alt.name == "")
    ##             alt.name <- paste0("Alternative ", i)

    ##         stop("Invalid scenario. ", alt.name, " contains an invalid attribute.",
    ##              " All attributes must be identical to one of: ", valid.attr)
    ##     }else
    ##     {  # levels bad

    ##     }
    ## }
    return(invisible())
}

checkValidAlternative <- function(alternative, par.names, attr.list)
{
    alt.attr.names <- tolower(names(alternative))
    attr.names <- tolower(names(attr.list))
    bad.idx <- which(!alt.attr.names %in% attr.names)
    if (any(bad.idx))
        return(paste0(paste(sQuote(alt.attr.names[bad.idx]), collapse = ","),
                      " needs to be one of ",
                      paste(sQuote(attr.names), collapse = ", "), "."))

    new.names <- paste(names(alternative), alternative, sep = ": ")
    bad.idx <- which(!tolower(new.names) %in% tolower(par.names))
    if (any(bad.idx))
    {
        msgs <- vapply(bad.idx, function(i){
            idx <- grep(names(alternative)[i], names(attr.list), ignore.case = TRUE)
            paste0(sQuote(alternative[i]), " needs to be one of ",
                   paste0(sQuote(attr.list[[idx]]), collapse = ", "),
                   ".")
          }, "")
        return(paste(msgs, collapse = " "))
    }
    return("")
}

## scenarioToDesign <- function(object, scenario)
## {
##     par.names <- object$param.names.list$unconstrained.respondent.pars
##     ## fix for DS-2056; breaks if attr. names or labels have a ": "
##     attr.list <- strsplit(par.names, ": ")
##     attr.names <- unique(vapply(attr.list, `[`, "", 1L))
##     has.asc <- attr.names[1] <- "Alternative"
##     level.names <- vapply(attr.list, `[`, "", 2L)
##     checkScenario()

## }

designToXmat <- function(object, design)
{
    odat <- object$processed.data
    n.att <- odat$n.attributes
    n.par <- odat$n.parameters

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
}
