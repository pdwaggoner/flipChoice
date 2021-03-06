context("Hierarchical Bayes")

data(eggs, package = "flipChoice")

test_that("HB", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, hb.warnings = FALSE,
                             hb.beta.draws.to.keep = 2)
    expect_error(print(result), NA)
    expect_equal(dim(result$beta.draws), c(2L, 380L, 13L))

    # If the number below needs to be increased due to additional outputs,
    # ensure that the output size does not get too big when there are multiple
    # classes and many iterations.
    expect_true(as.numeric(object.size(result)) < 1245000)
    # Add the following option to expect_true to print out the size in Travis
    # info = print(as.numeric(object.size(result))))
})

test_that("HB cross validation", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, tasks.left.out = 2,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("HB filter", {
    sub <- rep(FALSE, nrow(eggs.data))
    sub[1:100] <- TRUE
    attr(sub, "label") <- "first 100"
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, subset = sub, hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("HB weights", {
    wgt <- 1:length(nrow(eggs.data))
    expect_error(FitChoiceModel(experiment.data = eggs.data,
                                hb.iterations = 10, hb.chains = 1,
                                weights = wgt, hb.warnings = FALSE),
                 "Weights are not able to be applied for Hierarchical Bayes.")
})

test_that("HB 2 classes", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, n.classes = 2, hb.warnings = FALSE)
    expect_error(print(result), NA)

    ExtractParameterStats(result)
    PlotPosteriorIntervals(result)
    TracePlots(result)
})

test_that("HB diagonal", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, normal.covariance = "Diagonal",
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("HB diagonal 2 classes", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, normal.covariance = "Diagonal",
                             n.classes = 2, hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("HB prior attributes", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1,
                             hb.prior.mean = c(0, -10, 0, 0, 0, 0, 0, 20),
                             hb.prior.sd = rep(0.1, 8), hb.warnings = FALSE)
    expect_error(print(result), NA)
    # Weight parameter priors forced to be negative
    expect_equal(all(result$parameter.statistics[3:5, 1] < 0), TRUE)
    # Weight parameter priors decrease monotonically (ordered)
    expect_equal(all(diff(result$parameter.statistics[3:5, 1]) < 0), TRUE)
    # Price parameter prior forced to be positive
    expect_equal(result$parameter.statistics[13, 1] > 0, TRUE)
})

test_that("HB prior parameters", {
    prior.mean <- c(0, 0, 0, 0, 0, 0, 0, 0, -10, 0, 0, 0, 0)
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1,
                             hb.prior.mean = prior.mean,
                             hb.prior.sd = rep(0.1, 8), hb.warnings = FALSE)
    expect_error(print(result), NA)

    # Free range parameter prior forced to be negative
    expect_equal(result$parameter.statistics[9, 1] < 0, TRUE)

    expect_error(FitChoiceModel(experiment.data = eggs.data,
                                hb.iterations = 10,
                                hb.chains = 1,
                                hb.prior.mean = prior.mean,
                                hb.prior.sd = 1:7, hb.warnings = FALSE),
                 paste0("The supplied parameter hb.prior.sd is inappropriate. ",
                        "Based on the input data this needs to be a numeric ",
                        "vector of length 1, 8 \\(number of attributes\\)",
                        " or 13 \\(number of parameters\\)."))

    expect_error(FitChoiceModel(experiment.data = eggs.data,
                                hb.iterations = 10,
                                hb.chains = 1,
                                hb.prior.mean = prior.mean,
                                hb.prior.sd = 0:7, hb.warnings = FALSE),
             paste0("All prior standard deviations must be greater than 0."))
})

test_that("HB with fixed covariates", {
    data("eggs.cov", package = "flipChoice")
    result <- FitChoiceModel(experiment.data = eggs.data,
                             cov.formula = ~gender, cov.data = eggs.cov,
                             hb.iterations = 10,
                             hb.chains = 1, hb.warnings = FALSE)
    expect_error(print(result), NA)

    stat.names <- rownames(result$parameter.statistics)
    expect_equal(sum(grepl("Intercept", stat.names)),
                 sum(grepl("St. Dev", stat.names)))
    expect_equal(sum(grepl("gender", stat.names)),
                 sum(grepl("St. Dev", stat.names)))

    ExtractParameterStats(result)
    PlotPosteriorIntervals(result)
    TracePlots(result)
})

test_that("HB with grouped covariates", {
    data("cruise", package = "flipChoice")
    data("cruise.design", package = "flipChoice")

    choices <- cruise[, grep("choice", colnames(cruise))]
    tasks <- cruise[, grep("task", colnames(cruise))]

    # frml.fc <- ~TopDestination
    frml <- ~(1|TopDestination)
    data("eggs.cov", package = "flipChoice")
    result <- FitChoiceModel(design = cruise.design,
                             choices = choices,
                             questions = tasks,
                             cov.formula = frml, cov.data = cruise,
                             n.classes = 1, hb.iterations = 10,
                             hb.chains = 1, hb.warnings = FALSE)
    expect_error(print(result), NA)

    ExtractParameterStats(result)
    PlotPosteriorIntervals(result)
    TracePlots(result)
})


test_that("HB with multi-class covariates", {
    data("eggs.cov", package = "flipChoice")
    result <- FitChoiceModel(experiment.data = eggs.data,
                             cov.formula = ~gender, cov.data = eggs.cov,
                             hb.iterations = 10, n.classes = 2,
                             hb.chains = 1, hb.warnings = FALSE)
    expect_error(print(result), NA)

    ExtractParameterStats(result)
    PlotPosteriorIntervals(result)
    TracePlots(result)
})

test_that("Simulated data", {
    data("eggs.cov", package = "flipChoice")
    simulated.priors <- matrix(c(rep(0, 13), rep(2, 13)), ncol = 2)
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, hb.warnings = FALSE,
                             simulated.priors = simulated.priors)
    expect_error(print(result), NA)
})

test_that("Simulated data no priors supplied", {
    data("eggs.cov", package = "flipChoice")
    simulated.priors <- structure(character(0), .Dim = c(0L, 0L))
    expect_warning(result <- FitChoiceModel(experiment.data = eggs.data,
                                              hb.iterations = 10,
                                              hb.chains = 1,
                                              hb.warnings = FALSE,
                                        simulated.priors = simulated.priors),
                   paste0("No prior for simulated data was entered. ",
                          "The prior mean and standard deviations have ",
                          "been assummed to be zero."))
    expect_error(print(result), NA)
})
