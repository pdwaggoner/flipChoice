context("Data formats")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipChoice", mustWork = TRUE),
              file)
}

cho.file <- findInstDirFile("Training.cho")
cho.none.file <- findInstDirFile("none_option.cho")
cho.missing.file <- findInstDirFile("missing.cho")
jmp.design.file <- findInstDirFile("eggs_design.xlsx")
jmp.design.with.levels.file <- findInstDirFile("eggs_design_with_levels.xlsx")

attribute.levels.file.cho <- findInstDirFile("Attribute_labels_-_Training.xlsx")
attribute.levels.file.jmp <- findInstDirFile("eggs_labels.xlsx")

data(eggs, package = "flipChoice")
data(eggs.design, package = "flipChoice")
data(cho.ids, package = "flipChoice")
data(test.design.data, package = "flipChoice")

choices.jmp <- eggs.data[, 1:8]
choices.jmp.none.of.these <- choices.jmp
for (i in 1:8)
{
    v <- as.numeric(choices.jmp.none.of.these[[i]])
    v[1] <- 4
    choices.jmp.none.of.these[[i]] <- as.factor(v)
    levels(choices.jmp.none.of.these[[i]]) <- LETTERS[1:4]
}
tasks.jmp <- data.frame(t(matrix(1:3040, nrow = 8)))

attribute.levels <- list(Att1 = 1:2, Att2 = 1:2, Att3 = 1:3)
test.design <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                                 attribute.levels = attribute.levels,
                                 prior = NULL,
                                 n.questions = 10,
                                 n.versions = 2,
                                 alternatives.per.question = 2,
                                 seed = 1)
test.design.with.prior <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                                 attribute.levels = attribute.levels,
                                 prior = matrix(c(1, -2, 1, 3, 0, 1, 0.5, 1.5), ncol = 2),
                                 n.questions = 10,
                                 n.versions = 2,
                                 alternatives.per.question = 2,
                                 seed = 1)

test.design.none <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                                            attribute.levels = attribute.levels,
                                            prior = matrix(c(1, -2, 1, 3, 0, 1, 0.5, 1.5), ncol = 2),
                                            n.questions = 10,
                                            n.versions = 2,
                                            alternatives.per.question = 2,
                                            none.alternatives = 1,
                                            none.positions = 2,
                                            seed = 1)

test_that("experiment question simulated data", {
    exp.simulated.prior <- structure(c("Alt", "a", "b", "c", "", "mean", "0", "0", "0",
                "", "Weight", "a", "b", "c", "d", "mean", "0", "1", "2", "3",
                "sd", "0", "0.5", "1", "1.5", "Organic", "a", "b", "", "", "mean",
                "0", "1", "", "", "Charity", "a", "b", "", "", "mean", "0", "1",
                "", "", "Quality", "a", "b", "c", "", "Uniformity", "a", "b",
                "", "", "Feed", "a", "b", "c", "", "Price", "a", "", "", "",
                "mean", "-3", "", "", ""), .Dim = c(5L, 14L))
    suppressWarnings(result <- FitChoiceModel(experiment.data = eggs.data,
                                              hb.iterations = 10,
                                              hb.chains = 1,
                                              hb.warnings = FALSE,
                                              hb.beta.draws.to.keep = 2,
                                              simulated.priors = exp.simulated.prior))
    expect_error(print(result), NA)
})

test_that("design object", {
    result <- FitChoiceModel(design = test.design,
                             choices = test.design.data$choices,
                             questions = test.design.data$questions,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("design object simulated data", {
    simulated.priors <- matrix(c(0, 1, -2, 1, 3, 0, 0, 1, 0.5, 1.5), ncol = 2)
    result <- FitChoiceModel(design = test.design,
                             choices = test.design.data$choices,
                             questions = test.design.data$questions,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             simulated.priors = simulated.priors,
                             simulated.sample.size = 1000)
    expect_error(print(result), NA)
})

test_that("design object simulated data without priors", {
    simulated.priors <- matrix(c(0, 1, -2, 1, 3, 0, 0, 1, 0.5, 1.5), ncol = 2)
    expect_warning(result <- FitChoiceModel(design = test.design,
                             choices = test.design.data$choices,
                             questions = test.design.data$questions,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             simulated.priors.from.design = TRUE,
                             simulated.sample.size = 1000),
        paste0("The supplied design does not contain priors. ",
               "The prior mean and standard deviations have been assummed to be zero."))
    expect_error(print(result), NA)
})

test_that("design object simulated data without alternatives", {
    simulated.priors <- matrix(c(0, 1, -2, 1, 3, 0, 0, 1, 0.5, 1.5), ncol = 2)
    result <- FitChoiceModel(design = test.design,
                             choices = test.design.data$choices,
                             questions = test.design.data$questions,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             simulated.priors = simulated.priors[-1, ], # leave out alternative prior
                             simulated.sample.size = 1000)
    expect_error(print(result), NA)
})

test_that("design object simulated data entered priors", {
    expect_warning(result <- FitChoiceModel(design = test.design,
                             choices = test.design.data$choices,
                             questions = test.design.data$questions,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             simulated.priors = test.design.data$simulated.priors,
                             simulated.sample.size = 1000),
        paste0("Prior standard deviations were not supplied for one or more attributes. ",
               "These standard deviations have been assummed to be 0."))
    expect_error(print(result), NA)
})

test_that("design object simulated data partially entered priors", {
    partial.priors <- test.design.data$simulated.priors[, -2:-3] # remove Att1
    expect_warning(result <- FitChoiceModel(design = test.design,
                                            choices = test.design.data$choices,
                                            questions = test.design.data$questions,
                                            hb.iterations = 10, hb.chains = 1,
                                            hb.warnings = FALSE,
                                            simulated.priors = partial.priors,
                                            simulated.sample.size = 1000),
                   paste0("The following attribute\\(s\\) were missing from ",
                          "the priors and are assumed to have means and ",
                          "standard deviations of 0: Att1"))
    expect_error(print(result), NA)
})

test_that("design object simulated data incorrect entered priors", {
    incorrect.priors <- test.design.data$simulated.priors
    incorrect.priors[1, 2] <- "Att99"
    expect_warning(result <- FitChoiceModel(design = test.design,
                                            choices = test.design.data$choices,
                                            questions = test.design.data$questions,
                                            hb.iterations = 10, hb.chains = 1,
                                            hb.warnings = FALSE,
                                            simulated.priors = incorrect.priors,
                                            simulated.sample.size = 1000),
                   paste0("The following attribute\\(s\\) were supplied in ",
                          "the priors but could not be matched to the ",
                          "design: Att99"))
    expect_error(print(result), NA)
})

test_that("design object simulated data priors from design", {
    result <- FitChoiceModel(design = test.design.with.prior,
                             choices = test.design.data$choices,
                             questions = test.design.data$questions,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             simulated.priors.from.design = TRUE,
                             simulated.sample.size = 1000)
    expect_error(print(result), NA)
})


test_that("design object simulated data without choices", {
    simulated.priors <- matrix(c(0, 1, -2, 1, 3, 0, 0, 1, 0.5, 1.5), ncol = 2)
    result <- FitChoiceModel(design = test.design,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             simulated.priors = simulated.priors,
                             simulated.sample.size = 100)
    expect_error(print(result), NA)
})

test_that("design object none alternatives", {
    result <- FitChoiceModel(design = test.design.none,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             simulated.priors.from.design = TRUE,
                             simulated.sample.size = 100)
    expect_error(print(result), NA)
})

test_that("cho file", {
    result <- FitChoiceModel(cho.file = cho.file,
                             attribute.levels.file = attribute.levels.file.cho,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             respondent.ids = respondent.ids)
    expect_error(print(result), NA)
})

test_that("cho none file", {
    result <- FitChoiceModel(cho.file = cho.none.file,
                             attribute.levels.file = attribute.levels.file.cho,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             respondent.ids = respondent.ids)
    expect_error(print(result), NA)
})

test_that("cho file simulated data", {
    simulated.priors <- matrix(c(rep(0, 24), rep(2, 24)), ncol = 2)
    result <- FitChoiceModel(cho.file = cho.file,
                             attribute.levels.file = attribute.levels.file.cho,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             respondent.ids = respondent.ids,
                             simulated.priors = simulated.priors)
    expect_error(print(result), NA)
})

test_that("jmp format", {
    result <- FitChoiceModel(design.file = jmp.design.file,
                             attribute.levels.file = attribute.levels.file.jmp,
                             choices = choices.jmp, questions = tasks.jmp,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("jmp format none of these", {
    result <- FitChoiceModel(design.file = jmp.design.file,
                             attribute.levels.file = attribute.levels.file.jmp,
                             choices = choices.jmp.none.of.these,
                             questions = tasks.jmp,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("jmp format with labels", {
    result <- FitChoiceModel(design.file = jmp.design.with.levels.file,
                             choices = choices.jmp, questions = tasks.jmp,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("jmp format simulated data", {
    simulated.priors <- matrix(c(rep(0, 16), rep(2, 16)), ncol = 2)
    result <- FitChoiceModel(design.file = jmp.design.file,
                             attribute.levels.file = attribute.levels.file.jmp,
                             choices = choices.jmp,
                             questions = tasks.jmp,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             simulated.priors = simulated.priors)
    expect_error(print(result), NA)
})

test_that("Experiment missing data", {
    eggs.data.missing <- eggs.data
    eggs.data.missing[1, 1] <- NA
    expect_error(processExperimentData(experiment.data = eggs.data.missing,
                                 subset = NULL, weights = NULL,
                                 n.questions.left.out = 0,
                                 seed = 123, input.prior.mean = 0,
                                 input.prior.sd = 5,
                                 missing = "Error if missing data",
                                 covariates = NULL, simulated.priors = NULL),
                 paste0("The data contains missing values. ",
                        "Change the 'missing' option to run the analysis."))

    dat <- processExperimentData(experiment.data = eggs.data.missing,
                          subset = NULL, weights = NULL,
                          n.questions.left.out = 0,
                          seed = 123, input.prior.mean = 0,
                          input.prior.sd = 5,
                          missing = "Exclude cases with missing data",
                          covariates = NULL, simulated.priors = NULL)
    expect_equal(dat$n.respondents, 379)
    expect_equal(dim(dat$X.in), c(3032, 3, 13))
    expect_equal(length(dat$Y.in), 3032)
    expect_equal(dat$X.in[1, 1:3, 1:5], structure(c(0, 1, 0, 0, 0,
                                                    1, 0, 0, 0, 0,
                                                    0, 0, 0, 1, 0),
                                                  .Dim = c(3L, 5L)))
    expect_equal(dat$Y.in[1:16], c(3, 3, 2, 2, 2, 2, 3, 3,
                                   3, 2, 3, 3, 2, 3, 2, 3))

    dat <- processExperimentData(experiment.data = eggs.data.missing,
                                 subset = NULL, weights = NULL,
                                 n.questions.left.out = 0,
                                 seed = 123, input.prior.mean = 0,
                                 input.prior.sd = 5,
                                 missing = "Use partial data",
                                 covariates = NULL, simulated.priors = NULL)
    expect_equal(dat$n.respondents, 380)
    expect_equal(dim(dat$X.in), c(3039, 3, 13))
    expect_equal(dat$X.in[1, 1:3, 1:5], structure(c(0, 1, 0, 0, 0,
                                                    1, 0, 0, 1, 0,
                                                    0, 0, 1, 0, 0),
                                                  .Dim = c(3L, 5L)))
    expect_equal(dat$Y.in[1:16], c(3, 3, 1, 1, 2, 1, 1, 3,
                                   3, 2, 2, 2, 2, 3, 3, 3))
    expect_equal(dat$n.questions.left.in[1:5], c(7, 8, 8, 8, 8))

    dat <- processExperimentData(experiment.data = eggs.data.missing,
                                 subset = NULL, weights = NULL,
                                 n.questions.left.out = 1,
                                 seed = 123, input.prior.mean = 0,
                                 input.prior.sd = 5,
                                 missing = "Use partial data",
                                 covariates = NULL, simulated.priors = NULL)
    expect_equal(dat$Y.in[1:14], c(3, 3, 1, 2, 1, 1, 3,
                                   3, 2, 2, 2, 2, 3, 3))
    expect_equal(dat$n.questions.left.in[1:5], c(6, 7, 7, 7, 7))
})

test_that("Design file missing data", {
    choices.jmp.missing <- choices.jmp
    tasks.jmp.missing <- tasks.jmp
    choices.jmp.missing[1, 1] <- NA
    tasks.jmp.missing[2, 1] <- NA

    expect_error(processDesignFile(design.file = jmp.design.file,
                      attribute.levels.file = attribute.levels.file.jmp,
                      choices = choices.jmp.missing,
                      questions = tasks.jmp.missing,
                      subset = NULL, weights = NULL,
                      n.questions.left.out = 0,
                      seed = 123, input.prior.mean = 0, input.prior.sd = 5,
                      include.choice.parameters = TRUE,
                      missing = "Error if missing data",
                      covariates = NULL, simulated.priors = NULL,
                      simulated.sample.size = 100),
                 paste0("The data contains missing values. ",
                        "Change the 'missing' option to run the analysis."))

    dat <- processDesignFile(design.file = jmp.design.file,
                      attribute.levels.file = attribute.levels.file.jmp,
                      choices = choices.jmp.missing,
                      questions = tasks.jmp.missing,
                      subset = NULL, weights = NULL,
                      n.questions.left.out = 0,
                      seed = 123, input.prior.mean = 0, input.prior.sd = 5,
                      include.choice.parameters = TRUE,
                      missing = "Exclude cases with missing data",
                      covariates = NULL, simulated.priors = NULL,
                      simulated.sample.size = 100)
    expect_equal(dat$n.respondents, 378)
    expect_equal(dim(dat$X.in), c(3024, 3, 16))
    expect_equal(dat$X.in[1, 1:3, 1:5], structure(c(0, 1, 0, 0, 0,
                                                    1, 0, 1, 0, 0,
                                                    0, 0, 1, 0, 1),
                                                  .Dim = c(3L, 5L)))
    expect_equal(dat$Y.in[1:8], c(3, 2, 3, 3, 2, 3, 2, 3))

    dat <- processDesignFile(design.file = jmp.design.file,
                             attribute.levels.file = attribute.levels.file.jmp,
                             choices = choices.jmp.missing,
                             questions = tasks.jmp.missing,
                             subset = NULL, weights = NULL,
                             n.questions.left.out = 0,
                             seed = 123, input.prior.mean = 0, input.prior.sd = 5,
                             include.choice.parameters = TRUE,
                             missing = "Use partial data",
                             covariates = NULL, simulated.priors = NULL,
                             simulated.sample.size = 100)
    expect_equal(dat$n.respondents, 380)
    expect_equal(dat$X.in[1, 1:3, 1:5], structure(c(0, 1, 0, 0, 0,
                                                    1, 0, 0, 1, 0,
                                                    0, 0, 1, 0, 0),
                                                  .Dim = c(3L, 5L)))
    expect_equal(dat$Y.in[1:10], c(3, 3, 1, 1, 2, 1, 1, 3, 2, 2))
    expect_equal(dat$n.questions.left.in[1:5], c(7, 7, 8, 8, 8))
})

test_that("CHO file missing data", {
    expect_error(processChoFile(cho.file = cho.missing.file,
                          attribute.levels.file = attribute.levels.file.cho,
                          subset = NULL, weights = NULL,
                          n.questions.left.out = 0, seed = 123,
                          input.prior.mean = 0, input.prior.sd = 5,
                          include.choice.parameters = TRUE,
                          respondent.ids = respondent.ids,
                          missing = "Error if missing data",
                          covariates = NULL, simulated.priors = NULL),
                 paste0("The data contains missing values. ",
                        "Change the 'missing' option to run the analysis."))

    dat <- processChoFile(cho.file = cho.missing.file,
                          attribute.levels.file = attribute.levels.file.cho,
                          subset = NULL, weights = NULL,
                          n.questions.left.out = 0, seed = 123,
                          input.prior.mean = 0, input.prior.sd = 5,
                          include.choice.parameters = TRUE,
                          respondent.ids = respondent.ids,
                          missing = "Exclude cases with missing data",
                          covariates = NULL, simulated.priors = NULL)
    expect_equal(dat$n.respondents, 599)
    expect_equal(dim(dat$X.in), c(8984L, 4L, 24L))
    expect_equal(dat$X.in[1, , 1:8], structure(c(0, 1, 0, 0, 0, 0, 1, 0,
                                                 0, 0, 0, 1, 0, 1, 0, 1,
                                                 0, 0, 0, 0, 0, 0, 0, 0,
                                                 0, 0, 1, 0, 1, 0, 0, 0),
                                               .Dim = c(4L, 8L)))
    expect_equal(dat$X.in[15, , 1:8], structure(c(0, 1, 0, 0, 0, 0, 1, 0,
                                                  0, 0, 0, 1, 1, 0, 0, 0,
                                                  0, 0, 1, 0, 0, 0, 0, 0,
                                                  0, 0, 0, 0, 0, 1, 0, 0),
                                                .Dim = c(4L, 8L)))
    expect_equal(dat$n.questions.left.in[1:5], c(14, 15, 15, 15, 15))

    expect_equal(dat$Y.in[1:29], c(3, 2, 1, 4, 2, 2, 1, 1, 2, 2, 3, 1, 4, 4,
                                2, 2, 2, 1, 4, 3, 3, 3, 3, 2, 1, 4, 2, 3, 2))

    dat <- processChoFile(cho.file = cho.missing.file,
                          attribute.levels.file = attribute.levels.file.cho,
                          subset = NULL, weights = NULL,
                          n.questions.left.out = 0, seed = 123,
                          input.prior.mean = 0, input.prior.sd = 5,
                          include.choice.parameters = TRUE,
                          respondent.ids = respondent.ids,
                          missing = "Use partial data",
                          covariates = NULL, simulated.priors = NULL)
    expect_equal(dat$n.respondents, 600)
    expect_equal(dim(dat$X.in), c(8998L, 4L, 24L))
    expect_equal(dat$X.in[1, , 1:8], structure(c(0, 1, 0, 0, 0, 0, 1, 0,
                                                 0, 0, 0, 1, 0, 1, 0, 1,
                                                 0, 0, 0, 0, 0, 0, 0, 0,
                                                 0, 0, 1, 0, 1, 0, 0, 0),
                                               .Dim = c(4L, 8L)))
    expect_equal(dat$X.in[15, , 1:8], structure(c(0, 1, 0, 0, 0, 0, 1, 0,
                                                  0, 0, 0, 1, 0, 0, 0, 0,
                                                  0, 1, 0, 0, 0, 0, 0, 0,
                                                  0, 0, 0, 1, 1, 0, 1, 0),
                                                .Dim = c(4L, 8L)))
    expect_equal(dat$Y.in[1:28], c(3, 2, 1, 4, 2, 2, 1, 1, 2, 2, 3, 1, 4, 4,
                                   1, 1, 1, 3, 4, 1, 2, 3, 3, 2, 1, 2, 2, 2))
    expect_equal(dat$n.questions.left.in[1:5], c(14, 14, 15, 15, 15))
})

data("eggs.cov", package = "flipChoice")

test_that("cho file with fixed covariates", {
    f <- as.factor(rbinom(600, 1, .5))
    result <- FitChoiceModel(cho.file = cho.file,
                             cov.formula = ~f,
                             attribute.levels.file = attribute.levels.file.cho,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
    stat.names <- rownames(result$parameter.statistics)
    expect_equal(sum(grepl("Intercept", stat.names)),
                 sum(grepl("St. Dev", stat.names)))
    expect_equal(sum(grepl("f1__", stat.names)),
                 sum(grepl("St. Dev", stat.names)))
})

test_that("jmp format with fixed covariates", {
    result <- FitChoiceModel(design.file = jmp.design.file,
                             cov.formula = ~egg.choice2, cov.data = eggs.cov,
                             attribute.levels.file = attribute.levels.file.jmp,
                             choices = choices.jmp, questions = tasks.jmp,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
    stat.names <- rownames(result$parameter.statistics)
    expect_equal(sum(grepl("Intercept", stat.names)),
                 sum(grepl("St. Dev", stat.names)))
    expect_equal(sum(grepl("egg.choice2", stat.names)),
                 sum(grepl("St. Dev", stat.names)))
})

test_that("Chocolate experiment choice model",
{
    data(chocolate)
    data(chocolate.design)
    choices <- chocolate[, grepl("^choice", colnames(chocolate))]
    questions <- chocolate[, grepl("^task", colnames(chocolate))]
    result <- FitChoiceModel(design = chocolate.design,
                             choices = choices,
                             questions = questions,
                             cov.formula = ~gender, cov.data = chocolate,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             simulated.priors.from.design = FALSE,
                             simulated.sample.size = 1000)
    expect_error(print(result), NA)
})

test_that("Fast food experiment choice model",
{
    data(fast.food)
    data(fast.food.design)
    choices <- fast.food[, grepl("^choice", colnames(fast.food))]
    questions <- fast.food[, grepl("^task", colnames(fast.food))]
    result <- FitChoiceModel(design = fast.food.design,
                             choices = choices,
                             questions = questions,
                             cov.formula = ~diabetes, cov.data = fast.food,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             simulated.priors.from.design = FALSE,
                             simulated.sample.size = 1000)
    expect_error(print(result), NA)
})
