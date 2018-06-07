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

test_that("design object" {
    result <- FitChoiceModel(design = eggs.design,
                             choices = choices.jmp, questions = tasks.jmp,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
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

test_that("Experiment missing data", {
    eggs.data.missing <- eggs.data
    eggs.data.missing[1, 1] <- NA
    expect_error(processExperimentData(experiment.data = eggs.data.missing,
                                 subset = NULL, weights = NULL,
                                 n.questions.left.out = 0,
                                 seed = 123, input.prior.mean = 0,
                                 input.prior.sd = 5,
                                 missing = "Error if missing data",
                                 covariates = NULL),
                 paste0("The data contains missing values. ",
                        "Change the 'missing' option to run the analysis."))

    dat <- processExperimentData(experiment.data = eggs.data.missing,
                          subset = NULL, weights = NULL,
                          n.questions.left.out = 0,
                          seed = 123, input.prior.mean = 0,
                          input.prior.sd = 5,
                          missing = "Exclude cases with missing data",
                          covariates = NULL)
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
                                 covariates = NULL)
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
                                 covariates = NULL)
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
                      covariates = NULL),
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
                      covariates = NULL)
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
                             covariates = NULL)
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
                          covariates = NULL),
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
                          covariates = NULL)
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
                          covariates = NULL)
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
