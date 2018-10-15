context("Ensemble")

test_that("FitChoice Ensemble", {

    fit1 <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                   hb.chains = 1, tasks.left.out = 2,
                   hb.warnings = FALSE, n.classes = 1)
    fit2 <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                   hb.chains = 1, tasks.left.out = 2,
                   hb.warnings = FALSE, n.classes = 2)
    fit3 <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                   hb.chains = 1, tasks.left.out = 2,
                   hb.warnings = FALSE, n.classes = 3)

    expect_error(ChoiceEnsemble(list(fit1, fit2, fit3), compare.only = FALSE), NA)
    expect_error(ChoiceEnsemble(list(fit1, fit2, fit3), compare.only = TRUE), NA)

    data(chocolate)
    data(chocolate.design)
    choices <- chocolate[, grepl("^choice", colnames(chocolate))]
    questions <- chocolate[, grepl("^task", colnames(chocolate))]
    fit4 <- FitChoiceModel(design = chocolate.design,
                             choices = choices,
                             questions = questions,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)

    # cannot ensemble models with different data
    expect_error(ChoiceEnsemble(list(fit1, fit4), compare.only = FALSE),
                 "Models must have the same input data.")
    # can compare models with different data
    expect_error(ChoiceEnsemble(list(fit1, fit4), compare.only = TRUE), NA)
})
