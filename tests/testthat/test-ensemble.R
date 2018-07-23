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
})
