context("Ensemble")

test_that("FitChoice Ensemble", {

    fit <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 100,
                          hb.chains = 1, tasks.left.out = 2)

    expect_error(ChoiceEnsemble(list(fit, fit, fit), compare.only = FALSE), NA)
    expect_error(ChoiceEnsemble(list(fit, fit, fit), compare.only = TRUE), NA)
})
