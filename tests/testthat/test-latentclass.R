context("Latent class analysis")

tol <- 0.00001

data(eggs, package = "flipChoice")

test_that("LCA", {
    result <- FitChoiceModel(experiment.data = eggs.data, algorithm = "LCA")
    expect_true(abs(result$log.likelihood - (-2926.5237873149)) < tol)
    expect_true(abs(result$bic - 5930.26980091517) < tol)
    par.stats <- ParameterStatisticsLCA(result)
    expect_true(abs(par.stats[1, 1] - 0.072126523778819) < tol)
    expect_true(abs(par.stats[1, 2] - 0.0476143774410454) < tol)
    expect_true(abs(par.stats[1, 3] - 1.51480556199908) < tol)
    expect_true(abs(par.stats[1, 4] - 0.129926058794661) < tol)
})

test_that("LCA 2-classes", {
    result <- FitChoiceModel(experiment.data = eggs.data, algorithm = "LCA",
                             n.classes = 2)
    expect_true(abs(result$log.likelihood - (-2434.12103185236)) < tol)
    expect_true(abs(result$bic - 5028.62668752817) < tol)
    par.stats <- ParameterStatisticsLCA(result)
    expect_true(abs(par.stats[2, 1] - 0.172523477969344) < tol)
    expect_true(abs(par.stats[2, 2] - 0.0775289153359638) < tol)
    expect_true(abs(par.stats[2, 3] - 2.22527913903775) < tol)
    expect_true(abs(par.stats[2, 4] - 0.0261362615204639) < tol)
})

test_that("LCA filtered", {
    subset <- rep(FALSE, 380)
    subset[1:100] <- TRUE
    attr(subset, "label") <- "first 100"
    result <- FitChoiceModel(experiment.data = eggs.data, algorithm = "LCA",
                             subset = subset)
    expect_true(abs(result$log.likelihood - (-749.25502715472)) < tol)
    expect_true(abs(result$bic - 1558.37726672729) < tol)
    par.stats <- ParameterStatisticsLCA(result)
    expect_true(abs(par.stats[1, 1] - (-0.0505552970194017)) < tol)
    expect_true(abs(par.stats[1, 2] - 0.0933462112015716) < tol)
    expect_true(abs(par.stats[1, 3] - (-0.54158916970109)) < tol)
    expect_true(abs(par.stats[1, 4] - 0.588254839013794) < tol)
    expect_equal(dim(result$respondent.parameters), c(380L, 20L))
    expect_equal(dim(result$reduced.respondent.parameters), c(380L, 13L))
    expect_equal(length(result$prediction.accuracies), 380)
})

test_that("LCA filtered and weighted", {
    subset <- rep(FALSE, 380)
    subset[1:100] <- TRUE
    attr(subset, "label") <- "first 100"
    weights <- structure(c(7,6,3,6,5,3,3,2,3,4,9,9,8,7,8,4,4,
                          4,5,5,4,9,3,3,6,2,6,2,8,8,7,4,9,7,4,3,9,3,
                          4,9,9,3,2,2,8,10,3,5,5,4,3,8,5,10,10,8,4,3,
                          6,5,2,4,2,4,9,3,8,3,3,3,3,6,2,10,3,2,8,3,
                          2,6,2,8,5,5,4,4,3,8,6,9,5,8,4,3,5,8,8,5,9,
                          6,4,5,9,5,5,10,9,3,2,5,6,6,10,9,3,2,9,5,6,
                          6,4,9,3,4,5,10,9,7,4,2,2,9,6,4,2,6,2,8,2,
                          2,2,7,3,4,4,4,8,5,6,3,3,9,8,6,5,5,5,3,3,7,
                          9,3,8,2,4,5,8,10,9,2,4,2,8,6,8,3,8,6,9,3,
                          8,6,5,8,3,8,2,3,3,4,5,8,3,8,3,9,2,5,2,8,4,
                          8,6,3,3,7,6,8,3,3,8,5,8,8,9,5,5,6,9,10,3,
                          2,5,7,6,2,3,5,3,2,5,9,3,4,8,10,3,2,4,8,10,
                          10,3,2,2,3,4,2,2,2,6,8,4,8,9,3,9,7,8,3,9,
                          4,8,2,2,2,4,2,6,3,4,3,9,7,6,2,3,3,4,2,3,3,
                          2,6,4,3,3,3,5,3,3,7,7,4,9,7,5,3,2,5,4,3,6,
                          8,4,8,8,7,6,8,9,2,10,4,7,3,8,5,10,4,2,4,4,
                          2,3,8,4,9,2,8,3,3,8,3,2,5,2,2,3,2,8,3,8,5,
                          6,6,3,7,8,3,2,8,4,4,6,8,8,8,3,9,3,4,3,4,3,
                          10,3,9,3,7,3,10,6,4,2,2,4,4,3,9),
                        name = "Q15", label = "Age")
    attr(weights, "label") <- "Age"
    result <- FitChoiceModel(experiment.data = eggs.data, algorithm = "LCA",
                             subset = subset, weights = weights)
    expect_true(abs(result$log.likelihood - (-613.885430765335)) < tol)
    expect_true(abs(result$bic - 1285.11048561294) < tol)
    par.stats <- ParameterStatisticsLCA(result)
    expect_true(abs(par.stats[1, 1] - (-0.10873901320569)) < tol)
    expect_true(abs(par.stats[1, 2] - 0.103080168378728) < tol)
    expect_true(abs(par.stats[1, 3] - (-1.05489751245042)) < tol)
    expect_true(abs(par.stats[1, 4] - (0.291795935221641)) < tol)
})
