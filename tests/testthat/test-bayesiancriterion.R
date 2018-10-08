attribute.levels <- list(Att1 = 1:2, Att2 = 1:2, Att3 = 1:2,
                         Att4 = 1:4, Att5 = 1:4)

result <- ChoiceModelDesign(design.algorithm = "Random",
                            attribute.levels = attribute.levels,
                            prior = NULL,
                            n.questions = 18,
                            n.versions = 1,
                            alternatives.per.question = 2,
                            seed = 2)

prior <- matrix(c(-4:4, 0.5 * (1:9)), ncol = 2)
design <- encodeDesign(result$design[,-1:-4], effects = FALSE)

test_that("Quadrature integration vs monte carlo", {
    quadrature.bc <- quadratureBayesianCriterion(design, prior, 18, 2, 10, 1)
    monte.carlo.bc <- monteCarloBayesianCriterion(design, prior, 18,
                                                  2, 1000, 1)
    expect_true(abs((quadrature.bc - monte.carlo.bc) / monte.carlo.bc) < 0.01)
})

prior.2 <- prior
prior.2[1:8, 2] <- 0

test_that("Quadrature integration with zero variances vs monte carlo", {
    quadrature.bc <- quadratureBayesianCriterion(design, prior.2, 18, 2, 10, 1)
    monte.carlo.bc <- monteCarloBayesianCriterion(design, prior.2, 18,
                                                  2, 1000, 1)
    expect_true(abs((quadrature.bc - monte.carlo.bc) / monte.carlo.bc) < 0.01)
})

prior.3 <- prior
prior.3[1:8, 2] <- 0.000001

test_that("Quadrature integration with zero variances vs near zero variances", {
    quadrature.bc.zero <- quadratureBayesianCriterion(design, prior.2, 18,
                                                      2, 10, 1)
    quadrature.bc.near.zero <- quadratureBayesianCriterion(design, prior.3,
                                                           18, 2, 10, 1)
    expect_true(abs((quadrature.bc.zero - quadrature.bc.near.zero) /
                        quadrature.bc.zero) < 0.02)
})

test_that("Quadrature integration optimization speedup", {
    ptm <- proc.time()
    quadratureBayesianCriterion(design, prior.2, 18, 2, 10, 1)
    opt.time <- proc.time() - ptm

    ptm <- proc.time()
    quadratureBayesianCriterion(design, prior.3, 18, 2, 10, 1)
    orig.time <- proc.time() - ptm

    expect_true(orig.time[3] > 10 * opt.time[3])
})
