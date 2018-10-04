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

test_that("Quadrature integration vs monte carlo",
    expect_equal(quadratureBayesianCriterion(design, prior, 18, 2, 10, 1),
                 monteCarloBayesianCriterion(design, prior, 18, 2, 1000, 1),
                 tolerance = 0.2))

prior.2 <- prior
prior.2[1:8, 2] <- 0

test_that("Quadrature integration with zero variances vs monte carlo",
          expect_equal(quadratureBayesianCriterion(design, prior.2, 18, 2, 10, 1),
                       monteCarloBayesianCriterion(design, prior.2, 18, 2, 1000, 1),
                       tolerance = 0.2))

prior.3 <- prior
prior.3[1:8, 2] <- 0.000001

test_that("Quadrature integration with zero variances vs near zero variances",
          expect_equal(quadratureBayesianCriterion(design, prior.2, 18, 2, 10, 1),
                       quadratureBayesianCriterion(design, prior.3, 18, 2, 10, 1),
                       tolerance = 0.2))

test_that("Quadrature integration optimization speedup", {
    ptm <- proc.time()
    quadratureBayesianCriterion(design, prior.2, 18, 2, 10, 1)
    opt.time <- proc.time() - ptm

    ptm <- proc.time()
    quadratureBayesianCriterion(design, prior.3, 18, 2, 10, 1)
    orig.time <- proc.time() - ptm

    expect_true(orig.time[3] > 10 * opt.time[3])
})
