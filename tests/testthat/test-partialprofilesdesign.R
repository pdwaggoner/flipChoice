context("Partial profiles")

attribute.levels <- list(Att1 = 1:2, Att2 = 1:2, Att3 = 1:2,
                         Att4 = 1:4, Att5 = 1:4)

test_that("Utility neutral integrated algorithm",
{
    result <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                                attribute.levels = attribute.levels,
                                prior = NULL,
                                n.questions = 18,
                                n.versions = 1,
                                alternatives.per.question = 2,
                                n.constant.attributes = 3,
                                seed = 1)
    expect_equal(result$d.criterion, 1.22245332640841)
    expect_equal(result$d.error, 0.872992645672795)
})

test_that("Utility neutral extensive algorithm",
{
    result <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                                attribute.levels = attribute.levels,
                                prior = NULL,
                                n.questions = 18,
                                n.versions = 1,
                                alternatives.per.question = 2,
                                n.constant.attributes = 3,
                                extensive = TRUE,
                                seed = 1)

    expect_equal(result$d.criterion, 1.38629436111989)
    expect_equal(result$d.error, 0.857243982853073)
})

test_that("D-p optimal integrated algorithm",
{
    result <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                                attribute.levels = attribute.levels,
                                prior = c(1, -1, 1, 0, 1, 2, 0, -1, -2),
                                n.questions = 18,
                                n.versions = 1,
                                alternatives.per.question = 2,
                                n.constant.attributes = 3,
                                seed = 1)
    expect_equal(result$d.criterion, -1.03396669554188)
    expect_equal(result$d.error, 1.12174464087415)
})

test_that("Bayesian optimal integrated algorithm",
{
    pa <- cbind(c("price", "100", "125", "150", "175", "200"),
                c("time", "morn", "aft", "eve", "late night", ""))
    n.coef <- sum(pa[-1, ] != "") - ncol(pa)
    prior <- matrix(c(0, 2), nrow = n.coef, ncol = 2, byrow = TRUE)
    result <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                              attribute.levels = pa,
                              prior = prior,
                              n.questions = 10,
                              n.versions = 1,
                              alternatives.per.question = 3,
                              n.constant.attributes = 0,
                              seed = 1)
    expect_equal(result$d.criterion, -6.30145393214796)
    expect_equal(result$d.error, 2.73147951766982)
})

test_that("Partial profiles vs modified Federov with a prior distribution",
{
    prior <- matrix(c(-1, 0, 1, 0, -1, 0, 2, 2, 2, 2, 2, 2), ncol = 2)
    attribute.levels <- list(Att1 = 1:2, Att2 = 1:2, Att3 = 1:3, Att4 = 1:3)
    result.pp <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                                attribute.levels = attribute.levels,
                                prior = prior,
                                n.questions = 10,
                                n.versions = 1,
                                alternatives.per.question = 3,
                                n.constant.attributes = 0,
                                seed = 1)
    result.fed <- ChoiceModelDesign(design.algorithm = "Modfed",
                                attribute.levels = attribute.levels,
                                prior = prior,
                                n.questions = 10,
                                n.versions = 1,
                                alternatives.per.question = 3,
                                n.constant.attributes = 0,
                                seed = 1)
    expect_true(result.pp$d.error < result.fed$d.error)
})

test_that("HZ paper Table 2, 3^3/3/9",
{
    seed <- 1001
    d.err.pub <- .577 ## dummy coding
    levs <- rep.int(3L, 3)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 3
    n.q <- 9
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = NULL,
                           n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE,
                           none.alternative = FALSE)
    expect_equal(out$d.error, d.err.pub, tolerance = .03)
})

test_that("HZ paper Table 2, 3^4/2/15",
{
    seed <- 10101
    d.error.ave.relab <- .163
    levs <- rep.int(3L, 4)
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 2
    n.q <- 15
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = NULL,
                           n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE,
                           none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    mm <- model.matrix(~a+b+c+d, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(numeric(sum(levs - 1)), mm, apq)
    expect_equal(d.err, d.error.ave.relab, tolerance = .01)
})

test_that("HZ paper Table 2, 4^4/4/16",
{
    seed <- 101010
    d.error.ave.relab <- .157
    levs <- rep.int(4L, 4)
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 4
    n.q <- 16
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = NULL,
                           n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE,
                           none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    mm <- model.matrix(~a+b+c+d, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(numeric(sum(levs - 1)), mm, apq)
    expect_equal(d.err, d.error.ave.relab, tolerance = .01)
})

test_that("HZ paper Table 2, 4*3^3/3/48",
{
    seed <- 1010101
    d.error.ave.relab <- .102
    levs <- c(4, rep.int(3L, 3))
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 3
    n.q <- 48
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = NULL,
                           n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE,
                           none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(numeric(sum(levs - 1)), mm, apq)
    expect_true(d.err < d.error.ave.relab) # d.err should be around 0.04
})

test_that("HZ paper Table 2, 9*8*4*3^4*2^3/3/63",
{
    seed <- 10101010
    d.error.ave.relab <- .068
    levs <- c(9, 8, 4, rep.int(3L, 4), rep.int(2L, 3))
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 3
    n.q <- 63
    btime <- Sys.time()
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = NULL,
                           n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE,
                           none.alternative = FALSE)
    etime <- Sys.time()- btime
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(numeric(sum(levs - 1)), mm, apq)
    expect_equal(d.err, d.error.ave.relab, tolerance = .01)
})

test_that("HZ paper Table 2, 3^3/3/9, non-zero beta",
{
    seed <- 7
    d.err.orig <- .381
    levs <- c(a = 3, b = 3, c = 3)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 3
    n.q <- 9

    pmeans <- list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1))
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = beta, n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE, none.alternative = FALSE)

    ## get effects coding d-error
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    beta <- constrainedPrior(levs, pmeans, coding = "E")
    d.err <- idefix:::Derr(beta, mm, apq)
    expect_true(d.err < d.err.orig)
})

test_that("HZ paper Table 2, 3^4/2/15, non-zero beta",
{
    seed <- 777
    d.error.best.relab <- .297
    levs <- rep.int(3L, 4)
    names(levs) <- letters[1:4]
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 2
    n.q <- 15
    pmeans <- list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1),
                   d = c(-1, 0, 1))
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = beta,
                           n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE,
                           none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    mm <- model.matrix(~a+b+c+d, df, contrasts = ca)[, -1]
    beta <- constrainedPrior(levs, pmeans, coding = "E")
    d.err <- idefix:::Derr(beta, mm, apq)
    expect_equal(d.err, d.error.best.relab, tolerance = .02)
})

test_that("HZ paper Table 2, 4^4/4/16, non-zero beta",
{
    seed <- 77777
    d.error.best.relab <- .263
    levs <- rep.int(4L, 4)
    names(levs) <- letters[seq_along(levs)]
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 4
    n.q <- 16
    pmeans <- replicate(4, c(-1, -1/3, 1/3, 1), simplify = FALSE)
    names(pmeans) <- names(levs)
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = beta, n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    beta <- constrainedPrior(levs, pmeans, coding = "E")
    d.err <- idefix:::Derr(beta, mm, apq)
    expect_true(d.err/d.error.best.relab <= 1)
})

test_that("HZ paper Table 2, 3^3/3/9, non-zero beta*1.25",
{
    seed <- 22
    d.err.orig <- .475  ## effects coding
    levs <- c(a = 3, b = 3, c = 3)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 3
    n.q <- 9

    pmeans <- list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1))
    pmeans <- mapply(`*`, pmeans, 1.25, SIMPLIFY = FALSE)
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = beta,
                           n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE,
                           none.alternative = FALSE)

    ## get effects coding d-error
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    beta <- constrainedPrior(levs, pmeans, coding = "E")
    d.err <- idefix:::Derr(beta, mm, apq)
    expect_true(d.err/d.err.orig <= 1)
})

test_that("HZ paper Table 2, 3^4/2/15, non-zero beta*1.25",
{
    seed <- 22
    d.error.best.relab <- .335
    levs <- rep.int(3L, 4)
    names(levs) <- letters[1:4]
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 2
    n.q <- 15
    pmeans <- list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1),
                   d = c(-1, 0, 1))
    pmeans <- mapply(`*`, pmeans, 1.25, SIMPLIFY = FALSE)
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = beta,
                           n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE,
                           none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    mm <- model.matrix(~a+b+c+d, df, contrasts = ca)[, -1]
    beta <- constrainedPrior(levs, pmeans, coding = "E")
    d.err <- idefix:::Derr(beta, mm, apq)
    expect_equal(d.err, d.error.best.relab, tolerance = .05)
})

test_that("HZ paper Table 2, 4^4/4/16, non-zero beta*1.25",
{
    seed <- 222
    d.error.best.relab <- .301
    levs <- rep.int(4L, 4)
    names(levs) <- letters[seq_along(levs)]
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 4
    n.q <- 16
    pmeans <- replicate(4, c(-1, -1/3, 1/3, 1), simplify = FALSE)
    pmeans <- mapply(`*`, pmeans, 1.25, SIMPLIFY = FALSE)
    names(pmeans) <- names(levs)
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = beta, n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    beta <- constrainedPrior(levs, pmeans, coding = "E")
    d.err <- idefix:::Derr(beta, mm, apq)
    expect_true(d.err/d.error.best.relab <= 1)
})

test_that("HZ paper Table 2, 3^3/3/9, non-zero beta*.75",
{
    seed <- 9
    d.err.orig <- .305  ## effects coding
    levs <- c(a = 3, b = 3, c = 3)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 3
    n.q <- 9

    pmeans <- list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1))
    pmeans <- mapply(`*`, pmeans, .75, SIMPLIFY = FALSE)
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = beta, n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE, none.alternative = FALSE)

    ## get effects coding d-error
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    beta <- constrainedPrior(levs, pmeans, coding = "E")
    d.err <- idefix:::Derr(beta, mm, apq)
    expect_true(d.err/d.err.orig <= 1)
})

test_that("HZ paper Table 2, 3^4/2/15, non-zero beta*.75",
{
    seed <- 99
    d.error.best.relab <- .256
    levs <- rep.int(3L, 4)
    names(levs) <- letters[1:4]
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 2
    n.q <- 15
    pmeans <- list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1), d = c(-1, 0, 1))
    pmeans <- mapply(`*`, pmeans, .75, SIMPLIFY = FALSE)
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = beta, n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    mm <- model.matrix(~a+b+c+d, df, contrasts = ca)[, -1]
    beta <- constrainedPrior(levs, pmeans, coding = "E")
    d.err <- idefix:::Derr(beta, mm, apq)
    expect_true(d.err/d.error.best.relab <= 1)
})

test_that("HZ paper Table 2, 4^4/4/16, non-zero beta*.75",
{
    seed <- 999
    d.error.best.relab <- .222
    levs <- rep.int(4L, 4)
    names(levs) <- letters[seq_along(levs)]
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 4
    n.q <- 16
    pmeans <- replicate(4, c(-1, -1/3, 1/3, 1), simplify = FALSE)
    pmeans <- mapply(`*`, pmeans, .75, SIMPLIFY = FALSE)
    names(pmeans) <- names(levs)
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = beta, n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    beta <- constrainedPrior(levs, pmeans, coding = "E")
    d.err <- idefix:::Derr(beta, mm, apq)
    expect_true(d.err/d.error.best.relab <= 1)
})

test_that("Burgess and Street 1992, p. 91: 3x3x6/5/9",
{
    seed <- 11000
    data("bs1.design", package = "flipChoice")
    ca <- as.list(rep("contr.treatment", 3))
    names(ca) <- names(bs1.design)[-(1:2)]

    maxes <- as.numeric(apply(bs1.design, 2,
                            function(x) max(as.integer(x))))  # c(rep.int(4,5), 2, 8, 8, 9)
    n.q <- maxes[1]
    apq <- maxes[2]
    levs <- maxes[-(1:2)]
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- names(ca)
    mm <- model.matrix(~A+B+C, bs1.design, contrasts = ca)[, -1]
    d.err.pub <- idefix:::Derr(numeric(ncol(mm)), mm, apq)

    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE, none.alternative = FALSE)
    expect_true(out$d.error/d.err.pub <= 1)
})

test_that("Burgess and Street 1992, Appendix A.2: 4^5x2x8^2x9/3/99",
{
    skip_on_travis()
    skip_on_cran()
    seed <- 110011
    data("bs2.design", package = "flipChoice")
    ca <- as.list(rep("contr.treatment", ncol(bs2.design) - 2))
    names(ca) <- names(bs2.design)[-(1:2)]
    maxes <- as.numeric(apply(bs2.design, 2,
                            function(x) max(as.integer(x))))  # c(rep.int(4,5), 2, 8, 8, 9)
    n.q <- maxes[1]
    apq <- maxes[2]
    levs <- maxes[-(1:2)]
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- names(ca)

    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, bs2.design, contrasts = ca)[, -1]

    d.err.pub <- idefix:::Derr(numeric(ncol(mm)), mm, apq)

    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(numeric(ncol(mm)), mm, apq)
    expect_true(d.err/d.err.pub <= 1)
})

test_that("Burgess and Street 1992, Appendix 8 A.1: 4^4*2^2*8*36/3/288",
{
    skip_on_travis()
    skip_on_cran()
    seed <- 1101011
    data("bs3.design", package = "flipChoice")
    ca <- as.list(rep("contr.sum", ncol(bs3.design) - 2))
    names(ca) <- names(bs3.design)[-(1:2)]
    maxes <- as.numeric(apply(bs3.design, 2,
                            function(x) max(as.integer(x))))  # c(rep.int(4,5), 2, 8, 8, 9)
    n.q <- maxes[1]
    apq <- maxes[2]
    levs <- maxes[-(1:2)]
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- names(ca)

    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, bs3.design, contrasts = ca)[, -1]

    d.err.pub <- idefix:::Derr(numeric(ncol(mm)), mm, apq)  # dummy coding results in NA

    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(numeric(ncol(mm)), mm, apq)
    expect_true(d.err/d.error.pub <= 1)
})

test_that("Sandor and Wedel 2001, Table 5: 3^5/2/15",
{
    seed <- 11001100
    data("sw1.design", package = "flipChoice")
    ca <- as.list(rep("contr.sum", ncol(sw1.design) - 2))
    names(ca) <- names(sw1.design)[-(1:2)]
    maxes <- as.numeric(apply(sw1.design, 2,
                            function(x) max(as.integer(x))))  # c(rep.int(4,5), 2, 8, 8, 9)
    n.q <- maxes[1]
    apq <- maxes[2]
    levs <- maxes[-(1:2)]
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- names(ca)

    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, sw1.design, contrasts = ca)[, -1]

    prior.beta <- c(.272, -.617, -.291, -.51, .007, -.303, .649, .19,
                  -.331, .368)  ## Table 4 in S & W (2001)
    est.beta <- c(.559, -.706, -.652, .111, -.087, -.213, -.277,
                .487, .452, -.011)  ## Table 4 in S & W (2001)

    ## reported D-error in S & W matches with est.beta not prior.beta here for some reason
    d.err.pub <- idefix:::Derr(est.beta, mm, apq)

    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = prior.beta, n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(est.beta, mm, apq)
    expect_true(d.err/d.err.pub <= 1L)
})

test_that("Sandor and Wedel 2001: 3^4/2/15",
{
    seed <- 110011001
    data("sw2.design", package = "flipChoice")
    ca <- as.list(rep("contr.sum", ncol(sw2.design) - 2))
    names(ca) <- names(sw2.design)[-(1:2)]
    maxes <- as.numeric(apply(sw2.design, 2,
                            function(x) max(as.integer(x))))  # c(rep.int(4,5), 2, 8, 8, 9)
    n.q <- maxes[1]
    apq <- maxes[2]
    levs <- maxes[-(1:2)]
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- names(ca)

    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, sw2.design, contrasts = ca)[, -1]

    prior.coef <- rep(c(0, 1), times = 4)
    d.err.pub <- idefix:::Derr(prior.coef, mm, apq)

    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = prior.coef, n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(prior.coef, mm, apq)
    expect_true(d.err/d.err.pub <= 1L)

    # Bayesian criterion
    prior.coef.with.sd <- cbind(prior.coef, rep(1,8))
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                             attribute.levels = attr.list,
                             prior = prior.coef.with.sd,
                             n.questions = n.q,
                             seed = seed, alternatives.per.question = apq,
                             labeled.alternatives = FALSE,
                             none.alternative = FALSE)

    encoded.design <- encodeDesign(sw2.design[,-1:-2], TRUE)
    pub.bayesian.error <- dBError(encoded.design, prior.coef.with.sd,
                                  15, 2, 10, 1)
    expect_true(out$d.error < pub.bayesian.error)
})

test_that("Street, Burgess, Louviere 2005 Table 5: 4^5/2/16",
{
    seed <- 10101010
    data("sbl1.design", package = "flipChoice")
    ca <- as.list(rep("contr.sum", ncol(sbl1.design) - 2))
    names(ca) <- names(sbl1.design)[-(1:2)]
    maxes <- as.numeric(apply(sbl1.design, 2,
                            function(x) max(as.integer(x))))  # c(rep.int(4,5), 2, 8, 8, 9)
    n.q <- maxes[1]
    apq <- maxes[2]
    levs <- maxes[-(1:2)]
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- names(ca)

    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, sbl1.design, contrasts = ca)[, -1]

    d.err.pub <- idefix:::Derr(numeric(ncol(mm)), mm, apq)

    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(numeric(ncol(mm)), mm, apq)
    expect_true(d.err/d.err.pub <= 1)
})

test_that("Street, Burgess, Louviere 2005 Table 9: 2^2*4^2/3/16",
{
    seed <- 101100
    data("sbl2.design", package = "flipChoice")
    ca <- as.list(rep("contr.sum", ncol(sbl2.design) - 2))
    names(ca) <- names(sbl2.design)[-(1:2)]
    maxes <- as.numeric(apply(sbl2.design, 2,
                            function(x) max(as.integer(x))))  # c(rep.int(4,5), 2, 8, 8, 9)
    n.q <- maxes[1]
    apq <- maxes[2]
    levs <- maxes[-(1:2)]
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- names(ca)

    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, sbl2.design, contrasts = ca)[, -1]

    d.err.pub <- idefix:::Derr(numeric(ncol(mm)), mm, apq)

    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                           attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                           seed = seed, alternatives.per.question = apq,
                           labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(numeric(ncol(mm)), mm, apq)
    expect_equal(d.err, d.err.pub, tolerance = .01)
})

test_that("Kessels et al 2011, 2^(6-1) factorial design and Design 1 (Table II)",
{
    seed = 123

    data("kessels0.design", package = "flipChoice")
    data("kessels1.design", package = "flipChoice")

    prior <- matrix(c(-0.8, -0.8, -0.8, -0.8, -0.8, -0.8, rep(0.4, 6)), ncol = 2)

    encoded.design <- encodeDesign(kessels0.design[, -1:-2], FALSE)
    error0 <- dBError(encoded.design, prior, 8, 2, 10, seed)
    encoded.design <- encodeDesign(kessels1.design[, -1:-2], FALSE)
    error1 <- dBError(encoded.design, prior, 8, 2, 10, seed)

    expect_true(error1 < error0)

    attr.list <- list(A1 = 1:2, A2 = 1:2, A3 = 1:2, A4 = 1:2,
                      A5 = 1:2, A6 = 1:2)
    out <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                             attribute.levels = attr.list, prior = prior,
                             n.questions = 8,
                             seed = seed, alternatives.per.question = 2,
                             labeled.alternatives = FALSE, none.alternative = FALSE)

    expect_true(out$d.error < error1)
})

