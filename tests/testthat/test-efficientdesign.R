context("Efficient")

test_that("3*3*2/4/10 dummy coding; old interface",
{
    seed <- 3000
    pa <- cbind(c("price", "200", "250", "300"), c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", ""))
    prior <- NULL  # matrix(nrow = 0, ncol = 0)
    ## out <- efficientDesign(al, prior, 4, 10, dummy.coding = TRUE,
    ##                                    seed = seed)
    n.q <- 10
    apq <- 4
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                             attribute.levels = pa, prior = prior, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed)
    prior <- numeric(5)
    out2 <- ChoiceModelDesign(design.algorithm = "Efficient",
                             attribute.levels = pa, prior = prior, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed)
    expect_identical(out, out2)
    expect_equal(out$db.error, .52, tolerance = .0015)
    expect_true(all(out$model.matrix %in% c(0, 1)))
    expect_equal(colnames(out$design)[-3:-1], pa[1, ])
    expect_equal(unique(out$design[, 2]), 1:n.q)
    expect_equal(unique(out$design[, 3]), 1:apq)

    ## expect_true(all(grepl("^set[0-9]{1,2}[.]alt[1-4]", rownames(out$design))))
})

seed <- 765
pd <- cbind(c("price", "200", "250", "300"), c("Mean", 0, 1, 1),
            c("SD", 1, 1, 1),
            c("time", "morn", "aft", "eve"), c("Mean", 0, 0, 0),
            c("SD", 1, 1, 1),
            c("type", "train", "bus", ""), c("Mean", 2, 3, ""), c("SD", 1, 2, ""))
vnames <- pd[1, !pd[1,] %in% c("SD", "Mean")]
n.q <- 10
apq <- 4

out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = pd, prior = NULL, n.questions = n.q,
                         alternatives.per.question = apq, seed = seed,
                         output = "Labeled design")

test_that("Some prior inputs missing",
{

    expect_equal(out$db.error, .945, tolerance = .01)
    expect_true(all(out$model.matrix %in% c(0, 1)))
    expect_equal(colnames(out$design)[-3:-1], vnames)
    expect_equal(unique(out$design[, 2]), 1:n.q)
    expect_equal(unique(out$design[, 3]), 1:apq)

     ## expect_true(all(grepl("^set[0-9]{1,2}[.]alt[1-4]", rownames(out$design))))
})

test_that("ChoiceModelDesign: print labels working",
{

    tfile <- tempfile()
    withr::with_output_sink(tfile, {
        expect_is(print(out), "data.frame")
        expect_equal(levels(print(out)[[4]]), pd[-1, 1])
        expect_equal(levels(print(out)[[5]]),
                     pd[-1, pd[1,] == colnames(print(out))[5]])
        expect_named(print(out), c("Version", "Question", "Alternative", "price", "time", "type"))
    })
    unlink(tfile)
})


test_that("3^3/3/9 effects coding",
{
    seed <- 101
    pa <- cbind(c("price", "200", "250", "300"), c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", "car"))
    al <- pastedAttributesToVector(pa)
    prior <- numeric(sum(al) - length(al))
    out <- efficientDesign(al, prior, 4, 12,
                                       dummy.coding = FALSE,
                                       seed = seed)
    expect_true(all(out$model.matrix %in% c(-1, 0, 1)))
})

test_that("Efficient: bad prior",
{
    seed <- 331
    pd <- cbind(c("price", "200", "250", "300"), c("mean", 0, 1, ""),
                c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", ""), c("mean", 2, 3, ""), c("sd", 1, 2, ""))
    expect_error(ChoiceModelDesign("Efficient", pd,
                                   alternatives.per.question = 4, n.questions = 12,
                                       seed = seed), "price, 3")

    pd <- cbind(c("price", "200", "250", "300"), c("mean", 0, 1, "2"),
                c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", ""), c("mean", 2, 3, ""), c("sd", 1, "", ""))
    expect_error(ChoiceModelDesign("Efficient", pd,
                                   alternatives.per.question = 4, n.questions = 12,
                                       seed = seed), "type, 2")
})

test_that("Efficient: vector prior",
{
    seed <- 2218789
    pa <- cbind(c("price", "100", "125", "150", "175", "200"),
                c("time", "morn", "aft", "eve", "late night", ""),
                c("type", "train", "bus", "boat", "car", "bike"),
                c("food", "candy", "sandwich", "nuts", "", ""))
    al <- pastedAttributesToVector(pa)
    n.coef <- sum(pa[-1, ] != "") - ncol(pa)
    prior <- 1 + numeric(n.coef)
    out <- efficientDesign(al, prior, 5, 15,
                                       dummy.coding = FALSE,
                                       seed = seed)
    expect_equal(out$error, .325, tolerance = .05)
})

test_that("Efficient: prior means and variances old interface",
{
    seed <- 97
    pa <- cbind(c("price", "100", "125", "150", "175", "200"),
                c("time", "morn", "aft", "eve", "late night", ""))
    n.coef <- sum(pa[-1, ] != "") - ncol(pa)
    prior <- matrix(c(0, 2), nrow = n.coef, ncol = 2, byrow = TRUE)
    out <- ChoiceModelDesign(design.algorithm = "Efficient", attribute.levels = pa,
                             prior = prior, n.questions = 8, alternatives.per.question = 3,
                                       seed = seed)
    expect_equal(out$db.error, 2.43, tolerance = 1e-3)
})

test_that("Efficient: none alternatives",
{
    seed <- 20
    pa <- cbind(c("price", "200", "250", "300"), c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", ""))
    prior <- matrix(nrow = 0, ncol = 0)
    ## out <- efficientDesign(al, prior, 4, 10, dummy.coding = TRUE,
    ##                                    seed = seed)
    n.q <- 10
    apq <- 4
    n.a <- 2
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                             attribute.levels = pa, prior = NULL, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed,
                             none.alternatives = 2)
    expect_equal(sum(is.na(out$design.with.none[, 4L])), n.q*n.a)
    expect_equal(max(out$design.with.none[, 3L]), n.a + apq)
})

test_that("Efficient: labeled alternatives",
{
    seed <- 98
    lpa1 <- c(engine = 3, transmission = 2, colour = 7)
    lpa2 <- c(brand = 4, lpa1)
    prior <- matrix(nrow = 0, ncol = 0)
    ## out <- efficientDesign(al, prior, 4, 10, dummy.coding = TRUE,
    ##                                    seed = seed)
    n.q <- 20

    out <- efficientDesign(
                                   levels.per.attribute = lpa2,
                                   prior = NULL,
                                   lpa2[1],
                                   n.q,
                                   labeled.alternatives = TRUE,
                                   dummy.coding = TRUE,
                                   seed = seed,
                                   n.sim = 10)
    expect_equal(dim(out$design), c(lpa2[1]*n.q, 2 + length(lpa2)),
                 check.attributes = FALSE)
    expect_equal(colnames(out$design), c("Question", "Alternative", names(lpa2)))
    expect_equal(dim(out$model.matrix), c(lpa2[1]*n.q,
                                          sum(lpa2) - length(lpa2)),
                 check.attributes = FALSE)

    pa <- cbind(c("yamaha", "honda", "ducati", "kawasaki", "", "", ""),
                             c("125cc", "250cc", "500cc", "", "", "", ""),
                             c("manual", "automatic", "", "", "", "", ""),
                             c("red", "green", "blue", "yellow", "black", "white", "silver"))
    pa <- rbind(c("brand", "engine", "transmission", "colour"), pa)
    out2 <- ChoiceModelDesign(design.algorithm = "Efficient",
                             attribute.levels = pa, prior = NULL, n.questions = n.q,
                             seed = seed,
                             labeled.alternatives = TRUE)
    n.coef <- sum(pa[-1, ] != "") - ncol(pa)
    apq <- sum(pa[-1, 1] != "")
    expect_equal(dim(out2$design), c(apq*n.q, 3 + ncol(pa)),
                 check.attributes = FALSE)
    expect_equal(colnames(out2$design), c("Version", "Question", "Alternative", pa[1, ]))
    expect_equal(dim(out2$model.matrix), c(apq*n.q,
                                          n.coef),
                 check.attributes = FALSE)
})


test_that("Parsing of pasted prior with some means and sd's missing",
{
    pd <- cbind(c("price", "200", "250", "300"), c("Mean", 0, 1, 1),
                c("time", "morn", "aft", "eve"), c("Mean", 0, 0, 2),
                c("SD", 1, 3, 1),
                c("type", "train", "bus", ""), c("SD", 1, 2, ""))
    vnames <- pd[1, !pd[1,] %in% c("SD", "Mean")]
    n.q <- 10
    apq <- 4

    ## out <- ChoiceModelDesign(design.algorithm = "Efficient",
    ##                          attribute.levels = pd, prior = NULL, n.questions = n.q,
    ##                          alternatives.per.question = apq, seed = seed,
    ##                          output = "Labeled design")
    parsed <- parsePastedData(pd)
    expect_equal(names(parsed$lvls), vnames)
    expect_equal(parsed$lvls, c(3, 3, 2), check.attributes = FALSE)
    expect_equal(parsed$prior[, 1], c(1, 1, 0, 2, 0), check.attributes = FALSE)
    expect_equal(parsed$prior[, 2], c(1, 1, 3, 1, 2), check.attributes = FALSE)
})


test_that("Correct prior specification improves fit on sim data",
{
    seed <- 378
    seed.resp.sim <- 202
    n.respondents <- 15
    price.mean <- c(-2, 0, 2)
    time.mean <- 0:2
    type.mean <- 0:1
    pd <- cbind(c("price", "200", "250", "300"),
                c("Mean", price.mean),
                c("SD", c(1, 1, 1)),
                c("time", "morn", "aft", "eve"),
                c("Mean", time.mean),
                c("type", "train", "bus", ""),
                c("Mean", type.mean, ""))
    vnames <- pd[1, !pd[1,] %in% c("SD", "Mean")]
    n.q <- 10
    apq <- 4

    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                             attribute.levels = pd, prior = NULL, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed,
                             output = "Labeled design")
    set.seed(seed.resp.sim)

    true.coef <- c(price.mean[-1], time.mean[-1], type.mean[-1])
    cfun <- contr.treatment
    mm <- model.matrix(~as.factor(price)+as.factor(time)+as.factor(type),
                       data = as.data.frame(out$design), contrasts.arg = cfun)[, -1]
    y <- as.vector(replicate(n.respondents, idefix::RespondMNL(true.coef,
                                                     mm, n.alts = apq)))
    ml.model <- mlogitModel(out, as.logical(y))
    res.good <- summary(ml.model)$CoefTable
    sd.good <- summary(ml.model)$CoefTable[, 2]
    pval.good <- summary(ml.model)$CoefTable[, 4]


    pd.bad.prior <- cbind(c("price", "200", "250", "300"),
                c("Mean", rev(price.mean)),
                c("SD", c(1, 1, 1)),
                c("time", "morn", "aft", "eve"),
                c("Mean", time.mean),
                c("SD", c(1, 1, 1)),
                c("type", "train", "bus", ""),
                c("Mean", type.mean, ""),
                c("SD", c(1, 1, "")))

    out.bad.prior <- ChoiceModelDesign(design.algorithm = "Efficient",
                             attribute.levels = pd.bad.prior, prior = NULL, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed,
                             output = "Labeled design")
    mm <- model.matrix(~as.factor(price)+as.factor(time)+as.factor(type),
                       data = as.data.frame(out.bad.prior$design), contrasts.arg = cfun)[, -1]
    y <- as.vector(replicate(n.respondents, idefix::RespondMNL(true.coef,
                                                     mm, n.alts = apq)))
    ml.model.bad <- mlogitModel(out.bad.prior, y)
    sd.bad <- summary(ml.model.bad)$CoefTable[, 2]
    pval.bad <- summary(ml.model.bad)$CoefTable[, 4]
    summary(ml.model.bad)$CoefTable
    expect_true(sd.good["price300"] < sd.bad["price300"])
})

test_that("D-error calculation agrees with Huber-Zwerina Table 1 3^3/3/9",
{
    data("hz.design", package = "flipChoice")
    apq <- 3

    ## effects coding
    ca <- as.list(rep("contr.sum", 3))
    names(ca) <- names(hz.design)[-(1:2)]
    mm <- model.matrix(~A+B+C, hz.design, contrasts = ca)[, -1]
    expect_equal(idefix:::Derr(numeric(ncol(mm)), mm, apq),
          calculateDError(cbind(1, hz.design), attribute.levels = c(3,3,3), TRUE))
    expect_equal(idefix:::Derr(numeric(ncol(mm)), mm, apq),
                 .192, tolerance = .0005)

    ## non-zero beta
    lvls <- c(a = 3, b = 3, c = 3)
    pmeans <- list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1))
    beta <- constrainedPrior(lvls, pmeans, coding = "E")
    expect_equal(idefix:::Derr(beta, mm, apq),
                 .381, tolerance = .04)

    pmeans <- list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1))
    pmeans <- mapply(`*`, pmeans, 1.25, SIMPLIFY = FALSE)
    beta <- constrainedPrior(lvls, pmeans, coding = "E")
    expect_equal(idefix:::Derr(beta, mm, apq),
                 .475, tolerance = .025)

    pmeans <- list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1))
    pmeans <- mapply(`*`, pmeans, .75, SIMPLIFY = FALSE)
    beta <- constrainedPrior(lvls, pmeans, coding = "E")
    expect_equal(idefix:::Derr(beta, mm, apq),
                 .305, tolerance = .0005)

    ## dummy coding
    ca <- as.list(rep("contr.treatment", 3))
    names(ca) <- letters[1:3]
    mm <- model.matrix(~A+B+C, hz.design, contrasts = ca)[, -1]
    ## expect_equal(idefix:::Derr(numeric(ncol(mm)), mm, 3),
    ##       calculateDError(cbind(1, hz.design), attribute.levels = c(3,3,3), TRUE))
    expect_equal(idefix:::Derr(numeric(ncol(mm)), mm, apq),
                 .577, tolerance = .0005)


})

test_that("Efficient outperforms choiceDes",
{
    seed <- 777

    levs1 <- c(3,3,5,4)
    names(levs1) <- letters[seq_along(levs1)]
    n.q <- 16
    apq <- 4

    des <- choiceDes::dcm.design(levs1, nb = 1, sets = n.q, apq)
    cd.df <- des$levels
    attr.list <- lapply(levs1, seq.int)
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                             attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed,
                             output = "Labeled design")
    mm <- model.matrix(~as.factor(a)+as.factor(b)+as.factor(c)+as.factor(d),
                       as.data.frame(out$design))[, -1]
    expect_true(idefix:::Derr(numeric(ncol(mm)), mm, apq) <
                idefix:::Derr(numeric(ncol(mm)), as.matrix(des$effects$design), apq))
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
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                         seed = seed, alternatives.per.question = apq,
                         labeled.alternatives = FALSE, none.alternative = FALSE)
   out$d.error/d.err.pub
})


## test_that("HZ paper Table 2, 3^3/3/9",
## {
##     d.err.effects <- .192
##     d.err.dummy <- .577
##     levs <- rep.int(3L, 3)
##     attr.list <- lapply(levs, seq.int)
##     names(attr.list) <- letters[seq_along(levs)]
##     apq <- 3
##     n.q <- 9
##     out <- ChoiceModelDesign(design.algorithm = "Efficient",
##                          attribute.levels = attr.list, prior = NULL, n.questions = n.q,
##                          seed = seed, alternatives.per.question = apq,
##                          labeled.alternatives = FALSE, none.alternative = TRUE)

## })

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
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                         seed = seed, alternatives.per.question = apq,
                         labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    mm <- model.matrix(~a+b+c+d, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(numeric(sum(levs - 1)), mm, apq)
    d.err/d.error.ave.relab
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
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                         seed = seed, alternatives.per.question = apq,
                         labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    mm <- model.matrix(~a+b+c+d, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(numeric(sum(levs - 1)), mm, apq)
    d.err/d.error.ave.relab
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
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                         seed = seed, alternatives.per.question = apq,
                         labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(numeric(sum(levs - 1)), mm, apq)
    d.err/d.error.ave.relab
})


test_that("HZ paper Table 2, 9*8*4*3^4*2^3/3/63",
{
    ## extremely slow
    skip_on_cran()
    skip_on_travis()
    seed <- 10101010
    d.error.ave.relab <- .068
    levs <- c(9, 8, 4, rep.int(3L, 4), rep.int(2L, 3))
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 3
    n.q <- 63
    btime <- Sys.time()
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                         seed = seed, alternatives.per.question = apq,
                         labeled.alternatives = FALSE, none.alternative = FALSE)
    etime <- Sys.time()- btime
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    form <- as.formula(paste0("~", paste(names(attr.list), collapse = "+")))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(numeric(sum(levs - 1)), mm, apq)
    d.err/d.error.ave.relab
})

test_that("HZ paper Table 2, 3^3/3/9, non-zero beta",
{
    seed <- 7
    d.err.orig <- .381
    d.err.swap <- .280  ## effects coding
    levs <- c(a = 3, b = 3, c = 3)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 3
    n.q <- 9

    pmeans <- list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1))
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
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

test_that("HZ paper Table 2, 3^4/2/15, non-zero beta",
{
    seed <- 777
    d.error.best.relab <- .297
    d.error.swap <- .253
    levs <- rep.int(3L, 4)
    names(levs) <- letters[1:4]
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 2
    n.q <- 15
    pmeans <- list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1), d = c(-1, 0, 1))
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = attr.list, prior = beta, n.questions = n.q,
                         seed = seed, alternatives.per.question = apq,
                         labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    mm <- model.matrix(~a+b+c+d, df, contrasts = ca)[, -1]
    beta <- constrainedPrior(levs, pmeans, coding = "E")
    d.err <- idefix:::Derr(beta, mm, apq)
    d.err/d.error.best.relab
    d.err/d.error.swap
})

test_that("HZ paper Table 2, 4^4/4/16, non-zero beta",
{
    seed <- 77777
    d.error.ave.relab <- .307
    d.error.best.relab <- .263
    d.error.swap <- .198
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
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
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

test_that("HZ paper Table 2, 4*3^3/3/48, non-zero beta",
{
    seed <- 7777777
    d.error.ave.relab <- .231
    d.error.best.relab <- .199
    d.error.swap <- .142
    levs <- c(4, rep.int(3L, 3))
    names(levs) <- letters[seq_along(levs)]
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- names(levs)
    apq <- 3
    n.q <- 48
    pmeans <- vector("list", length(levs))
    pmeans[[1]] <- c(-1, -1/3, 1/3, 1)
    pmeans[2:4] <- replicate(3, c(-1, 0, 1), simplify = FALSE)
    names(pmeans) <- names(levs)
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
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
    d.err.swap <- .311
    levs <- c(a = 3, b = 3, c = 3)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 3
    n.q <- 9

    pmeans <- list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1))
    pmeans <- mapply(`*`, pmeans, 1.25, SIMPLIFY = FALSE)
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
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

test_that("HZ paper Table 2, 3^4/2/15, non-zero beta*1.25",
{
    seed <- 22
    d.error.best.relab <- .335
    d.error.swap <- .265
    levs <- rep.int(3L, 4)
    names(levs) <- letters[1:4]
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 2
    n.q <- 15
    pmeans <- list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1), d = c(-1, 0, 1))
    pmeans <- mapply(`*`, pmeans, 1.25, SIMPLIFY = FALSE)
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = attr.list, prior = beta, n.questions = n.q,
                         seed = seed, alternatives.per.question = apq,
                         labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    mm <- model.matrix(~a+b+c+d, df, contrasts = ca)[, -1]
    beta <- constrainedPrior(levs, pmeans, coding = "E")
    d.err <- idefix:::Derr(beta, mm, apq)
    d.err/d.error.best.relab
    d.err/d.error.swap
})

test_that("HZ paper Table 2, 4^4/4/16, non-zero beta*1.25",
{
    seed <- 222
    d.error.ave.relab <- .384
    d.error.best.relab <- .301
    d.error.swap <- .208
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
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
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

test_that("HZ paper Table 2, 4*3^3/3/48, non-zero beta*1.25",
{
    seed <- 2222
    d.error.ave.relab <- .295
    d.error.best.relab <- .238
    d.error.swap <- .146
    levs <- c(4, rep.int(3L, 3))
    names(levs) <- letters[seq_along(levs)]
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- names(levs)
    apq <- 3
    n.q <- 48
    pmeans <- vector("list", length(levs))
    pmeans[[1]] <- c(-1, -1/3, 1/3, 1)
    pmeans[2:4] <- replicate(3, c(-1, 0, 1), simplify = FALSE)
    pmeans <- mapply(`*`, pmeans, 1.25, SIMPLIFY = FALSE)
    names(pmeans) <- names(levs)
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
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
    d.err.swap <- .259
    levs <- c(a = 3, b = 3, c = 3)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- letters[seq_along(levs)]
    apq <- 3
    n.q <- 9

    pmeans <- list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1))
    pmeans <- mapply(`*`, pmeans, .75, SIMPLIFY = FALSE)
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
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
    d.error.swap <- .231
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
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = attr.list, prior = beta, n.questions = n.q,
                         seed = seed, alternatives.per.question = apq,
                         labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    ca <- as.list(rep("contr.sum", length(attr.list)))
    names(ca) <- names(attr.list)
    mm <- model.matrix(~a+b+c+d, df, contrasts = ca)[, -1]
    beta <- constrainedPrior(levs, pmeans, coding = "E")
    d.err <- idefix:::Derr(beta, mm, apq)
    d.err/d.error.best.relab
    d.err/d.error.swap
})

test_that("HZ paper Table 2, 4^4/4/16, non-zero beta*.75",
{
    seed <- 999
    d.error.ave.relab <- .244
    d.error.best.relab <- .222
    d.error.swap <- .188
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
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
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

test_that("HZ paper Table 2, 4*3^3/3/48, non-zero beta*.75",
{
    seed <- 9999
    d.error.ave.relab <- .178
    d.error.best.relab <- .163
    d.error.swap <- .133
    levs <- c(4, rep.int(3L, 3))
    names(levs) <- letters[seq_along(levs)]
    n.attr <- length(levs)
    attr.list <- lapply(levs, seq.int)
    names(attr.list) <- names(levs)
    apq <- 3
    n.q <- 48
    pmeans <- vector("list", length(levs))
    pmeans[[1]] <- c(-1, -1/3, 1/3, 1)
    pmeans[2:4] <- replicate(3, c(-1, 0, 1), simplify = FALSE)
    pmeans <- mapply(`*`, pmeans, .75, SIMPLIFY = FALSE)
    names(pmeans) <- names(levs)
    beta <- constrainedPrior(levs, pmeans, coding = "D")
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
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

    out <- ChoiceModelDesign(design.algorithm = "Efficient",
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

    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                         seed = seed, alternatives.per.question = apq,
                         labeled.alternatives = FALSE, none.alternative = FALSE)
    out$d.error/d.err.pub
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

    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                         seed = seed, alternatives.per.question = apq,
                         labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(numeric(ncol(mm)), mm, apq)
    d.err/d.error.pub <= 1
})

test_that("Sandor and Wedel 2001: 3^5/2/15",
{
    seed <- 11001100
    data("sw1.design", package = "flipChoice")
    ca <- as.list(rep("contr.treatment", ncol(sw1.design) - 2))
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

    d.err.pub <- idefix:::Derr(numeric(ncol(mm)), mm, apq)

    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                         seed = seed, alternatives.per.question = apq,
                         labeled.alternatives = FALSE, none.alternative = FALSE)
   out$d.error/d.err.pub
})

test_that("Sandor and Wedel 2001: 3^4/2/15",
{
    seed <- 110011001
    data("sw2.design", package = "flipChoice")
    ca <- as.list(rep("contr.treatment", ncol(sw2.design) - 2))
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

    d.err.pub <- idefix:::Derr(numeric(ncol(mm)), mm, apq)

    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                         seed = seed, alternatives.per.question = apq,
                         labeled.alternatives = FALSE, none.alternative = FALSE)
   expect_true(out$d.error/d.err.pub <= 1)
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

    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                         seed = seed, alternatives.per.question = apq,
                         labeled.alternatives = FALSE, none.alternative = FALSE)
    df <- as.data.frame(apply(out$design, 2, as.factor))
    mm <- model.matrix(form, df, contrasts = ca)[, -1]
    d.err <- idefix:::Derr(numeric(ncol(mm)), mm, apq)
    d.err/d.err.pub
})

test_that("Street, Burgess, Louviere 2005 Table 5: 2^2*4^2/3/16",
{
    seed <- 101100
    data("sbl2.design", package = "flipChoice")
    ca <- as.list(rep("contr.treatment", ncol(sbl2.design) - 2))
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

    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                         seed = seed, alternatives.per.question = apq,
                         labeled.alternatives = FALSE, none.alternative = FALSE)
    expect_equal(out$d.error, d.err.pub, tolerance = .005)
})

## test_that("SAS tech doc. mr2010f,"
## {
##     seed <- 11
##     pd <- cbind(c("brand", letters[1:4]), c("price", 1:3, ""))
##     n.q <- 18
##     apq <- 4
##     out <- ChoiceModelDesign(design.algorithm = "Efficient",
##                              attribute.levels = pd, prior = NULL, n.questions = n.q,
##                              seed = seed, alternatives.per.question = 4,
##                              labeled.alternatives = FALSE, none.alternative = TRUE)
## })
