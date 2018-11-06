context("Choice model prediction")

data(chocolate)
data(chocolate.design)
choices <- chocolate[, grepl("^choice", colnames(chocolate))]
tasks <- chocolate[, grepl("^task", colnames(chocolate))]
fit <- FitChoiceModel(design = chocolate.design,
                         choices = choices,
                         tasks = tasks,
                         hb.iterations = 10, hb.chains = 1,
                         include.choice.parameters = FALSE,
                         hb.warnings = FALSE)

fit.keep <- FitChoiceModel(design = chocolate.design,
                         choices = choices,
                         tasks = tasks,
                         cov.formula = ~diabetes, cov.data = chocolate,
                         hb.iterations = 10, hb.chains = 1,
                         hb.beta.draws.to.keep = 10,
                         include.choice.parameters = TRUE,
                         hb.warnings = FALSE)



scen <- list("alt. 1" = c("Brand" = "Dove", "Price" = "$0.99", "Cocoa Strength" = "Dark",
                          "Sugar" = "Standard", "Origin" = "Switzerland",
                          "Nuts" = "Hazelnuts", "Ethical" = "BLANK"),
             "alt. 2" = c("Brand" = "Hershey", "Price" = "$0.99", "Cocoa Strength" = "Dark",
                          "Sugar" = "Standard", "Origin" = "USA",
                          "Nuts" = "Hazelnuts", "Ethical" = "BLANK"))

test_that("prediction error msg if invalid scenario",
{
    par.names <- fit$param.names.list$unconstrained.respondent.pars
    al <- chocolate.design$attribute.levels
    expect_equal(checkValidAlternative(scen[[1]], par.names, al), "")
    expect_equal(checkValidAlternative(scen[[2]], par.names, al), "")
    bad.scen <- scen
    names(bad.scen[[2]])[1] <- "not an attribute"
    expect_error(checkScenario(bad.scen, par.names, al),
                 names(bad.scen[[2]])[1])
    bad.scen[[1]][2] <- "BlahBlah"
    expect_error(checkScenario(bad.scen, par.names, al),
                 bad.scen[[1]][2])
    expect_error(checkScenario(bad.scen, par.names, al),
                 paste(sQuote(al[[names(bad.scen[[1]][2])]]), collapse = ", "),
                 fixed = TRUE)
})


test_that("Prediction no none alternative",
{
    out <- predict(fit, scen)
    expect_equal(nrow(out), fit$n.respondents)
    expect_equal(ncol(out), length(scen))
    expect_equal(colnames(out), names(scen))
    expect_equal(rowSums(out), rep.int(1, fit$n.respondents))

    out <- predict(fit, scen, rule = "first choice respondent")
    expect_equal(nrow(out), fit$n.respondents)
    expect_equal(ncol(out), length(scen))
    expect_equal(colnames(out), names(scen))
    expect_equal(rowSums(out), rep.int(1, fit$n.respondents))
})

test_that("Prediction with none alternative",
{
    al <- list(Att1 = 1:2, Att2 = 1:2, Att3 = 1:3)
    des.none <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                                  attribute.levels = al,
                                  prior = matrix(c(1, -2, 1, 3, 0, 1, 0.5, 1.5), ncol = 2),
                                  n.questions = 12,
                                  n.versions = 2,
                                  alternatives.per.question = 2,
                                  none.alternatives = 2,
                                  none.positions = 2:3,
                                  seed = 1)
    fit.none <- FitChoiceModel(design = des.none,
                          hb.iterations = 15, hb.chains = 2,
                          hb.warnings = FALSE,
                          simulated.priors.from.design = TRUE,
                          simulated.sample.size = 100)

    scen.none <- list("Alt. 1" = c("Att1" = 1, "Att2" = 2, "Att3" = 3),
                      "Alt. 2" = c("Att1" = 2, "Att2" = 2, "Att3" = 1),
                      "none 1" = c("Alternative" = "2 (none of these)"))
    out <- predict(fit.none, scen.none)
    expect_equal(nrow(out), fit.none$n.respondents)
    expect_equal(ncol(out), length(scen.none))
    expect_equal(colnames(out), names(scen.none))
    expect_equal(rowSums(out), rep.int(1, fit.none$n.respondents))
})

utilities <- array(0, dim = c(2, 1, 6))  # 2 iter, 1 resp, 6 alt
utilities[1, 1, ] <- c(0, 0, 0, 0, 100, 101)
utilities[2, 1, ] <- c(0, 0, 0, 0, 1.001, 1.00)

test_that("logit draw rule calculation",
{
    out <- calcLDprobs(utilities)
    prob <- apply(drop(utilities), 1, softmax)
    ans <- rowMeans(prob)
    expect_equal(drop(out), ans)
})

test_that("logit respondent rule calculation",
{
    out <- calcLRprobs(utilities)

    ans <- colMeans(drop(utilities))
    ans <- softmax(ans)
    expect_equal(drop(out), ans)
})

test_that("first choice draw rule calculation",
{
    out <- calcFCDprobs(utilities)

    choices <- apply(drop(utilities), 1, which.max)
    ans <- tabulate(choices, dim(utilities)[3])
    ans <- ans/dim(utilities)[1]
    expect_equal(drop(out), ans)
})

test_that("first choice respondent rule calculation",
{
    out <- calcFCRprobs(utilities)

    mu <- colMeans(drop(utilities))
    ans <- numeric(dim(utilities)[3])
    ans[which.max(mu)] <- 1
    expect_equal(drop(out), ans)
})

test_that("prediction none alternative and no ASC",
{
    ## **************************************************
    ## broken until DS-2232 completed
    ## **************************************************

    ## al <- list(Att1 = 1:2, Att2 = 1:3, Att3 = 1:3)
    ## des.none <- ChoiceModelDesign(design.algorithm = "Partial profiles",
    ##                               attribute.levels = al,
    ##                               n.questions = 12,
    ##                               n.versions = 2,
    ##                               alternatives.per.question = 2,
    ##                               none.alternatives = 1,
    ##                               none.positions = 3,
    ##                               seed = 1)
    ## fit.none <- suppressWarnings(FitChoiceModel(design = des.none,
    ##                       hb.iterations = 14, hb.chains = 1,
    ##                       hb.warnings = FALSE,
    ##                       include.choice.parameters = FALSE,
    ##                       simulated.priors.from.design = TRUE,
    ##                       simulated.sample.size = 100))

    ## scen.none <- list("Alt. 1" = c("Att1" = 1, "Att2" = 2, "Att3" = 3),
    ##                   "Alt. 2" = c("Att1" = 2, "Att2" = 2, "Att3" = 1),
    ##                   "none 1" = c("Alternative" = "2 (none of these)"))
    ## out <- predict(fit.none, scen.none)
    ## expect_equal(nrow(out), fit$n.respondents)
    ## expect_equal(ncol(out), length(scen))
    ## expect_equal(colnames(out), names(scen))
    ## expect_equal(rowSums(out), rep.int(1, fit$n.respondents))

})

test_that("prediction with ASCs specified",
{
    scen3 <- list("A1" = c("Brand" = "Lindt", "Price" = "$0.99",
                              "Cocoa Strength" = "Milk", Alternative = 1,
                          "Sugar" = "Standard", "Origin" = "Switzerland",
                          "Nuts" = "Hazelnuts", "Ethical" = "BLANK"),
             "A2" = c("Brand" = "Hershey", "Price" = "$0.99", "Cocoa Strength" = "Dark",
                          "Sugar" = "Standard", "Origin" = "USA", Alternative = 2,
                      "Nuts" = "Hazelnuts", "Ethical" = "BLANK"),
             "A3" = c("Brand" = "Hershey", "Price" = "$0.99", "Cocoa Strength" = "Dark",
                      "Sugar" = "Standard", "Origin" = "USA", Alternative = 3,
                      "Nuts" = "Hazelnuts", "Ethical" = "Fair trade"))
    out <- predict(fit.keep, scen3, rule = "first choice draw")
    ## table(apply(out, 1, which.max))
    expect_equal(nrow(out), fit.keep$n.respondents)
    expect_equal(ncol(out), length(scen3))
    expect_equal(colnames(out), names(scen3))
    expect_equal(rowSums(out), rep.int(1, fit.keep$n.respondents))

})

## test_that("prediction with numeric ASCs")

## test_that("prediction error if wrong ASCs specified")

## test_that("prediction error no none estimated")

offset <- matrix(seq(0, 3, length = length(scen)*fit$n.respondents),
                 fit$n.respondents, length(scen))
scale <- seq(0, 1, length = fit$n.respondents)
test_that("prediction with offset", {
    out <- predict(fit, scen, rule = "first choice respondent",
                   offset = offset)
    expect_equal(tabulate(apply(out, 1, which.max), 2),
                 c(0, fit$n.respondents))
    ## incorrect dimension
    expect_error(predict(fit, scen, rule = "first choice respondent",
                   offset = offset[-1, ]), ".offset. must be a numeric matrix")
})

test_that("prediction with scale", {
    out <- predict(fit, scen, rule = "first choice respondent",
                   offset = offset, scale = scale)
    expect_equal(tabulate(apply(out, 1, which.max), 2),
                 c(0, fit$n.respondents))

    ## scale with no offset
    out <- predict(fit, scen, rule = "first choice respondent",
                   scale = scale)
    expect_equal(rowSums(out), rep.int(1, fit$n.respondents))

    ## incorrect length
    expect_error(predict(fit, scen, rule = "first choice respondent",
                         scale = scale[-(1:10)]),
                 paste0(".scale. must have length ", fit$n.respondents))


})

test_that("prediction error with certain rules if no beta draws kept",
{
    expect_error(predict(fit, scen, rule = "logit draw"),
                 "^Rule .logit draw. is not available")
    expect_error(predict(fit, scen, rule = "first choice draw"),
             "^Rule .first choice draw. is not available")
})


## test_that("prediction without named alternatives")

## test_that("prediction with covariates")

test_that("Prediction with labelled alternatives",
{
    al <- list(Brand = LETTERS[1:3], Att2 = 1:2, Att3 = 1:3)
    des <- ChoiceModelDesign(attribute.levels = al,
                                  n.questions = 10,
                                  n.versions = 4,
                                  alternatives.per.question = 3,
                                  none.alternatives = 1,
                                  labeled.alternatives = TRUE,
                                  seed = 10)
    fit.la <- suppressWarnings(FitChoiceModel(design = des,
                          hb.iterations = 15, hb.chains = 1,
                          hb.warnings = FALSE,
                          include.choice.parameters = FALSE,
                          simulated.priors.from.design = TRUE,
                          simulated.sample.size = 80))

    scen.la <- list("Alt. 1" = c("Brand" = "A", "Att2" = 2, "Att3" = 3),
                      "Alt. 2" = c("Brand" = "A", "Att2" = 2, "Att3" = 1))
    ## scen.none <- list("A" = c("Att2" = 2, "Att3" = 3),
    ##                   "B" = c("Att2" = 2, "Att3" = 1))
    out <- predict(fit.la, scen.la, rule = "first choice respondent")
    expect_equal(nrow(out), fit.la$n.respondents)
    expect_equal(ncol(out), length(scen.la))
    expect_equal(colnames(out), names(scen.la))
    expect_equal(rowSums(out), rep.int(1, fit.la$n.respondents))
})

## DS-1819
## test_that("prediction with continuous attributes")

test_that("Prediction for LCA",
{
    fit.lca <- FitChoiceModel(design = chocolate.design,
                         choices = choices,
                         tasks = tasks,
#                         cov.formula = ~diabetes, cov.data = chocolate,
                         algorithm = "LCA",
                         include.choice.parameters = TRUE,
                         hb.warnings = FALSE, n.classes = 3)
    out <- predict(fit.lca, scen)
    expect_equal(nrow(out), fit.lca$n.respondents)
    expect_equal(ncol(out), length(scen))
    expect_equal(colnames(out), names(scen))
    expect_equal(rowSums(out), rep.int(1, fit.lca$n.respondents))

})

test_that("prediction scenario attributes are case-insensitive",
{
    scen.bad <- list("alt. 1" = c(brand = "Dove", "PrIce" = "$0.99", "Cocoa Strength" = "Dark",
                          "Sugar" = "Standard", "Origin" = "Switzerland",
                          "Nuts" = "Hazelnuts", "Ethical" = "BLANK"),
             "alt. 2" = c("Brand" = "Hershey", "Price" = "$0.99", "Cocoa Strength" = "Dark",
                          "Sugar" = "Standard", OrigiN = "UsA",
                          "Nuts" = "Hazelnuts", "ETHICAL" = "BLANK"))
    scen <- list("alt. 1" = c(Brand = "Dove", "Price" = "$0.99", "Cocoa strength" = "Dark",
                          "Sugar" = "Standard", "Origin" = "Switzerland",
                          "Nuts" = "Hazelnuts", "Ethical" = "BLANK"),
             "alt. 2" = c("Brand" = "Hershey", "Price" = "$0.99", "Cocoa Strength" = "Dark",
                          "Sugar" = "Standard", Origin = "USA",
                          "Nuts" = "Hazelnuts", "Ethical" = "BLANK"))
    out <- predict(fit, scen)
    out.bad <- predict(fit, scen.bad)
    expect_identical(out, out.bad)
    expect_equal(rowSums(out), rep.int(1, fit$n.respondents))
})
