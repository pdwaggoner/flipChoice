
## Note if seeing errors from the 2nd call to FitChoiceModel when trying to use mulitple chains such as:
## Error in error.handler(e) :
##   8 nodes produced errors; first error: object 'model_choicemodelFC' not found
## then you need to update the installed package (the error comes from multiple versions of the
## package being installed
## http://discourse.mc-stan.org/t/building-r-package-that-uses-stan-running-multiple-chains-error/2485
node.name <- Sys.info()[["nodename"]]
is.rserver <- node.name == "reusdev" || grepl("^reustest.*", node.name) ||
                  grepl("^reusprod.*", node.name)
if (!is.rserver){
    devtools::load_all("~/flip/flipChoice")
    save.dir <- "~/Documents/Features/ChoiceModelCovariates/simres/"
}else{
    save.dir <- "./"
    .libPaths("/usr/lib/opencpu/library")
    library(flipChoice)
}

library(rstan)
options(mc.cores = parallel::detectCores())

createSawtoothDualCSV <- FALSE

data("chocolate", package = "flipChoice")
data("chocolate.design", package = "flipChoice")

## remove 3 respondents who didn't state gender
## Need to subset here until DS-2057 completed

## chocolate <- chocolate[chocolate$gender != "Other / Prefer not to say", ]
## subset <- chocolate$gender != "Other / Prefer not to say"
## chocolate <- chocolate[subset, ]
## chocolate$gender <- droplevels(chocolate$gender)

## subset <- chocolate$age != "Under 15 years"
## chocolate <- chocolate[subset, ]
## chocolate$age <- droplevels(chocolate$age)

## levels(chocolate$age) <- c("Under 35", "Under 35", "35 to 54 years", "35 to 54 years",
##                            "55 years and over", "55 years and over", "Under 35")

## 45 states + other in state variable;
## missing: vermont, DC, alaska, Maine, Delaware, South Dakota
chocolate$region <- chocolate$state
levels(chocolate$region) <- c("South",     # Alabama
                             "West",       # Arizona
                             "South",      # Arkansas
                             "West",       # California
                             "West",       # Colorado,
                             "Northeast",  # Conneticut
                             "South",      # Florida
                             "South",      # Georgia
                             "West",       # "Hawaii"
                             "Other",      # "I do not reside in the United States"
                             "West",       # "Idaho"
                             "Midwest",    # "Illinois"
                             "Midwest",    # "Indiana"
                             "Midwest",    # "Iowa"
                             "Midwest",    # "Kansas"
                             "South",      # "Kentucky"
                             "South",      # "Louisiana"
                             "South",      # "Maryland"
                             "Northeast",  # "Massachusetts"
                             "Midwest",    # "Michigan"
                             "Midwest",    # "Minnesota"
                             "South",      # "Mississippi"
                             "Midwest",    # "Missouri"
                             "West",       # "Montana"
                             "Midwest",    # "Nebraska"
                             "West",       # "Nevada"
                             "Northeast",  # "New Hampshire"
                             "Northeast",  # "New Jersey"
                             "West",       # "New Mexico"
                             "Northeast",  # "New York"
                             "South",      # "North Carolina"
                             "Midwest",    # "North Dakota"
                             "Midwest",    # "Ohio"
                             "South",      # "Oklahoma"
                             "West",       # "Oregon"
                             "Northeast",  # "Pennsylvania"
                             "Northeast",  # "Rhode Island"
                             "South",      # "South Carolina"
                             "South",      # "Tennessee"
                             "South",      # "Texas"
                             "West",       # "Utah"
                             "South",      # "Virginia"
                             "West",       # "Washington"
                             "South",      # "West Virginia"
                             "Midwest",    # "Wisconsin"
                             "West"        # "Wyoming"
                             )

chocolate$bmi <- chocolate$weight/chocolate$height^2*703
chocolate$overweight <- factor(chocolate$bmi > 25, labels = c("no", "yes"))
chocolate$obese <- factor(chocolate$bmi > 30, labels = c("no", "yes"))
chocolate$under45 <- chocolate$age
levels(chocolate$under45) <- c("Under 45", "Under 45", "Under 45", "45 years and over",
                           "45 years and over", "45 years and over", "Under 45")

chocolate$caucasian <- chocolate$ethnicity
levels(chocolate$caucasian) <- c("Non-white", "Non-white", "Non-white", "Non-white",
                          "Non-white", "White")

chocolate$age.numeric <- vapply(as.numeric(chocolate$age), function(x) switch(x, "1" = 20, "2" = 30,
                                                              "3" = 40, "4" = 50,
                                                              "5" = 60, "6" = 70, "7" = 10),
                0)
chocolate$income.numeric <- vapply(as.numeric(chocolate$income),
                                   function(x) switch(x, "1" = 12500, "2" = 125000,
                                                      "3" = 137500, "4" = 175000,
                                                      "6" = 225000, "7" = 37500,
                                                      "8" = 62500, "9" = 87500),
                                   0)

data(fast.food, package = "flipChoice")
cf <- fast.food[, grep("^choice", colnames(fast.food))]
cc <- chocolate[, grep("^choice", colnames(chocolate))]
i.f <- which(apply(cf, 1, function(x) sd(x) == 0))
i.c <- which(apply(cc, 1, function(x) sd(x) == 0))

bad.idx <- c(i.f, i.c[length(i.c)],
             which(chocolate$state == "I do not reside in the United States"))
## subset <- chocolate$region != "Other"
chocolate <- chocolate[-bad.idx, ]
chocolate$region <- droplevels(chocolate$region)
chocolate$state <- droplevels(chocolate$state)
## scale after dropping bad obs
chocolate$age.numeric <- drop(scale(chocolate$age.numeric))
chocolate$bmi.s <- drop(scale(chocolate$bmi))
chocolate$income.numeric <- drop(scale(chocolate$income.numeric))
chocolate$delivery.numeric <- drop(scale(as.numeric(chocolate$delivery.under.30min)))

choices <- chocolate[, grepl("^choice", colnames(chocolate))]
questions <- chocolate[, grepl("^task", colnames(chocolate))]

## frml <- ~age2+gender
## frml <- ~(1|age)  # +income+high.blood.pressure
## frml <- ~(1|age) + (1|ethnicity)
## frml.fc <- ~ethnicity + region
## frml.rc <- ~(1|ethnicity) + (1|region)
# frml.fc <- ~age.numeric + bmi.s + income
# frml.rc <- ~age.numeric + bmi.s + (1|income)
frml.fc <- ~diabetes
frml.rc <- ~(1|diabetes)

GetStats <- function(res){
    samps <- as.array(res$stan.fit)
    samps <- samps[, , grepl("theta|resp_fixed_coef|resp_rand_eff", dimnames(samps)[[3]]),
                   drop = FALSE]
    chain.stats <- monitor(samps, warmup = 0, probs = .5, print = FALSE)
    rhats.t <- chain.stats[, "Rhat"]
    neffs.t <- chain.stats[, "n_eff"]
    samps <- as.array(res$stan.fit)
    samps <- samps[, , grepl("^sig(ma|_fc|_rc)", dimnames(samps)[[3]]), drop = FALSE]
    chain.stats <- monitor(samps, warmup = 0, probs = .5, print = FALSE)
    rhats.s <- chain.stats[, "Rhat"]
    neffs.s <- chain.stats[, "n_eff"]
    c(mean.rhat.theta = mean(rhats.t), mean.neff.theta = mean(neffs.t),
                            mean.neff.per.sec.theta = mean(neffs.t)/res$time.take,
                            mean.rhat.sigma = mean(rhats.s), mean.neff.sigma = mean(neffs.s),
             mean.neff.per.sec.sigma = mean(neffs.s)/res$time.take,
             max.rhat = max(rhats.t, rhats.s), min.neff = min(neffs.t, neffs.s),
             min.neff.per.sec = min(neffs.t, neffs.s)/res$time.take,
             in.acc = res$in.sample.accuracy,
             out.acc = res$out.sample.accuracy, time = res$time.take)
}

reduced <- TRUE
include.choice.parameters <- FALSE  # indicator for alternative number
sim.setting <- if (is.rserver){
                   as.integer(commandArgs(trailingOnly = TRUE)[[2L]])
               }else
                   1

if (sim.setting == 1){  # defaults
    hb.sigma.prior.shape <- 1.394357
    hb.sigma.prior.scale <- 0.394357
    hb.lkj.prior.shape <- 4
}else if (sim.setting == 2){  # zero correlation
    hb.sigma.prior.shape <- 1.394357
    hb.sigma.prior.scale <- 0.394357
    hb.lkj.prior.shape <- 1e8
}else{  # zero corr. and near-zero var.
    hb.sigma.prior.shape <- 10
    hb.sigma.prior.scale <- 10000
    hb.lkj.prior.shape <- 1e8
}

n.iter <- 1000
n.sims <- 3
n.leave.out.q <- 5
n.chains <- parallel::detectCores()  # 1
sseed <- 38911


if (reduced){
    attr.name <- "Sugar"
    cnames <- c("Version", "Task", "Question", "Alternative", attr.name)
    chocolate.design$design.with.none <- chocolate.design$design.with.none[, cnames]
    chocolate.design$attribute.levels <- chocolate.design$attribute.levels[attr.name]
    chocolate.design$n.attributes <- 1
}

if (createSawtoothDualCSV){
    fprefix <- if (reduced){ make.names(attr.name) }else "chocolate"
    for (i in seq_len(n.sims)){
      fname <- paste0(fprefix, sseed+i)
      SawtoothDualCsv(design = chocolate.design,
                 respondent.data = chocolate,
                 covariates.data = chocolate[, 20:48],
                 dual.response.none = FALSE,
                 design.file = paste0(save.dir, fname, "_design.csv"),
                 design.out.file = paste0(save.dir, fname, "_design_out.csv"),
                 respondent.file = paste0(save.dir, fname, "_data.csv"),
                 respondent.out.file = paste0(save.dir, fname, "_data_out.csv"),
                 covariates.file = paste0(save.dir, fname, "_covariates.csv"),
                 n.questions.left.out = n.leave.out.q, subset = rep(TRUE, nrow(fast.food)),
                 include.choice.parameters = include.choice.parameters,
                 seed = sseed+i)
    }
    zip(paste0(attr.name, ".zip"), list.files(save.dir, pattern = fprefix))
}


comp.stats <- array(dim = c(n.sims, 3, 12))
## origin.stanModel.b <- body(flipChoice:::stanModel)[[3]]
orig.stanModel <- flipChoice:::stanModel
pb <- utils::txtProgressBar(min = 0, max = n.sims*3, initial = 0, char = "*",
                    width = NA, style = 3)
for (i in 1:n.sims)
{
    ## body(flipChoice:::stanModel)[[3]] <- quote(stanmodels$choicemodelRC)
    ## assignInNamespace("stanModel", orig.stanModel, "flipChoice")
    result <- try(FitChoiceModel(design = chocolate.design, choices = choices,
                                 questions = questions, hb.iterations = n.iter,
##                                 subset = subset,
##                             cov.formula = frml, cov.data = chocolate,
                                 include.choice.parameters = include.choice.parameters,
                                 hb.chains = n.chains, hb.warnings = FALSE,
                                 hb.sigma.prior.shape = hb.sigma.prior.shape,
                                 hb.sigma.prior.scale = hb.sigma.prior.scale,
                                 hb.lkj.prior.shape = hb.lkj.prior.shape,
                                 tasks.left.out = n.leave.out.q,
                                 hb.beta.draws.to.keep = n.iter, hb.keep.samples = TRUE,
                                 hb.stanfit = TRUE,
                                 seed = i+sseed))

    if (!inherits(result, "try-error"))
        comp.stats[i, 1, ] <- GetStats(result)
    utils::setTxtProgressBar(pb, 3*(i-1)+1)

    frml <- frml.fc
    result <- try(FitChoiceModel(design = chocolate.design, choices = choices,
                                 questions = questions, hb.iterations = n.iter,
#                                 subset = subset,
                                 cov.formula = frml, cov.data = chocolate,
                                 include.choice.parameters = include.choice.parameters,
                                 hb.chains = n.chains, hb.warnings = FALSE,
                                 hb.sigma.prior.shape = hb.sigma.prior.shape,
                                 hb.sigma.prior.scale = hb.sigma.prior.scale,
                                 hb.lkj.prior.shape = hb.lkj.prior.shape,
                                 tasks.left.out = n.leave.out.q,
                             seed = i+sseed))
    ## samps <- extract(result$stan.fit, pars = c("theta", "sigma"))
    ## samps <- do.call(cbind, samps)
    if (!inherits(result, "try-error"))
        comp.stats[i, 2, ] <- GetStats(result)
    utils::setTxtProgressBar(pb, 3*(i-1)+2)

    # body(flipChoice:::stanModel)[[3]] <- origin.stanModel.b
    frml <- frml.rc
    assignInNamespace("stanModel", function(a, b, c) flipChoice:::stanmodels$choicemodelRCdiag,
                      "flipChoice")
    result <- try(FitChoiceModel(design = chocolate.design, choices = choices,
                                 questions = questions, hb.iterations = n.iter,
 #                                subset = subset,
                                 cov.formula = frml, cov.data = chocolate,
                                 hb.chains = n.chains, hb.warnings = FALSE,
                                 tasks.left.out = n.leave.out.q,
                                 hb.beta.draws.to.keep = n.iter, hb.keep.samples = TRUE,
                                 hb.sigma.prior.shape = hb.sigma.prior.shape,
                                 hb.sigma.prior.scale = hb.sigma.prior.scale,
                                 hb.lkj.prior.shape = hb.lkj.prior.shape,
                                 include.choice.parameters = include.choice.parameters,
                                 hb.stanfit = TRUE,
                                 seed = i+sseed))
    if (!inherits(result, "try-error"))
        comp.stats[i, 3, ] <- GetStats(result)
    utils::setTxtProgressBar(pb, 3*i)
    flush.console()

    assignInNamespace("stanModel", orig.stanModel, "flipChoice")
}
dimnames(comp.stats) <- list(NULL, c("No Cov.", "Fixed", "Random"),
                             c("mean.rhat.theta", "mean.neff.theta",
                               "mean.neff.per.sec.theta", "mean.rhat.sigma", "mean.neff.sigma",
                               "mean.neff.per.sec.sigma", "max.rhat", "min.neff",
                               "min.neff.per.sec", "in.acc", "out.acc", "time"))
attr(comp.stats, "n.sims") <- n.sims
attr(comp.stats, "n.iter") <- n.iter
attr(comp.stats, "n.chains") <- n.chains
attr(comp.stats, "n.questions.left.out") <- n.leave.out.q
attr(comp.stats, "start.seed") <- sseed
attr(comp.stats, "formula.iter") <- frml
saveRDS(comp.stats, paste0(save.dir, "chocolate",
                           n.sims, "sims", n.leave.out.q, "QLeftOutCovar_",
                           paste(all.vars(frml), collapse = "_"),
                           switch(sim.setting, "1" = "DefaultPriors",
                                  "2" = "ZeroCorrPrior",
                                  "ZeroScalePrior"),
                           if (reduced) "SugarAttrOnly", Sys.Date(), ".rds"))
colMeans(comp.stats, dim = 1)

