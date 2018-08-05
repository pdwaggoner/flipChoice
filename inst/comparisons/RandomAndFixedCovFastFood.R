
## Note if seeing errors from the 2nd call to FitChoiceModel when trying to use mulitple chains such as:
## Error in error.handler(e) :
##   8 nodes produced errors; first error: object 'model_choicemodelFC' not found
## then you need to update the installed package (the error comes from multiple versions of the
## package being installed
## http://discourse.mc-stan.org/t/building-r-package-that-uses-stan-running-multiple-chains-error/2485
is.rserver <- is.rserver <- Sys.info()[["nodename"]] == "reusdev" || grepl("^reustest.*", node.name) ||
                  grepl("^reusprod.*", node.name)
if (!is.rserver){
    devtools::load_all("~/flip/flipChoice")
    save.dir <- "../../Documents/Features/ChoiceModelCovariates/"
}else{
    save.dir <- "./"
    .libPaths("/usr/")
    library(flipChoice)
}

library(rstan)
options(mc.cores = parallel::detectCores())

data("fast.food", package = "flipChoice")
data("fast.food.design", package = "flipChoice")

## remove 3 respondents who didn't state gender
## Need to subset here until DS-2057 completed

## fast.food <- fast.food[fast.food$gender != "Other / Prefer not to say", ]
## subset <- fast.food$gender != "Other / Prefer not to say"
## fast.food <- fast.food[subset, ]
## fast.food$gender <- droplevels(fast.food$gender)

## subset <- fast.food$age != "Under 15 years"
## fast.food <- fast.food[subset, ]
## fast.food$age <- droplevels(fast.food$age)

## levels(fast.food$age) <- c("Under 35", "Under 35", "35 to 54 years", "35 to 54 years",
##                            "55 years and over", "55 years and over", "Under 35")
levels(fast.food$age) <- c("Under 45", "Under 45", "Under 45", "45 years and over",
                           "45 years and over", "45 years and over", "Under 45")
levels(fast.food$ethnicity) <- c("Non-white", "Non-white", "Non-white", "Non-white",
                                 "Non-white", "White")


choices <- fast.food[, grepl("^choice", colnames(fast.food))]
questions <- fast.food[, grepl("^task", colnames(fast.food))]

## frml <- ~age2+gender
## frml <- ~(1|age)  # +income+high.blood.pressure
frml <- ~(1|age) + (1|ethnicity)
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

n.iter <- 500
n.sims <- 15
n.leave.out.q <- 10
n.chains <- parallel::detectCores()/2  # 1
sseed <- 3334
comp.stats <- array(dim = c(n.sims, 3, 12))
## origin.stanModel.b <- body(flipChoice:::stanModel)[[3]]
orig.stanModel <- flipChoice:::stanModel
for (i in 1:n.sims)
{
    ## body(flipChoice:::stanModel)[[3]] <- quote(stanmodels$choicemodelRC)
    assignInNamespace("stanModel", orig.stanModel, "flipChoice")
    result <- try(FitChoiceModel(design = fast.food.design, choices = choices,
                                 questions = questions, hb.iterations = n.iter,
#                                 subset = subset,
#                             cov.formula = frml, cov.data = fast.food,
                             hb.chains = n.chains, hb.warnings = FALSE, tasks.left.out = n.leave.out.q,
                             seed = i+sseed))
    if (!inherits(result, "try-error"))
        comp.stats[i, 1, ] <- GetStats(result)

    result <- try(FitChoiceModel(design = fast.food.design, choices = choices,
                                 questions = questions, hb.iterations = n.iter,
#                                 subset = subset,
                             cov.formula = frml, cov.data = fast.food,
                             hb.chains = n.chains, hb.warnings = FALSE, tasks.left.out = n.leave.out.q,
                             seed = i+sseed))
    ## samps <- extract(result$stan.fit, pars = c("theta", "sigma"))
    ## samps <- do.call(cbind, samps)
    if (!inherits(result, "try-error"))
        comp.stats[i, 2, ] <- GetStats(result)

    # body(flipChoice:::stanModel)[[3]] <- origin.stanModel.b
    assignInNamespace("stanModel", function(a, b, c) flipChoice:::stanmodels$choicemodelRCdiag,
                      "flipChoice")
    result <- try(FitChoiceModel(design = fast.food.design, choices = choices,
                                 questions = questions, hb.iterations = n.iter,
 #                                subset = subset,
                             cov.formula = frml, cov.data = fast.food,
                             hb.chains = n.chains, hb.warnings = FALSE, tasks.left.out = n.leave.out.q,
                             seed = i+sseed))
    if (!inherits(result, "try-error"))
        comp.stats[i, 3, ] <- GetStats(result)

}
dimnames(comp.stats) <- list(NULL, c("No Cov.", "Fixed", "Random"),
                             c("mean.rhat.theta", "mean.neff.theta",
                               "mean.neff.per.sec.theta", "mean.rhat.sigma", "mean.neff.sigma",
                               "mean.neff.per.sec.sigma", "max.rhat", "min.neff",
                               "min.neff.per.sec", "in.acc", "out.acc", "time"))
saveRDS(comp.stats, paste0(save.dir, "fastfood",
                           n.iter, "sims", n.leave.out.q, "QLeftOutCovar_",
                           paste(all.vars(frml), collapse = "_"),
                           "RandomDiag", Sys.Date(), ".rds"))
colMeans(comp.stats, dim = 1)
