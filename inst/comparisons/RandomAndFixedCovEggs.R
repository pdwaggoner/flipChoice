#!/usr/bin/Rscript

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
    save.dir <- "../../Documents/Features/ChoiceModelCovariates/"
}else{
    save.dir <- "./"
    .libPaths("/usr/lib/opencpu/library")
    library(flipChoiceMWM)
}

options(mc.cores = parallel::detectCores())
library(rstan)

data("eggs", package = "flipChoice")
data("eggs.cov", package = "flipChoice")

start.age <- as.numeric(sub("-[0-9]{2}", "", levels(eggs.cov$age)))
end.age <- as.numeric(sub("[0-9]{2}-", "", levels(eggs.cov$age)))
mid.age <- mapply(function(x,y) mean(c(x,y)), start.age, end.age)
age.n <- eggs.cov$age
levels(age.n) <- mid.age
age.n <- as.numeric(levels(age.n)[as.numeric(age.n)])
eggs.cov$age.numeric <- scale(age.n)

## frml <- ~age2+gender
## frml <- ~egg.choice2
frml.fc <- ~egg.choice+age.numeric
frml.rc <- ~age.numeric+(1|egg.choice)

GetStats <- function(res){
    samps <- as.array(res$stan.fit)
    samps <- samps[, , grepl("theta|resp_fixed_coef|resp_rand_eff", dimnames(samps)[[3]]), drop = FALSE]
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

n.iter <- 750
n.sims <- 2
n.leave.out.q <- 7
n.chains <- parallel::detectCores()
sseed <- 210

comp.stats <- array(dim = c(n.sims, 3, 12))
## origin.stanModel.b <- body(flipChoice:::stanModel)[[3]]
orig.stanModel <- flipChoice:::stanModel
pb <- utils::txtProgressBar(min = 0, max = n.sims*3, initial = 0, char = "*",
                    width = NA, style = 3)
for (i in 1:n.sims)
{
    ## body(flipChoice:::stanModel)[[3]] <- quote(stanmodels$choicemodelRC)
    assignInNamespace("stanModel", orig.stanModel, "flipChoice")
    result <- try(FitChoiceModel(experiment.data = eggs.data, hb.iterations = n.iter,
                  hb.chains = n.chains, tasks.left.out = n.leave.out.q, seed = i +sseed))
    if (!inherits(result, "try-error"))
        comp.stats[i, 1, ] <- GetStats(result)
    utils::setTxtProgressBar(pb, 3*(i-1)+1)

    frml <- frml.fc
    result <- try(FitChoiceModel(experiment.data = eggs.data, hb.iterations = n.iter,
                             cov.formula = frml, cov.data = eggs.cov,
                             hb.chains = n.chains, hb.warnings = FALSE, tasks.left.out = n.leave.out.q,
                             seed = i+sseed))
    ## samps <- extract(result$stan.fit, pars = c("theta", "sigma"))
    ## samps <- do.call(cbind, samps)
    if (!inherits(result, "try-error"))
        comp.stats[i, 2, ] <- GetStats(result)
    utils::setTxtProgressBar(pb, 3*(i-1)+2)

    ## body(flipChoice:::stanModel)[[3]] <- origin.stanModel.b
    frml <- frml.rc
    assignInNamespace("stanModel", function(a, b, c) flipChoice:::stanmodels$choicemodelRCdiag,
                      "flipChoice")
    result <- try(FitChoiceModel(experiment.data = eggs.data, hb.iterations = n.iter,
                             cov.formula = frml, cov.data = eggs.cov,
                             hb.chains = n.chains, hb.warnings = FALSE, tasks.left.out = n.leave.out.q,
                             seed = i+sseed))
    if (!inherits(result, "try-error"))
        comp.stats[i, 3, ] <- GetStats(result)
    utils::setTxtProgressBar(pb, 3*i)
    flush.console()
}
dimnames(comp.stats) <- list(NULL, c("NoCov", "Fixed", "Random"),
                             c("mean.rhat.theta", "mean.neff.theta",
                               "mean.neff.per.sec.theta", "mean.rhat.sigma", "mean.neff.sigma",
                               "mean.neff.per.sec.sigma", "max.rhat", "min.neff",
                               "min.neff.per.sec", "in.acc", "out.acc", "time"))
saveRDS(comp.stats, paste0(save.dir, "eggs",
                           n.sims, "sims", n.leave.out.q, "QLeftOutCovar_",
                           paste(all.vars(frml), collapse = "_"),
                           "RandomDiag", Sys.Date(), ".rds"))
colMeans(comp.stats, dim = 1)
