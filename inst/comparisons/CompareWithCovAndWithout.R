
## Note if seeing errors from the 2nd call to FitChoiceModel when trying to use mulitple chains such as:
## Error in error.handler(e) :
##   8 nodes produced errors; first error: object 'model_choicemodelFC' not found
## then you need to update the installed package (the error comes from multiple versions of the
## package being installed
## http://discourse.mc-stan.org/t/building-r-package-that-uses-stan-running-multiple-chains-error/2485
devtools::load_all("~/flip/flipChoice")

data("eggs", package = "flipChoice")
data("eggs.cov", package = "flipChoice")

frml <- ~age2+gender

GetStats <- function(res){
    samps <- as.array(res$stan.fit)
    samps <- samps[, , grepl("theta", dimnames(samps)[[3]]), drop = FALSE]
    chain.stats <- monitor(samps, warmup = 0, probs = .5, print = FALSE)
    rhats.t <- chain.stats[, "Rhat"]
    neffs.t <- chain.stats[, "n_eff"]
    samps <- as.array(res$stan.fit)
    samps <- samps[, , grepl("sigma", dimnames(samps)[[3]]), drop = FALSE]
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


n.iter <- 100
n.sims <- 10
n.leave.out.q <- 2
n.chains <- 8
comp.stats <- array(dim = c(n.sims, 2, 12))
for (i in 1:n.sims)
{
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = n.iter,
                             cov.formula = NULL, cov.data = NULL,
                                 hb.chains = n.chains, hb.warnings = FALSE, tasks.left.out = n.leave.out.q,
                                 seed = i+213)
    ## samps <- extract(result$stan.fit, pars = c("theta", "sigma"))
    ## samps <- do.call(cbind, samps)
    comp.stats[i, 1, ] <- GetStats(result)

    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = n.iter,
                                                          cov.formula = frml, cov.data = eggs.cov,
                                 hb.chains = n.chains, hb.warnings = FALSE, tasks.left.out = n.leave.out.q,
                                 seed = i+213)
    comp.stats[i, 2, ] <- GetStats(result)
}
dimnames(comp.stats)[[3]] <- c("mean.rhat.theta", "mean.neff.theta",
                               "mean.neff.per.sec.theta", "mean.rhat.sigma", "mean.neff.sigma",
                               "mean.neff.per.sec.sigma", "max.rhat", "min.neff",
                               "min.neff.per.sec", "in.acc", "out.acc", "time")
saveRDS(comp.stats, paste0("../../Documents/ChoiceModelCovariates/eggs",
        n.iter, "sims", n.chains, "chainsCovar_", paste(all.vars(frml), collapse = "_"), ".rds"))
colMeans(comp.stats, dim = 1)
