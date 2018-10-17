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
    save.dir <- "~/Documents/Features/ChoiceModelCovariates/simres/diffpriors/"
}else{
    save.dir <- "./"
    .libPaths("/usr/lib/opencpu/library")
    library(flipChoiceMWM)
}

options(mc.cores = parallel::detectCores())
library(rstan)

data("eggs", package = "flipChoice")
data("eggs.cov", package = "flipChoice")
data("eggs.design", package = "flipChoice")
eggs.data <- cbind(version = 1:nrow(eggs.data), eggs.data)
idx <- colnames(eggs.data) == "Choices"
colnames(eggs.data)[idx] <- paste0("choice", 1:8)
for (i in which(idx))
    eggs.data[, i] <- vapply(as.character(eggs.data[, i]), switch, 1, A = 1, B = 2, C = 3)
## eggs.data <- lapply(eggs.data[, idx], function(x) as.numeric(levels(x))[x])


start.age <- as.numeric(sub("-[0-9]{2}", "", levels(eggs.cov$age)))
end.age <- as.numeric(sub("[0-9]{2}-", "", levels(eggs.cov$age)))
mid.age <- mapply(function(x,y) mean(c(x,y)), start.age, end.age)
age.n <- eggs.cov$age
levels(age.n) <- mid.age
age.n <- as.numeric(levels(age.n)[as.numeric(age.n)])
eggs.cov$age.numeric <- scale(age.n)

choices <- eggs.data[, grepl("Choices", colnames(eggs.data))]
questions <- matrix(unique(eggs.design$design[, "Task"]), ncol = ncol(choices),
                byrow = TRUE)
## frml <- ~age2+gender
## frml <- ~egg.choice2
## frml.fc <- ~egg.choice+age.numeric
## frml.rc <- ~age.numeric+(1|egg.choice)
frml.fc <- ~egg.choice
frml.rc <- ~(1|egg.choice)


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

reduced <- TRUE
include.choice.parameters <- FALSE  # indicator for alternative number
## sim.setting <- if (is.rserver){
##                    as.integer(commandArgs(trailingOnly = TRUE)[[2L]])
##                }else
##                    1
sim.settings <- 1:3

n.iter <- 750
n.sims <- 3
n.leave.out.q <- c(7, 4, 1)
n.chains <- parallel::detectCores()
sseed <- c(21012, 110, 98)

if (reduced){
    attr.name <- "Quality"
    ## cnames <- c("Choices", attr.name)
    ## cnames.all <- colnames(eggs.data)[colnames(eggs.data) %in% cnames]
    ## eggs.data <- eggs.data[, colnames(eggs.data) %in% cnames]
    ## colnames(eggs.data) <- cnames.all
    cnames <- c("Version", "Task", "Question", "Alternative", attr.name)
    eggs.design$design <- eggs.design$design[, cnames]
    eggs.design$design.with.none <- eggs.design$design.with.none[, cnames]
    eggs.design$attribute.levels <- eggs.design$attribute.levels[attr.name]

}

comp.stats <- array(dim = c(n.sims, 3, 12))
comp.stats.all <- array(dim = c(length(sim.settings), length(n.leave.out.q),
                                n.sims, 3, 12))
## origin.stanModel.b <- body(flipChoice:::stanModel)[[3]]
orig.stanModel <- flipChoice:::stanModel
pb <- utils::txtProgressBar(min = 0, max = n.sims*3*length(n.leave.out.q),
                            initial = 0, char = "*",
                            width = NA, style = 3)
for (sim.setting in sim.settings){
    if (sim.setting == 1){  # defaults
        hb.sigma.prior.shape <- 1.394357
        hb.sigma.prior.scale <- 0.394357
        hb.lkj.prior.shape <- 4
    }else if (sim.setting == 2){  # zero correlation
        hb.sigma.prior.shape <- 1.394357
        hb.sigma.prior.scale <- 0.394357
        hb.lkj.prior.shape <- 1e6
    }else{  # zero corr. and near-zero var.
        hb.sigma.prior.shape <- 10
        hb.sigma.prior.scale <- 10000
        hb.lkj.prior.shape <- 1e6
    }

    for (j in seq_along(n.leave.out.q))
    {
      for (i in 1:n.sims)
      {
        ## body(flipChoice:::stanModel)[[3]] <- quote(stanmodels$choicemodelRC)
        assignInNamespace("stanModel", orig.stanModel, "flipChoice")
        ## result <- try(FitChoiceModel(experiment.data = eggs.data, hb.iterations = n.iter,
        ##                              hb.chains = n.chains, tasks.left.out = n.leave.out.q[j],
        ##                              hb.sigma.prior.shape = hb.sigma.prior.shape,
        ##                              hb.sigma.prior.scale = hb.sigma.prior.scale,
        ##                              hb.lkj.prior.shape = hb.lkj.prior.shape,
        ##                              include.choice.parameters = include.choice.parameters,
        ##                              seed = i + sseed[j], open_progress = FALSE))
        result <- try(FitChoiceModel(design = eggs.design, choices = choices,
                                 questions = questions, hb.iterations = n.iter,
##                                 subset = subset,
##                             cov.formula = frml, cov.data = fast.food,
                                 include.choice.parameters = include.choice.parameters,
                                 hb.chains = n.chains, hb.warnings = FALSE,
                                 hb.sigma.prior.shape = hb.sigma.prior.shape,
                                 hb.sigma.prior.scale = hb.sigma.prior.scale,
                                 hb.lkj.prior.shape = hb.lkj.prior.shape,
                                 tasks.left.out = n.leave.out.q[j],
                                 seed = i+sseed[j], open_progress = FALSE))
        if (!inherits(result, "try-error"))
            comp.stats[i, 1, ] <- GetStats(result)
        utils::setTxtProgressBar(pb,
                     length(sim.settings)*(sim.setting-1)+length(n.leave.out.q)*(j-1)+3*(i-1)+1)

        frml <- frml.fc
        result <- try(FitChoiceModel(design = eggs.design, choices = choices,
                                 questions = questions, hb.iterations = n.iter,
##                                 subset = subset,
                                 cov.formula = frml, cov.data = eggs.cov,
                                 include.choice.parameters = include.choice.parameters,
                                 hb.chains = n.chains, hb.warnings = FALSE,
                                 hb.sigma.prior.shape = hb.sigma.prior.shape,
                                 hb.sigma.prior.scale = hb.sigma.prior.scale,
                                 hb.lkj.prior.shape = hb.lkj.prior.shape,
                                 tasks.left.out = n.leave.out.q[j],
                                 seed = i+sseed[j], open_progress = FALSE))
        ## samps <- extract(result$stan.fit, pars = c("theta", "sigma"))
        ## samps <- do.call(cbind, samps)
        if (!inherits(result, "try-error"))
            comp.stats[i, 2, ] <- GetStats(result)
          utils::setTxtProgressBar(pb,
                       length(sim.settings)*(sim.setting-1)+length(n.leave.out.q)*(j-1)+3*(i-1)+2)

        ## body(flipChoice:::stanModel)[[3]] <- origin.stanModel.b
        frml <- frml.rc
        assignInNamespace("stanModel", function(a, b, c) flipChoice:::stanmodels$choicemodelRCdiag,
                          "flipChoice")
        result <- try(FitChoiceModel(design = eggs.design, choices = choices,
                                 questions = questions, hb.iterations = n.iter,
##                                 subset = subset,
                                 cov.formula = frml, cov.data = eggs.cov,
                                 include.choice.parameters = include.choice.parameters,
                                 hb.chains = n.chains, hb.warnings = FALSE,
                                 hb.sigma.prior.shape = hb.sigma.prior.shape,
                                 hb.sigma.prior.scale = hb.sigma.prior.scale,
                                 hb.lkj.prior.shape = hb.lkj.prior.shape,
                                 tasks.left.out = n.leave.out.q[j],
                                 seed = i+sseed[j], open_progress = FALSE))

        if (!inherits(result, "try-error"))
            comp.stats[i, 3, ] <- GetStats(result)
        utils::setTxtProgressBar(pb,
                    length(sim.settings)*(sim.setting-1)+length(n.leave.out.q)*(j-1)+3*(i-1))
        flush.console()
      }
      comp.stats.all[sim.setting, j, , , ] <- comp.stats
      dimnames(comp.stats) <- list(NULL, c("NoCov", "Fixed", "Random"),
                                   c("mean.rhat.theta", "mean.neff.theta",
                                     "mean.neff.per.sec.theta", "mean.rhat.sigma", "mean.neff.sigma",
                                     "mean.neff.per.sec.sigma", "max.rhat", "min.neff",
                                     "min.neff.per.sec", "in.acc", "out.acc", "time"))
      attr(comp.stats, "n.sims") <- n.sims
      attr(comp.stats, "n.iter") <- n.iter
      attr(comp.stats, "n.chains") <- n.chains
      attr(comp.stats, "n.questions.left.out") <- n.leave.out.q[j]
      attr(comp.stats, "start.seed") <- sseed[j]
      attr(comp.stats, "formula.iter") <- frml
      saveRDS(comp.stats, paste0(save.dir, "eggs",
                                 n.sims, "sims", n.leave.out.q[j], "QLeftOutCovar_",
                                 paste(all.vars(frml), collapse = "_"),
                                 switch(sim.setting, "1" = "DefaultPriors",
                                        "2" = "ZeroCorrPrior",
                                        "ZeroScalePrior"),
                                 if (reduced) "QualityAttrOnly", Sys.Date(), ".rds"))

  }
}
dimnames(comp.stats.all) <- list(c("default", "zero corr", "zero var"),
                             paste0("left.out", n.leave.out.q, "q"),
                             c("NoCov", "Fixed", "Random"),
                             NULL,
                                   c("mean.rhat.theta", "mean.neff.theta",
                                     "mean.neff.per.sec.theta", "mean.rhat.sigma", "mean.neff.sigma",
                                     "mean.neff.per.sec.sigma", "max.rhat", "min.neff",
                                     "min.neff.per.sec", "in.acc", "out.acc", "time"))
saveRDS(comp.stats.all, "~/Documents/Features/ChoiceModelCovariates/simres/diffpriors/eggsAll.rds")
colMeans(comp.stats, dim = 1)
