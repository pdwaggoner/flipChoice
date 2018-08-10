
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

library(rstan)
options(mc.cores = parallel::detectCores())

data("chocolate", package = "flipChoiceMWM")
data("chocolate.design", package = "flipChoiceMWM")

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

data(fast.food, package = "flipChoiceMWM")
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

choices <- chocolate[, grepl("^choice", colnames(chocolate))]
questions <- chocolate[, grepl("^task", colnames(chocolate))]

## frml <- ~age2+gender
## frml <- ~(1|age)  # +income+high.blood.pressure
## frml <- ~(1|age) + (1|ethnicity)
frml.fc <- ~ethnicity + region
frml.rc <- ~(1|ethnicity) + (1|region)

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

n.iter <- 750
n.sims <- 3
n.leave.out.q <- 5
n.chains <- parallel::detectCores()  # 1
sseed <- 331341
comp.stats <- array(dim = c(n.sims, 3, 12))
## origin.stanModel.b <- body(flipChoiceMWM:::stanModel)[[3]]
orig.stanModel <- flipChoiceMWM:::stanModel
pb <- utils::txtProgressBar(min = 0, max = n.sims*3, initial = 0, char = "*",
                    width = NA, style = 3)
for (i in 1:n.sims)
{
    ## body(flipChoice:::stanModel)[[3]] <- quote(stanmodels$choicemodelRC)
    ## assignInNamespace("stanModel", orig.stanModel, "flipChoice")
    result <- try(FitChoiceModel(design = chocolate.design, choices = choices,
                                 questions = questions, hb.iterations = n.iter,
#                                 subset = subset,
#                             cov.formula = frml, cov.data = chocolate,
                             hb.chains = n.chains, hb.warnings = FALSE, tasks.left.out = n.leave.out.q,
                             seed = i+sseed))
    if (!inherits(result, "try-error"))
        comp.stats[i, 1, ] <- GetStats(result)
    utils::setTxtProgressBar(pb, 3*(i-1)+1)

    frml <- frml.fc
    result <- try(FitChoiceModel(design = chocolate.design, choices = choices,
                                 questions = questions, hb.iterations = n.iter,
#                                 subset = subset,
                             cov.formula = frml, cov.data = chocolate,
                             hb.chains = n.chains, hb.warnings = FALSE, tasks.left.out = n.leave.out.q,
                             seed = i+sseed))
    ## samps <- extract(result$stan.fit, pars = c("theta", "sigma"))
    ## samps <- do.call(cbind, samps)
    if (!inherits(result, "try-error"))
        comp.stats[i, 2, ] <- GetStats(result)
    utils::setTxtProgressBar(pb, 3*(i-1)+2)

    # body(flipChoice:::stanModel)[[3]] <- origin.stanModel.b
    frml <- frml.rc
    assignInNamespace("stanModel", function(a, b, c) flipChoiceMWM:::stanmodels$choicemodelRCdiag,
                      "flipChoiceMWM")
    result <- try(FitChoiceModel(design = chocolate.design, choices = choices,
                                 questions = questions, hb.iterations = n.iter,
 #                                subset = subset,
                             cov.formula = frml, cov.data = chocolate,
                             hb.chains = n.chains, hb.warnings = FALSE, tasks.left.out = n.leave.out.q,
                             seed = i+sseed))
    if (!inherits(result, "try-error"))
        comp.stats[i, 3, ] <- GetStats(result)
    utils::setTxtProgressBar(pb, 3*i)
    flush.console()

    assignInNamespace("stanModel", orig.stanModel, "flipChoiceMWM")
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
                           "RandomDiag", Sys.Date(), ".rds"))
colMeans(comp.stats, dim = 1)
