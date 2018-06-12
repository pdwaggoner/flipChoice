context("Experimental design")

# Manual test case
manual.attribute.levels <- list(c("yamaha", "honda", "ducati", "triumph", "bmw", "kawasaki"),
                                c("125cc", "250cc", "500cc"),
                                c("manual", "automatic"),
                                c("red", "green", "blue", "yellow", "black", "white", "silver"))
names(manual.attribute.levels) <- c("brand", "engine size", "transmission", "colour")
manual.prohibitions <- matrix(c("ducati", "125cc", "manual", "red", "ducati", "", "manual", "red", "honda", "All", "", "yellow"),
                              ncol = 4, byrow = TRUE)

# Automated test cases
experiment <- CreateExperiment(c(3, 5, 7, 10), 20)
experiment2 <- CreateExperiment(c(4, 4, 4, 4), 0)
experiment3 <- CreateExperiment(c(8), 0)

levels.test.cases <- list(manual.attribute.levels, experiment$attribute.levels, experiment2$attribute.levels, experiment3$attribute.levels)
prohibitions.test.cases <- list(manual.prohibitions, experiment$prohibitions, experiment2$prohibitions, experiment3$prohibitions)
questions.test.cases <- c(10, 25, 6, 10)
versions.test.cases <- c(2, 1, 6, 1)
alternatives.test.cases <- c(3, 3, 6, 4)
none.test.cases <- c(0, 2, 0, 1)
labeled.test.cases <- c(FALSE, TRUE, FALSE, FALSE)

d.errors <- structure(c(0.7120561, 0.7112814, 0.2140327,
                        1.344702, 0.5107438, 0.5283405, 0.1834341,
                        1.271414, 0.5231956, 0.5631054, 0.1850687,
                        1.271414, 0.5803393, 0.5910509, 0.1885955,
                        1.28026), .Dim = c(4L, 4L), .Dimnames = list(NULL, c("Random",
                                                                                      "Complete enumeration", "Balanced overlap", "Shortcut")))

derr <- matrix(0, ncol = 4, nrow = 4)

tfile <- tempfile()
withr::with_output_sink(tfile, {
    for (model in c("Shortcut")) {  # can't handle prohibitions with "Shortcut"
        for (i in seq(length(levels.test.cases))) {
            test_that(paste(model, "Test case", i), {
                prohibitions <- if(model == "Shortcut") NULL else prohibitions.test.cases[[i]]
                cmd <- ChoiceModelDesign(design.algorithm = model,
                                         attribute.levels = levels.test.cases[[i]],
                                         prior = NULL,
                                         n.questions = questions.test.cases[i],
                                         n.versions = versions.test.cases[i],
                                         alternatives.per.question = alternatives.test.cases[i],
                                         prohibitions = prohibitions,
                                         none.alternatives = none.test.cases[i],
                                         labeled.alternatives = labeled.test.cases[i])
                expect_error(print(cmd), NA)
                expect_equivalent(cmd$d.error, d.errors[i, match(model, colnames(d.errors))], tolerance = 1e-6)
            })
        }
    }
})
unlink(tfile)

experiment4 <- CreateExperiment(c(4, 3, 2, 1), 0)
test_that("Insufficient levels", {
    expect_error(ChoiceModelDesign(design.algorithm = "Random",
                                   attribute.levels = experiment4$attribute.levels,
                                   n.questions = 6,
                                   alternatives.per.question = 3), "All attributes must have at least 2 levels.")
})

# Warning about priors
no.prior <- structure(c("Brand", "Hershey", "Dove", "Godiva", "Lindt", "Price",
               "$0.99", "$1.49", "$1.99", "$2.49", "Cocoa strength", "70%",
               "Dark", "Milk", "White", "Sugar", "Standard", "50% reduced sugar",
               "Sugar free", "", "Origin", "USA", "Switzerland", "Belgium",
               "Belgian (Single origin Venezuelan Criollo beans)", "Nuts", "Almonds",
               "Hazelnuts", "No", "", "Ethical", "Fair trade", "BLANK", "",
               ""), .Dim = c(5L, 7L))

has.prior <- structure(c("Brand", "Hershey", "Dove", "Godiva", "Lindt", "mean",
                         "1", "2", "3", "4", "Price", "$0.99", "$1.49", "$1.99", "$2.49",
                         "Cocoa strength", "70%", "Dark", "Milk", "White", "Sugar", "Standard",
                         "50% reduced sugar", "Sugar free", "", "Origin", "USA", "Switzerland",
                         "Belgium", "Belgian (Single origin Venezuelan Criollo beans)",
                         "Nuts", "Almonds", "Hazelnuts", "No", "", "Ethical", "Fair trade",
                         "BLANK", "", ""), .Dim = c(5L, 8L))

expect_warning(ChoiceModelDesign(design.algorithm = "Shortcut",
                            attribute.levels = no.prior,
                            prior = NULL,
                            n.questions = 6,
                            n.versions = 3,
                            alternatives.per.question = 4,
                            seed = 1), NA)

expect_warning(ChoiceModelDesign(design.algorithm = "Shortcut",
                            attribute.levels = has.prior,
                            prior = NULL,
                            n.questions = 6,
                            n.versions = 3,
                            alternatives.per.question = 4,
                            seed = 1),
               paste0("Prior data will be ignored as it can only be used ",
                      "with algorithms 'Efficient', 'Modfed', or 'Partial profiles'."))
