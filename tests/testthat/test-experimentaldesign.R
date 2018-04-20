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

d.errors <- structure(c(0.712056054283735, 0.7112814, 0.214032716759785,
                        1.34470226920378, 0.5107438, 0.5283405, 0.1834377,
                        1.271414, 0.5124096, 0.5402751, 0.1833373,
                        1.271414, 0.580339263817946, 0.5910509, 0.188595544954104,
                        1.28025988703947), .Dim = c(4L, 4L), .Dimnames = list(NULL, c("Random",
                                                                                      "Complete enumeration", "Balanced overlap", "Shortcut")))

derr <- matrix(0, ncol = 4, nrow = 4)

tfile <- tempfile()
withr::with_output_sink(tfile, {
    for (model in c("Random", "Complete enumeration", "Balanced overlap", "Shortcut")) {  # can't handle prohibitions with "Shortcut"
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
                print(cmd$d.error)
                #expect_equivalent(cmd$d.error, d.errors[i, match(model, colnames(d.errors))], tolerance = 1e-6)
            })
        }
    }
})
#unlink(tfile)

experiment4 <- CreateExperiment(c(4, 3, 2, 1), 0)
test_that(paste("Test case"), {
    expect_error(ChoiceModelDesign(design.algorithm = "Random",
                                   attribute.levels = experiment4$attribute.levels,
                                   n.questions = 6,
                                   alternatives.per.question = 3), "All attributes must have at least 2 levels.")
})
