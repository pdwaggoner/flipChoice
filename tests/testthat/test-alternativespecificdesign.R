context("Alternative specific design")

# Small test case
small.levels <- list(car = list(time = c("10", "20", "30"), comfort = c("H", "M", "L")),
                         bus = list(time = c("20", "30", "40"), crowded = c("Y", "N"), wait = c("5", "10", "15")),
                         walk = list(time = c("30", "45", "60"), weather = c("Rain", "Sun", "Cloud")),
                         bike = list(time = c("15", "25", "35"), weather = c("Rain", "Sun", "Cloud")))

# Large test case
large.levels <- list(A = list(a = seq(2), b = seq(4), c = seq(6), d = seq(8), e = seq(10)),
                         B = list(a = seq(2), b = seq(2), c = seq(2), d = seq(2), e = seq(2), f = seq(2), g = seq(2), h = seq(2), i = seq(5)),
                         C = list(a = seq(5), b = seq(5), c = seq(5), d = seq(5), e = seq(5), f = seq(5)),
                         D = list(a = seq(3), b = seq(3), c = seq(5) ,d = seq(5), e = seq(12) ,f = seq(12)))


test_that("Small design", {
    expect_error(asd <- ChoiceModelDesign(attribute.levels = small.levels,
                                          n.questions = 6,
                                          n.versions = 10,
                                          max.subsample = 1e5,
                                          none.alternatives = 2,
                                          none.position = c(2, 4),
                                          design.algorithm = "Alternative specific - Federov"), NA)

    expect_error(asd.2 <- ChoiceModelDesign(attribute.levels = small.levels,
                                          n.questions = 6,
                                          n.versions = 10,
                                          max.subsample = 1e3,
                                          design.algorithm = "Alternative specific - Federov"), NA)

    expect_error(asd.3 <- ChoiceModelDesign(attribute.levels = small.levels,
                                          n.questions = 6,
                                          n.versions = 10,
                                          design.algorithm = "Alternative specific - Random"), NA)

    expect_lt(asd$d.error, asd.2$d.error)
    expect_lt(asd.2$d.error, asd.3$d.error)
})

test_that("Large design", {
    expect_error(asd <- ChoiceModelDesign(attribute.levels = large.levels,
                                          n.questions = 6,
                                          n.versions = 40,
                                          max.subsample = 1e4,
                                          design.algorithm = "Alternative specific - Federov"), NA)

    expect_error(asd.2 <- ChoiceModelDesign(attribute.levels = large.levels,
                                            n.questions = 6,
                                            n.versions = 40,
                                            max.subsample = 1e3,
                                            design.algorithm = "Alternative specific - Federov"), NA)

    expect_error(asd.3 <- ChoiceModelDesign(attribute.levels = large.levels,
                                            n.questions = 6,
                                            n.versions = 40,
                                            none.alternatives = 2,
                                            none.position = c(2, 4),
                                            design.algorithm = "Alternative specific - Random"), NA)

    expect_lt(asd$d.error, asd.2$d.error)
    expect_lt(asd.2$d.error, asd.3$d.error)
})

