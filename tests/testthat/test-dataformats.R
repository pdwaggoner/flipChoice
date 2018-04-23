context("Data formats")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipChoice", mustWork = TRUE),
              file)
}

cho.file <- findInstDirFile("Training.cho")
cho.none.file <- findInstDirFile("none_option.cho")
jmp.design.file <- findInstDirFile("eggs_design.xlsx")
jmp.design.with.levels.file <- findInstDirFile("eggs_design_with_levels.xlsx")

attribute.levels.file.cho <- findInstDirFile("Attribute_labels_-_Training.xlsx")
attribute.levels.file.jmp <- findInstDirFile("eggs_labels.xlsx")

data(eggs, package = "flipChoice")

choices.jmp <- eggs.data[, 1:8]
choices.jmp.none.of.these <- choices.jmp
for (i in 1:8)
{
    v <- as.numeric(choices.jmp.none.of.these[[i]])
    v[1] <- 4
    choices.jmp.none.of.these[[i]] <- as.factor(v)
    levels(choices.jmp.none.of.these[[i]]) <- LETTERS[1:4]
}
tasks.jmp <- data.frame(t(matrix(1:3040, nrow = 8)))

respondent.ids <- structure(c(3, 1, 5, 6, 7, 9, 10, 12, 14, 16, 17, 19, 21, 22,
            23, 24, 25, 26, 28, 29, 31, 32, 33, 34, 35, 36, 37, 39, 40, 41,
            42, 45, 46, 47, 48, 49, 50, 53, 55, 58, 59, 62, 67, 68, 69, 70,
            71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 87,
            88, 90, 91, 92, 93, 95, 96, 98, 100, 101, 102, 103, 107, 109,
            112, 115, 116, 117, 118, 119, 121, 122, 123, 127, 128, 129, 131,
            134, 135, 137, 138, 139, 140, 141, 143, 145, 147, 151, 152, 153,
            156, 160, 161, 163, 164, 165, 166, 167, 168, 169, 171, 172, 173,
            174, 175, 176, 179, 180, 181, 184, 186, 188, 189, 192, 193, 194,
            195, 196, 197, 198, 199, 201, 204, 205, 207, 208, 210, 211, 213,
            214, 218, 219, 221, 222, 223, 226, 229, 230, 231, 232, 233, 234,
            235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247,
            249, 250, 253, 254, 256, 257, 258, 259, 261, 263, 264, 269, 270,
            271, 272, 273, 274, 275, 276, 278, 279, 280, 283, 284, 285, 286,
            288, 290, 291, 293, 294, 295, 296, 297, 298, 299, 301, 303, 306,
            307, 308, 310, 311, 312, 313, 314, 315, 316, 317, 319, 320, 321,
            324, 325, 327, 328, 331, 332, 333, 335, 338, 339, 340, 341, 343,
            345, 350, 353, 355, 356, 357, 358, 359, 360, 361, 363, 364, 365,
            366, 367, 368, 369, 371, 372, 373, 374, 376, 377, 378, 379, 380,
            381, 382, 383, 384, 385, 387, 388, 390, 392, 394, 395, 397, 399,
            400, 401, 402, 403, 405, 406, 408, 412, 414, 415, 419, 420, 421,
            422, 423, 426, 427, 429, 430, 432, 439, 440, 442, 446, 447, 449,
            450, 451, 452, 453, 454, 455, 457, 458, 459, 460, 461, 464, 469,
            471, 474, 480, 482, 485, 486, 487, 489, 490, 493, 494, 495, 500,
            502, 504, 506, 507, 508, 509, 510, 511, 513, 515, 517, 518, 520,
            521, 524, 527, 528, 529, 530, 532, 533, 535, 537, 539, 541, 542,
            543, 544, 545, 546, 549, 551, 552, 554, 558, 560, 561, 562, 565,
            566, 567, 568, 570, 571, 572, 573, 574, 577, 579, 580, 581, 582,
            584, 585, 587, 588, 589, 590, 591, 593, 596, 597, 598, 599, 603,
            604, 605, 606, 608, 611, 613, 614, 615, 616, 617, 618, 619, 620,
            622, 624, 625, 626, 628, 629, 630, 631, 632, 633, 634, 635, 637,
            638, 639, 640, 641, 642, 644, 645, 646, 647, 648, 649, 650, 651,
            653, 658, 659, 661, 663, 664, 666, 667, 669, 670, 671, 672, 677,
            681, 682, 683, 684, 688, 691, 693, 694, 696, 697, 698, 699, 702,
            703, 704, 705, 706, 707, 708, 710, 711, 712, 713, 714, 715, 716,
            717, 719, 720, 723, 724, 726, 728, 730, 731, 732, 736, 737, 739,
            740, 741, 745, 746, 747, 748, 749, 751, 752, 753, 754, 756, 757,
            758, 761, 763, 764, 765, 766, 767, 768, 770, 773, 776, 779, 780,
            781, 782, 783, 784, 786, 787, 791, 792, 793, 794, 795, 796, 797,
            798, 799, 804, 805, 806, 807, 808, 811, 812, 814, 815, 817, 819,
            820, 821, 822, 828, 829, 831, 833, 834, 837, 839, 841, 843, 844,
            846, 847, 849, 850, 853, 854, 857, 858, 859, 861, 863, 864, 865,
            867, 868, 870, 875, 876, 879, 880, 881, 884, 885, 886, 887, 889,
            890, 892, 893, 894, 895, 896, 897, 898, 901, 902, 903, 906, 908,
            910, 911, 913, 914, 915, 917, 921, 922, 925, 927, 929, 930, 931,
            932, 933, 934, 935), questiontype = "Number", name = "RespondentID",
            label = "Respondent ID", question = "Respondent ID")

test_that("cho file", {
    result <- FitChoiceModel(cho.file = cho.file,
                             attribute.levels.file = attribute.levels.file.cho,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             respondent.ids = respondent.ids)
    expect_error(print(result), NA)
})

test_that("cho none file", {
    result <- FitChoiceModel(cho.file = cho.none.file,
                             attribute.levels.file = attribute.levels.file.cho,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE,
                             respondent.ids = respondent.ids)
    expect_error(print(result), NA)
})

test_that("jmp format", {
    result <- FitChoiceModel(design.file = jmp.design.file,
                             attribute.levels.file = attribute.levels.file.jmp,
                             choices = choices.jmp, questions = tasks.jmp,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("jmp format none of these", {
    result <- FitChoiceModel(design.file = jmp.design.file,
                             attribute.levels.file = attribute.levels.file.jmp,
                             choices = choices.jmp.none.of.these,
                             questions = tasks.jmp,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("jmp format with labels", {
    result <- FitChoiceModel(design.file = jmp.design.with.levels.file,
                             choices = choices.jmp, questions = tasks.jmp,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("Missing data", {
    eggs.data.missing <- eggs.data
    eggs.data.missing[1, 1] <- NA
    dat <- processExperimentData(experiment.data = eggs.data.missing,
                                 subset = NULL, weights = NULL,
                                 n.questions.left.out = 0,
                                 seed = 123, input.prior.mean = 0,
                                 input.prior.sd = 5)
    expect_equal(dat$n.respondents, 379)
    expect_equal(dim(dat$X.in), c(379L, 8L, 3L, 13L))
    expect_equal(dim(dat$Y.in), c(379L, 8L))
})

data("eggs.cov", package = "flipChoice")

test_that("cho file with fixed covariates", {
    f <- as.factor(rbinom(600, 1, .5))
    result <- FitChoiceModel(cho.file = cho.file,
                             cov.formula = ~f,
                             attribute.levels.file = attribute.levels.file.cho,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
    stat.names <- rownames(result$parameter.statistics)
    expect_equal(sum(grepl("Intercept", stat.names)),
                 sum(grepl("St. Dev", stat.names)))
    expect_equal(sum(grepl("f1__", stat.names)),
                 sum(grepl("St. Dev", stat.names)))
})

test_that("jmp format with fixed covariates", {
    result <- FitChoiceModel(design.file = jmp.design.file,
                             cov.formula = ~egg.choice2, cov.data = eggs.cov,
                             attribute.levels.file = attribute.levels.file.jmp,
                             choices = choices.jmp, questions = tasks.jmp,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
    stat.names <- rownames(result$parameter.statistics)
    expect_equal(sum(grepl("Intercept", stat.names)),
                 sum(grepl("St. Dev", stat.names)))
    expect_equal(sum(grepl("egg.choice2", stat.names)),
                 sum(grepl("St. Dev", stat.names)))
})
