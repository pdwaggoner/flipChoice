#' Data from a fast food discrete choice experiment
#'
#' A dataset containing responses to a partial profiles discrete choice experiment
#' examining preferences for fast food. 403 respondents were each
#' asked twelve choice questions involving choosing between four fast food
#' alternatives. The question asked each respondent for the choice
#' questions was
#' "If you were ordering food delivery on typical Saturday night where nothing special was happening, which of these would you choose? Assume the options are identical in all other ways." The partial profiles
#' design is available as \code{\link{fast.food.design}}.
#'
#' \itemize{
#' \item \code{version} - integer; version number of the design shown to the respondent
#' \item \code{task1} - integer; task number of first question shown to the respondent
#' \item \code{task2} - integer; task number of second question
#' \item \code{task3} - integer; task number of third question
#' \item \code{task4} - integer; task number of fourth question
#' \item \code{task5} - integer; task number of fifth question
#' \item \code{task6} - integer; task number of sixth question
#' \item \code{task7} - integer; task number of seventh question
#' \item \code{task8} - integer; task number of eighth question
#' \item \code{task9} - integer; task number of ninth question
#' \item \code{task10} - integer; task number of tenth question
#' \item \code{task11} - integer; task number of the eleventh question
#' \item \code{task12} - integer; task number of the twevelth question
#' \item \code{choice1} - integer value from 1 to 4 indicating the respondent's choice
#' for the first question
#' \item \code{choice2} - integer value from 1 to 4 indicating the respondent's choice
#' for the second question
#' \item \code{choice3} - integer value from 1 to 4 indicating the respondent's choice
#' for the third question
#' \item \code{choice4} - integer value from 1 to 4 indicating the respondent's choice
#' for the fourth question
#' \item \code{choice5} - integer value from 1 to 4 indicating the respondent's choice
#' for the fifth question
#' \item \code{choice6} - integer value from 1 to 4 indicating the respondent's choice
#' for the sixth question
#' \item \code{choice7} - integer value from 1 to 4 indicating the respondent's choice
#' for the seventh question
#' \item \code{choice8} - integer value from 1 to 4 indicating the respondent's choice
#' for the eighth question
#' \item \code{choice9} - integer value from 1 to 4 indicating the respondent's choice
#' for the nineth question
#' \item \code{choice10} - integer value from 1 to 4 indicating the respondent's choice
#' for the tenth question
#' \item \code{choice11} - integer value from 1 to 4 indicating the respondent's choice
#' for the eleventh question
#' \item \code{choice12} - integer value from 1 to 4 indicating the respondent's choice
#' for the twelveth question
#' \item \code{gender} - factor with two levels indicating respondent's gender
#' \item \code{state} - factor with 46 levels indicating respondent's state of residence
#' in the United States
#' \item \code{age} - factor with seven levels giving the respondent's age
#' \item \code{income} - factor with eight levels giving the respondent's income
#' \item \code{ethnicity} - factor with six levels giving the responent's reported ethinicty
#' \item \code{num.ppl.in.household} - factor with five levels giving the number of people
#' currently residing in the respondent's household
#' \item \code{price.under.20} - factor with five levels indicating how important it is to
#' the respondent that a fast food restaurant has meal options under $20
#' \item \code{locally.owned}- factor with five levels indicating how important it is to
#' the respondent that a fast food restaurant is locally owned
#' \item \code{kids.menu}- factor with five levels indicating how important it is to
#' the respondent that a fast food restaurant has a kids menu
#' \item \code{online.ordering}- factor with five levels indicating how important it is to
#' the respondent that a fast food restaurant has online ordering available
#' \item \code{recylable.packaging}- factor with five levels indicating how important it is to
#' the respondent that a fast food restaurant has recylable packaging
#' \item \code{delivery.under.30min}- factor with five levels indicating how important it is to
#' the respondent that a fast food restaurant offers delivery in under 30 minutes
#' \item \code{nutrition.info.available}- factor with five levels indicating how important it is to
#' the respondent that a fast food restaurant has nutrition information available
#' \item \code{gluten.free.available}- factor with five levels indicating how important it is to
#' the respondent that a fast food restaurant has gluten-free food options
#' \item \code{dessert.available}- factor with five levels indicating how important it is to
#' the respondent that a fast food restaurant has dessert options
#' \item \code{vegan.available}- factor with five levels indicating how important it is to
#' the respondent that a fast food restaurant has vegan options
#' \item \code{organic.available}- factor with five levels indicating how important it is to
#' the respondent that a fast food restaurant has organic food options
#' \item \code{healty.opts.available}- factor with five levels indicating how important it is to
#' the respondent that a fast food restaurant has healthy meal options
#' \item \code{weight} - integer; respondent's self-reported weight in pounds
#' \item \code{height.ft} - integer; respondent's self
#' \item \code{height.inches}
#' \item \code{health.q.other}
#' \item \code{nut.allergy} - factor with two levels indicating if the respondent reported having
#' a nut allergy
#' \item \code{high.blood.pressure} - factor with two levels indicating if the respondent reported having
#' high blood pressure
#' \item \code{diabetes} - factor with two levels indicating if the respondent reported having
#' diabetes
#' \item \code{bad.back} - factor with two levels indicating if the respondent reported having
#' a bad back
#' \item \code{high.cholesterol} - factor with two levels indicating if the respondent reported having
#' high cholesterol
#' \item \code{no.health.condition} - factor with two levels indicating if the respondent did not
#' report having any health conditions
#' \item \code{other.health.condition} - factor with two levels indicating if they had an additional
#' health condtion that was not a nut allergy, high blood pressure, diabetes, a bad back, or high
#' cholesterol
#' }
#' @docType data
#' @keywords datasets
#' @name fast.food
#' @usage data(fast.food)
#' @format A data frame with 403 rows and 53 columns
NULL
