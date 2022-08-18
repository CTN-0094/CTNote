#' @title All Treatment Outcomes for CTN-0094 Participants
#' 
#' @description This data set is a table of 53 treatment outcomes calculated on
#'    3560 participants from three clinical trials to assess the efficacy of
#'    medication-assisted treatment for opioid use disorder.
#'
#' @details These outcomes are based on a harmonized set of data from three
#'    clinical trials. The harmonized data from these trials are contained in
#'    the packages `ctn0094data` and `ctn0094DataExtra`. These outcomes are
#'    calculated in the three abstinence, relapse, and reduction "library"
#'    vignettes of this package. The data dictionary is currently stored as an
#'    Excel spreadsheet in `inst/suppl_docs/definitions_20220405.xlsx`.
#'
#' @docType data
#'
#' @usage data(outcomesCTN0094)
#'
#' @format A tibble with `r scales::comma(nrow(outcomesCTN0094))` rows and
#'    `r ncol(outcomesCTN0094)` columns. These columns include
#'    \describe{
#'      \item{who}{Patient ID}
#'      \item{usePatternUDS}{A character string containing the "use pattern
#'        word", which represents the weekly opioid use for each participant
#'        after their day of randomization. For more information, see
#'        \code{link[ctn0094DataExtra]{derived_weeklyOpioidPattern}}}
#'      \item{...}{The calculated treatment outcomes for 53 endpoints. Some
#'        endpoints are composites of "time to event" and an "event" indicator,
#'        but these are included as two separate columns and are named `*_time`
#'        and `*_event`, respectively.}
#' }
"outcomesCTN0094"
