#' @title Opioid Use by Study Day for Example CTN-0094 Participants
#' 
#' @description This data set is a table with daily positive opioid use
#'    indicator for 10 participants from the CTN-0094 harmonized data sets. This
#'    subset is to be used as an example of timeline-style opioid use data.
#'
#' @details This data is created in the script
#'    `inst/scripts/create_allDrugs_opioid_subset_20220916.R`. The "when" column
#'    measures the number of days after signed consent for the participant that
#'    the opioid-positive urine screen was collected.
#'
#' @docType data
#'
#' @usage data(egOpioidsCTN0094)
#'
#' @format A tibble with `r scales::comma(nrow(egOpioidsCTN0094))` rows and
#'    `r ncol(egOpioidsCTN0094)` columns. These columns include
#'    \describe{
#'      \item{who}{Patient ID}
#'      \item{what}{A factor indicating what substance(s) were present in the
#'        urine on day `when`; trivially, all substances are "opioids" for this
#'        example data.}
#'      \item{when}{The number of days since signed study consent}
#' }
"egOpioidsCTN0094"
