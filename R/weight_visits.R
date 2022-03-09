#' Weight Visits in a Subject Use Pattern
#'
#' @description Add numeric weights to a use pattern string
#'
#' @param use_pattern A character string showing the daily, by visit, or weekly
#'   substance use pattern for a single subject
#' @param weights_num 
#' @param posPenalty_num A numeric vector showing the penalty for having a 
#'   positive UDS (\code{"+"} or \code{"*"}) at each week in the use pattern.
#'   Defaults to \code{NULL}, implying that a positive UDS should have the same
#'   weight at any week in the pattern.
#' @param missPenalty_num A numeric vector showing the penalty for having a 
#'   missing UDS (\code{"o"}) at each week in the use pattern. Defaults to
#'   \code{NULL}, implying that a missing UDS should have the same weight at any
#'   week in the pattern.
#' @param scale 
#' @param scaleMax 
#' 
#' @return A numeric value: the results of the sum of all the visit weight
#'   values for the use pattern
#'   
#' @details This function exists to code the treatment outcome defined in Ling
#'   et al. (1976): \url{https://doi.org/10.1001/archpsyc.1976.01770060043007}.
#'   This definition requires other CTNote:: functions as well, but this
#'   function was written specifically for that definition.
#' 
#' @importFrom stringr str_replace_all fixed 
#' @export
#' 
#'
#' @examples
#'   pattern_char <- "__++++*o-------+--+-o-o-o+o+"
#'   
#'   # Default: change all missing weeks to positive
#'   recode_missing_visits(pattern_char)
#'   
#'   # Other example: remove all weeks with no UDS by design
#'   recode_missing_visits(pattern_char, missing_is = "_", missing_becomes = "")
#'   
weight_positive_visits <- function(
    use_pattern,
    weights_num = c(`+` = 1, `*` = 0.5, `o` = 0.22, `-` = 0),
    posPenalty_num = NULL,
    missPenalty_num = NULL,
    scale = TRUE,
    scaleMax = 120) {
  
  # browser()
  
  ###  Setup  ###
  x <- str_split(use_pattern, pattern = "")[[1]]
  origVals_char <- names(weights_num)
  newVals_num   <- unname(weights_num)
  
  
  ###  Recode to Numeric  ###
  xNew <- x
  for ( i in seq_along(origVals_char) ) {
    xNew[x == origVals_char[i]] <- newVals_num[i]
  }
  x_num <- as.numeric(xNew)
  
  
  ###  Weight  ###
  if (is.null(posPenalty_num)) {
    posPenalty_num <- rep(1L, length(x))
    posPenaltyTrunc_num <- posPenalty_num
  } else {
    isPos_lgl  <- x %in% c("+", "*")
    posPenaltyTrunc_num[!isPos_lgl] <- 1L
  }
  
  if (is.null(missPenalty_num)) {
    missPenalty_num <- rep(1L, length(x))
    missPenaltyTrunc_num <- missPenalty_num
  } else {
    isMiss_lgl <- x == "o"
    missPenaltyTrunc_num[!isMiss_lgl] <- 1L  
  }
  
  out <- x_num * posPenaltyTrunc_num * missPenaltyTrunc_num
  
  
  ###  Return  ###
  if (scale) {
    weeklyWorstCase_num <- pmax(
      newVals_num["+"] * posPenalty_num,
      newVals_num["o"] * missPenalty_num,
    )
    scaleMax * sum(out) / sum(weeklyWorstCase_num)
  } else {
    # for debugging only
    out
  }
  
}
