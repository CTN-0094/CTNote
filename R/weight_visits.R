#' Weight Visits in a Subject Use Pattern
#'
#' @description Add numeric weights to a use pattern string
#'
#' @param use_pattern A character string showing the daily, by visit, or weekly
#'   substance use pattern for a single subject
#' @param weights_num A named numeric vector mapping the symbols in the use
#'   pattern to non-negative numeric values. The default values, 1 for positive
#'   UDS (\code{"+"}), 0.22 for missing UDS (\code{"o"}), and 0 for negative UDS
#'   (\code{"-"}), are the values set in Ling et al. (1976); see the "Details"
#'   section for more information. The default value for a mixed weekly result
#'   (\code{"*"}: at least one positive and at least one negative UDS in a
#'   single week) is the average of a positive and negative weight (0.5 by 
#'   default).
#' @param posPenalty_num A numeric vector showing the penalty for having a 
#'   positive or mixed UDS (\code{"+"} or \code{"*"}) at each week in the use
#'   pattern. Defaults to \code{NULL}, implying that a positive UDS should have
#'   the same weight at any week in the pattern. One other useful option would
#'   be to set the weights to increase along the length of the study protocol,
#'   so that the penalty for having a positive UDS is grows larger the longer a
#'   participant is in treatment. See the Examples for more information.
#' @param missPenalty_num A numeric vector showing the penalty for having a 
#'   missing UDS (\code{"o"}) at each week in the use pattern. Defaults to
#'   \code{NULL}, implying that a missing UDS should have the same weight at any
#'   week in the pattern. One other useful option would be to set the weights to
#'   decrease along the length of the study protocol, so that the penalty for
#'   missing a clinic visit grows smaller the longer a participant has continued
#'   in treatment.
#' @param scaleMax Standardize the score to which maximum? Defaults to 120 to
#'   match the scoring scale in Ling et al. (1976). See "Details" for more.
#' @param scale Scale the resulting score to a standard maximum (given by the
#'   \code{scaleMax} argument). This defaults to \code{TRUE}; this should not be
#'   changed unless you are a developer and you are trying to debug your code.
#' 
#' @return A numeric value: the results of the sum of all the visit weight
#'   values for the use pattern
#'   
#' @details This function exists to code the treatment outcome defined in Ling
#'   et al. (1976): \doi{10.1001/archpsyc.1976.01770060043007}. This definition
#'   requires other CTNote:: functions as well, but this function was written
#'   specifically for that definition.
#'   
#'   The \code{weights_num} argument is the static "penalty" for positive and
#'   missing UDS values; this will not change over the protocol weeks. These
#'   values are then multiplied by the penalty vectors (\code{posPenalty_num}
#'   and \code{missPenalty_num}).
#' 
#' @importFrom stringr str_split
#' @export
#' 
#'
#' @examples
#'   pattern_char <- "++o+*-------+--+-o-o-o+o+"
#'   
#'   
#'   ###  Defaults  ###
#'   # See how the weights map to the symbols (DO NOT use in practice)
#'   weight_positive_visits(pattern_char, scale = FALSE)
#'   
#'   # Score this use pattern via default settings
#'   weight_positive_visits(pattern_char)
#'   
#'   
#'   ###  Increase Static Weight of Missing UDS  ###
#'   # Because the score for a missing UDS from the Ling et al. (1976) paper was
#'   #   an estimated value from their data, other weights may better represent
#'   #   modern addiction behavior patterns. For instance, we believe that 
#'   #   missing UDS values may be worse than a positive UDS in some instances,
#'   #   because they indicate that the subject is no longer participating in 
#'   #   treatment at all. We then should change the static weights to reflect
#'   #   this.
#'   weight_positive_visits(
#'     pattern_char,
#'     weights_num = c(`+` = 0.8, `*` = 0.4, `o` = 1, `-` = 0)
#'   )
#'   
#'   
#'   ###  Increasing Positive UDS Penalty  ###
#'   # Score this use pattern using an increasing positive UDS penalty (similar
#'   #   to that shown in Lint et al. (1976))
#'   newPosPenal_num <- seq(
#'     from = 1, to  = 5, 
#'     length = stringr::str_length(pattern_char)
#'   )
#'   weight_positive_visits(pattern_char, posPenalty_num = newPosPenal_num)
#'   
#'   
#'   ###  Variable Missing UDS Penalty  ###
#'   # Score this use pattern using a step-down missing UDS penalty (based on
#'   #   the idea that missing values during the treatment induction period are
#'   #   much worse than missing any other time in the study)
#'   newMissPenal_num <- rep(1, stringr::str_length(pattern_char))
#'   newMissPenal_num[1:4] <- 3
#'   weight_positive_visits(pattern_char, missPenalty_num = newMissPenal_num)
#'   
#'   
#'   ###  Composite Penalties  ###
#'   # Score this use pattern with both increasing positive UDS and step-down
#'   #   missing UDS penalties, while adjusting the weights.
#'   weight_positive_visits(
#'     pattern_char,
#'     weights_num = c(`+` = 0.8, `*` = 0.4, `o` = 1, `-` = 0),
#'     posPenalty_num = newPosPenal_num,
#'     missPenalty_num = newMissPenal_num
#'   )
#'   
weight_positive_visits <- function(
    use_pattern,
    weights_num = c(`+` = 1, `*` = 0.5, `o` = 0.22, `-` = 0),
    posPenalty_num = NULL,
    missPenalty_num = NULL,
    scaleMax = 120L,
    scale = TRUE) {
  
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
    posPenaltyTrunc_num <- posPenalty_num
    posPenaltyTrunc_num[!isPos_lgl] <- 1L
  }
  
  if (is.null(missPenalty_num)) {
    missPenalty_num <- rep(1L, length(x))
    missPenaltyTrunc_num <- missPenalty_num
  } else {
    isMiss_lgl <- x == "o"
    missPenaltyTrunc_num <- missPenalty_num
    missPenaltyTrunc_num[!isMiss_lgl] <- 1L  
  }
  
  out <- x_num * posPenaltyTrunc_num * missPenaltyTrunc_num
  
  
  ###  Return  ###
  if (scale) {
    weeklyWorstCase_num <- pmax.int(
      weights_num["+"] * posPenalty_num,
      weights_num["o"] * missPenalty_num
    )
    scaleMax * sum(out) / sum(weeklyWorstCase_num)
  } else {
    # for debugging only
    out
  }
  
}
