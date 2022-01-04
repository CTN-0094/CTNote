#' Measure the Length of the Longest Abstinent Period
#'
#' @description Find the longest number of sequential visits in the pattern with
#'   negative UDS
#'
#' @param use_pattern_binary A character string showing the daily, by visit, or
#'   weekly substance use pattern for a single subject, recoded to binary use
#'   or abstinence indicators.
#' @param use_is Which single character is used to mark positive UDS in the
#'   use pattern string? Defaults to \code{"+"}.
#'
#' @return An integer measuring the longest number of sequential visits for
#'   which the subject was abstinent from the substance(s) of interest. If the
#'   subject's entire use pattern is missing, then this will be 0.
#'   
#' @details The use pattern MUST be in binary code for this function to work.
#'   For use patterns coded in our default language (see the "dictionary" in the
#'   Details section for \code{\link{measure_retention}}), the binary form would
#'   have all "_" (missing by study protocol) visits replaced with "-" and all
#'   "o" (missing for non-compliance) and "*" (mixed positive and negative)
#'   visits replaced with "+". See the example below.
#' 
#' @importFrom stringr str_split fixed str_length
#' @export
#' 
#'
#' @examples
#'   pattern_char <- "__++++*o-------+--+-o-o-o+o+oooooo"
#'   
#'   # replace "_" with "-"
#'   pattern2_char <- recode_missing_visits(
#'     pattern_char,
#'     missing_is = "_",
#'     missing_becomes = "-"
#'   )
#'   # replace "o" with "+"
#'   pattern3_char <- recode_missing_visits(pattern2_char)
#'   # replace "*" with "+"
#'   pattern4_char <- recode_missing_visits(pattern3_char, missing_is = "*")
#'   
#'   measure_abstinence_period(pattern4_char)
#'   
measure_abstinence_period <- function(use_pattern_binary,
                                      use_is = "+") { 
  
  subpatterns_ls <- str_split(
    string = use_pattern_binary,
    pattern = fixed(use_is)
  )
  max(
    str_length(
      unlist(
        subpatterns_ls
      )
    )
  )
  
}
