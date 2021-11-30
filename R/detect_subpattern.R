#' Detect a Consecutive Sub-Pattern
#' 
#' @description Given a use pattern string, detect if that pattern contains a
#'   consecutive sub-pattern of interest
#'
#' @param use_pattern A character string showing the daily, by visit, or weekly
#'   substance use pattern for a single subject
#' @param subpattern A character string containing the sub-pattern of interest.
#'   For example, if study dropout is seven consecutive missing UDS, then the
#'   sub-pattern would be "ooooooo".
#' @param start These two arguments give the integer range wherein to look for
#'   the use sub-pattern of interest. Usually, start should be the week of
#'   randomization
#' @param end The end of the detection range. This is often the end of followup
#'   (denoted by -1, which represents the last item in the string), or this
#'   could be a set number of weeks or visits, such as 12 weeks or 48 visits.
#'
#' @return A single logical value indicating if the use subpattern is present in
#'   the overall use pattern string
#'   
#' @details This function can be used to detect consecutive periods of drug
#'   abstinence, drug use, or study non-compliance (as measured by failure to
#'   supply urine). For example, to detect if the subject had three consecutive
#'   use weeks, the sub-pattern would be set to \code{"+++"}. 
#'   
#' 
#' @importFrom stringr str_sub str_detect fixed
#' @export
#'
#' @examples 
#'   # This pattern represents 6 weeks before randomization (week -5 to week 0),
#'   #   followed by 26 weeks of treatment UDS
#'   pattern_char <- "_____++++++o-------+--+-o-o-o+o+"
#'   
#'   # Replace any missing UDS ("o") with positive
#'   cleanPattern_char <- recode_missing(pattern_char)
#'   
#'   # Detect if the subject was able to stay clean for at least 4 weeks after
#'   #   randomization (week 6), but before the end of a 12-week observation
#'   #   period
#'   detect_subpattern(
#'     cleanPattern_char,
#'     subpattern = "----",
#'     start = abs(-5) + 1 + 1,
#'     end = abs(-5) + 1 + 12
#'   )
#'   
detect_subpattern <- function(use_pattern, subpattern, start, end = -1) {
  
  use_pattern_trimmed <- str_sub(
    string = use_pattern,
    start = start,
    end = end
  )
  str_detect(use_pattern_trimmed, pattern = fixed(subpattern))
  
}