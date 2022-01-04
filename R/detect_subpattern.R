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
#'   randomization. Defaults to 1.
#' @param end The end of the detection range. This is often the end of followup
#'   (denoted by -1, the default value, which represents the last item in the
#'   string), or this could be a set number of weeks or visits, such as 12 weeks
#'   or 48 visits.
#'
#' @return A single logical value indicating if the use subpattern is present in
#'   the overall use pattern string
#'   
#' @details This function can be used to detect consecutive periods of drug
#'   abstinence, drug use, or study non-compliance (as measured by failure to
#'   supply urine). For example, to detect if the subject had three consecutive
#'   use weeks, the sub-pattern would be set to \code{"+++"}. 
#'   
#'   At current, we allow for many symbols in the use pattern "word", such as
#'   "_" for missing by study design, "o" missing for protocol non-compliance
#'   (the most common form of missing), "+" for positive, "-" for negative, and
#'   "*" for mixed positive and negative results (this usually comes up when the
#'   visit represents multiple days and there are both positive and negative
#'   results in those days; for example, a subject is tested weekly; they
#'   provided a positive test on Tuesday but came back to provide a negative
#'   test the following day).
#'   
#' 
#' @importFrom stringr str_length str_sub str_detect fixed
#' @export
#'
#' @examples 
#'   # This pattern represents 26 weeks of treatment UDS
#'   pattern_char <- "+++++o-------+--+-o-o-o+o+"
#'   
#'   # Replace any missing UDS ("o") with positive
#'   cleanPattern_char <- recode_missing_visits(pattern_char)
#'   
#'   # Example: detect if the subject was able to stay clean for at least 4
#'   #   weeks after randomization but before the end of a 12-week observation
#'   #   period
#'   detect_subpattern(
#'     cleanPattern_char,
#'     subpattern = "----",
#'     end = 12
#'   )
#'   
#'   # Example: detect if the subject was abstinent during the last 3 weeks
#'   detect_subpattern(
#'     cleanPattern_char,
#'     subpattern = "---",
#'     start = -3, 
#'     end = -1
#'   )
#'   
detect_subpattern <- function(use_pattern,
                              subpattern,
                              start = 1, end = -1) {
  
  # # UPDATE 2021-12-20: because we are removing the "baseline" pattern, we
  # #   don't need to check for bad values here
  # # If start or end are > the length of use_pattern, then error
  # nChars <- str_length(use_pattern)
  # if (abs(start) > nChars | abs(end) > nChars) {
  #   stop(
  #     "start and end must be less than the length of use_pattern",
  #     call. = FALSE
  #   )
  # }
  
  use_pattern_trimmed <- str_sub(
    string = use_pattern,
    start = start,
    end = end
  )
  
  str_detect(use_pattern_trimmed, pattern = fixed(subpattern))
  
}
