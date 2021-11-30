#' Count Periods of Substance Use / Abstinence
#' 
#' @description Given a use pattern string, count the number of times that
#'   pattern contains a certain substance use indicator
#'
#' @param use_pattern A character string showing the daily, by visit, or weekly
#'   substance use pattern for a single subject
#' @param match_is A single character value of the use indicator of interest; 
#'   e.g. \code{"+"} is use positive or \code{"-"} is use negative.
#' @param start These two arguments give the integer range wherein to look for
#'   the use sub-pattern of interest. Usually, start should be the week of
#'   randomization
#' @param end The end of the detection range. This is often the end of followup
#'   (denoted by -1, which represents the last item in the string), or this
#'   could be a set number of weeks or visits, such as 12 weeks or 48 visits.
#' @param proportion Should this function return the count or proportion of
#'   matching use periods? Defaults to \code{FALSE}, signifying that the count
#'   of matches will be returned. Note that if the remaining string (after 
#'   trimming to the values of \code{start} and \code{end}) has length 0, then 
#'   this function will return 0 rather than NaN (0/0). When interpreting this
#'   value, 0 represents that the subject had no use periods matching the chosen
#'   outcome.
#'
#' @return A single integer (real) value measuring the count (proportion) of the
#'   use periods for which the subject had the substance use value of interest
#'   
#' 
#' @importFrom stringr str_sub str_length str_count fixed
#' @export
#'
#'
#' @examples 
#'   # This pattern represents 26 weeks of treatment UDS
#'   pattern_char <- "+++++o-------+--+-o-o-o+o+"
#'   
#'   # Replace any missing UDS ("o") with positive
#'   cleanPattern_char <- recode_missing(pattern_char)
#'   
#'   # Example: find the proportion of the subject's negative use weeks after
#'   #   randomization but before the end of a 12-week observation
#'   #   period
#'   count_matches(
#'     cleanPattern_char,
#'     match_is = "-",
#'     start = 1,
#'     end = 12,
#'     proportion = TRUE
#'   )
#'   
#'   # Example: find the number of times the subject used non-study opioids
#'   #   after being clean at least one week
#'   count_matches(
#'     cleanPattern_char,
#'     match_is = "-+",
#'     start = 1,
#'     end = -1
#'   )
#'   
count_matches <- function(use_pattern,
                          match_is,
                          start, end = -1,
                          proportion = FALSE) {
  
  ###  Subset  ###
  # If start or end are > the length of use_pattern, then error
  nChars <- str_length(use_pattern)
  if (abs(start) > nChars | abs(end) > nChars) {
    stop(
      "start and end must be less than the length of use_pattern",
      call. = FALSE
    )
  }
  use_pattern_trimmed <- str_sub(
    string = use_pattern,
    start = start,
    end = end
  )
  pattern_trim_length <- str_length(use_pattern_trimmed)
  if (pattern_trim_length == 0) {
    return(0)
  }
  
  
  ###  Summarize  ###
  match_count <- str_count(use_pattern_trimmed, pattern = fixed(match_is))
  
  if (proportion) {
    possible_matches <- pattern_trim_length - length(match_is) + 1
    match_count / possible_matches
  } else {
    match_count
  }
  
}
