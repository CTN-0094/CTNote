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
#'   randomization. Defaults to 1.
#' @param end The end of the detection range. This is often the end of followup
#'   (denoted by -1, the default value, which represents the last item in the
#'   string), or this could be a set number of weeks or visits, such as 12 weeks
#'   or 48 visits.
#' @param mixed_results_are A single character value indicating a partial use
#'    week; e.g. \code{"*"} means that the subject had at least one positive and
#'    at least one negative test of the substance(s) of interest for that week.
#'    Defaults to \code{NULL}.
#' @param mixed_weight A fraction showing the proportional use value for a 
#'    mixed result week. For example, some studies state that a positive UDS
#'    counts as three days of use (3/7), while other studies regard a positive
#'    UDS as five days of use (5/7). This value should be substance-specific, 
#'    but we default to 0.5.
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
#' @details At current, we allow for many symbols in the use pattern "word",
#'   such as "_" for missing by study design, "o" missing for protocol
#'   non-compliance (the most common form of missing), "+" for positive, "-" for
#'   negative, and "*" for mixed positive and negative results (this usually
#'   comes up when the visit represents multiple days and there are both
#'   positive and negative results in those days; for example, a subject is
#'   tested weekly; they provided a positive test on Tuesday but came back to
#'   provide a negative test the following day).
#'   
#' 
#' @importFrom stringr str_sub str_length str_count fixed
#' @export
#'
#'
#' @examples 
#'   # This pattern represents 26 weeks of treatment UDS
#'   pattern_char <- "++++*o-------+--+-o-o-o+o+"
#'   
#'   # Replace any missing UDS ("o") with positive
#'   cleanPattern_char <- recode_missing_visits(pattern_char)
#'   
#'   # Example: find the proportion of the subject's negative use weeks after
#'   #   randomization but before the end of a 12-week observation
#'   #   period
#'   count_matches(
#'     cleanPattern_char,
#'     match_is = "-",
#'     end = 12,
#'     mixed_results_are = "*",
#'     mixed_weight = 0.5,
#'     proportion = TRUE
#'   )
#'   
#'   # Example: find the number of times the subject used non-study opioids
#'   #   after being clean at least one week
#'   count_matches(
#'     cleanPattern_char,
#'     match_is = "-+"
#'   )
#'   
count_matches <- function(use_pattern,
                          match_is,
                          start = 1, end = -1,
                          mixed_results_are = NULL,
                          mixed_weight = 0.5,
                          proportion = FALSE) {
  
  ###  Subset  ###
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
  pattern_trim_length <- str_length(use_pattern_trimmed)
  if (pattern_trim_length == 0) {
    return(0)
  }
  
  
  ###  Summarize  ###
  if (is.null(mixed_results_are)) {
    match_count <- str_count(use_pattern_trimmed, pattern = fixed(match_is))  
  } else {
    
    mixed_match_count <- str_count(
      use_pattern_trimmed,
      pattern = fixed(mixed_results_are)
    )
    match_count <- 
      str_count(use_pattern_trimmed, pattern = fixed(match_is)) +
        mixed_weight * mixed_match_count
    
  }
  
  
  if (proportion) {
    possible_matches <- pattern_trim_length - length(match_is) + 1
    match_count / possible_matches
  } else {
    match_count
  }
  
}
