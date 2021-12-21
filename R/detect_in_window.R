#' Detect Visits before Matching a Fuzzy Sub-Pattern
#' 
#' @description Given a use pattern string, use fuzzy logic to detect if a
#'   sub-pattern of interest is present within a specified detection window and
#'   how many visits are required to find said match.
#'
#' @param use_pattern A character string showing the daily, by visit, or weekly
#' @param window_width How wide should the inspection window be? Defaults to
#'   four visits.
#' @param threshold How many successes or failures are required within the
#'   specified window? Defaults to three.
#' @param match_is What constitutes the outcome of interest? \code{"+"}
#'   represents positive UDS as the visit event of interest, while \code{"-"}
#'   is negative.
#'   
#' @return A two column data frame with (a) column \code{time} representing the
#'   number of visits \strong{before} the first window with a match and (b)
#'   column \code{event} indicating if the match occurs in any window of the use
#'   pattern.
#'   
#' @details This function can be used to calculate time-to-event (survival)
#'   metrics for the subjects in treatment. For example, the default arguments
#'   represent a definition of relapse ("three or more positive UDS within a
#'   four-week window"). Thus, the output of this function under default values
#'   would be time until first relapse and a relapse indicator.
#'   
#' 
#' @importFrom stringr str_split
#' @export
#'
#' @examples 
#'   # Find the first relapse event
#'   detect_in_window("o-o+++")
#'   
#'   # Find the first positive UDS
#'   detect_in_window("o-o+++", window_width = 1L, threshold = 1L)
#'   
detect_in_window <- function(use_pattern,
                             window_width = 4L,
                             threshold = 3L,
                             match_is = c("+", "-")){
  # browser()
  
  match_is <- match.arg(match_is)
  if (threshold > window_width) {
    stop(
      "required number of matches (threshold) must be <= window_width",
      call. = FALSE
    )
  }
  
  obsUnit_char <- str_split(use_pattern, pattern = "", n = Inf)[[1]]
  nWindows_int <- length(obsUnit_char) - window_width + 1L
  if (nWindows_int < 1) {
    warning(
      "use_pattern must be longer than window_width.",
      call. = FALSE
    )
    return(
      data.frame("time" = NA_integer_, "event" = NA_integer_)
    )
  }
  
  # Rolling Window
  res_lgl <- logical(length = nWindows_int)
  for (idx in seq_len(nWindows_int)) {
    subUnits_char <- obsUnit_char[idx:(idx + window_width - 1)]
    res_lgl[idx] <- sum( subUnits_char == match_is ) >= threshold
  }
  
  # First Match
  if (any(res_lgl)) {
    timeMatch  <- min(which(res_lgl))
    eventMatch <- 1L
  } else {
    timeMatch  <- nWindows_int
    eventMatch <- 0L
  }
  
  # Return
  data.frame("time" = timeMatch, "event" = eventMatch)
  
}
