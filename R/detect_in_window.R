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
#' @param offset In the case of an event, where in the detection window should
#'   the event time be recorded? Defaults to \code{window_width - threshold};
#'   that is, the start of the actual pattern. Another option would be 0, which
#'   indicates that the event should be recorded at the start of the window in
#'   which it was detected. 
#' @param match_is What constitutes the outcome of interest? \code{"+"}
#'   represents positive UDS as the visit event of interest, \code{"-"} is
#'   negative, and \code{"o"} means that the subject did not provide a valid UDS
#'   during the time period in question.
#'   
#' @return A two column data frame with (a) column \code{time} representing the
#'   number of visits until the first window with a match is detected and (b)
#'   column \code{event} indicating if the match occurs in any window of the use
#'   pattern.
#'   
#' @details This function can be used to calculate time-to-event (survival)
#'   metrics for the subjects in treatment. For example, the default arguments
#'   represent a definition of relapse ("three or more positive UDS within a
#'   four-week window"). Thus, the output of this function under default values
#'   would be time until first relapse and a relapse indicator.
#'   
#'   Concerning the \code{offset} argument: take, for example, the use pattern
#'   \code{"-------++++"}. The subject began to use the substance(s) of interest 
#'   by week 8. If our relapse definition was to detect 4 weeks of use in a 4
#'   week window, then the relapse time would be recorded at week 8. Similarly, 
#'   we would expect that if our relapse definition was to detect 2 or 3 weeks
#'   of use in a 4 week window, that the relapse time would also be week 8. This
#'   is why the default value of the \code{offset} argument is what it is: we
#'   "shift" the detection of the use until the start of the use. However, if
#'   you see the pattern above and believe that the relapse should be recorded
#'   at week 7 for relapse defined as 3 weeks of use in a 4-week window or at
#'   week 6 for 2 weeks of use in a 4-week window, then set \code{offset = 0}.
#'   
#'   Defining the use symbols: at current, we allow for many symbols in the use
#'   pattern "word", such as "_" for missing by study design, "o" missing for
#'   protocol non-compliance (the most common form of missing), "+" for
#'   positive, "-" for negative, and "*" for mixed positive and negative results
#'   (this usually comes up when the visit represents multiple days and there
#'   are both positive and negative results in those days; for example, a
#'   subject is tested weekly; they provided a positive test on Tuesday but came
#'   back to provide a negative test the following day).
#'   
#' 
#' @importFrom stringr str_split
#' @export
#'
#' @examples 
#'   # Find the first relapse event
#'   detect_in_window("o-o+++")
#'   
#'   # Find the start of the window that contains the first relapse event
#'   detect_in_window("o-o+++", offset = 0)
#'   
#'   # Find the first positive UDS
#'   detect_in_window("o-o+++", window_width = 1L, threshold = 1L)
#'   
detect_in_window <- function(use_pattern,
                             window_width = 4L,
                             threshold = 3L,
                             offset = window_width - threshold,
                             match_is = c("+", "o", "-")){
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
  data.frame("time" = timeMatch + offset, "event" = eventMatch)
  
}
