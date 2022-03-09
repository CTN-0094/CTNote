#' Recode Missing or Ambiguous UDS in a Subject Use Pattern
#'
#' @description Replace all missing UDS \code{"o"} in a use pattern string
#'
#' @param use_pattern A character string showing the daily, by visit, or weekly
#'   substance use pattern for a single subject
#' @param missing_is Which single character is used to mark missing UDS in a 
#'   use pattern string? Defaults to \code{"o"}.
#' @param missing_becomes How should missing UDS be treated? Defaults to marking
#'   the subject as positive for that missing period. Options are \code{"+"} for
#'   positive, \code{""} for ignore, or \code{"-"} for negative (note that this
#'   last option should only be used in case where the "missing" UDS is a
#'   regular and expected artefact of the study design and the preceding and 
#'   following UDS are both negative).
#'
#' @return A character string with all missing UDS values (marked as \code{"o"}
#'   unless a different value is supplied to \code{missing_is}) replaces by the
#'   value supplied to \code{missing_becomes}.
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
recode_missing_visits <- function(use_pattern, 
                                  missing_is = "o",
                                  missing_becomes = c("+", "", "-")) {
  # browser()
  
  ###  Avoid "" in match.arg()  ###
  # https://stackoverflow.com/questions/41441170/failure-of-match-arg-for-the-empty-string
  if ( !identical(missing_becomes, "") ) {
    missing_becomes <- match.arg(missing_becomes)
  }
  
  str_replace_all(
    string = use_pattern,
    pattern = fixed(missing_is),
    replacement = fixed(missing_becomes)
  )
  
}
