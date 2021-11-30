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
#'   regular and expected artefact of the study design and the preceeding and 
#'   following UDS are both negative).
#'
#' @return A character string with all missing UDS values (marked as \code{"o"}
#'   unless a different value is supplied to \code{missing_is}) replaces by the
#'   value supplied to \code{missing_becomes}.
#' 
#' @importFrom stringr str_replace_all fixed 
#' @export
#' 
#'
#' @examples
#'   pattern_char <- "_____+++++*o-------+--+-o-o-o+o+"
#'   recode_missing(pattern_char)
#'   recode_missing(pattern_char, missing_is = "_", missing_as = "")
#'   
recode_missing <- function(use_pattern, 
                           missing_is = "o",
                           missing_becomes = c("+", "", "-")) {
  
  missing_becomes <- match.arg(missing_becomes)
  str_replace_all(
    string = use_pattern,
    pattern = fixed(missing_is),
    replacement = fixed(missing_becomes)
  )
  
}
