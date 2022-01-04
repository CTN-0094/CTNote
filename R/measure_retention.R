#' Measure Length of Use Pattern before Dropout
#'
#' @description Find the number of visits in the pattern until the last subject
#'   contact
#'
#' @param use_pattern A character string showing the daily, by visit, or weekly
#'   substance use pattern for a single subject
#' @param missing_is Which single character is used to mark missing UDS in a 
#'   use pattern string? Defaults to \code{"o"}.
#'
#' @return An integer measuring the number of visits before the subject was lost
#'   to follow-up. If the subject's entire use pattern is missing, then this
#'   will be 0.
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
#' @importFrom stringi stri_locate_last_fixed 
#' @importFrom stringr str_split
#' @export
#' 
#'
#' @examples
#'   pattern_char <- "__++++*o-------+--+-o-o-o+o+oooooo"
#'   measure_retention(pattern_char)
#'   
#'   pattern2_char <- "ooooooooooo"
#'   measure_retention(pattern2_char)
#'   
measure_retention <- function(use_pattern,
                              missing_is = "o") {
  # browser()
  
  # Get all non-missing matches
  all_chars <- unique(
    str_split(use_pattern, pattern = "")[[1]]
  )
  all_chars <- all_chars[all_chars != missing_is]
  
  
  # Find last match
  matches_mat <- stri_locate_last_fixed(use_pattern, pattern = all_chars)
  matches_mat[is.na(matches_mat)] <- 0L
  # If no non-missing values are included in the pattern, the matrix has 0 rows
  matches_mat <- rbind(matches_mat, matrix(0, nrow = 1, ncol = 2))
  
  max(matches_mat[, "end", drop = TRUE])
  
}
