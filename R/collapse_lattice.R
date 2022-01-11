#' Combine Multiple Simple Study Design "Lattices"
#' 
#' @description Given a vector of simple study visits patterns, combine them
#'   into a more complex study visit lattice
#'
#' @param lattice_patterns A character vector of simple study design lattices
#' @param times An integer vector indicating the number of times to repeat each
#'   simple lattice. The length of the \code{times} argument should match the
#'   length of the \code{lattice_patterns} argument.
#'   
#' @return The combined lattice pattern, built up from combinations of the
#'   smaller lattices.
#'   
#' @details This function was designed to handle outcomes where the clinical
#'   trial protocol has staggered or non-sequential visits. For example, if we
#'   observed the pattern \code{"-ooo"}, in most cases we would interpret this
#'   to mean that the subject was present in the clinic for the first visit, but
#'   missed the next three visits. Some trial outcomes would then count the
#'   three missing visits as all positive or as a relapse. However, this use
#'   pattern could have been observed late during subject follow-up, during 
#'   which time the protocol could be that the subject was only required to
#'   visit the clinic monthly (so long as the supplied UDS was negative).
#'   
#'   This function is basically a wrapper around the \code{\link[base]{paste0}}
#'   function with \code{collapse = ""}. We added support to repeat subpatterns.
#'   
#' 
#' @export
#'
#' @examples 
#'   
#'   # Example 1: a 16 week observation period with visits every 4 weeks:
#'   # "___o___o___o___o"
#'   collapse_lattice(
#'     lattice_patterns = "___o",
#'     times = 4
#'   )
#'   
#'   # Example 2: 24 week observation period with weekly visits for the first
#'   #   12 weeks, then monthly visits (during the second week) thereafter:
#'   # "oooooooooooo_o___o___o__"
#'   collapse_lattice(
#'     lattice_patterns = c("o", "_o__"),
#'     times = c(12, 3)
#'   )
#'   
#'   # Example 3: 6 week observation period with clinic visits on set days,
#'   #   M-W-F for the first 3 weeks, then Monday only for the next 3 weeks:
#'   # "o_o_o__o_o_o__o_o_o__o______o______o______"
#'   collapse_lattice(
#'     lattice_patterns = c("o_o_o__", "o______"),
#'     times = c(3, 3)
#'   )
#'   
#'   
collapse_lattice <- function(lattice_patterns, times) {
  
  if (length(lattice_patterns) != length(times)) {
    stop("The times vector should have the same length as the patterns vector")
  }
  
  lattice_char <- rep(lattice_patterns, times)
  paste0(lattice_char, collapse = "")
  
}
