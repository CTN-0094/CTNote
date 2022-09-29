#' View a Pattern through a Study Design "Lattice"
#' 
#' @description Given a use pattern string and the pattern of study visits, 
#'   view only the use pattern string components which are congruent with the
#'   study design
#'
#' @param use_pattern A character string showing the daily, by visit, or weekly
#'   substance use pattern for a single subject
#' @param lattice_pattern A character string containing the study design for 
#'   subject visits. For example, a study design with required visits on Monday,
#'   Wednesday, and Friday would have the lattice pattern \code{"o_o_o__"},
#'   where \code{"o"} represents a day wherein the subject should visit the
#'   clinic (per study protocol) and \code{"_"} represents the other non-visit
#'   days. These symbols are set by \code{visit_is} and \code{no_visit_is},
#'   respectively.
#' @param visit_is Symbol indicating that the subject should appear in the
#'   clinic for that time period, as dictated by the study protocol. Defaults to
#'   \code{"o"} (because the outcome of the UDS will be unknown before the study
#'   has been designed).
#' @param no_visit_is Symbol indicating that the subject is not required to
#'   appear in the clinic for that time period, as dictated by the study
#'   protocol. Defaults to \code{"_"}.
#'   
#' @return A variant of \code{use_pattern}, but with all non-protocol visits set
#'   to \code{"_"}. In most cases, this use pattern vector would be then passed
#'   to \code{\link{impute_missing_visits}} so that the \code{"_"} visits could
#'   be replaced with the last observed UDS. See the Examples below.
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
#'   Many lattice patterns can be easily constructed by using the function
#'   \code{\link{collapse_lattice}}. For example, to repeat the
#'   "Monday-Wednesday-Friday" visit pattern noted above 4 times, you can use
#'   \code{collapse_lattice("o_o_o__", 4)} to create a vector of weekly visit
#'   patterns, then collapse (concatenate) them all together. More complex visit
#'   patterns can be constructed in a similar manner: first create a vector of
#'   simple repeating visit patterns, then collapse them all together, setting
#'   the number of repeats with the \code{times} argument.
#'   
#'   At current, this package allows for many symbols in the use pattern "word",
#'   such as "_" for missing by study design, "o" missing for protocol
#'   non-compliance (the most common form of missing), "+" for positive, "-" for
#'   negative, and "*" for mixed positive and negative results (this usually
#'   comes up when the visit represents multiple days and there are both
#'   positive and negative results in those days; for example, a subject is
#'   tested weekly; they provided a positive test on Tuesday but came back to
#'   provide a negative test the following day).
#'   
#' 
#' @importFrom stringr str_split
#' @export
#'
#' @examples 
#'   # This pattern represents 26 weeks of treatment UDS
#'   pattern_char  <- "+++++o-------+--+-o-o-o+o+"
#'   pattern2_char <- recode_missing_visits(pattern_char)
#'   
#'   # Example 1: a 16 week observation period with visits every 4 weeks
#'   lattice1_char <- "___o___o___o___o"
#'   useLattice1_char <- view_by_lattice(
#'     use_pattern = pattern2_char,
#'     lattice_pattern = lattice1_char
#'   )
#'   useLattice1_char
#'   impute_missing_visits(useLattice1_char, method = "locf", missing_is = "_")
#'   
#'   # Example 2: 24 week observation period with weekly visits for the first
#'   #   12 weeks, then monthly visits (during the second week) thereafter
#'   lattice2_char <- "oooooooooooo_o___o___o__"
#'   useLattice2_char <- view_by_lattice(
#'     use_pattern = pattern2_char,
#'     lattice_pattern = lattice2_char
#'   )
#'   useLattice2_char
#'   impute_missing_visits(useLattice2_char, method = "locf", missing_is = "_")
#'      
view_by_lattice <- function(use_pattern,
                            lattice_pattern,
                            visit_is = "o",
                            no_visit_is = "_") {
  
  ###  Split Patterns ###
  latticeSplit_char    <- str_split(lattice_pattern, "")[[1]]
  usePatternSplit_char <- str_split(use_pattern, "")[[1]]
  nLattice_int  <- length(latticeSplit_char)
  nObserved_int <- length(usePatternSplit_char)
  skipVisit_lgl <- latticeSplit_char == no_visit_is
  
  
  ###  Ensure Patterns have Equal Lengths  ###
  if ( nLattice_int > nObserved_int ) {
    pad_char <- rep(visit_is, nLattice_int - nObserved_int)
    usePatternSplit_char <- c(usePatternSplit_char, pad_char)
  }
  if ( nLattice_int < nObserved_int ) {
    usePatternSplit_char <- usePatternSplit_char[seq_len(nLattice_int)]
  }
  
  
  ###  Apply Lattice to Pattern
  usePatternLattice_char <- usePatternSplit_char
  usePatternLattice_char[skipVisit_lgl] <- no_visit_is
  
  paste0(usePatternLattice_char, collapse = "")
  
}
