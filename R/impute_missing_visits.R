#' Naively Impute Missing Visits
#'
#' @description Given a use pattern string with missing visits, make naive
#'   imputations for each missing visit
#'
#' @param use_pattern A character string showing the daily, by visit, or weekly
#'   substance use pattern for a single subject
#' @param method Which naive imputation method should be used? Current supported
#'   options are \code{"locf"} (last observation carried forward),
#'   \code{"locfD"} (last observation carried forward until dropout), and
#'   \code{"mode"} (most common non-missing value).
#' @param missing_is Which single character is used to mark missing UDS in a 
#'   use pattern string? Defaults to \code{"o"}.
#'
#' @return A use pattern string the same length as \code{use_pattern} with 
#'   missing values imputed according to the chosen imputation method.
#'   
#' @details If you would like to replace all UDS for missing visits with a
#'   single, pre-specified value (such as positive), please use
#'   \code{\link{recode_missing_visits}} instead. Furthermore, there will most
#'   likely still be missing values in the use pattern even after imputation.
#'   This would occur if all the values are missing, if the first values of the
#'   use pattern are missing (if LOCF is used), or if the first and/or last
#'   values of the use pattern are missing (if LOCF-D) is used. Because of this,
#'   you may need to call \code{\link{recode_missing_visits}} in a pipeline 
#'   after this function to replace or remove the remaining non-imputable
#'   missing visits.
#' 
#' @importFrom stringi stri_locate_last_fixed 
#' @importFrom stringr str_split
#' @export
#' 
#'
#' @examples
#'   pattern_char <- "__++++*o-------+--+-o-o-o+o+oooooo"
#'   impute_missing_visits(pattern_char)
#'   impute_missing_visits(pattern_char, method = "locfD")
#'   impute_missing_visits(pattern_char, method = "mode")
#'   
#'   pattern2_char <- "ooooooooooo"
#'   impute_missing_visits(pattern2_char)
#'   
impute_missing_visits <- function(use_pattern,
                                  method = c("locf", "locfD", "mode"),
                                  missing_is = "o") {
  # browser()
  
  
  ###  Get all non-missing matches  ###
  all_chars <- str_split(use_pattern, pattern = "")[[1]]
  nVisits   <- length(all_chars)
  unique_chars <- unique(all_chars)
  unique_chars <- unique_chars[unique_chars != missing_is]
  if (length(unique_chars) == 0) {
    warning("No non-missing visits. No imputation done.", call. = FALSE)
    return(use_pattern)
  }
  
  
  ###  Switch on Method  ###
  method <- match.arg(method)
  out_pattern_split <- switch (method,
    locf  = {
      
      # We are sure that there are non-missing visits in the use pattern string
      #   here, but the use pattern could still start with missing values. If
      #   the string doesn't start with missing values, then we want a 0-length
      #   string to prepend to the imputed string.
      observed_start <- min(which(all_chars != missing_is))
      observed_use   <- all_chars[observed_start:nVisits]
      unobserved_use <- all_chars[0:(observed_start - 1)]
      
      imputed_use <- .impute_locf(x = observed_use, missing_is = missing_is)
      c(unobserved_use, imputed_use)
      
    },
    
    locfD = {
      
      observed_start <- min(which(all_chars != missing_is))
      observed_end   <- max(which(all_chars != missing_is))
      observed_use   <- all_chars[observed_start:observed_end]
      
      head_use <- all_chars[0:(observed_start - 1)]
      if (observed_end == nVisits) {
        tail_use <- character(0)
      } else {
        tail_use <- all_chars[(observed_end + 1):nVisits]
      }
      
      imputed_use <- .impute_locf(x = observed_use, missing_is = missing_is)
      c(head_use, imputed_use, tail_use)
      
    },
    
    mode  = {
      .impute_mode(all_chars, missing_is = missing_is)
    }
  )
  
  
  ###  Return  ###
  paste0(out_pattern_split, collapse = "")
  
}


######  LOCF  #################################################################
.impute_locf <- function(x, missing_is){
  
  # Initialise
  imputed_visits <- character(length = length(x))
  imputed_visits[1] <- x[1]
  
  # Loop
  for (visit in 2:length(imputed_visits)) {
    if (x[visit] == missing_is) {
      imputed_visits[visit] <- imputed_visits[visit - 1]
    } else {
      imputed_visits[visit] <- x[visit]
    }
  }
  
  # Return
  imputed_visits
  
}



######  Mode  #################################################################
.impute_mode <- function(x, missing_is){
  
  clean_x <- x[x != missing_is]
  most_common <- which.max( table(clean_x) )
  # If there are ties, take the first
  uds_mode <- names(most_common)[1]
  
  imputed_x <- x
  imputed_x[imputed_x == missing_is] <- uds_mode
  imputed_x
  
}

