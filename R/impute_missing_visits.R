#' Naively Impute Missing Visits
#'
#' @description Given a use pattern string with missing visits, make naive
#'   imputations for each missing visit
#'
#' @param use_pattern A character string showing the daily, by visit, or weekly
#'   substance use pattern for a single subject
#' @param method Which naive imputation method should be used? Current supported
#'   options are \code{"locf"} (last observation carried forward),
#'   \code{"locfD"} (last observation carried forward until dropout),
#'   \code{"mode"} (most common non-missing value), and \code{"kNV"} (k nearest
#'   visits).
#' @param missing_is Which single character is used to mark missing UDS in a 
#'   use pattern string? Defaults to \code{"o"}.
#' @param mixed_is Which single character is used to mark mixed UDS (both
#'   positive and negative UDS for the visit block) in a use pattern string?
#'   Defaults to \code{"*"}. When imputing by the mode, all mixed result UDS
#'   will be assigned the \code{tiebreaker} value in order to calculate the mode
#'   but will remain unchanged in the returned use pattern string.
#' @param tiebreaker In the event of ties between two modes, should positive or
#'   negative UDS be the mode? Defaults to positive (\code{"+"}).
#' @param k The number of nearest visits to use in kNV imputation. This defaults
#'   to 1; we recommend that this parameter stays at 1 unless the use patterns
#'   in your data have extraordinarily few missing values.
#' @param knvWeights_num A named vector matching the use pattern word "letters"
#'   to their numerical use values. The names of this vector should match the
#'   "letters" of the use pattern word exactly; use backticks to escape special
#'   characters. For example, if the study protocol counts a mixed result (one
#'   positive and one negative UDS in a single observation period \[week\]) as
#'   worth three "use days", then mixed results should have a  weight of 3/7.
#'   Additionally, a study protocol may count missing values as five "use days"
#'   out of a week. The defaults for this function are to leave \code{"o"} as
#'   missing (\code{NA}), and give weights of 1, 1/2, and 0 for visits with
#'   \code{"+"}, \code{"*"}, and \code{"-"} UDS, respectively. 
#' @param quietly Should warning messages be muted? Defaults to \code{FALSE}
#'
#' @return A use pattern string the same length as \code{use_pattern} with 
#'   missing values imputed according to the chosen imputation method.
#'   
#' @details If you would like to replace all UDS for missing visits with a
#'   single, pre-specified value (such as positive), please use
#'   \code{\link{recode_missing_visits}} instead. Furthermore, there will most
#'   likely still be missing values in the use pattern even after imputation.
#'   This would occur if all the values are missing, if the first values of the
#'   use pattern are missing (if LOCF is used), if the first and/or last values
#'   of the use pattern are missing (if LOCF-D is used), or if there are back to
#'   back missing visits (if kNV with \code{k = 1} is used). Because of this,
#'   you may need to call \code{\link{recode_missing_visits}} in a pipeline 
#'   after this function to replace or remove the remaining non-imputable
#'   missing visits.
#'   
#'   If you are using the kNV imputation option, there are some caveats to
#'   consider. Due to rounding rules, any rounding ties are broken by order of
#'   the values to the \code{knvWeights_num} vector. For instance, consider a 
#'   subject who had a negative UDS in one week, then a missing UDS for the next
#'   week, and then two UDS in the following week (of which one was positive and
#'   the other was negative). This is represented by the use pattern
#'   \code{"-o*"}. The default behavior of the kNV method is to impute this to
#'   \code{"-**"} because the order of the \code{knvWeights_num} vector has
#'   \code{"+"}, then \code{"*"}, then \code{"-"} UDS values. In this order, a
#'   positive result trumps a mixed result, and a mixed result trumps a negative
#'   result. Similarly, the use pattern \code{"+o*"} will be imputed to
#'   \code{"++*"} by default.
#'   
#'   At current, we allow for many symbols in the use pattern "word", such as
#'   "_" for missing by study design, "o" missing for protocol non-compliance
#'   (the most common form of missing), "+" for positive, "-" for negative, and
#'   "*" for mixed positive and negative results (this usually comes up when the
#'   visit represents multiple days and there are both positive and negative
#'   results in those days; for example, a subject is tested weekly; they
#'   provided a positive test on Tuesday but came back to provide a negative
#'   test the following day).
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
                                  method = c("locf", "locfD", "mode", "kNV"),
                                  missing_is = "o",
                                  mixed_is = "*",
                                  tiebreaker = "+",
                                  k = 1,
                                  knvWeights_num = c(
                                    `o` = NA,
                                    `+` = 1,
                                    `*` = 0.5,
                                    `-` = 0
                                  ),
                                  quietly = FALSE) {
  # browser()
  
  
  ###  Get all non-missing matches  ###
  all_chars <- str_split(use_pattern, pattern = "")[[1]]
  nVisits   <- length(all_chars)
  unique_chars <- unique(all_chars)
  unique_chars <- unique_chars[unique_chars != missing_is]
  if (length(unique_chars) == 0) {
    if (!quietly) {
      warning("No non-missing visits. No imputation done.", call. = FALSE)
    }
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
      .impute_mode(
        all_chars,
        missing_is = missing_is,
        mixed_is = mixed_is,
        tiebreaker = tiebreaker
      )
    },
    
    kNV  = {
      if (!quietly) {
        warning("kNV imputation in development. Expect bugs.", call. = FALSE)
      }
      .impute_kNV(
        all_chars,
        missing_is = missing_is,
        k = k,
        weights_num = knvWeights_num
      )
    }
  )
  
  
  ###  Return  ###
  paste0(out_pattern_split, collapse = "")
  
}


######  LOCF  #################################################################
.impute_locf <- function(x, missing_is){
  # browser()
  
  nX <- length(x)
  if (nX == 1L) {
    return(x)
  }
  
  # Initialise
  imputed_visits <- character(length = nX)
  imputed_visits[1] <- x[1]
  
  # Loop
  for (visit in 2:nX) {
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
.impute_mode <- function(x, missing_is, mixed_is, tiebreaker){
  # browser()
  
  clean_x <- x[x != missing_is]
  
  # All mixed results UDS will be assigned the tiebreaker value
  clean_x[clean_x == mixed_is] <- tiebreaker
  # which.max() gives priority to the 1st mode; in the case of ties, we need to
  #   allow the user to control this
  most_common <- c(
    which.max( table(clean_x) ),
    which.max( rev( table(clean_x) ) )
  )
  uds_mode <- names(most_common)
  # If there are ties, take the tiebreaker
  if (tiebreaker %in% uds_mode) {
    uds_mode <- tiebreaker
  } else {
    # This will be a problem if there is a third major category
    uds_mode <- uds_mode[1]
  }
  
  imputed_x <- x
  imputed_x[imputed_x == missing_is] <- uds_mode
  imputed_x
  
}



######  kNV  ##################################################################
.impute_kNV <- function(x, missing_is, k = 1, weights_num){
  # browser()
  
  # The case_when() syntax works great here when we are coding interactively;
  #   it's much more ugly when the LHS and RHS are programatically defined
  origVals_char <- names(weights_num)
  newVals_num   <- unname(weights_num)
  visits_int    <- length(x)
  
  
  # Recode to numeric
  xNew <- x
  for ( i in seq_along(origVals_char) ) {
    xNew[x == origVals_char[i]] <- newVals_num[i]
  }
  x_num <- as.numeric(xNew)
  
  
  # Boundary and Domain
  domain_int <- seq.int(from = 1 + k, to = visits_int - k)
  toImpute_idx <- which( is.na( x_num ) )
  toImputeInDomain_idx <- toImpute_idx[toImpute_idx %in% domain_int]
  
  
  # Imputation
  xImpute_num <- x_num
  for ( i in toImputeInDomain_idx ) {
    
    nbhd_idx <- seq.int(from = i - k, i + k)
    deletedNbhd_idx <- nbhd_idx[nbhd_idx != i]
    
    # average in the window, allowing missings to trump other values (this will 
    #   work for k > 1 only in cases where missing values are rare)
    xImpute_num[i] <- mean( x_num[deletedNbhd_idx], na.rm = FALSE )
    
  }
  
  
  # Recode back to character
  numericWeights_num <- weights_num[!is.na(weights_num)]
  xRound_num <- .round_to(xImpute_num, numericWeights_num)
  xOut_char <- character(length = visits_int)
  for ( i in seq_along(newVals_num) ) {
    xOut_char[xRound_num == newVals_num[i]] <- origVals_char[i]
  }
  xOut_char[is.na(xRound_num)] <- missing_is
  
  # Return
  xOut_char
  
}

.round_to <- function(num, values_num) {
  
  dist_mat <- outer(
    X = num,
    Y = values_num,
    FUN = function(x, y) { abs(x - y) }
  )
  # which.min() returns integer(0) instead of NA for all NA values
  mins_ls <- apply(X = dist_mat, MARGIN = 1, FUN = which.min, simplify = FALSE)
  mins_ls[lengths(mins_ls) == 0] <- NA_integer_
  values_num[unlist(mins_ls)]
  
}
