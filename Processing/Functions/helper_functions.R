#' Function to compute the mean of the previous n values
#' @param x the vector to be transformed
#' @param w the number of values to be averaged (window size)
#' @param shifted whether the average should be of the PREVIOUS w days or should include
#' the current day
#' 
#' @return A vector of length x where each item is the average of the previous w values 
prev_n_mean <- function(x, w, shifted=T) {
  x_padded <- c(rep(NA, w-1), x, rep(NA, w-1))
  x_rolled <- x_padded %>% rollmean(k=w, align='right',na.rm=T) 
  if (shifted == T) {
    x_rolled <- x_rolled %>% shift(n=1)
  }
  x_rolled <- x_rolled[1:length(x)]
  return(x_rolled)
}



#' Min-max normalisation
#' @param x
minmax_norm <- function(x) {
  return((x-min(x, na.rm=T))/(max(x, na.rm=T)-min(x, na.rm=T)))
}



#' Compute TKEO (Teager-Kaiser energy operator), a non-linear energy operator
#' Translated directly from the "TsanasBOX" Toolbox in MATLAB
#' Credit: Athanasios Tsanas (Last modified 18 Feb 2016)
#' @param x a vector representing a time-series 
TKEO <- function(x, normalize=TRUE, include_endpoints=FALSE) {
  energy <- x^2 - (shift(x, n=1)*shift(x, n=-1))
  if (include_endpoints) {
    energy[1] <- x[1]^2
    energy[length(x)] <- x[length(x)]^2
  }
  if (normalize) {
    energy <- energy/length(x)
  }
  return (energy)
}

#' Function to compute RMSSD given function x
compute_RMSSD <- function(x) {
  res <- diff(x)^2 %>% mean(na.rm=T) %>% sqrt()
  return(res)
}


#' Function to compute the number of consecutive NA sequences in a vector (with
#' length of more than "consec_lim")
#' @param x Vector to compute sequences over
#' @param consec_lim The min number of consecutive NA entries that should be counted 
#' as a "missing sequence"
#' @return the number of "missing sequences", i.e., number of consecutive NA 
#' sequences that are at least "consec_lim" long
num_consec_missing <- function(x, consec_lim) {
  res <- rle(is.na(x))
  consec_seqs <- which(res$values == TRUE & res$lengths >= consec_lim)
  return (length(consec_seqs))
}

#' Function to check for runs of missing values in vector and output a corresponding
#' vector that is TRUE if the value is part of a run (of length `consec_lim`), or
#' corresponding vector that is TRUE only if that run is at the end of the vector
#' @param x Vector to compute sequences over
#' @param consec_lim The min number of consecutive NA entries that should be counted
#' as a missing sequence
#' @param end_only Boolean. Whether only runs of NA at the end of vector should be output
#' @return A list containing either a) vector with TRUE if value part of NA run (FALSE otherwise)
#'  if end_only is FALSE, or if end_only is TRUE, then return
#'   b) vector with TRUE if value part of NA run at end of x
is_consec_missing <- function(x, consec_lim, end_only = FALSE) {
  res <- rle(is.na(x))
  consec_seqs <- which(res$values == TRUE & res$lengths >= consec_lim)
  output_ls <- rep(FALSE, length(x))
  end_ls <- rep(FALSE, length(x)) # TRUE only if run of missing values at end of vector
  
  num_runs <- length(res$values)
  if (num_runs == 1 & all(res$values == TRUE)) {
    output_ls <- rep(TRUE, length(x))
    end_ls <- rep(TRUE, length(x))
  } else if (num_runs > 1 & length(consec_seqs) > 0) {
    for (i in 1:length(consec_seqs)) {
      consec_idx <- consec_seqs[i]
      start_idx <- res$lengths[1:(consec_idx-1)] %>% sum() + 1
      end_idx <- res$lengths[1:consec_idx] %>% sum()
      output_ls[start_idx:end_idx] <- TRUE
      if (end_idx == length(x)) {
        end_ls[start_idx:end_idx] <- TRUE
      }
    }
  } 
  if (end_only) {
    return(end_ls)
  } else {
    return(output_ls)
  }
}


# Helper functions  ------------------------------------------------------------
# function to compute the moving average, including the end points
# INPUTS: x - vector to compute average over
#         w - size of window
moving_summary = function(x, w, fun=mean, overlap = NA,
                          rmv_na = TRUE, type='centered') {
  if (is.na(overlap)) {overlap <- w-1}
  
  if (type == 'centered') {
    xfill <- c(rep(NA, floor(w/2)), x, rep(NA, floor(w/2))) 
  } else if (type == 'start') {
    xfill <- c(x, rep(NA, w))  # should this be w-1??
  }
  if (overlap == w-1) {
    meanvec <- rep(NA, length(x))
    for (i in 1:length(x)) {
      meanvec[i] <- fun(shift(xfill, n=i-1, type='lead')[1:w], na.rm=rmv_na)
    }
  } else {
    overlap_diff <- w - overlap
    meanvec <- rep(NA, ceiling(length(x)/overlap_diff)+1)
    k <- 1
    for (i in seq(1, length(x), overlap_diff)) {
      meanvec[k] <- fun(shift(xfill, n=i-1, type='lead')[1:w], na.rm=rmv_na)
      k <- k + 1
    }
  }
  return (meanvec)
}
