# Extract menses

#' Function to add period information to diary_dt
#' 
#' @param diary_dt The processed daily diary table.
#' @param lag The number of days in advance of the period to take into account
#' @return diary_dt with added variables `dayofperiod` and `period_ordered`
#'
add_menses <- function(diary_dt) {
  diary_dt[, period_ordered := period %>% get_period_vec(), by=c("record_id","cycle")]
  diary_dt[, num_periods := ifelse(sum(!is.na(period_ordered)) == 0, 0,
                                   max(period_ordered, na.rm=T)), by=c("record_id","cycle")]
  # make vector with the day of period
  diary_dt[, dayofperiod := as.numeric(NA)]

  diary_dt[period_ordered==1, dayofperiod := as.numeric(difftime(record_date, period_1_start, units='day')),
           by=c("record_id",'cycle')]
  diary_dt[period_ordered==2, dayofperiod := as.numeric(difftime(record_date, period_2_start, units='day')),
           by=c("record_id",'cycle')]
 
  diary_dt[, period1_day_diff := difftime(record_date, period_1_start, units='days')]
  diary_dt[, period2_day_diff := difftime(record_date, period_2_start, units='days')]
  
  diary_dt[abs(period1_day_diff) <=14, mens_cycle_num := 1]
  diary_dt[abs(period2_day_diff) <=14, mens_cycle_num := 2]
  diary_dt[period1_day_diff <=14 & period1_day_diff >0 & 
             period2_day_diff >= -10 & period2_day_diff <0, mens_cycle_num := 2]
  diary_dt[period1_day_diff <=10 & period1_day_diff >0 & 
             period2_day_diff >= -14 & period2_day_diff <0, mens_cycle_num := 1]
  
  
  # get vector giving 10 days before and after each period
  diary_dt[, mens_cycle_day := ifelse(is.na(mens_cycle_num), NA, 
                                      ifelse(mens_cycle_num ==1, period1_day_diff,
                                             period2_day_diff))]
  diary_dt[, mens_cycle_day := NA]
  diary_dt[mens_cycle_num ==1, mens_cycle_day := period1_day_diff]
  diary_dt[mens_cycle_num ==2, mens_cycle_day := period2_day_diff]
  
  
  # adjust menstrual cycle number to make sure that when going from one cycle
  # to the next they are not considered the same
  diary_dt[, mens_cycle_num_adj := mens_cycle_num + 10*(cycle-1)]
  
  # get the menstrual cycle number across all cycles from that participant, 
  # instead of starting new from each cycle
  diary_dt[, mens_num_course := extract_mens_num(mens_cycle_num_adj), by=record_id]
  
  
  diary_dt[, `:=`(period1_day_diff=NULL, period2_day_diff=NULL,
                  period_1_start=NULL, period_1_end=NULL,
                  period_2_start=NULL, period_2_end=NULL)]
  return(diary_dt)
}


# HELPER functions ------


get_n_periods <- function(period) {
  res <- rle(period)
  res_dt <- data.table(values=res$values, lengths=res$lengths)
  res_dt <- res_dt[values == 1]
  return(nrow(res_dt))
}

get_period_lengths <- function(period) {
  res <- rle(period)
  res_dt <- data.table(values=res$values, lengths=res$lengths)
  res_dt <- res_dt[values == 1]
  return(res_dt$lengths)
}

#' Function to take period vector (e.g., 000001111000110 where 1 represents period)
#' and output a vector with the period number instead of just 1, so 
#' 000011110000222200 where there are two periods present
#' 
#' @param period The period vector (e.g., 000001111000110)
#' 
get_period_vec <- function(period) {
  res <- period %>% rle()
  res_dt <- data.table(values=res$values, lengths=res$lengths)
  res_dt[values==0, newval := as.numeric(NA)]
  res_dt[values ==1, newval := 1:.N]
  res_dt <- res_dt[, .(lengths, newval)]
  newvec <- 
    res_dt %>% apply(MARGIN=1, FUN=function(x){rep(as.numeric(x[2]), as.numeric(x[1]))})
  newvec <- newvec %>% unlist() %>% as.vector()
  
  return(newvec)
}


extract_mens_num <- function(vec) {
  res <- rle(vec)
  rle_vals <- res$values
  rle_vals[!is.na(rle_vals)] <- 1:length(rle_vals[!is.na(rle_vals)])
  res$values <- rle_vals
  return(inverse.rle(res))
}
