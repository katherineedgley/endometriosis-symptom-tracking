
#' HELPER Function to clean duplicates in daily diary, and remove multiple valid entries
#' if required
#' @param diary_dt Processed daily diary within extract_diaries output
#' @param remove_duplicates Boolean, whether extra valid diary entries for
#' a given day should be removed (the latest entry will be retained)
clean_duplicates <- function(diary_dt, remove_duplicates, symptom_cols) {
  diary_dt[, nduplicate := .N, by=c("record_id","cycle","record_date")]
  diary_dt[, timestamp := as.POSIXct(cycle_daily_diary_timestamp, 
                                     format="%Y-%m-%d %H:%M:%S")]
  diary_dt[, correct_time := (timestamp >= as.POSIXct(paste(record_date,"17:00"),
                                                      format="%Y-%m-%d %H:%M")) &
             (timestamp <= as.POSIXct(paste(record_date+1, "05:00"),
                                      format="%Y-%m-%d %H:%M"))]
  diary_dt[, possible_prev := (timestamp >= as.POSIXct(paste(record_date-1,"17:00"),
                                                       format="%Y-%m-%d %H:%M")) &
             (timestamp <= as.POSIXct(paste(record_date, "05:00"),
                                      format="%Y-%m-%d %H:%M"))]
  
  diary_dt[possible_prev == TRUE, `:=`(record_date = record_date -1)]
                                       #day_of_cycle = day_of_cycle-1,
                                       #day_of_course = day_of_course-1)]
  diary_dt[, correct_time := (timestamp >= as.POSIXct(paste(record_date,"17:00"),
                                                            format="%Y-%m-%d %H:%M")) &
                   (timestamp <= as.POSIXct(paste(record_date+1, "05:00"),
                                            format="%Y-%m-%d %H:%M"))]
  
  diary_dt[, nduplicate := .N, by=c("record_id","cycle","record_date")]
  diary_dt[, ncorrect_time := sum(correct_time, na.rm=T), by=c("record_id","cycle","record_date")]
  diary_dt <- diary_dt[!(ncorrect_time > 0 & correct_time == FALSE)] # remove entries where there is another correct time recorded
  diary_dt[, nduplicate := .N, by=c("record_id","cycle","record_date")]
  
  diary_dt[, with_na := rowSums(is.na(.SD))>0, .SDcols=symptom_cols]
  diary_dt[, num_with_missing := sum(with_na), by=c("record_id","cycle","record_date")]
  diary_dt <- diary_dt[!(nduplicate > 1 & num_with_missing > 0 & 
                                       with_na == TRUE)]
  diary_dt[, nduplicate := .N, by=c("record_id","cycle","record_date")]
  
  diary_dt[, feasible_time := (timestamp >= as.POSIXct(paste(record_date,"00:00"),
                                                       format="%Y-%m-%d %H:%M")) &
             (timestamp <= as.POSIXct(paste(record_date+2, "00:00"),
                                      format="%Y-%m-%d %H:%M"))]
  diary_dt[, nfeasible := sum(feasible_time, na.rm=T), by=.(record_id,cycle,record_date)]
  diary_dt <- diary_dt[!(nduplicate > 1 & nfeasible > 0 & feasible_time == FALSE)]
  
  diary_dt$is_dup <- diary_dt[, mget(c("record_id","cycle","record_date",
                                       "nduplicate",symptom_cols))] %>% duplicated()

  diary_dt <- diary_dt[is_dup == FALSE]
  
  diary_dt[, nduplicate := .N, by=c("record_id","cycle","record_date")]
  
  if (remove_duplicates == TRUE) {
    # order by timestamp completed (so later timestamps are taken)
    diary_dt <- diary_dt[order(record_id, cycle, record_date, -timestamp), .SD[1],
                         by=.(record_id, cycle, record_date)] 
  }
  
  diary_dt[, `:=`(nduplicate = NULL, timestamp=NULL,
                  correct_time=NULL, ncorrect_time = NULL,
                  with_na=NULL, num_with_missing=NULL,
                  feasible_time=NULL, nfeasible=NULL, 
                  is_dup=NULL, possible_prev=NULL)]
  
  return(diary_dt)
}
