

#' Function to extract diary data and process it from raw REDCap file
#' @param alldata Raw REDCap file, with missing values set to blank
#' @param include_missing_dates Boolean: Whether to include rows for dates within 
#' cycle when no diaries were recorded
#' @param rmv_duplicates If multiple valid diary entries for a given day are included,
#' whether to retain them or not. Default remove all duplicates.
#' @return Processed data.table of diaries 
extract_diaries <- function(alldata,
                            include_missing_dates = TRUE, rmv_duplicates=TRUE) {
  # inital variables -----
  
  fatigue_cols <- c("fatigue_now", "fatigue_24hr", "fatigue_worst",
                    "fatigue_activity", "fatigue_mood",
                    "fatigue_walking", "fatigue_work",
                    "fatigue_relations", "fatigue_enjoyment")
  pain_cols <- c("pain_worst","pain_avg")
  
  symptom_cols <- c(fatigue_cols, pain_cols)
  
  

  # Extract cycle diaries ------------------------------------------------------

  diaries_cycle1 <- get_cycle_diary(alldata, 1)
  
  diaries_cycle2 <- get_cycle_diary(alldata, 2)
  
  diaries_cycle3 <- get_cycle_diary(alldata, 3)
  
  diary_dt <- rbind(diaries_cycle1, diaries_cycle2, diaries_cycle3)

  stopifnot(sum(is.na(diary_dt$watch_start_date)) == 0)
  
  
  # Process combined diaries ---------------------------------------------------
  # removing empty rows and adding in missing dates if designated
  

  stopifnot(sum(!is.na(diary_dt[is.na(record_date)]$pain_avg)) == 0)
  diary_dt <- diary_dt[!is.na(record_date)]
  

  # check that watch_start_date always before or same as first record_date
  stopifnot(nrow(diary_dt[is.na(watch_start_date)]) == 0)

  diary_dt[record_date < watch_start_date, .(record_id, cycle_daily_diary_timestamp,
                                             record_date, cycle, watch_start_date, watch_end_date)]
  
  diary_dt <- diary_dt[record_date >= watch_start_date]
  stopifnot(all(diary_dt[, watch_start_date <= min(record_date), 
                         by=c("record_id", "cycle")]$V1 == TRUE))
  
  
  # check if any record entries given after recorded watch removal
  record_after_watchend <- diary_dt[, max(record_date) > watch_calc_end_date, 
                                    by=c("record_id", "cycle")]$V1
  if (all(record_after_watchend == FALSE | is.na(record_after_watchend)) == FALSE) {
    print("There are some diary entries after watch taken off")
  }
  

  diary_dt[, cycle_end := watch_calc_end_date]
  # print following to check which cycles have longer than 28 day cycle  
  diary_dt[cycle_duration == 28 &
             (watch_end_date > watch_calc_end_date + 1), cycle_end := watch_end_date]
  stopifnot("There are records with no calculated end-date"=nrow(diary_dt[is.na(cycle_end)]) == 0)

  # include all record dates (even if NA) if specified
  cycle_dates <- diary_dt[, .(watch_start_date, watch_end_date,
                              cycle_end, record_id, cycle)] %>% unique()
  
  # check that we've adjusted for any cycles that are stopped early (for surgery)
  # and go straight into second cycle before 42 days
  cycle_dates_shifted <- cycle_dates[, .(record_id,watch_start_date, cycle_end, cycle)]
  cycle_dates_shifted[, cycle := cycle - 1]
  colnames(cycle_dates_shifted) <- c("record_id", "nextcycle_start_date", "nextcycle_end_date", "cycle")
  cycle_dates <- cycle_dates %>% merge(cycle_dates_shifted, by=c('record_id','cycle'), all.x=T)
  
  cycle_dates[nextcycle_start_date < cycle_end, cycle_end := watch_end_date]
  

  # make sure the cycle dates updated end dates agree with diary_dt:
  diary_dt <- diary_dt[, !c("cycle_end","watch_start_date","watch_end_date")] %>%
    merge(cycle_dates[, .(record_id, cycle, watch_start_date, watch_end_date, cycle_end)],
          by=c("record_id","cycle"))
  
  # make sure no duplicate dates:


  stopifnot(nrow(cycle_dates[nextcycle_start_date < cycle_end]) == 0)
  date_seq <- cycle_dates[, .(record_date = seq(watch_start_date, by="day", 
                                                to=cycle_end+1) %>% as.IDate()), 
                          by=c("record_id", "cycle", "watch_start_date", "watch_end_date", "cycle_end")]
  
  
  # bring in original diary_dt again to catch any lost dates:
  diary_orig_dt <- rbind(diaries_cycle1, diaries_cycle2, diaries_cycle3)
  
  # get the actual cycle that these removed dates fit with by merging with date sequence
  diary_removed <- diary_orig_dt[record_date < watch_start_date, !c("cycle","watch_start_date","watch_end_date")] %>%
    merge(date_seq, by=c("record_id","record_date"))
  
  # re-append to original diary
  diary_dt <- diary_dt %>% rbind(diary_removed)
  
  
  # Deal with duplicate dates ---------------------------------------------------
  
  diary_dt <- clean_duplicates(diary_dt, remove_duplicates=rmv_duplicates, symptom_cols)
  if (rmv_duplicates==TRUE) {
    stopifnot(all(diary_dt[, .N, by=c("record_id","cycle","record_date")]$N == 1))
  }
  
  # intentionally don't merge on "cycle" as there are some dates assigned to wrong cycle
  diary_dt <- diary_dt %>% 
    merge(date_seq,
          by=c("record_id","cycle", "record_date","watch_start_date",
               "watch_end_date","cycle_end"), all.x=TRUE, all.y=TRUE)
  cycle_dur_dt <- 
    diary_dt[!is.na(cycle_duration),.(cycle_duration = unique(cycle_duration)), by=.(record_id)]
  
  diary_dt[, cycle_duration := NULL]
  diary_dt <- diary_dt %>% merge(cycle_dur_dt, by='record_id')
  
  
  
  
  # Cycle day variables:
  diary_dt[, day_of_cycle := difftime(record_date, watch_start_date, units='day')]
  stopifnot(nrow(diary_dt[is.na(day_of_cycle)]) == 0)
  diary_dt <- diary_dt[day_of_cycle >= 0] # remove invalid recordings, where it's done prior to start
  stopifnot(nrow(diary_dt[day_of_cycle < 0]) == 0)
  
  diary_dt <- diary_dt[order(record_id, cycle, record_date)]
  diary_dt[, cycle1_start := first(watch_start_date), by=record_id]
  diary_dt[, day_of_course := difftime(record_date, cycle1_start, units='day')]
  diary_dt[, cycle1_start := NULL]
  
  
  # heck that some diary entries are not in wrong cycle:
  diary_dt[, course_start := min(day_of_course, na.rm=T), by=.(record_id, cycle)]
  colnames(date_seq) <- c("record_id","cycle_seq", "watch_start_date_seq","watch_end_date_seq",
                          "cycle_end_seq","record_date")

  # Fix dates that are in the wrong cycle
  diary_wrong_cycle <- diary_dt[day_of_course >= course_start + 45]  %>% copy() %>%
    merge(date_seq, by=c("record_id","record_date"), all.x=T, all.y=F)
  diary_wrong_cycle <- diary_wrong_cycle[!is.na(cycle_seq)]
  diary_wrong_cycle[, `:=`(cycle=cycle_seq,
                           watch_start_date = watch_start_date_seq,
                           watch_end_date = watch_end_date_seq,
                           cycle_end = cycle_end_seq)]
  diary_wrong_cycle <- diary_wrong_cycle[, mget(colnames(diary_dt))]
  # combine with original diary table:
  diary_dt <- diary_dt[!(day_of_course >= course_start + 45)] %>% 
    rbind(diary_wrong_cycle)
  
  # reset course dates:
  diary_dt[, day_of_cycle := difftime(record_date, watch_start_date, units='day')]
  stopifnot(nrow(diary_dt[is.na(day_of_cycle)]) == 0)
  diary_dt <- diary_dt[day_of_cycle >= 0] # remove invalid recordings, where it's done prior to start
  diary_dt <- diary_dt[order(record_id, cycle, record_date)]
  stopifnot(nrow(diary_dt[day_of_cycle >= 45]) == 0)
  
  
  # fix duplicates again - these are to do with dates being recorded in consecutive cycles
  diary_dt[, n_day_dup := .N, by=.(record_id, record_date)]
  diary_dt <- clean_duplicates(diary_dt, remove_duplicates=rmv_duplicates, symptom_cols)
  diary_dt[, cycle_length := difftime(cycle_end, watch_start_date, unit='days')]
  diary_dt[n_day_dup > 1, n_complete := sum(!is.na(pain_avg)), by=.(record_id, record_date)]
  diary_dt <- diary_dt[(n_day_dup ==1) | n_day_dup > 1 & n_complete >=1 & !is.na(pain_avg)]
  diary_dt[, n_day_dup := .N, by=.(record_id, record_date)]
  diary_dt <- diary_dt[(n_day_dup == 1) | (n_day_dup > 1 & day_of_cycle <= cycle_length)]
  diary_dt[, n_day_dup := .N, by=.(record_id, record_date)]
  diary_dt[n_day_dup > 1][, .(record_id, cycle, record_date, watch_start_date, cycle_end, fatigue_now,
                              day_of_cycle, day_of_course, cycle_length)]
  stopifnot(nrow(diary_dt[n_day_dup > 1]) == 0)
  diary_dt[, `:=`(n_day_dup = NULL, course_start = NULL,
                  n_complete = NULL)]
  
  
  # When excluding missing dates:
  
  if (!include_missing_dates) {
    diary_dt[, missing_entry := apply(.SD, MARGIN=1, FUN=function(x){sum(is.na(x))>0}), .SDcols=symptom_cols]
    diary_dt <- diary_dt[missing_entry == FALSE]
  }
  

  stopifnot(all(diary_dt[, uniqueN(record_date)/.N, by=record_id]$V1 == 1))
  
  
  # Add in holiday/period info --------------------------------------------------
  
  diary_dt[, holiday := 0]
  diary_dt[, period := 0]
  
  cycle_data <- rbind(get_holiday_menses(alldata, 1), get_holiday_menses(alldata, 2),
                      get_holiday_menses(alldata, 3))
  
  
  unq_cycles <- diary_dt[, .(record_id, cycle)] %>% unique()
  
  # merge in holiday/period times into diary_dt
  diary_dt <- diary_dt %>% merge(cycle_data, by=c("record_id", "cycle"), 
                                 all.x=TRUE, all.y=FALSE)
  # mark dates where holiday or period takes place:
  diary_dt[(includes_holiday == 1 | !is.na(holiday_start)) & 
             record_date >= holiday_start & record_date <= holiday_end, 
           holiday := 1]
  diary_dt[(includes_period == 1 | !is.na(period_1_start)) & 
             record_date >= period_1_start & record_date <= period_1_end, 
           period := 1]
  diary_dt[!is.na(period_2_start) & !is.na(period_2_end) & 
             record_date >= period_2_start & record_date <= period_2_end,
           period := 1]
  
  cols2drop <- c("cycle_end",
                 "redcap_event_name","redcap_repeat_instrument",
                 "redcap_repeat_instance","cycle_daily_diary_complete",
                 "includes_holiday", "holiday_start", "holiday_end",
                 "includes_period", 
                 "includes_nightshift")
  diary_dt[, (cols2drop) := NULL]
  
  
  diary_dt[, (fatigue_cols):= lapply(.SD, function(x){x-1}), .SDcols=fatigue_cols]
  
  stopifnot(all(diary_dt[, mget(fatigue_cols)] %>% sapply(function(x){max(x,na.rm=T)}) == 10))
  stopifnot(all(diary_dt[, mget(fatigue_cols)] %>% sapply(function(x){min(x,na.rm=T)}) == 0))
  stopifnot(all(diary_dt[, mget(pain_cols)] %>% sapply(function(x){min(x,na.rm=T)}) == 1))
  stopifnot(all(diary_dt[, mget(pain_cols)] %>% sapply(function(x){max(x,na.rm=T)}) == 10))
  
  diary_dt[, bfi_total := apply(.SD, MARGIN=1, FUN=sum), .SDcols=fatigue_cols]
  diary_dt[, bfi_total := bfi_total/9]
  diary_dt[, pain_total := (pain_avg + pain_worst)/2]
  
  return (diary_dt)
}



# Helper functions ------------------------------------------------------------

#' HELPER function to extract table of holiday/period start and end times from REDCap data
#' @param alldata Raw REDCap data 
#' @param cycle_num Cycle number to be extracted (1-3)
#' 
#' @return data.table of holiday/period times with attached cycle number as column
get_holiday_menses <- function(alldata, cycle_num) {
  visit_num <- cycle_num + 1
  
  cycle_data <- 
    alldata[(redcap_event_name == paste0("visit_", visit_num, "_arm_1")) & 
              (redcap_repeat_instrument == "")]
  visit_cols <- c("yn_holiday", "date_holiday_start",
                  "date_holiday_end", "yn_period_mid_cycle",
                  "period_1_start_date", "period_1_end_date",
                  "period_2_start_date", "period_2_end_date",
                  "yn_night_shifts") %>% 
    sapply(function(x){paste0("crf_v", visit_num, "_", x)}) %>% unname()
  
  missing_cols <- visit_cols[!visit_cols %in% colnames(cycle_data)]
  if (length(missing_cols) > 0) {cycle_data[, (missing_cols):= NA]}
  cycle_data <- cycle_data[, mget(c("record_id", visit_cols))]
  colnames(cycle_data) <- c("record_id", "includes_holiday", "holiday_start",
                            "holiday_end", "includes_period", 
                            "period_1_start", "period_1_end", "period_2_start",
                            "period_2_end", "includes_nightshift")
  date_colnames <- c("holiday_start", "holiday_end", "period_1_start", 
                     "period_1_end", "period_2_start", "period_2_end")
  cycle_data[, (date_colnames) := lapply(.SD, function(x) {as.Date(x)}), 
             .SDcols=date_colnames]
  cycle_data[, cycle := cycle_num]
  return(cycle_data)
}

#' HELPER function to extract all diaries for a specific cycle
#' @param alldata Raw REDcap data
#' @param cycle_num Number of cycle: between 1 and 3
#' @return table containing diary entries for specific cycle
get_cycle_diary <- function(alldata, cycle_num) {
  
  stopifnot("Invalid cycle number"= cycle_num >=1 | cycle_num <= 3)
  cycle_starts <- get_cycle_starts(alldata, cycle_num)
  
  # get 'word' referencing cycle num - first, second, or third
  cycle_word <- ifelse(cycle_num == 1, "first", ifelse(cycle_num == 2, "second",
                                                       "third"))
  
  symptom_cols <- config$symptom_col_mapping %>% unlist()
  # colnames that all diaries should share
  shared_colnames <- c("record_id", "redcap_event_name", "redcap_repeat_instrument",
                       "redcap_repeat_instance", "cycle_daily_diary_timestamp",
                       "cycle_daily_diary_complete",
                       "record_date", symptom_cols, config$diary_cols, 
                       paste0("yn_pain_med_drug_", 1:27))
  date_cols <- c("record_date", "watch_start_date", "watch_end_date") # date colnames
  char_cols <- c("cycle_daily_diary_timestamp")  # character column names
  
  # cycle-specific symptom colnames
  dd_cyc_names <- c("date", names(config$symptom_col_mapping),
                    config$diary_cols, 
                    paste0("yn_pain_med_drug_", 1:27)) %>%
    sapply(function(x) {paste0("dd_cyc", cycle_num, "_", x)}) %>% unname()
  # cycle-specific colnames (all of them)
  cycle_colnames <- c("record_id", "redcap_event_name", "redcap_repeat_instrument",
                      "redcap_repeat_instance", 
                      paste0(cycle_word, "_cycle_daily_diary_timestamp"),
                      paste0(cycle_word, "_cycle_daily_diary_complete"), 
                      dd_cyc_names)
  
  stopifnot(length(cycle_colnames) == length(shared_colnames))
  
  which_diary <- paste0(cycle_word, "_cycle_daily_diary")
  diary <- alldata[redcap_repeat_instrument == which_diary &
                     record_id %in% cycle_starts$record_id][, mget(cycle_colnames)]
  colnames(diary) <- shared_colnames
  diary[, cycle := cycle_num]
  diary <- diary %>% merge(cycle_starts, by="record_id", all = TRUE)
  diary[, (symptom_cols):= lapply(.SD, as.numeric), .SDcols=symptom_cols]
  diary[, (date_cols):= lapply(.SD, as.IDate), .SDcols = date_cols]
  diary[, (char_cols) := lapply(.SD, as.character), .SDcols = char_cols]
  
  return(diary)
}

#' HELPER function to extract start and end dates of watch cycles from "alldata" table
#' @param alldata data.table containing all REDCap data
#' @param cycle_num Number of cycle (1-3)
#' @return data.table containing all cycles of data with corresponding 
#'  watch start and end dates
get_cycle_starts <- function(alldata,cycle_num) {
  
  # get 'word' referencing cycle num - first, second, or third
  cycle_word <- ifelse(cycle_num == 1, "first", ifelse(cycle_num == 2, "second",
                                                       "third"))
  visit_num <- cycle_num + 1
  visit_complete_var <- paste0("visit_", visit_num, 
                               "_crf_end_of_", cycle_word,"_cycle_complete")
  cycle_start_var <- paste0("wl_cyc", cycle_num, "_start_date")
  cycle_end_var <- paste0("wl_cyc", cycle_num, "_end_date")
  
  dd_date_var <- paste0("dd_cyc", cycle_num, "_date")
  
  
  watch_location_var <- paste0("wl_cyc", cycle_num, "_worn_where")
  
  
  dd_dates <- alldata[, mget(c("record_id", dd_date_var))][!is.na(get(dd_date_var))]
  dd_starts <- dd_dates[, .('dd_start_date' = min(get(dd_date_var), na.rm=T)), by=record_id]
  
  cycle_starts <- 
    alldata[
      , mget(c("record_id", cycle_start_var,cycle_end_var, 
               watch_location_var, "cos_date"))][!is.na(get(cycle_start_var))]
                                                         
  cycle_starts <- cycle_starts %>% 
    merge(alldata[redcap_event_name == "visit_1_arm_1", .(record_id, crf_v1_consent_date)],
          by='record_id')

  cycle_starts <- cycle_starts %>% merge(dd_starts, by='record_id')
  
  # when the logged watch start date is wrong we change this to the first daily diary
  # date entry - only if it's more than 30 days previous, 
  # otherwise can pick up on diaries entered in wrong cycle
  cycle_starts[dd_start_date < get(cycle_start_var)-30][[cycle_start_var]] <- 
    cycle_starts[dd_start_date < get(cycle_start_var)-30]$dd_start_date
  
  # for participants consented after 1 April 2023, only do 4 week cycles instead of 6
  cycle_starts[, cycle_duration := 42]
  cycle_starts[crf_v1_consent_date >= as.Date("2023-04-01", format="%Y-%m-%d"), cycle_duration := 28]

  cycle_starts[, cycle_calc_end := get(cycle_start_var) + cycle_duration]
 
  

  cycle_starts[, `:=`(watch_location = get(watch_location_var),
                      watch_start_date = as.IDate(get(cycle_start_var)),
                      watch_end_date = as.IDate(get(cycle_end_var)),
                      watch_calc_end_date = as.IDate(cycle_calc_end))]
  cycle_starts[, (cycle_start_var) := NULL]
  cycle_starts[, (cycle_end_var) := NULL]
  cycle_starts[, cycle_calc_end := NULL]
  cycle_starts[, (watch_location_var) := NULL]
  cycle_starts[, dd_start_date := NULL]
  
  
  cycle_starts[, watch_dom_wrist:= ifelse(watch_location == 2 | 
                                           watch_location == 4, TRUE, FALSE)]
  stopifnot(sum(is.na(cycle_starts$watch_location)) == 0)
  return (cycle_starts)
}



#' Function to get table of watch start/stop and location info for each 
#' participant cycle
#' @param alldata Raw REDCap file, with missing values set to blank
#' 
#' @return table of watch data
get_watch_info <- function(alldata) {
  
  cycle1_starts <- get_cycle_starts(alldata, 1)
  cycle1_starts[, cycle := 1]
  cycle2_starts <- get_cycle_starts(alldata, 2)
  cycle2_starts[, cycle := 2]
  cycle3_starts <- get_cycle_starts(alldata, 3)
  cycle3_starts[, cycle := 3]
  
  all_cycle_starts <- rbind(cycle1_starts, cycle2_starts, cycle3_starts)
  
}


