# Function to process alldata into diary and include all dates up to 28 days 
# including for participants that dropped out early
# label timestamp as correct/feasible/paper/other (retrospective)
prepare_adherence <- function(alldata, allmetrics_dt) {

  # inital variables -----
  
  fatigue_cols <- c("fatigue_now", "fatigue_24hr", "fatigue_worst",
                    "fatigue_activity", "fatigue_mood",
                    "fatigue_walking", "fatigue_work",
                    "fatigue_relations", "fatigue_enjoyment")
  pain_cols <- c("pain_worst","pain_avg")
  
  symptom_cols <- c(fatigue_cols, pain_cols)
  
  dropped_pats <- alldata[crf_cos_yn_status_change == 1]$record_id

  
  # Extract cycle diaries ------------------------------------------------------

  diaries_cycle1 <- get_cycle_diary(alldata, 1)
  
  diaries_cycle2 <- get_cycle_diary(alldata, 2)
  
  diaries_cycle3 <- get_cycle_diary(alldata, 3)
  
  diary_dt <- rbind(diaries_cycle1, diaries_cycle2, diaries_cycle3)

  stopifnot(sum(is.na(diary_dt$watch_start_date)) == 0)
  
  
  # Process combined diaries ---------------------------------------------------
  # removing empty rows and adding in missing dates if designated
  
  # remove rows with no given record_date
  # First check no actual symptom entries (using pain_avg)
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
  diary_removed <- diary_orig_dt[, !c("cycle","watch_start_date","watch_end_date")] %>%
    merge(date_seq, by=c("record_id","record_date"), all=T)
  
  # re-append to original diary
  diary_dt <- diary_dt %>% rbind(diary_removed)
  
  diary_dt[, day_of_cycle := difftime( record_date,watch_start_date, units='days'), by=.(record_id, cycle)]
  diary_dt <- diary_dt[!is.na(cycle)]
  diary_dt <- diary_dt[day_of_cycle > 0 & day_of_cycle <= 28]
  
  
  diary_dt[, cycle_end := watch_calc_end_date]
  
  diary_dt[, nduplicate := .N, by=c("record_id","cycle","record_date")]
  diary_dt[, timestamp := as.POSIXct(cycle_daily_diary_timestamp, 
                                     format="%Y-%m-%d %H:%M:%S")]
  diary_dt[, correct_time := (timestamp >= as.POSIXct(paste(record_date,"00:00"),
                                                      format="%Y-%m-%d %H:%M")) &
             (timestamp <= as.POSIXct(paste(record_date+1, "05:00"),
                                      format="%Y-%m-%d %H:%M"))]
  
  diary_dt[, possible_prev := (timestamp >= as.POSIXct(paste(record_date-1,"17:00"),
                                                       format="%Y-%m-%d %H:%M")) &
             (timestamp <= as.POSIXct(paste(record_date, "05:00"),
                                      format="%Y-%m-%d %H:%M"))]
  
  
  diary_dt[, ncorrect_time := sum(correct_time, na.rm=T), by=c("record_id","cycle","record_date")]
  
  diary_dt <- diary_dt[!(ncorrect_time > 0 & correct_time == FALSE)] # remove entries where there is another correct time recorded
  diary_dt[, nduplicate := .N, by=c("record_id","cycle","record_date")]
  
  diary_dt[, with_na := rowSums(is.na(.SD))>0, .SDcols=symptom_cols]
  diary_dt[, num_with_missing := sum(with_na), by=c("record_id","cycle","record_date")]
  
  diary_dt <- diary_dt[!(nduplicate > 1 & num_with_missing > 0 & 
                           with_na == TRUE & (nduplicate != num_with_missing))]
  diary_dt[, nduplicate := .N, by=c("record_id","cycle","record_date")]
  
  diary_dt[, feasible_time := (timestamp >= as.POSIXct(paste(record_date+1,"05:00"),
                                                       format="%Y-%m-%d %H:%M")) &
             (timestamp <= as.POSIXct(paste(record_date+1, "23:59"),
                                      format="%Y-%m-%d %H:%M"))]
  diary_dt[, nfeasible := sum(feasible_time, na.rm=T), by=.(record_id,cycle,record_date)]
  
  diary_dt <- diary_dt[!(nduplicate > 1 & nfeasible > 0 & feasible_time == FALSE)]
  
  
  diary_dt$is_dup <- diary_dt[, mget(c("record_id","cycle","record_date",
                                       "nduplicate",symptom_cols))] %>% duplicated()
  
  
  diary_dt <- diary_dt[is_dup == FALSE]
  
  diary_dt[, nduplicate := .N, by=c("record_id","cycle","record_date")]
  
  
  diary_dt <- diary_dt[order(record_id, cycle, record_date, -timestamp), .SD[1],
                       by=.(record_id, cycle, record_date)] 
  
  diary_dt[, .N, by=.(record_id,cycle)]$N %>% table()
  entry_dt <- diary_dt[, .N, by=.(record_id,cycle)]
  # all cycles should have exactly 28 entries
  stopifnot(nrow(entry_dt[N < 28]) == 0)
  
  
  # now label entries
  diary_dt[correct_time==F | is.na(correct_time)][
    , .( record_id, cycle, timestamp, record_date,
         correct_time, pain_avg, watch_calc_end_date)][1:20]
  
  
  diary_dt[, paper := with_na == F & is.na(timestamp)]
  diary_dt[paper == T, `:=`(correct_time = FALSE, feasible_time = FALSE)]
  diary_dt[, response_type := ifelse(with_na, 'missing',
                                     ifelse(correct_time, 'correct', 
                                            ifelse(feasible_time, 'feasible',
                                                   ifelse(paper, 'paper','other'))))]
  diary_dt[, day_of_cycle := difftime(record_date, watch_start_date, units='day')]
  diary_dt$response_type %>% table()
  
  diary_dt[, entry_num := 1:.N, by=.(record_id, cycle)]
  
  # fix overwriting issue before fixed in June, means first entry recorded as last
  diary_dt[entry_num == 1 & response_type=="other" & record_date < as.Date("22-06-2022", format="%d-%m-%Y"),
           response_type := "correct"]
  

  npats_cycle <- diary_dt[,.(record_id, cycle)] %>% unique()
  npats_cycle <- npats_cycle[, .(total_enrolled = .N), by=cycle]
  diary_dt <- diary_dt %>% merge(npats_cycle, by='cycle')
  
  
  diary_dt <- diary_dt %>% merge(allmetrics_dt[, .(record_id, record_date, perc_wear, nonwear_perc_day_GGIR)],
                                 by=c('record_id','record_date'), all.x=T, all.y=F)
  diary_dt[, .N, by=.(record_id,cycle)]$N %>% unique()
  # make sure no days after cycle duration
  diary_dt[is.na(perc_wear), perc_wear := 0] # where no watch data available (e.g. withdrew) set to 0 NOT WORN
  
  file_dt <- fread(paste0(config$source_data_dir, "file_dt.csv"), colClasses = c('record_id'='character'))
  file_dt[, has_watch_data := TRUE]
  # where there are two files for a cycle, set end date to second file and remove extra row
  file_dt[, end_date := last(end_date), by=.(record_id,cycle)]
  file_dt<- file_dt[, .SD[1], by=.(record_id,cycle)]
  diary_dt <- diary_dt %>% merge(unique(file_dt[, .(record_id,cycle, start_date,end_date,
                                                    has_watch_data)]),
                                 by=c("record_id",'cycle'), all.x=T, all.y=T)
  diary_dt[is.na(has_watch_data), has_watch_data := FALSE]
  dropped_pats <- alldata[crf_cos_yn_status_change == 1]$record_id
  diary_dt[, dropped_pat := record_id %in% dropped_pats]
  # for those whose data was missing but didn't drop out set to NA
  diary_dt[has_watch_data == F, perc_wear := NA]
  # For cycles where there were 2 watches and one went missing e.g., set this to NA as well
  diary_dt[record_date < start_date, `:=`(has_watch_data = F, perc_wear = NA)]
  # mark whole cycle as missing if two watches used 
  diary_dt[, has_watch_data := sum(has_watch_data==F) == 0, by=.(record_id,cycle)]
  # reassign perc_wear as NA
  diary_dt[has_watch_data==F, perc_wear := NA]

  diary_dt[has_watch_data==F, .(record_id,cycle, perc_wear)] %>% unique()
  
  # Remove cycle where no watch data and no entries, not counted as 
  # participated in cycle
  diary_dt[, diary_missing := response_type == 'missing']
  diary_dt[, num_diaries := sum(diary_missing==F), by=.(record_id,cycle)]
  diary_dt[has_watch_data == F & num_diaries == 0, .(record_id,cycle)]
  diary_dt <- diary_dt[!(has_watch_data==F & num_diaries==0)]
  return(diary_dt)
}
