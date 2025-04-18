# Process PROMs

# Load in files  --------------------------------------------

alldata <- fread(paste0(config$source_data_dir, "alldata_processed.csv"),
                 colClasses=c("record_id"='character'))

diary_dt <- fread(paste0(config$processed_output_dir, "diary_dt.csv"),
                  colClasses = c('record_id'='character'))

file_dt <- fread(paste0(config$source_data_dir, "file_dt.csv"),
                 colClasses = c('record_id'='character',
                                'start_date'='IDate',
                                'end_date' = 'IDate'))

# Daily diaries ---------------------------------------------------------------

diary_dt[, cycle_daily_diary_timestamp := 
        cycle_daily_diary_timestamp %>% as.POSIXct(format="%d/%m/%Y %H:%M", tz="UTC")]
diary_dt[, retrospective := (record_date != as.Date(cycle_daily_diary_timestamp)) |
        is.na(cycle_daily_diary_timestamp)]
diary_dt[, record_date := as.Date(record_date)]

diary_dt <- add_menses(diary_dt)




# Combine daily metrics and CRF info ------------------------------------------

# load daysummary:
daysum <- fread(paste0(config$ggir_output_dir,
                       "/results/part5_daysummary_MM_L40M100V400_T5A5.csv"))
daysumWW <- fread(paste0(config$ggir_output_dir,
                       "/results/part5_daysummary_WW_L40M100V400_T5A5.csv"))
stopifnot(daysum$calendar_date %>% as.Date(format="%d/%m/%Y") %>% is.na() %>% sum() == 0)
stopifnot(daysumWW$calendar_date %>% as.Date(format="%d/%m/%Y") %>% is.na() %>% sum() == 0)

daysum[, `:=`(record_id = as.character(ID), ID=NULL,
              record_date = as.Date(calendar_date, format="%d/%m/%Y"),
              calendar_date = NULL)]
daysumWW[, `:=`(record_id = as.character(ID), ID=NULL,
              record_date = as.Date(calendar_date, format="%d/%m/%Y"),
              calendar_date = NULL)]

# make sure all IDs match with filename
stopifnot(nrow(daysum[substr(filename, 1,4) != record_id]) == 0)
stopifnot(nrow(daysumWW[substr(filename, 1,4) != record_id]) == 0)


daysum[, n_dup := .N, by=.(record_id, record_date)]
daysumWW[, n_dup := .N, by=.(record_id, record_date)]

# remove duplicates due to daylight savings
daysum <- daysum[, .SD[1], by=.(record_id, record_date)]
daysum[, n_dup := .N, by=.(record_id, record_date)]
stopifnot(nrow(daysum[n_dup > 1]) == 0)


# load nightsummary
nightsummary <- fread(paste0(config$ggir_output_dir, "results/part4_nightsummary_sleep_cleaned.csv"))
nightsummary[, `:=`(record_id = as.character(ID), ID=NULL,
                    record_date = as.Date(calendar_date, format="%d/%m/%Y"),
                    calendar_date = NULL)]


# make sure all IDs match filename
stopifnot(nrow(nightsummary[substr(filename, 1,4) != record_id]) == 0)
# set GGIR nightsummary variables to have GGIR in name
sleep_cols_nightsum_GGIR <- config$sleep_cols_nightsum_GGIR %>% sapply(function(x) {paste0(x, "_GGIR")})
setnames(nightsummary, old=config$sleep_cols_nightsum_GGIR, new=sleep_cols_nightsum_GGIR)
nightsummary[, record_date_shifted := record_date+1] # GGIR nights are shifted one

nightsummary[, (sleep_cols_nightsum_GGIR) := lapply(.SD, as.numeric), 
             .SDcols=sleep_cols_nightsum_GGIR]


##### Refined diary_dt for allmetrics_dt #####
pat_ls <- diary_dt$record_id %>% unique()
diary_clean_dt <- data.table()
for (id in pat_ls) {
  for (cycle_num in 1:3) {
    diary <- diary_dt[record_id == id & cycle == cycle_num]
    if (nrow(diary) > 0) {
      diary_part1 <- diary[, .(record_id, cycle, record_date, retrospective, pain_avg, pain_worst, 
                               pain_total, fatigue_now, fatigue_24hr, fatigue_worst, fatigue_activity,
                               fatigue_mood, fatigue_walking, fatigue_work,
                               fatigue_relations, fatigue_enjoyment, bfi_total, 
                               period, holiday, day_of_cycle, dayofperiod,
                               mens_cycle_day, day_of_course,
                               gnrh_taken, coc_taken, IUD_taken,
                               pop_taken, prog_taken, hrt_taken,
                               yn_flare, yn_flare_meds, more_less_meds, more_less_flare_meds,
                               yn_diff_flare_meds, yn_bleeding, bleeding_type,
                               surg_or_recovery, in_hospital, days_from_admit,
                               post_deep_surgery, post_surg_type, had_surg, surg_cycle,
                               changed_horm,
                               hormones_taken, deep_study_pat)]
      diary_part2 <- diary[, mget(paste0('yn_pain_med_drug_', 1:27))]
      diary <- cbind(diary_part1, diary_part2)
      
      # had before for 28 day cycles
      start_date <- diary$record_date %>% min(na.rm=T)
      end_date <- diary$record_date %>% max(na.rm=T)
      date_seq<-seq(start_date,by="day",to=end_date) %>% as.Date()
      date_table <- data.table(record_date = date_seq,
                               mens_cycle=rep(1:ceiling(length(date_seq)/28),
                                              each=28,
                                              length.out=length(date_seq)))
      date_table[, mens_cycle_start := min(record_date), by=mens_cycle]
      diary <- diary %>% merge(date_table, by="record_date", all.x=TRUE, all.y=TRUE)
      diary[, cycle28_day := difftime(record_date, mens_cycle_start, unit="day") %>% as.numeric()+1]
      
      diary_clean_dt <- rbind(diary_clean_dt, diary)
    }
  }
}


for (i in 1:nrow(file_dt)) {
  print(i)
  file <- file_dt$filename[i]
  print(paste(i,":", file))
  id <- file_dt[filename == file]$record_id
  cycle_num <- file_dt[filename == file]$cycle
  if ("daily_dt.rda" %in% list.files(paste0(config$actigraphy_output_dir, file))) {
    stopifnot(unique(nightsummary[filename == paste0(file, ".bin.RData")]$record_id) == id)
    
    sub_nightsummary <- nightsummary[filename == paste0(file, ".bin.RData")]
    sub_daysum <- daysum[filename == paste0(file, ".bin.RData")]
 

    load(paste0(config$actigraphy_output_dir, file, "/daily_dt.rda"))
    setnames(daily_dt, "time_date", "record_date")
    diary <- diary_clean_dt[record_id == id & cycle == cycle_num]
    
    # day summary: 
    stopifnot(unique(daysum[filename == paste0(file, ".bin.RData")]$record_id) == id)
    cols2exclude <- daysum[, .(filename, weekday, window_number, start_end_window,
                                    sleeponset_ts, wakeup_ts, night_number, cleaningcode,
                                    guider, sleeplog_used, acc_available,
                               boutcriter.in,boutcriter.lig, boutcriter.mvpa,
                               boutdur.in, boutdur.lig, boutdur.mvpa,
                              # GGIRversion,  # version only included for 3.1.0+
                               daytype, n_dup, M5TIME, L5TIME,
                               dur_day_spt_min, tail_expansion_minutes)] %>% colnames()
    activity_dt <- daysum[filename == paste0(file, ".bin.RData")][
      , !(cols2exclude), with=F]

    colnames(activity_dt)[3:ncol(activity_dt)] <- 
      colnames(activity_dt)[3:ncol(activity_dt)] %>% sapply(function(x){paste0(x, "_GGIR")})
    
    # diurnal rhythms:
    
    daily_rhythms_dt <- fread(paste0(config$processed_output_dir, "daily_rhythms.csv"))
    daily_rhythms_dt <- daily_rhythms_dt[record_id == id & cycle == cycle_num]
    daily_rhythms_dt <- daily_rhythms_dt[, !grepl("alldays|day", colnames(daily_rhythms_dt)), with=F]
    daily_rhythms_dt[, `:=`(record_date = as.Date(time_date),
                            record_id = as.character(record_id),
                            time_date=NULL)]

    
    combined_dt <- daily_dt %>% merge(diary, by=c("record_date"), all.x=T, all.y=T) %>%
      merge(activity_dt, by=c('record_date', 'record_id'), all.x=T) %>%
      merge(daily_rhythms_dt[, !c('cycle')], by=c('record_date', 'record_id'), all.x=T) %>%
      merge(sub_nightsummary[, mget(c("record_date", sleep_cols_nightsum_GGIR))],
            by=c('record_date'),all.x=T,all.y=T)

    combined_dt[, record_id := id]
    combined_dt[, cycle := cycle_num]
    save(combined_dt, 
              file=paste0(config$actigraphy_output_dir, "/", file, "/combined_dt.rda"))
      
    }
}



# save AllMetrics table

allmetrics_dt <- data.table()
pat_ls <- diary_dt$record_id %>% unique()
for (id in pat_ls) {
  for (cycle_num in 1:3) {
    print(id)
    print(cycle_num)
    diary <- diary_clean_dt[record_id == id & cycle == cycle_num]
    if (nrow(file_dt[record_id == id & cycle==cycle_num]) > 0) {
      for (file in file_dt[record_id == id & cycle==cycle_num]$filename) {
        if ("combined_dt.rda" %in% list.files(paste0(config$actigraphy_output_dir, file))) {
          load(paste0(config$actigraphy_output_dir, file, "/combined_dt.rda"))
          combined_dt[, record_id := id]
          combined_dt[, `:=`(day_midnight=NULL, day_noon=NULL)]
          allmetrics_dt <- allmetrics_dt %>% rbind(combined_dt, fill=T)
        } else {
          print('Combined_dt not generated')
        }
      }
    } else if (nrow(diary) > 0) {
      allmetrics_dt <- allmetrics_dt %>% rbind(diary, fill=T)
    }
  }
}

# Fix own extraction of L5 time so if someones L5 at 1:30pm, this would be plus 24 hrs
allmetrics_dt[, L5_time := ifelse(L5_time < 12, L5_time + 24, L5_time)]


allmetrics_dt[, sleep_onset_hrs := difftime(sleep_onset, record_date-1, units='hour') %>%
                as.numeric()]
allmetrics_dt[, sleep_offset_hrs := difftime(sleep_offset, record_date-1, units='hour') %>%
                as.numeric()]

allmetrics_dt[, lowvar_onset_hrs := difftime(lowvar_onset, record_date-1, units = "hour") %>% 
                as.numeric()]
allmetrics_dt[, lowvar_offset_hrs := difftime(lowvar_offset, record_date-1, units = "hour") %>% 
                as.numeric()]


allmetrics_dt[, `:=`(lowvar_onset =NULL, lowvar_offset = NULL,
                     sleep_onset = NULL, sleep_offset = NULL)]


# remove days after watch taken off:
watch_dt <- get_watch_info(alldata)

allmetrics_dt <- allmetrics_dt %>% merge(watch_dt,
                                         by=c("record_id", "cycle"), all.x=T, all.y=T)

# remove cycles with no watch data back
file_dt[, unq_record := paste0(record_id, '_',cycle)]
allmetrics_dt[, unq_record := paste0(record_id, '_',cycle)]
allmetrics_dt[, has_file := unq_record %in% file_dt$unq_record]
allmetrics_dt[has_file == F, .(record_id,record_date,cycle)] 
allmetrics_dt[has_file == F, .(record_id,cycle)]  %>% unique()
#allmetrics_dt <- allmetrics_dt[has_file == T]

# check  days after watch was taken off - some will be missing and removed by when 'day-of-cycle missing'
allmetrics_dt[record_date > watch_calc_end_date+1][, .(record_id, record_date, day_of_cycle, perc_wear,
                                                       pain_avg, cycle)]

allmetrics_dt[is.na(day_of_cycle) & record_date <= watch_calc_end_date+1] %>% dim()
allmetrics_dt[is.na(day_of_cycle) & record_date <= watch_calc_end_date+1, 
              .(record_id, record_date, day_of_cycle, perc_wear,pain_avg, cycle)] 

allmetrics_dt <- allmetrics_dt[!is.na(day_of_cycle)]
# remaining records out of official cycle but leaving in as still valid
allmetrics_dt[record_date > watch_calc_end_date+1][, .(record_id, record_date, day_of_cycle, perc_wear,
                                                       pain_avg, cycle)]
#allmetrics_dt <- allmetrics_dt[record_date <= watch_calc_end_date]

# same for days before they started wearing the watch
stopifnot(nrow(allmetrics_dt[record_date < watch_start_date]) == 0)
#allmetrics_dt <- allmetrics_dt[record_date >= watch_start_date]

print(dim(allmetrics_dt))

# Since a few participants have 2 watches in the cycle, need to remove
# the duplicate days
allmetrics_dt[, n_dup := .N, by=.(record_id,cycle, record_date)]
allmetrics_dt$n_dup %>% table()
allmetrics_dt[n_dup > 1, sum(is.na(perc_wear)) > 0, by=.(record_id,cycle,record_date)]
allmetrics_dt <- allmetrics_dt[n_dup == 1 | (n_dup > 1 & !is.na(perc_wear))]
allmetrics_dt[, n_dup := .N, by=.(record_id,cycle, record_date)]
stopifnot(unique(allmetrics_dt$n_dup) == 1) 
allmetrics_dt[, n_dup := NULL]



# ADD OTHER FEATURES

allmetrics_dt[, is_weekend := (weekdays(record_date, abbreviate = T) %in%
                                 c("Sat", "Sun"))]
allmetrics_dt[, is_dayoff := (is_weekend | holiday==1)]



# Merge in baseline data
base_dt <- fread(paste0(config$processed_output_dir, "baseline_dt.csv"),
                 colClasses=c('record_id'='character'))
colnames(base_dt)[2:ncol(base_dt)] <-paste0(colnames(base_dt)[2:ncol(base_dt)], '_base')
base_dt %>% colnames()

cycle_dt <- extract_crf(alldata)

allmetrics_dt <- merge(allmetrics_dt, 
                       cycle_dt[, .(record_id, cycle,
                                    yn_reg_pain_drugs_taken, yn_flr_pain_drugs_taken,
                                    date_overnight_addmission)],
                       all.x=T, all.y=F, by=c("record_id","cycle")) 

allmetrics_dt <- merge(allmetrics_dt, base_dt, by=c("record_id"), all.x=T, all.y=F)


cols2drop <- c('unq_record','has_file','crf_v1_consent_date','watch_location',
               'cos_date','watch_calc_end_date','watch_start_date', 'night',
               'watch_end_date','cycle_duration','date_overnight_addmission')
allmetrics_dt <- allmetrics_dt[, !cols2drop, with=F]


# make sure no duplicate dates - first remove duplicates from daylight savings:
allmetrics_dt[, n_dup := .N, by=.(record_id, record_date)]
print(paste('Number of duplicate rows:', nrow(allmetrics_dt[n_dup > 1])))
allmetrics_dt <- allmetrics_dt[!(n_dup >1)]
stopifnot(nrow(allmetrics_dt[n_dup > 1]) == 0)
allmetrics_dt[, n_dup := NULL]

cols2exclude <- daysum[, .(record_id,record_date,
                           filename, weekday, window_number, start_end_window,
                           sleeponset_ts, wakeup_ts, night_number, cleaningcode,
                           guider, sleeplog_used, acc_available,
                           boutcriter.in,boutcriter.lig, boutcriter.mvpa,
                           boutdur.in, boutdur.lig, boutdur.mvpa,
                           #GGIRversion,
                           daytype, n_dup,
                           dur_day_spt_min, tail_expansion_minutes,
                           L5TIME, M5TIME)] %>% colnames()
ggir_act_cols <- colnames(daysum)[!colnames(daysum) %in% cols2exclude] %>%
  sapply(function(x){paste0(x, "_GGIR")})
#ggir_act_cols <- ggir_act_cols[!ggir_act_cols %in% config$nonwear_cols]
#sleep_cols_nightsum_GGIR <- sleep_cols_nightsum_GGIR[!sleep_cols_nightsum_GGIR %in% config$nonwear_cols]

ggir_sleep_cols <- c(ggir_act_cols[grepl("spt|wakeup|sleep|onset", ggir_act_cols)],
                     sleep_cols_nightsum_GGIR)
ggir_day_cols <- ggir_act_cols[!ggir_act_cols %in% ggir_sleep_cols]


self_act_cols <- config$actigraphy_cols_self
self_diurnal_cols <- config$actigraphy_cols_diurnal
self_cols <- c(self_act_cols, self_diurnal_cols)
stopifnot(length(self_cols[!self_cols %in% colnames(allmetrics_dt)]) == 0)
self_cols <- self_cols[self_cols %in% colnames(allmetrics_dt)]
self_sleep_cols <- self_cols[grepl('SPT|spt|sleep|night|temp.mean|temp.sd|WASO|L5|lowvar|NA_', self_cols)]
self_day_cols <- self_cols[!self_cols %in% self_sleep_cols]
day_cols <- c(self_day_cols, ggir_day_cols)
sleep_cols <- c(self_sleep_cols, ggir_sleep_cols)
act_cols <- c(day_cols, sleep_cols)
act_cols <- act_cols %>% unique()

allmetrics_dt[, n_dup := .N, by=.(record_id, record_date)]
error_pats <- allmetrics_dt[n_dup > 1]$record_id %>% unique()
stopifnot(length(error_pats) == 0)
allmetrics_dt[, n_dup := NULL]

acc_GGIR_cols <- allmetrics_dt[, grepl('ACC', colnames(allmetrics_dt)), with=F] %>% colnames()
allmetrics_dt[, (acc_GGIR_cols) := lapply(.SD, function(x){nafill(x, fill=0)}), .SDcols=acc_GGIR_cols]


# don't want to remove any columns, just set to NA as we want to shift variables
allmetrics_dt[perc_wear < (.75)| is.na(perc_wear), (day_cols) := NA]
# allmetrics_dt[, perc_wear_noon_plus1 := shift(perc_wear_noon, -1), by=.(record_id,cycle)]
allmetrics_dt[perc_wear_noon < (.75) | is.na(perc_wear_noon), (ggir_sleep_cols) := NA]
allmetrics_dt[perc_wear_noon < (.75) | is.na(perc_wear_noon), (self_sleep_cols) := NA]
allmetrics_dt[SriFractionValid_GGIR < .8, SleepRegularityIndex_GGIR := NA]


write.csv(allmetrics_dt, 
          paste0(config$processed_output_dir, "allmetrics_dt.csv"),
          row.names=F)

#### Variable names ####

stopifnot(length(self_diurnal_cols[!self_diurnal_cols %in% colnames(allmetrics_dt)])==0)
stopifnot(length(self_cols[!self_cols %in% colnames(allmetrics_dt)])==0)

colnames(allmetrics_dt)[
  !colnames(allmetrics_dt) %in% c(self_act_cols, self_diurnal_cols) &
    !grepl("base|fatigue|GGIR|pain_med|pain|bfi|taken", 
           colnames(allmetrics_dt))]

ggir_cols <- colnames(allmetrics_dt)[grepl('GGIR', colnames(allmetrics_dt))]
ggir_cols <- ggir_cols[!ggir_cols %in% config$nonwear_cols]
stopifnot(length(ggir_cols[!ggir_cols %in% c(ggir_sleep_cols, ggir_day_cols)]) == 0)
ggir_day_sleep_cols <- c(ggir_sleep_cols, ggir_day_cols)
stopifnot(length(ggir_day_sleep_cols[!ggir_day_sleep_cols %in% 
                                       c(ggir_cols,config$nonwear_cols)]) == 0)

temp_cols <- config$temp_cols
stopifnot(length(temp_cols[!temp_cols %in% self_cols]) == 0)

cols_dt <- data.table('Variable' = c(ggir_cols, self_cols))
cols_dt[Variable %in% c(ggir_cols), Source := "GGIR"]
cols_dt[Variable %in% c(ggir_sleep_cols), Type := 'Sleep']
cols_dt[Variable %in% c(ggir_day_cols), Type := 'Physical activity/Diurnal Rhythms']

cols_dt[Variable %in% c(self_act_cols), Source := "Tsanas et al. (2020)"]
cols_dt[Variable %in% c(self_sleep_cols), Type := 'Sleep']
cols_dt[Variable %in% c(self_day_cols), Type := 'Physical activity/Diurnal Rhythms']
cols_dt[grepl('lowvar', Variable), Source := 'Original']

cols_dt[Variable %in% c(temp_cols), `:=` (Source = "Edgley et al. (2023)")]
cols_dt[Variable %in% c(self_diurnal_cols), `:=` 
        (Source="Carr et al. (2018)", Type = 'Physical activity/Diurnal Rhythms')]


write.csv(cols_dt,paste0(config$processed_output_dir, "variable_list.csv"),
          row.names = F)


######### SHIFT variables ########

allmetrics_dt %>% dim()

symptom_cols <- c("pain_total","bfi_total", "fatigue_24hr")
prev_cols <- c(symptom_cols, act_cols)
prev_cols <- prev_cols[!prev_cols %in% config$nonwear_cols]
cols_prev5days <- paste0(prev_cols, "_prev5days")
allmetrics_dt[, (cols_prev5days) := lapply(.SD, function(x){prev_n_mean(x, 5, shifted=T)}),
   .SDcols=prev_cols, by=.(record_id, cycle)]


allmetrics_dt[, (prev_cols) := lapply(.SD, as.numeric), .SDcols=prev_cols]
for (i in 1:5) {
  shifted_colnames <- paste0(prev_cols, "_minus", i)
  allmetrics_dt[, (shifted_colnames) := lapply(.SD, function(x){shift(x, i, fill=NA)}),
     .SDcols = prev_cols, by=c("record_id","cycle")]
}


# SHIFT records with missing first cycle to start at first available cycle -
allmetrics_dt[, first_cycle_adjust := min(cycle)-1, by=record_id]
allmetrics_dt[, cycle := cycle - first_cycle_adjust]

write.csv(allmetrics_dt, paste0(config$processed_output_dir, "allmetrics_with_shifted.csv"),
          row.names=F)




