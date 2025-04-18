
# LOAD IN CONFIG
config <- read.config(file="./processing_config.yaml")


# process CRF and include hormone changes in daily diary
sapply(paste0("./Processing/Functions/", list.files("./Processing/Functions")),
       source)

alldata <- fread(paste0(config$source_data_dir, "alldata_processed.csv"),
                 colClasses=c('record_id'='character'))

hormone_cols <- config$hormone_cols
surg_cols <- config$gyne_surg_cols


desf_dt <- alldata[!is.na(desf_date), mget(c('record_id', 'desf_date', 'desf_operation_date', 
                                             config$deep_disease_cols))]
desf_dt[, deep_diag := apply(.SD, FUN=sum, MARGIN=1) > 0, .SDcols = config$deep_disease_cols]
desf_dt <- desf_dt %>% 
  merge(alldata[crf_v3_surg_other_detials != "", .(record_id, crf_v3_surg_other_detials)], 
        by='record_id', all.x=T, all.y=F)
write.csv(desf_dt, file=paste0(config$processed_output_dir, "/deep_dt.csv"), row.names=FALSE)
deep_study_pats <- desf_dt[deep_diag == T]$record_id %>% unique()



#' Function to take in the individual patient diary and update the exact days that 
#' any medication changed
update_diary <- function(pat_diary, id, cycle_num) {
  sub_crf <- crf[record_id == id & cycle == cycle_num]
  
  if (cycle_num==1) {
    changed_horms <- which(base_dt[record_id == id, mget(hormone_cols)] != 
                             crf[record_id == id & cycle==1, mget(hormone_cols)])
  } else {
    changed_horms <- which(crf[record_id == id & cycle==(cycle_num-1), mget(hormone_cols)] !=  
                             crf[record_id == id & cycle==cycle_num, mget(hormone_cols)])
  }
  
  changed_horms2 <- which(!is.na(crf[record_id == id & cycle==cycle_num,
                                     mget(paste0('horm_date_end_', 1:11))]))
  
  changed_horms <- c(changed_horms, changed_horms2)
  
  if (length(changed_horms) == 0) {
    pat_diary[cycle==cycle_num, (hormone_cols) := crf[record_id == id & 
                                                     cycle==cycle_num, mget(hormone_cols)]]
  } else {
    for (num in changed_horms) {
      print('changed hormones')
      date_dt <- crf[cycle == cycle_num & record_id == id,
                     mget(c(paste0("horm_date_start_", num), 
                            paste0("horm_date_end_", num)))]
      date_dt <- date_dt %>% melt(measure.vars = c(paste0("horm_date_start_", num), 
                                                   paste0("horm_date_end_", num))) %>% as.data.table()
      if (nrow(date_dt[!is.na(value)]) == 0) {
        # no dates given - assume beginning of cycle
        pat_diary[cycle==cycle_num, (hormone_cols[num]) := sub_crf[[hormone_cols[num]]]]
      }
      if (nrow(date_dt[!is.na(value)]) > 0) {
        change_date <- date_dt[!is.na(value)]$value[[1]]
        
        pat_diary[cycle == cycle_num & record_date >= change_date, 
                  (hormone_cols[num]) := sub_crf[[hormone_cols[num]]]]
        pat_diary[cycle==cycle_num, changed_horm := TRUE]
      }
      
      # case where both start and stop date are filled out
      if (nrow(date_dt[!is.na(value)]) == 2) {
        pat_diary[cycle==cycle_num & record_date >= date_dt$value[2],
                  (hormone_cols[num]) := 0]
      }
    }
  }
  
  # surgical dates:
  date_cols <- config$gyne_surg_cols$date
  had_surg <- (sub_crf[, mget(date_cols)] %>% 
                 apply(FUN=function(x){sum(!is.na(x)) > 0}, MARGIN=1)) |
    (nrow(desf_dt[record_id == id]) > 0 & cycle_num == 2)
  if (had_surg) {
    num_nights <- regmatches(sub_crf$no_of_nights,
                            gregexpr("\\b\\d{1,2}\\b", sub_crf$no_of_nights))
    num_nights <- as.numeric(num_nights[[1]][1]) # take first number in case there are other numbers in notes
    num_nights <- ifelse(is.na(num_nights), 0, num_nights) # if missing but had surgery set to 0
    deep_op_date <- desf_dt[record_id == id]$desf_operation_date
    deep_op_date <- ifelse(length(deep_op_date) == 0, as.Date(NA), deep_op_date)
    op_date <- if (is.na(deep_op_date)) as.IDate(sub_crf[, mget(date_cols)] %>% 
      apply(FUN=function(x){unique(x[!is.na(x)])}, MARGIN=1)) else deep_op_date
    op_date <- ifelse(length(op_date) > 1 & !is.na(deep_op_date), deep_op_date, op_date)
    if (length(op_date) > 1) {
      print('more than one operation')
    }
      # ifelse(is.na(sub_crf$date_overnight_addmission),
      #                 )
    pat_diary[, surg_date := op_date[1]]
    pat_diary[record_date >= sub_crf$op_date & 
                record_date <= (sub_crf$op_date + num_nights),
              in_hospital := TRUE]
    pat_diary[cycle==cycle_num, days_from_admit := difftime(record_date, 
                                            as.IDate(op_date) + num_nights, 
                                            units='day') %>% as.numeric()]
    pat_diary[in_hospital==TRUE, surg_or_recovery := TRUE]
    pat_diary[days_from_admit >=0 & days_from_admit <=7, surg_or_recovery := TRUE]

    had_gyn_surg <- if (!is.na(deep_op_date)) TRUE else sub_crf[, rowSums(.SD, na.rm=T)>0, 
              .SDcols=config$gyne_surg_cols$bool[config$gyne_surg_cols$bool != 'yn_surg_other']]
    pat_diary[cycle==cycle_num, post_surg_type := ifelse(had_gyn_surg, 'gyn', 'other')]
    
  }
  
  if(id %in% deep_study_pats & cycle_num == 2) {
    print('deep')
    pat_diary[cycle==2, post_deep_surgery := TRUE]
    pat_diary[cycle==2, post_surg_type := 'gyn']
  }
  return (pat_diary)
}


# Function to clean up dates
clean_dates <- function(dates) {
  # Convert dates to the Date format
  dates <- as.Date(dates, format = c("%d %b %y", "%d/%m/%y", "%d %b %Y", "%d/%m/%Y"))
  # Filter out NA and dates outside the desired range
  dates <- dates[!is.na(dates) & dates >= as.Date("1900-01-01") & dates <= Sys.Date()]
  return(dates)
}

crf <- extract_crf(alldata)
base_dt <- extract_baseline(alldata)
diary_dt <- extract_diaries(alldata,
                            include_missing_dates = TRUE, rmv_duplicates = TRUE)

med_diary <- copy(diary_dt)
med_diary[, (hormone_cols) := 0]
med_diary[, surg_date := as.Date(NA)]
med_diary[, surg_or_recovery := FALSE]
med_diary[, in_hospital := FALSE]
med_diary[, days_from_admit := as.numeric(NA)]
med_diary[, post_deep_surgery := FALSE]
med_diary[, post_surg_type := as.character(NA)]
med_diary[, changed_horm := FALSE]




final_diaries <- data.table()
for (id in base_dt$record_id) {
  print(id)
  pat_diary <- med_diary[record_id == id]
  
  for (cycle_num in 1:3) {
    print(cycle_num)
    if (nrow(pat_diary[cycle==cycle_num]) > 0) {
      if (cycle_num == 1) {
        pat_diary[cycle == 1, (hormone_cols) := base_dt[record_id == id, mget(hormone_cols)]]
      } else {
        pat_diary[cycle == cycle_num, (hormone_cols) := crf[record_id == id & 
                                                           cycle==(cycle_num-1), mget(hormone_cols)]]
      }
      
      if (nrow(crf[record_id == id & cycle==cycle_num]) > 0) {
        pat_diary <- update_diary(pat_diary, id, cycle_num)
      }
    }
  }
  final_diaries <- rbind(final_diaries, pat_diary)
}


final_diaries[, hormones_taken := apply(.SD, FUN= function(x){sum(x)> 0}, MARGIN=1), .SDcols = hormone_cols]

final_diaries[, deep_study_pat := record_id %in% deep_study_pats]


final_diaries[post_surg_type == 'gyn', gyn_surg_date := surg_date]
final_diaries[, gyn_surg_date := unique(gyn_surg_date[!is.na(gyn_surg_date)]), by=record_id]
final_diaries[, had_surg := sum(post_surg_type == 'gyn', na.rm=T) > 0, by=record_id]
final_diaries[post_surg_type == 'gyn', surg_cycle := cycle]
final_diaries[had_surg == T, surg_cycle := first(surg_cycle[!is.na(surg_cycle)]), by=record_id]

# variable for duration since surgical treatment
final_diaries <- final_diaries %>% merge(base_dt[, .(record_id,last_surg_treat)], by='record_id')
final_diaries[, time_since_surg := difftime(record_date, gyn_surg_date, units='days') %>% as.numeric()]
final_diaries[time_since_surg < 0, time_since_surg := NA]
final_diaries[is.na(time_since_surg)|time_since_surg < 0, time_since_surg := difftime(record_date,
                                                                                      as.Date(paste(last_surg_treat, "-01-01", sep = ""),
                                                                                              format = "%Y-%m-%d"), units='days') %>% as.numeric()]
final_diaries[, last_surg_treat := NULL]

final_diaries[, month_num := month(record_date)]
final_diaries[, season := 'winter']
final_diaries[(month_num >=9 & month_num <= 11), season := 'autumn']
final_diaries[(month_num >=6 & month_num <= 8), season := 'summer']
final_diaries[(month_num >=3 & month_num <= 5), season := 'spring']

write.csv(final_diaries, file=paste0(config$processed_output_dir,"diary_dt.csv"), row.names = F)
write.csv(base_dt, file=paste0(config$processed_output_dir,"baseline_dt.csv"), row.names = F)
