#### extract smartwatch cycle-specific info - collected at end of each cycle ####

extract_cycle_cols <- function(alldata, cycle_num) {
  base_hormone_cols <- paste0("crf_v1_hormones_taken___", 1:11)
  base_drug_cols <- paste0("crf_v1_yn_reg_pain_drug_", 1:26)
  base_meds <- 
    alldata[redcap_event_name == "visit_1_arm_1" & redcap_repeat_instrument == "",
            mget(c("record_id", "crf_v1_yn_hormones_taken",base_hormone_cols,
                   "crf_v1_yn_reg_pain_drugs_taken", "crf_v1_yn_flr_pain_drugs_taken",
                   base_drug_cols))]
  
  base_hormone_cols <- paste0("base_hormones_taken_", 1:11)
  base_drug_cols <- paste0("base_reg_painkillers_taken_", 1:26)
  colnames(base_meds) <- c("record_id","base_hormones_taken",
                               base_hormone_cols,
                               "base_reg_pain_drugs_taken","base_flr_pain_drugs_taken",
                               base_drug_cols)
  
  
  cols <- 
    c(# surgeries in last 6 weeks ---
      "date_completed",
      "yn_night_shifts",
      unlist(config$gyne_surg_cols),
      # hormones in last 6 weeks ---
      "yn_hormones", paste0("change_horm_", 1:11),
      paste0("yn_horm_", 1:11),#config$hormone_cols, 
      paste0("horm_date_start_", 1:11),#paste0(config$hormone_cols, '_start_date'),
      paste0("horm_date_end_", 1:11),#paste0(config$hormone_cols, '_end_date'),
      "y_baseline_horms", # whether hormones are same as baseline 
      "mc_analgesia_change", # 1=same, 2=more, 3=less analgesic use compared to baseline
      "yn_reg_pain_drugs_taken", # any regular analgesia for pain flares
      "yn_flr_pain_drugs_taken", # any additional analgesia for pelvic pain flares
      paste0("yn_flr_pain_drug_", 1:26),
      paste0("yn_reg_pain_drug_", 1:26)
    )
  
  if (cycle_num == 1) {
    cols <- cols[cols != "y_baseline_horms"]
  }
  
  visit_num <- cycle_num + 1
  cols_full <- paste0("crf_v", visit_num, "_", cols)
  event_name <- paste0("visit_", visit_num, "_arm_1")
  complete_colname <- ifelse(cycle_num == 1, "visit_2_crf_end_of_first_cycle_complete",
                             ifelse(cycle_num == 2, "visit_3_crf_end_of_second_cycle_complete",
                                    "visit_4_crf_end_of_third_cycle_complete"))
  
  cycle_dt <- 
    alldata[redcap_event_name == event_name &
              redcap_repeat_instrument == "", mget(c("record_id",
                                                     complete_colname, cols_full))]
  colnames(cycle_dt) <- c("record_id", "cycle_complete", cols)
  
  if (cycle_num == 1) {
    cycle_dt[, y_baseline_horms := NA]
  }
  
  # change surg date cols to date class (in case all missing)
  date_cols <- cols[!grepl(paste(c(config$gyne_surg_cols$bool, config$gyne_surg_cols$str), 
                                 collapse='|'), cols)]
  date_cols <- date_cols[grepl(paste(config$gyne_surg_cols$date,
                                     collapse='|'), date_cols)] # exclude bool cols
  cycle_dt[, (date_cols) := lapply(.SD, as.IDate), .SDcols=date_cols]
    
  cycle_dt <- cycle_dt[!is.na(cycle_complete) & !is.na(date_completed)]  # remove info from cycles underway
  cycle_dt <- cycle_dt %>% merge(base_meds, by='record_id')
  
  hormone_cols <- paste0("yn_horm_", 1:11)
  
  # get total hormones taken for each person
  cycle_dt$horm_sum <- cycle_dt[, mget(hormone_cols)] %>% apply( MARGIN=1, FUN=sum)
  cycle_dt[, hormones_taken := 0]
  
  # make sure that if "same as baseline" is ticked we have the exact baseline
  # hormones for the cycle
  if (cycle_num != 1) {
    cycle_dt[horm_sum == 0 & y_baseline_horms==1, (hormone_cols) := mget(base_hormone_cols)]
    #cycle_dt[y_baseline_horms == 1, (hormone_cols) := mget(base_hormone_cols)]
    cycle_dt[is.na(yn_hormones) & (y_baseline_horms==1), hormones_taken := base_hormones_taken]
  }
  cycle_dt[, stopped_horm := rowSums(!is.na(cycle_dt[, paste0("horm_date_end_", 1:11)])) > 0]
  # when no hormones stopped but saying yes stopped or are taking hormones 
  cycle_dt[yn_hormones == 1 & horm_sum == 0 & stopped_horm == F,
           (hormone_cols) := mget(base_hormone_cols)]
  cycle_dt[yn_hormones == 1 & horm_sum == 0 & stopped_horm == F,
           hormones_taken := 1]
  

  cycle_dt[horm_sum > 0, hormones_taken := 1]
  dropcols <- c(paste0("change_horm_", 1:11), base_hormone_cols, "base_hormones_taken",
                "cycle_complete")
  cycle_dt <- cycle_dt[, !dropcols, with=F]
  date_cols <- c(paste0("horm_date_start_", 1:11),paste0("horm_date_end_", 1:11))
  cycle_dt[, (date_cols) := lapply(.SD, as.IDate), .SDcols = date_cols]
  
  cycle_dt[, cycle := cycle_num]

  # analgesia:
  cycle_dt$sum_reg_drugs <- cycle_dt[, mget(paste0("yn_reg_pain_drug_", 1:26))] %>% 
    apply(MARGIN=1, FUN=sum, na.rm=T)
  cycle_dt$base_sum_reg_drugs <- cycle_dt[, mget(base_drug_cols)] %>% 
    apply(MARGIN=1, FUN=sum, na.rm=T)
  
  cycle_dt$sum_flr_drugs <- cycle_dt[, mget(paste0("yn_flr_pain_drug_", 1:26))] %>% 
    apply(MARGIN=1, FUN=sum, na.rm=T)
  
  reg_painkiller_cols <- paste0("yn_reg_pain_drug_", 1:26)
  
  cycle_dt[is.na(yn_reg_pain_drugs_taken) & 
             mc_analgesia_change ==1,
           `:=`(yn_reg_pain_drugs_taken = base_reg_pain_drugs_taken,
                sum_reg_drugs = base_sum_reg_drugs)]
  cycle_dt[is.na(yn_reg_pain_drugs_taken) & 
             mc_analgesia_change ==1,(reg_painkiller_cols):=mget(base_drug_cols)]
  
  cycle_dt[yn_reg_pain_drugs_taken == 1 & sum_reg_drugs == 0][, .(record_id, cycle,mc_analgesia_change,
                                                                  base_sum_reg_drugs)]
  # if regular analgesia taken, same as baseline, but none recorded, fill in with baseline amounts
  cycle_dt[yn_reg_pain_drugs_taken == 1 & sum_reg_drugs == 0 & 
             mc_analgesia_change ==1, (reg_painkiller_cols) := mget(base_drug_cols)]
  # recompute analgesia sum:
  cycle_dt$sum_reg_drugs <- cycle_dt[, mget(paste0("yn_reg_pain_drug_", 1:26))] %>% 
    apply(MARGIN=1, FUN=sum, na.rm=T)
  cycle_dt[yn_reg_pain_drugs_taken == 1 & sum_reg_drugs == 0][, .(record_id, cycle,mc_analgesia_change)]
  
  # for those where analgesia change is NOT "same" but no regular drugs given, 
  # revert to baseline
  cycle_dt[sum_reg_drugs == 0 & yn_reg_pain_drugs_taken == 1, sum_reg_drugs := base_sum_reg_drugs]
  
  
  highpain_drugs <- c(paste0("yn_reg_pain_drug_", c(9,10,11)),
                      paste0("yn_flr_pain_drug_", c(9,10,11)))
  
  
  cycle_dt$sum_highpain_drugs <- cycle_dt[, mget(highpain_drugs)] %>% 
    apply(MARGIN=1, FUN=sum, na.rm=T)
  
  antid_drugs <- c(paste0("yn_reg_pain_drug_", c(18,19,20,21,22,23)),
                   paste0("yn_flr_pain_drug_", c(18,19,20,21,22,23)))
  
  cycle_dt$sum_antid_drugs <- cycle_dt[, mget(antid_drugs)] %>% 
    apply(MARGIN=1, FUN=sum, na.rm=T)
  
  
  cycle_dt[,  (config$hormone_cols) := mget(paste0("yn_horm_", 1:11))]
  
  cols2drop <- c(paste0("yn_flr_pain_drug_", 1:26),
                 paste0("yn_reg_pain_drug_", 1:26),
                 paste0("yn_horm_", 1:11))
  cycle_dt <- cycle_dt[, !cols2drop, with=F]
  
  
  return(cycle_dt)
}

extract_crf <- function(alldata) {
  
  cycle1_dt <- extract_cycle_cols(alldata, 1)
  cycle2_dt <- extract_cycle_cols(alldata, 2)
  cycle3_dt <- extract_cycle_cols(alldata, 3)
  
  cycle_dt <- rbind(cycle1_dt, cycle2_dt, cycle3_dt)
  
  return(cycle_dt)
}


