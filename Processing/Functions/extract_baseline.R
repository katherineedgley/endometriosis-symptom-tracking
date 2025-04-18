
extract_baseline <- function(alldata) {
  
  baseline_cols <- config$baseline_col_mapping %>% names()
  
  base_dt <- alldata[redcap_event_name == "visit_1_arm_1", ..baseline_cols]
  # rename columns from config file
  base_dt <- base_dt %>% plyr::rename(replace=unlist(config$baseline_col_mapping))

  base_dt[, record_id := as.character(record_id)]
  
  stopifnot(nrow(base_dt[is.na(spe_diag)]) == 0)
  stopifnot(nrow(base_dt[is.na(ovarian_diag)]) == 0)
  
  
  opiate_drugs_reg <- paste0("yn_reg_pain_drug_", 2:11)
  opiate_drugs_flr <- paste0("yn_flr_pain_drug_", 2:11)
  base_dt[, opiates_taken_reg := apply(.SD, FUN=function(x){sum(x,na.rm=T)>0}, MARGIN=1),
          .SDcols=opiate_drugs_reg]
  base_dt[grepl('morph', other_pain_drug_reg), opiates_taken_reg := T]
  base_dt[, opiates_taken_flr := apply(.SD, FUN=function(x){sum(x,na.rm=T)>0}, MARGIN=1),
          .SDcols=opiate_drugs_flr]
  base_dt[grepl('morph', other_pain_drug_flr), opiates_taken_flr := T]
  
  base_dt$yn_reg_pain_drug_26
  alldata$crf_v1_reg_pain_drug_26 %>% table()
  alldata$crf_v1_flr_pain_drug_26 %>% table()
  
  
  # treatment years SPE:
  # add diag year to list of surgeries in case not already added
  base_dt[, spe_surg_yr_withdiag := paste0(spe_surg_yr, ' ', spe_diag_yr)]
  base_dt[, spe_surg_year := str_extract_all(spe_surg_yr_withdiag,"[[:digit:]]{4}")]

  # number of surgeries, though does not include where 2 in same year
  base_dt[, spe_num_surg := unlist(lapply(spe_surg_year, uniqueN))]
  base_dt[, spe_yr_last_surg := unlist(lapply(spe_surg_year,
                                           function(x) {ifelse(length(x)==0, NA, max(as.numeric(x)))}))]
  base_dt[, spe_yr_first_surg := unlist(lapply(spe_surg_year,
                                               function(x) {ifelse(length(x)==0, NA, min(as.numeric(x)))}))]
  base_dt[spe_last_removed == 1, spe_yr_last_treat := spe_yr_last_surg]
  # Make conservative assumption that last treatment was first surgery
  base_dt[spe_last_removed==0 & spe_prev_removal==1, 
          spe_yr_last_treat := spe_yr_first_surg]
  base_dt[, spe_treated_anytime := spe_prev_removal == 1 | spe_last_removed == 1]
  
  # treatment years deep endometriosis:
  base_dt[, die_surg_yr_withdiag := paste0(die_surg_yr, ' ', die_diag_yr)]
  base_dt[, die_surg_year := str_extract_all(die_surg_yr_withdiag,"[[:digit:]]{4}")]
  base_dt[, die_num_surg := unlist(lapply(die_surg_year, uniqueN))]
  base_dt[, die_yr_last_surg := unlist(lapply(die_surg_year,
                                           function(x) {ifelse(length(x)==0, NA, max(as.numeric(x)))}))]
  base_dt[, die_yr_first_surg := unlist(lapply(die_surg_year,
                                              function(x) {ifelse(length(x)==0, NA, min(as.numeric(x)))}))]
  base_dt[, die_treated_anytime := die_prev_treat ==1]

  # treatment years ovarian endometriosis:
  base_dt[, ovarian_surg_yr_withdiag := paste0(ovarian_surg_yr, ' ', ovarian_diag_yr)]
  base_dt[, ovarian_surg_year := str_extract_all(ovarian_surg_yr_withdiag,"[[:digit:]]{4}")]
  base_dt[, ovarian_num_surg := unlist(lapply(ovarian_surg_year, uniqueN))]
  base_dt[, ovarian_yr_last_surg := unlist(lapply(ovarian_surg_year,
                                           function(x) {ifelse(length(x)==0, NA, max(as.numeric(x)))}))]
  base_dt[, ovarian_yr_first_surg := unlist(lapply(ovarian_surg_year,
                                                  function(x) {ifelse(length(x)==0, NA, min(as.numeric(x)))}))]
  base_dt[, ovarian_treated_anytime := ovarian_prev_treat ==1]
  
  
  base_dt[, `:=`(ovarian_surg_year = NULL, die_surg_year = NULL, spe_surg_year = NULL)]
  

  base_dt[, last_surg_overall := apply(.SD, MARGIN=1,
                                       function(x){ifelse(sum(!is.na(x)) ==0, NA,
                                                          max(x, na.rm=T))}),
          .SDcols = c("ovarian_yr_last_surg","die_yr_last_surg","spe_yr_last_surg")]
  base_dt[!is.na(last_surg_overall), last_surg_timediff := time_length(difftime(visit_date, 
                                                      as.Date(paste0(last_surg_overall, '-01-01'))),
                                             'years')]
  # take SPE last treatment year if available, otherwise take deep or ovarian
  base_dt[, last_surg_treat := spe_yr_last_treat]
  base_dt[is.na(last_surg_treat) & die_prev_treat == 1, last_surg_treat := die_yr_last_surg]
  base_dt[is.na(last_surg_treat) & ovarian_prev_treat == 1, last_surg_treat := ovarian_yr_last_surg]
  base_dt[!(ovarian_treated_anytime | die_treated_anytime | spe_treated_anytime), last_surg_treat := NA]

  # Set the following so later year is used when deep or ovarian treatment likely 
  # completed after
  base_dt[!is.na(spe_yr_last_treat) & die_prev_treat == 1 & die_yr_last_surg > spe_yr_last_treat, last_surg_treat := die_yr_last_surg]
  base_dt[!is.na(spe_yr_last_treat) & ovarian_prev_treat == 1 & ovarian_yr_last_surg > spe_yr_last_treat, last_surg_treat := ovarian_yr_last_surg]
  
  base_dt[, .(record_id, last_surg_overall, last_surg_treat,spe_yr_last_treat, die_prev_treat, ovarian_prev_treat, die_surg_yr_withdiag, ovarian_surg_yr_withdiag)]
  
  base_dt[!is.na(last_surg_treat), last_surg_treat_timediff := time_length(difftime(visit_date, 
                                                                                as.Date(paste0(last_surg_treat, '-01-01'))),
                                                                       'years')]
  
  
  # add extra cols for summarizing previous treatment and active endometriosis
  base_dt[, prev_treat_general := spe_treated_anytime | die_treated_anytime | ovarian_treated_anytime]

  base_dt[, endo_active := FALSE]
  base_dt[spe_last_removed == 0, endo_active := TRUE]
  base_dt[die_untreated == 1, endo_active := TRUE]
  base_dt[ovarian_untreated == 1, endo_active := TRUE]
  
  
  stopifnot(all(base_dt[ovarian_untreated == 1]$ovarian_diag == 1))
  stopifnot(all(base_dt[die_untreated == 1]$die_diag == 1))
  stopifnot(all(base_dt[spe_last_removed == 1]$spe_diag == 1))

  
  treatment_cols <- c("die_diag","die_diag_img","die_untreated","die_prev_treat",
                      "spe_diag","spe_prev_removal","spe_last_removed",
                      "ovarian_diag","ovarian_diag_img","ovarian_prev_treat",
                      "ovarian_untreated")

  stopifnot(nrow(base_dt[spe_diag == 0 & die_diag == 0 & ovarian_diag == 0]) == 0)
        
  
  # other useful features
  base_dt[, spe_only := 0]
  base_dt[spe_diag == 1 & die_diag == 0 & ovarian_diag == 0, spe_only := 1]
  base_dt[, parity_zero := ifelse(parity==0, 1, 0)]
                                                               
                                                               
  # ethnicity
  ethnic_levels <- c("asian","black","mixed","white","other")
  base_dt[, ethnicity := factor(ethnicity, levels=1:5)]
  levels(base_dt$ethnicity) <- ethnic_levels

  #education
  education_levels <- c("primary","secondary","tertiary")
  base_dt[, education := factor(education, levels=1:3)]
  levels(base_dt$education) <- education_levels
  
  gyne_cols <- c("heavy_bleeding","adenomyosis","fibroids","PID")
  base_dt[other_gyn_history == 0, (gyne_cols) := 0]
  base_dt[, other_gyn_history := NULL] # other gyn history question gives dropdown to options so not needed
  
  factor_cols <- c("heavy_bleeding",
                   "adenomyosis","fibroids","PID","hysterectomy","oopherectomy",
                   "pelvic_pain_months","pain_flares","period_flares","other_pain_flares",
                   "hormones_taken", 
                   "coc_taken", "IUD_taken", "patch_taken", "nexpl_taken",
                   "depo_taken", "ring_taken", "pop_taken", "prog_taken",
                   "gnrh_taken","hrt_taken", "other_contra_taken", 
                   "sleep_meds","sleep_disorder", "waitlist", 
                   "reg_painkillers_taken",
                   "add_painkillers_taken",
                   "smoker","gender",
                   "night_shift")

  comorbid_cols <- c("fibromyalgia","rheu_arthe","ibs","adpkd","migraine","othr_arthritis",
                     "cfs","pancreatitis","painful_bladder","other_chronic_pain")
  base_dt[, (comorbid_cols):= lapply(.SD, function(x){ifelse(is.na(x), NA, as.numeric(x ==1))}),
          .SDcols = comorbid_cols]

  
  cols2drop <- c(paste0('yn_reg_pain_drug_', 1:27),
                 paste0('yn_flr_pain_drug_', 1:27),
                 'other_pain_drug_reg','other_pain_drug_flr')
  base_dt <- base_dt[, !cols2drop, with=F]
  
  
  dropped_pats <- alldata[crf_cos_yn_status_change == 1]$record_id
  base_dt[, dropped_pat := record_id %in% dropped_pats]
  
  return(base_dt)
}


