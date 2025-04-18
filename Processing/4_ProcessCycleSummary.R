### Process cycle data for cycle-level analysis


# GLOBAL VARIABLES TO DEFINE
min_pat_days <- 20 # drop participants with fewer than this num total days
min_pat_mn_days <- 10 # drop participants with fewer than this num total days

min_cycle_mn_days <- 10 # drop cycles with fewer than this num total days
min_cycle_days <- 20 # drop cycles with fewer than this num total days


alldata <- fread(paste0(config$source_data_dir, "alldata_processed.csv"),
                 colClasses=c("record_id"='character'))

diary_dt <- fread(paste0(config$processed_output_dir, "diary_dt.csv"),
                  colClasses = c('record_id'='character'))

file_dt <- fread(paste0(config$source_data_dir, "file_dt.csv"),
                 colClasses = c('record_id'='character',
                                'start_date'='IDate',
                                'end_date' = 'IDate'))

symptom_cols <- c("pain_avg", "pain_worst",'pain_total',
                  "bfi_total",  "fatigue_now", "fatigue_24hr",
                  "fatigue_worst","fatigue_activity","fatigue_walking",
                  "fatigue_enjoyment", "fatigue_work",
                  "fatigue_mood","fatigue_relations")
diary_dt[, (symptom_cols) := lapply(.SD, as.numeric), .SDcols=symptom_cols]

diary_dt[, unq_record := paste0(record_id, "_cycle", cycle)]

diary_dt[, total_diaries_record := sum(!is.na(pain_avg)), by=record_id]
diary_dt[, total_diaries_cycle := sum(!is.na(pain_avg)), by=.(record_id,cycle)]
diary_dt[, .(total_diaries_record, record_id)] %>% unique()

diary_dt[total_diaries_record >= min_pat_mn_days]$record_id %>% uniqueN()
diary_dt[total_diaries_record >= min_pat_days]$record_id %>% uniqueN()

diary_dt[, suff_diaries_pat_mn := total_diaries_record >= min_pat_mn_days]
diary_dt[, suff_diaries_pat := total_diaries_record >= min_pat_days]
diary_dt[, suff_diaries_cycle_mn := total_diaries_cycle >= min_cycle_mn_days]
diary_dt[, suff_diaries_cycle := total_diaries_cycle >= min_cycle_days]

diary_dt[suff_diaries_cycle_mn == T, .(record_id,cycle)] %>% uniqueN()
diary_dt[suff_diaries_cycle == T, .(record_id,cycle)] %>% uniqueN()


#### Symptoms cols ####


mean_upper <- function(x) {
  upper_lim <- quantile(x, .75, na.rm=T)
  return(x[x >= upper_lim] %>% mean(na.rm=T))
}
mean_lower <- function(x) {
  lower_lim <- quantile(x, .25, na.rm=T)
  return(x[x <= lower_lim] %>% mean(na.rm=T))
}

compute_skew <- function(x) {
  x <- x[!is.na(x)]
  return(sum((x - mean(x))^3)/((length(x)-1)*sd(x)^3))
}
# Define functions and their corresponding names
summary_functions_diary <- list(
  mn = function(x) mean(x, na.rm = TRUE),
  sd = function(x) sd(x, na.rm = TRUE),
 upper_mn = mean_upper,
 lower_mn = mean_lower,
  iqr = function(x) IQR(x, na.rm = TRUE),
  skew = function(x) compute_skew(x)
)

other_functions <- list(
  rmssd = function(x) compute_RMSSD(x),
  tkeo = function(x) mean(TKEO(x), na.rm=T)
  )



pat_summary <- data.table(record_id=c(unique(diary_dt$record_id)))
pat_summary_excl <- data.table(record_id=c(unique(diary_dt$record_id)))
for (fn_name in names(summary_functions_diary)) {
  fn <- summary_functions_diary[[fn_name]]
  if (fn_name == 'mn') {
    diary <- diary_dt[suff_diaries_pat_mn == T]
  } else {
    diary <- diary_dt[suff_diaries_pat == T]
  }
  summary_dt <- diary[, lapply(.SD, fn), by=.(record_id), .SDcols = symptom_cols]
  summary_excl_dt <- diary[is.na(surg_cycle) | cycle < surg_cycle,
                           lapply(.SD, fn), by=.(record_id), .SDcols = symptom_cols]
  colnames(summary_dt)[2:ncol(summary_dt)] <- paste0(symptom_cols, "_",fn_name)
  colnames(summary_excl_dt)[2:ncol(summary_excl_dt)] <- paste0(symptom_cols, "_",fn_name)
  pat_summary <- merge(pat_summary, summary_dt, by='record_id', all=T)
  pat_summary_excl <- merge(pat_summary_excl, summary_excl_dt, by='record_id', all=T)
}



pat_symptom_dt <-  merge(pat_summary, unique(diary_dt[, .(record_id, total_diaries_record)]),
                                               by='record_id',all=T) 
pat_symptom_excl <- pat_summary_excl


diary_imputed <- diary_dt[suff_diaries_cycle == T]
diary_imputed[, (symptom_cols) := lapply(.SD, function(x){na_interpolation(x,maxgap=3)}),
              .SDcols=symptom_cols, by=.(record_id,cycle)]

all_functions <- c(summary_functions_diary, other_functions)

cycle_summary <- diary_dt[, .(record_id, cycle)] %>% unique()
for (fn_name in names(all_functions)) {
  fn <- all_functions[[fn_name]]
  if (fn_name == 'mn') {
    diary <- diary_dt[suff_diaries_cycle_mn == T]
  } else if (fn_name %in% names(summary_functions_diary)) {
    diary <- diary_dt[suff_diaries_cycle == T]
  } else if (fn_name %in% names(other_functions)) {
    diary <- diary_imputed
  } else {
    print("Error")
  }
  summary_dt <- diary[, lapply(.SD, fn), by=.(record_id, cycle), .SDcols = symptom_cols]
  colnames(summary_dt)[3:ncol(summary_dt)] <- paste0(symptom_cols, "_",fn_name)
  cycle_summary <- merge(cycle_summary, summary_dt, by=c('record_id','cycle'),
                         all=T)
}


cycle_symptom_dt <-
  merge(cycle_summary, unique(diary_dt[, .(record_id, cycle, total_diaries_cycle)]), by=c('record_id','cycle'),
              all=T) %>%
  merge(diary_dt[, .(start_date = first(record_date),
                     surg_cycle = unique(surg_cycle)), by=.(record_id,cycle)],
        by=c('record_id','cycle'),all=T)



#### Actigraphy data: #####

dt <- fread(paste0(config$processed_output_dir, "/allmetrics_dt.csv"), colClasses=c("record_id"='character'))

dt[, total_entries_record := sum(!is.na(M10)), by=record_id]
dt[, .(total_entries_record, record_id)] %>% unique()
dt[, total_entries_cycle := sum(!is.na(M10)), by=.(record_id, cycle)]

dt[, suff_act_pat_mn := total_entries_record >= min_pat_mn_days]
dt[, suff_act_pat := total_entries_record >= min_pat_days]
dt[, suff_act_cycle_mn := total_entries_cycle >= min_cycle_mn_days]
dt[, suff_act_cycle := total_entries_cycle >= min_cycle_days]

dt[suff_act_pat_mn == T]$record_id %>% uniqueN()
dt[suff_act_pat == T]$record_id %>% uniqueN()
dt[suff_act_cycle_mn== T]$record_id %>% uniqueN()
dt[suff_act_cycle_mn== T, .(record_id,cycle)] %>% uniqueN()
dt[suff_act_cycle== T]$record_id %>% uniqueN()
dt[suff_act_cycle== T, .(record_id,cycle)] %>% uniqueN()


ggir_cols <-  colnames(dt)[grepl('GGIR', colnames(dt))]
self_cols_names <- paste0(config$actigraphy_cols_self, '_self')
dt[, (self_cols_names) := .SD, .SDcols = config$actigraphy_cols_self]
dt[, (config$actigraphy_cols_self) := NULL]
act_cols <- c(self_cols_names, ggir_cols)
act_cols <- act_cols[act_cols %in% colnames(dt) & !act_cols %in% c('lowvar_onset_hrs_self',
                                                                   'lowvar_offset_hrs_self',
                                                                   'sleeponset_GGIR', 'wakeup_GGIR')]

stopifnot(length(act_cols[dt[, mget(act_cols)] %>% 
           apply(FUN=function(x){sum(!is.na(x))}, MARGIN=2) < .66*nrow(dt)]) == 0)
# act_cols <- act_cols[dt[, mget(act_cols)] %>% 
#                        apply(FUN=function(x){sum(!is.na(x))}, MARGIN=2) > .66*nrow(dt)]

dt[, (act_cols) := lapply(.SD, as.numeric), .SDcols=act_cols]


pat_summary <- data.table(record_id=c(unique(dt$record_id)))
pat_summary_excl <- data.table(record_id=c(unique(dt$record_id)))

# Define functions and their corresponding names
summary_functions <- c(summary_functions_diary,
                       list(med = function(x) as.numeric(median(x, na.rm=T)),
                            upper = function(x) quantile(x, 0.75, na.rm = TRUE),
                            lower = function(x) quantile(x, 0.25, na.rm = TRUE),
                            upper90 = function(x) quantile(x, .9, na.rm=T)))


all_functions <- c(summary_functions, other_functions)

for (fn_name in names(summary_functions)) {
  fn <- summary_functions[[fn_name]]
  if (fn_name == 'mn') {
    sub_dt <- dt[suff_act_cycle_mn == T]
  } else if (fn_name %in% names(summary_functions)) {
    sub_dt <- dt[suff_act_cycle == T]
  } else {
    print("Shouldn't get here - error")
  }
  summary_dt <- sub_dt[, lapply(.SD, fn), by=.(record_id), .SDcols = act_cols]
  summary_excl_dt <- sub_dt[is.na(surg_cycle) | cycle < surg_cycle,
                            lapply(.SD, fn), by=.(record_id), .SDcols = act_cols]
  colnames(summary_dt)[2:ncol(summary_dt)] <- paste0(act_cols, "_",fn_name)
  pat_summary <- merge(pat_summary, summary_dt, by='record_id', all=T)
  colnames(summary_excl_dt)[2:ncol(summary_excl_dt)] <- paste0(act_cols, "_",fn_name)
  pat_summary_excl <- merge(pat_summary_excl, summary_excl_dt, by='record_id', all=T)
}


pat_act_dt <-  merge(pat_summary, unique(dt[, .(record_id, total_entries_record)]),
                         by='record_id', all=T) 
pat_act_excl <- pat_summary_excl


dt[total_entries_cycle < min_cycle_days]$record_id %>% unique()

dt_imputed <- dt[suff_act_cycle == T]
dt_imputed[, (act_cols) := lapply(.SD, function(x){na_interpolation(x,maxgap=3)}),
   .SDcols=act_cols, by=.(record_id,cycle)]

cycle_summary <- dt[, .(record_id, cycle)] %>% unique()
for (fn_name in names(all_functions)) {
  fn <- all_functions[[fn_name]]
  if (fn_name == 'mn') {
    sub_dt <- dt[suff_act_cycle_mn == T]
  } else if (fn_name %in% names(summary_functions)) {
    sub_dt <- dt[suff_act_cycle == T]
  } else if (fn_name %in% names(other_functions)) {
    sub_dt <- dt_imputed
  } else {
    print("Shouldn't get here - error")
  }
  summary_dt <- sub_dt[, lapply(.SD, fn), by=.(record_id, cycle), .SDcols = act_cols]
  colnames(summary_dt)[3:ncol(summary_dt)] <- paste0(act_cols, "_",fn_name)
  cycle_summary <- merge(cycle_summary, summary_dt, by=c('record_id','cycle'), all=T)
}


cycle_act_dt <-
  merge(cycle_summary,unique(dt[, .(record_id, cycle, total_entries_cycle)]), by=c('record_id','cycle'),
        all=T) %>%
  merge(dt[, .(start_date = first(record_date),
               watch_dom_wrist = first(watch_dom_wrist)#,
               ), by=.(record_id,cycle)],
        by=c('record_id','cycle'),all=T)


# 2 people that changed watch
dt[, .(watch_dom_wrist=uniqueN(watch_dom_wrist)), by=.(record_id)]

pat_act_dt <- pat_act_dt %>%  
  merge(dt[, .(watch_dom_wrist_start=unique(watch_dom_wrist)[1]), by=.(record_id)],
        by=c("record_id"))
pat_act_excl <- pat_act_excl %>%  
  merge(dt[post_deep_surgery==F, .(watch_dom_wrist_start=unique(watch_dom_wrist)[1]), by=.(record_id)],
        by=c("record_id"))


## PROCESS surgery and change in hormone data - add to cycle_symptom_dt:
flare_dt <- dt[, .(days_flr_meds_diary = sum(!is.na(yn_flare_meds)), flare_diary = !all(is.na(yn_flare)),
                   days_flr_diary = sum(!is.na(yn_flare))), 
               by=.(record_id,cycle)]
flare_dt[flare_diary == FALSE, `:=`(days_flr_meds_diary = NA,
                                    days_flr_diary = NA)]


cycle_surg_dt <- diary_dt[, .(post_deep_surgery = unique(post_deep_surgery),
       surg_type = unique(post_surg_type),
       changed_horm = unique(changed_horm)), by=.(record_id,cycle)]

cycle_surg_dt[is.na(surg_type),surg_type := 'none']

gnrh_dt <- diary_dt[, .(gnrh_start = first(gnrh_taken), gnrh_change = uniqueN(gnrh_taken) > 1), 
                    by=.(record_id, cycle)]
gnrh_dt[, gnrh_status := ifelse(gnrh_change==F, 
                                ifelse(gnrh_start==0, "No","Yes"),
                                ifelse(gnrh_start == 0, "Started","Stopped"))]

gnrh_dt[!is.na(gnrh_status), 
        gnrh_change := ifelse(all(gnrh_status %in% c("Yes","No")),
                              uniqueN(gnrh_status)>1, TRUE), by=record_id]

cycle_symptom_dt <- cycle_symptom_dt %>% merge(cycle_surg_dt, by=c("record_id",'cycle'), all.x=T) %>%
  merge(gnrh_dt, by=c("record_id","cycle"), all.x=T)



# COMBINE:

pat_dt <- pat_symptom_dt %>% merge(pat_act_dt, by=c("record_id"))
pat_dt_excl <- pat_symptom_excl %>% merge(pat_act_excl, by='record_id')

cycle_dt <- cycle_symptom_dt[, !c('start_date')] %>%
  merge(cycle_act_dt, by=c("record_id","cycle")) 




#### End-of-cycle Questionnaires ####

base_dt <- fread(paste0(config$processed_output_dir, "baseline_dt.csv"), 
                 colClasses=c("record_id"='character'))

cycle_crf <- extract_crf(alldata)
cycle_crf <- cycle_crf %>% 
  merge(flare_dt[, .(record_id,cycle,days_flr_meds_diary,
                     days_flr_diary)], by=c("record_id","cycle"), all.x=T, all.y=F)


cycle_crf[, took_flr_meds_filled := ifelse(!is.na(days_flr_meds_diary), 
                                                     days_flr_meds_diary > 0,
                                                     yn_flr_pain_drugs_taken ==1)]
cycle_crf[took_flr_meds_filled != yn_flr_pain_drugs_taken]

# QUESTIONNAIRES
quest_dt <- extract_questionnaires(alldata)


ehp30_1a_cols <- colnames(quest_dt)[grepl("ehp30_1a", colnames(quest_dt))]

pain_ehp30_cols <- ehp30_1a_cols[1:11]
control_ehp30_cols <- ehp30_1a_cols[12:17]
emotion_ehp30_cols <- ehp30_1a_cols[18:23]
social_ehp30_cols <- ehp30_1a_cols[24:27]
self_ehp30_cols <- ehp30_1a_cols[28:30]

bfi_quest_cols <- colnames(quest_dt)[grepl('bfi', colnames(quest_dt))]
quest_dt[, bfi_total := apply(.SD, MARGIN=1, FUN=sum), .SDcols = bfi_quest_cols[2:length(bfi_quest_cols)]]

standardize_ehp30 <- function(x) {
  100*sum(x-1)/(length(x)*4)
}
quest_dt[, ehp30_overall := apply(.SD, MARGIN=1, 
                                  FUN=standardize_ehp30), .SDcols=ehp30_1a_cols]
quest_dt[, pain_ehp30 := apply(.SD, MARGIN=1, FUN=standardize_ehp30), .SDcols=pain_ehp30_cols]
quest_dt[, control_ehp30 := apply(.SD, MARGIN=1, FUN=standardize_ehp30), .SDcols=control_ehp30_cols]
quest_dt[, social_ehp30 := apply(.SD, MARGIN=1, FUN=standardize_ehp30), .SDcols=social_ehp30_cols]
quest_dt[, emotion_ehp30 := apply(.SD, MARGIN=1, FUN=standardize_ehp30), .SDcols=emotion_ehp30_cols]
quest_dt[, self_ehp30 := apply(.SD, MARGIN=1, FUN=standardize_ehp30), .SDcols=self_ehp30_cols]

ehp30_cols <- c('ehp30_overall','pain_ehp30','control_ehp30','social_ehp30',
                'emotion_ehp30','self_ehp30')

ehp30_q3_cols <- c("pelvic_pain","vomiting","tired","urine_pain",
                   "bowel_pain","diarrhoea","ireg_bleeding","period_pain",
                   "4wk_last_period")
ehp30_q3_cols <- c(#"ehp30_3a_relation_status",
  paste0("ehp30_3a_yn_", ehp30_q3_cols))
quest_complete_dt <- quest_dt[, mget(c(ehp30_q3_cols, "record_id","cycle"))][!is.na(ehp30_3a_yn_vomiting)]
quest_dt[, (ehp30_q3_cols) := lapply(.SD, as.numeric), .SDcols=ehp30_q3_cols]


quest_pat_dt <- quest_dt[, lapply(.SD, mean, na.rm=T), by=.(record_id), .SDcols=ehp30_cols]
quest_dt <- quest_dt %>% merge(unique(diary_dt[, .(record_id,cycle, post_deep_surgery)]), 
                   by=c("record_id",'cycle'), all.x=T)
quest_pat_excl <- quest_dt[is.na(post_deep_surgery)|post_deep_surgery==F,
                           lapply(.SD, mean, na.rm=T), by=.(record_id), .SDcols=ehp30_cols]
print("Fix quest pat excl, probably can just combine with original quest instead")

deep_dt <- fread(paste0(config$processed_output_dir, 'deep_dt.csv'), 
                 colClasses=c('record_id'='character'))
deep_study_pats <- deep_dt[deep_diag == T]$record_id
quest_dt[, deep_pat := record_id %in% deep_study_pats]


write.csv(quest_dt, paste0(config$processed_output_dir, "quest_dt.csv"), row.names = F)


combined_dt <- quest_dt[, !c('post_deep_surgery')] %>%
  merge(cycle_dt, by=c("record_id","cycle"), all=TRUE)

# for each participant
combined_pat_dt <- pat_dt %>% merge(quest_pat_dt, by=c("record_id"))
combined_pat_excl <- pat_dt_excl %>% merge(quest_pat_excl, by='record_id')


constant_cols <- c("record_id")
base_colnames <- colnames(base_dt[, !..constant_cols])
colnames(base_dt)[which(colnames(base_dt) %in% base_colnames)] <-
  paste0(base_colnames, '_base')
base_dt %>% colnames()

combined_dt <- combined_dt %>% merge(base_dt, by='record_id', all.x=T, all.y=T) %>%
  merge(cycle_crf, all.x=T, all.y=F, by=c("record_id","cycle"))
combined_symptom_dt <- cycle_symptom_dt %>% merge(base_dt, by='record_id', all.x=T, all.y=T) %>%
  merge(cycle_crf, by=c('record_id', 'cycle'), all.x=T, all.y=T) %>%
  merge(quest_dt, by=c("record_id","cycle",'post_deep_surgery'),  all.x=T, all.y=T)
combined_pat_dt <- combined_pat_dt %>% merge(base_dt, by='record_id', all.x=T, all.y=F)
combined_pat_excl <- combined_pat_excl %>% merge(base_dt, by='record_id', all.x=T, all.y=F)


combined_symptom_dt[, deep_pat_followup := FALSE]
combined_symptom_dt[record_id %in% deep_study_pats & cycle==3, 
                    deep_pat_followup := TRUE]

write.csv(combined_symptom_dt, paste0(config$processed_output_dir,
                                      "symptom_cycle_dt.csv"), row.names = F)
write.csv(combined_dt,paste0(config$processed_output_dir,
                             "combined_cycle_dt.csv"), row.names = F)
write.csv(combined_pat_dt,paste0(config$processed_output_dir,
                                 "combined_pat_dt.csv"), row.names = F)
write.csv(combined_pat_excl,
          paste0(config$processed_output_dir,
                 "combined_pat_excl_deepsurgery.csv"), row.names = F)
