
base_dt <- fread(paste0(config$processed_output_dir, "baseline_dt.csv"),
                 colClasses=c('record_id'='character'))

tableNA = function (..., useNA = 'ifany') base::table(..., useNA = useNA)


base_dt$record_id %>% uniqueN()

base_dt[is.na(age)]
base_dt$age %>% mean()
base_dt$age %>% range()
base_dt$age %>% sd()


base_dt[is.na(bmi)]
base_dt$bmi %>% mean(na.rm=T)
base_dt$bmi %>% range(na.rm=T)
base_dt$bmi %>% sd(na.rm=T)
base_dt[, .(.N, .N/nrow(base_dt)), by=bmi < 18.5]
base_dt[, .(.N, .N/nrow(base_dt)), by=bmi >= 18.5 & bmi <25] 
base_dt[, .(.N, .N/nrow(base_dt)), by=bmi >= 25 & bmi <30] 
base_dt[, .(.N, .N/nrow(base_dt)), by=bmi >= 30 & bmi <40] 
base_dt[, .(.N, .N/nrow(base_dt)), by=bmi >=40] 



base_dt$ethnicity %>% tableNA()
base_dt[ethnicity == 'other']$record_id

base_dt$education %>% tableNA()


base_dt$gender %>% tableNA()

base_dt$smoker %>% tableNA()

base_dt$night_shift %>% tableNA()

base_dt$parity %>% tableNA()

base_dt[, .(.N, .N/nrow(base_dt)), by=hysterectomy]
base_dt[, .(.N, .N/nrow(base_dt)), by=oopherectomy]


base_dt$spe_diag %>% tableNA()
base_dt[, .(.N, .N/nrow(base_dt)), by=spe_diag]

base_dt[, .(.N, .N/nrow(base_dt)), by=die_diag]
base_dt$die_diag %>% tableNA()
base_dt[is.na(die_diag)]$record_id


base_dt$ovarian_diag %>% tableNA()
base_dt[, .(.N, .N/nrow(base_dt)), by=ovarian_diag]

base_dt$last_surg_overall  %>% tableNA()
base_dt[, .(.N/nrow(base_dt), .N), by=prev_treat_general]


base_dt$pelvic_pain_months %>% mean(na.rm=T)
base_dt$pelvic_pain_months %>% sd(na.rm=T)
base_dt$pelvic_pain_months %>% range(na.rm=T)

base_dt[is.na(pelvic_pain_months), .(record_id)]

base_dt$reg_painkillers_taken %>% tableNA()
base_dt[, .(.N/nrow(base_dt), .N), by=reg_painkillers_taken]
base_dt[, .(.N/nrow(base_dt), .N), by=add_painkillers_taken]

# opiates
base_dt[, .(.N/nrow(base_dt), .N), by=opiates_taken_reg]
base_dt[, .(.N/nrow(base_dt), .N), by=opiates_taken_flr]


#### Hormone and comorbidity figures:

base_dt[, .(.N, .N/nrow(base_dt)), by=hormones_taken]
base_dt[, .(.N, .N/nrow(base_dt)), by=gnrh_taken]
base_dt[, .(.N, .N/nrow(base_dt)), by=hrt_taken]
base_dt[, .(.N, .N/nrow(base_dt)), by=.(gnrh_taken,hrt_taken)]
base_dt[, .(.N, .N/nrow(base_dt)), by=coc_taken]
base_dt[, .(.N, .N/nrow(base_dt)), by=IUD_taken]
base_dt[, .(.N, .N/nrow(base_dt)), by=pop_taken]
base_dt[, .(.N, .N/nrow(base_dt)), by=patch_taken]
base_dt[, .(.N, .N/nrow(base_dt)), by=nexpl_taken]
base_dt[, .(.N, .N/nrow(base_dt)), by=depo_taken]
base_dt[, .(.N, .N/nrow(base_dt)), by=prog_taken]
base_dt[, .(.N, .N/nrow(base_dt)), by=ring_taken]
base_dt[, depo_nexpl := depo_taken == 1 | nexpl_taken == 1]
base_dt[, .(.N, .N/nrow(base_dt)), by=depo_nexpl]



hormone_cols <- c("coc_taken", "IUD_taken","patch_taken", "nexpl_taken","depo_taken",
                  "pop_taken","gnrh_taken","hrt_taken","ring_taken",
                  "prog_taken",
                  "other_contra_taken")

hormone_names <- c("COC pill", "Hormonal\nIUS", "Patch", "Nexplanon",
                   "Depo-\nprovera", "Prog.\nOnly\nPill",
                   "GnRH\nagonist", "HRT", "Ring", "Other\nprogesterone","Other")
hormone_nums <- base_dt[, lapply(.SD, function(x) {paste0(round(mean(x,na.rm=T)*100, 1), "%")}), .SDcols = hormone_cols]
hormone_nums %>% rbind(base_dt[, lapply(.SD, function(x) {sum(x==1,na.rm=T)}),
                  .SDcols = hormone_cols])
name_dt <- data.table('nm' = hormone_names, 'perc' = t(hormone_nums))
name_dt[, label := paste0(nm, "\n(", perc.V1, ")")]


base_dt[, (name_dt$label) := lapply(.SD, function(x) {x==1}),
        .SDcols = hormone_cols]

pat_melted <- base_dt[, mget(c('record_id_random','spe_only',name_dt$label))] %>% 
  melt(id.vars=c('record_id_random', "spe_only")) %>% as.data.table()
pat_melted <- pat_melted[order(spe_only)]

ggplot(data = pat_melted, aes(x = variable, y = as.numeric(record_id_random), color='black')) +
  geom_tile(aes(fill = value))+ 
  scale_fill_manual(values = c('FALSE' = 'white', "TRUE" = "red"))  +
  scale_y_continuous(breaks=seq(5,68,5)) + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + labs(x="", y="")


#### Comorbidites ####
comorbid_cols <- c("fibromyalgia", "rheu_arthe", "ibs", "adpkd", "migraine",
                   "othr_arthritis", "cfs", "pancreatitis", "painful_bladder",
                   "other_chronic_pain")

comorbid_names <- c("Fibro-\nmyalgia", "Rheumatoid arthritis", "IBS", "ADPKD",
                    "Migraine", "Other arthritis", "Chronic fatigue syndrome",
                    "Pancreatitis", "Painful bladder syndrome", "Other chronic pain")
comorbid_nums <- base_dt[, lapply(.SD, function(x) {paste0(round(mean(x,na.rm=T)*100, 1), "%")}),
                         .SDcols = comorbid_cols]
comorbid_nums %>% rbind(base_dt[, lapply(.SD, function(x) {sum(x==1,na.rm=T)}),
                                .SDcols = comorbid_cols])
name_dt <- data.table('nm' = comorbid_names, 'perc' = t(comorbid_nums))
name_dt[, label := paste0(nm, " (", perc.V1, ")")]
name_dt

base_dt[, (name_dt$label) := lapply(.SD, function(x) {x==1}),
        .SDcols = comorbid_cols]

pat_melted <- base_dt[, mget(c('record_id_random',name_dt$label))] %>% 
  melt(id.vars=c('record_id_random'))

ggplot(data = pat_melted, aes(x = variable, y = as.numeric(record_id_random), color='black')) +
  geom_tile(aes(fill = value))+ scale_x_discrete(labels = label_wrap(10)) + 
  scale_fill_manual(values = c('FALSE' = 'white', "TRUE" = "red"), na.value='grey')  +
  scale_y_continuous(breaks=seq(5,68,5)) + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + labs(x="", y="")


#### Gyne comorbidities ####

# gynecological comorbidities
gyn_cols <- c("adenomyosis", "heavy_bleeding", "fibroids",
              "PID")
gyn_names <- c("Adenomyosis", "Heavy bleeding", "Fibroids",
               "Previous Pelvic Inflammatory Disease (PID)")
gyn_nums <- base_dt[, lapply(.SD, function(x) {paste0(round(mean(x,na.rm=T)*100, 1), "%")}), .SDcols = gyn_cols]
gyn_nums %>% rbind(base_dt[, lapply(.SD, function(x) {sum(x==1,na.rm=T)}),
                           .SDcols = gyn_cols])
name_dt <- data.table('nm' = gyn_names, 'perc' = t(gyn_nums))
name_dt[, label := paste0(nm, " (", perc.V1, ")")]
name_dt

base_dt[, (name_dt$label) := lapply(.SD, function(x) {x==1}),
        .SDcols = gyn_cols]

pat_melted <- base_dt[, mget(c('record_id_random',name_dt$label))] %>% 
  melt(id.vars=c('record_id_random'))


ggplot(data = pat_melted, aes(x = variable, y = as.numeric(record_id_random), color='black')) +
  geom_tile(aes(fill = value))+ scale_x_discrete(labels = label_wrap(10)) + 
  scale_fill_manual(values = c('FALSE' = 'white', "TRUE" = "red"))  +
  scale_y_continuous(breaks=seq(5,68,5)) + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + labs(x="", y="")




#### Age comparisons ####


base_dt[, mean(age), by=spe_only]
base_dt[, sd(age), by=spe_only]



