
source("./Analysis/Functions/cor_functions.R")

id_mapping <- fread(config$ID_mapping_path,
                    colClasses=c('record_id' = 'character'))


dt <- fread(paste0(config$processed_output_dir, "allmetrics_with_shifted.csv"),
            colClasses=c("record_id"='character','record_date' = 'IDate'))
combined_dt <- fread(paste0(config$processed_output_dir, "combined_cycle_dt.csv"),
                     colClasses = c("record_id" = 'character'))
diary_dt <- fread(paste0(config$processed_output_dir, "diary_dt.csv"))
file_dt <- fread(paste0(config$source_data_dir, "file_dt.csv"),
                 colClasses = c('record_id'='character',
                                'start_date'='IDate',
                                'end_date' = 'IDate'))
file_dt <- file_dt %>% merge(id_mapping, all=T)


### Numbers reported in paper ###

dt[perc_wear > 0.75] %>% dim()
dt[perc_wear > 0.75]$record_id %>% uniqueN()

dt[!is.na(bfi_total) & !is.na(pain_total)] %>% dim()
dt[!is.na(bfi_total) & !is.na(pain_total)]$record_id %>% uniqueN()

# set mapped IDs for paper
dt <- merge(dt,id_mapping, by='record_id',all.x=T, all.y=F )

dt[, is_period := period==1]


dt[is.na(surg_cycle) | cycle != surg_cycle,
   `:=` (nvalues = sum(!is.na(SleepRegularityIndex_GGIR) & !is.na(pain_total)),
           pain_std = sd(pain_total, na.rm=T),
           bfi_std = sd(bfi_total,na.rm=T)), by=record_id]

dt %>% dim()

# took out sleep_perc_24hr
ggir_cols <- colnames(dt)[grepl('GGIR', colnames(dt)) & !grepl('minus', colnames(dt)) &
                            !grepl('prev5days', colnames(dt))]
self_act_cols <-  colnames(dt)[colnames(dt) %in% config$actigraphy_cols_self]



#### Cycle type: Define and plot symptoms by cycle type #####


dt[, unq_cycle := paste0(record_id, '_', cycle)]
combined_dt[, unq_cycle := paste0(record_id,'_',cycle)]

nohormone_cycles <- dt[(is.na(surg_cycle)|cycle < surg_cycle), all(hormones_taken) == 0,
                       by=.(unq_cycle)][V1==T]$unq_cycle
gnrh_cycles <- dt[(is.na(surg_cycle)|cycle < surg_cycle), sum(gnrh_taken) > 0,
                  by=.(unq_cycle)][V1==T]$unq_cycle
combined_dt[, cycle_type := "Other hormones"]
combined_dt[unq_cycle %in% nohormone_cycles, cycle_type := 'No hormones']
combined_dt[unq_cycle %in% gnrh_cycles, cycle_type := 'GnRH agonist']
combined_dt[!is.na(surg_cycle) & cycle==surg_cycle, cycle_type := 'Post surgery']
combined_dt[(!is.na(surg_cycle) & cycle>surg_cycle & deep_pat==T),
            # last_surg_treat_timediff_base < 3,
            cycle_type := 'Surgery followup']

combined_dt[is.na(last_surg_treat_timediff_base) & 
              (is.na(surg_cycle) | cycle < surg_cycle), 
            surgery_base_type := 'No surgical treatment']
combined_dt[!is.na(last_surg_treat_timediff_base), surgery_base_type := 
              ifelse(last_surg_treat_timediff_base < 3, 
                     "Surgical treatment\nin prev. 3 years",
                     'Surgical treatment\nover 3 years ago')]
combined_dt[!is.na(surg_cycle) & 
              cycle >= surg_cycle, surgery_base_type := 'Surgical treatment\nin prev. 3 years']

combined_dt[, hrt_label := ifelse(hrt_taken == 1, T, NA)]


#### Overview of sleep and physical activity #####

dt[, sleep_med := median(SleepDurationInSpt_GGIR, na.rm=T), by=record_id]

dt <- dt[order(record_id_shifted)]
dt[, pat_sorted := factor(record_id_shifted, levels=unique(record_id_shifted))]

png(filename=paste0(config$plots_output_dir, "sleep_summary.png"),
    width=10, height=4, units='in', res=300)
ggplot(dt, aes(x=factor(record_id_shifted), y=SleepDurationInSpt_GGIR)) + 
  geom_jitter(aes(color=factor(record_id)),alpha=.3, width=.4) +
  geom_boxplot(color='black', alpha=0) + 
  theme_minimal() + 
  labs(x='Participant ID', y='Sleep duration (hours)') + 
  theme(legend.position = 'none',
        axis.text.x = element_text(angle=90, size=10,vjust = .5),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(size=18))
dev.off()

dt[, MVPA_total := dur_day_MVPA_bts_1_5_min_GGIR + dur_day_MVPA_bts_10_min_GGIR +
     dur_day_MVPA_bts_5_10_min_GGIR]
# dt[, MVPA_med := median(MVPA_total, na.rm=T), by=record_id]
# dt <- dt[order(MVPA_med)]
# dt[, pat_sorted := factor(record_id_shifted, levels=unique(record_id_shifted))]
png(filename=paste0(config$plots_output_dir, "activity_summary.png"),
    width=10, height=4, units='in', res=300)
ggplot(dt, aes(x=factor(record_id_shifted), y=MVPA_total)) + 
  geom_jitter(aes(color=factor(record_id)),alpha=.3, width=.4) +
  geom_boxplot(color='black', alpha=0) + 
  theme_minimal() + 
  labs(x='Participant ID', y='MPVA (minutes)') + 
  theme(legend.position = 'none',
        axis.text.x = element_text(angle=90, size=10, vjust=.5),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(size=18))
dev.off()

ggplot(dt, aes(x=factor(record_id), y=sleepinSPT_duration)) + 
  geom_boxplot(aes(fill=factor(record_id), color=factor(record_id)), alpha=.3) + 
  geom_jitter(aes(color=factor(record_id)),alpha=.3, width=.3) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(angle=90))


##### Intra participant correlations ######


act_ls <- c("SleepDurationInSpt_GGIR",
            "SleepDurationInSpt_GGIR_minus1", "WASO_GGIR","WASO_GGIR_minus1", 
            "M10",'M10_minus1',
            "SleepRegularityIndex_GGIR",
            "IV1","IV2",'IS1', 
            'number_sib_wakinghours_GGIR')
act_colnames <- c( "Sleep\ndur.\n(after)", 
                  "Sleep\ndur.\n(before)",'WASO\n(after)', "WASO\n(before)",
                  'M10', 'M10\nprev.\nday',
                  "Sleep\nregularity",
                  'IV1',"IV2","IS1", 
                  'No. of\nday SIBs')


dt[nvalues > 20, 
              lapply(.SD, function(x){sum(!is.na(x))}),
              .SDcols = act_ls, by=.(record_id)]



##### Look at number of values being compared #####
dt[nvalues > 20] %>% dim()
dt[nvalues > 20]$record_id %>% uniqueN()
dt[nvalues > 20 & !is.na(M10) & !is.na(pain_avg)] %>% dim()
dt[nvalues > 20 & !is.na(M10_minus1) & !is.na(pain_avg)] %>% dim()
dt[nvalues > 20 & !is.na(SleepDurationInSpt_GGIR_minus1) & !is.na(pain_avg)] %>% dim()
dt[nvalues > 20 & !is.na(SleepDurationInSpt_GGIR) & !is.na(pain_avg)] %>% dim()
dt[nvalues > 20 & !is.na(SleepRegularityIndex_GGIR) & !is.na(pain_avg)] %>% dim()
dt[nvalues > 20 & !is.na(SleepRegularityIndex_GGIR) & !is.na(pain_avg)] %>% dim()


cor_dt <- dt[nvalues > 20,
             lapply(.SD, function(x) {cor(x, pain_total, use='pairwise.complete.obs', method='pearson')}),
             .SDcols = act_ls, by=.(record_id)]
colnames(cor_dt) <- c('record_id', act_colnames)
cor_dt <- cor_dt %>% merge(id_mapping, by='record_id', all.x=T, all.y=F)

cor_dt[, highlight_id := 'All']
cor_dt[record_id_shifted == 5 |
         record_id_shifted == 3|
         record_id_shifted == 12 , highlight_id := paste0('P',record_id_shifted)]
cor_dt[, record_id_shifted := NULL]

p1_paincor <- cor_dt %>% melt(id.vars=c('record_id','highlight_id')) %>%
  ggplot(aes(x=variable, y=value)) + geom_boxplot(outliers = F) + 
  geom_jitter(width=.1, aes(color=highlight_id, alpha=highlight_id == 'All')) +
  scale_color_manual(values=c('All' = 'black','P3' = 'red',
                              'P5'='blue','P12' = 'green'),
                     breaks=c('P3', 'P5', 'P12')) + 
  scale_alpha_discrete(range=c(1, .2), guide='none')+
  theme_minimal() + labs(y='Intra-person correlation\nwith global pain score', x='') + 
  ylim(c(-.8, .8)) +
  geom_hline(aes(yintercept=-.3), color='red', linetype=2, alpha=.5) + 
  geom_hline(aes(yintercept=.3), color='red', linetype=2, alpha=.5) + 
  geom_hline(aes(yintercept=0), color='blue', linetype=2, alpha=.5) + 
  theme(axis.text.x = element_text(angle=0, size=14),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.title = element_blank(),
        legend.text = element_text(size=16)) + 
  guides(color = guide_legend(override.aes = list(size = 2))) 
p1_paincor

##### Pairwise correlations with fatigue #####

cor_dt <- dt[nvalues > 20,
             lapply(.SD, function(x) {cor(x, bfi_total, use='pairwise.complete.obs', method='pearson')}),
             .SDcols = act_ls, by=.(record_id)] 
colnames(cor_dt) <- c('record_id', act_colnames)
cor_dt <- cor_dt %>% merge(id_mapping, by='record_id', all.x=T, all.y=F)

cor_dt[, highlight_id := 'All']
cor_dt[record_id_shifted == 5 |
         record_id_shifted == 3|
         record_id_shifted == 12 , highlight_id := paste0('P',record_id_shifted)]
cor_dt[, record_id_shifted := NULL]

p2_bficor <- cor_dt%>% melt(id.vars=c('record_id','highlight_id')) %>%
  ggplot(aes(x=variable, y=value)) + geom_boxplot(outliers=F) + 
  geom_jitter(width=.1, aes(color=highlight_id, alpha=highlight_id == 'All')) +
  scale_color_manual(values=c('All' = 'black','P3' = 'red',
                              'P5'='blue','P12' = 'green'),
                     breaks=c('P3', 'P5', 'P12')) + 
  scale_alpha_discrete(range=c(.9, .1), guide='none')+
  theme_minimal() + labs(y='Intra-person correlation\nwith global BFI score', x='') + 
  ylim(c(-.8, .8)) +
  geom_hline(aes(yintercept=-.3), color='red', linetype=2, alpha=.5) + 
  geom_hline(aes(yintercept=.3), color='red', linetype=2, alpha=.5) + 
  geom_hline(aes(yintercept=0), color='blue', linetype=2, alpha=.5) + 
  theme(axis.text.x = element_text(angle=0, size=14),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.title = element_blank(),
        legend.text = element_text(size=16)) + 
  guides(color = guide_legend(override.aes = list(size = 2))) 
p2_bficor

##### Partial correlations #####

sub_dt <- dt[nvalues > 20] #& (is.na(surg_cycle)| cycle != surg_cycle)]
res_full <- data.table()
omitted_dt <- data.table()
for (id in unique(sub_dt$record_id)) {
  sub_pat_dt <- na.omit(sub_dt[record_id == id
                               , .(pain_total,
                                   bfi_total, 
                                   SleepDurationInSpt_GGIR, SleepDurationInSpt_GGIR_minus1,
                                   M10, M10_minus1,
                                   SleepRegularityIndex_GGIR, 
                                   WASO_GGIR, WASO_GGIR_minus1
                               )])
  print(nrow(sub_pat_dt))
  res <- pcor(sub_pat_dt)
  res_dt <- as.data.table(res$estimate[, 1:2], keep.rownames = T)
  res_dt$rn <- c("Global\npain",
                 "Global\nBFI",
                 "Sleep\ndur.\n(after)",'Sleep\ndur.\n(before)',
                 'M10',"M10\nprev.\nday",# "'Low-var' dur.",
                 "Sleep\nregularity",# 'Sleep\nefficiency'#'SRI prev. day',
                 "WASO\n(after)", "WASO\n(before)"
  )
  res_dt[, record_id := id]
  res_full <- rbind(res_full, res_dt)
  omitted_dt <- rbind(omitted_dt, sub_pat_dt)
}

res_full[, rn := factor(rn, levels=unique(res_full$rn))]
res_full <- res_full %>% merge(id_mapping, by='record_id',all.x=T, all.y=F)
res_full[, highlight_id := 'All']
res_full[record_id_shifted == 5 |
         record_id_shifted == 3|
         record_id_shifted == 12 , highlight_id := paste0('P',record_id_shifted)]
res_full[, record_id_shifted := NULL]

p1_pain_pcor <- ggplot(res_full[!rn %in% c("Global\npain")], aes(x=rn, y=pain_total)) + 
  geom_boxplot(outliers=F) + 
  geom_jitter(width=.2, aes(color=highlight_id, alpha=highlight_id == 'All')) +
  scale_color_manual(values=c('All' = 'black','P3' = 'red',
                              'P5'='blue','P12' = 'green'),
                     breaks=c('P3', 'P5', 'P12')) + 
  scale_alpha_discrete(range=c(1, .2), guide='none')+
  scale_x_discrete(label = label_wrap(10)) +
  geom_hline(aes(yintercept=-.3), color='red', linetype=2, alpha=.5) + 
  geom_hline(aes(yintercept=.3), color='red', linetype=2, alpha=.5) + 
  geom_hline(aes(yintercept=0), color='blue', linetype=2, alpha=.5) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=0, size=12),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.text=element_text(size=16),
        legend.title=element_blank())+ 
  ylim(c(-1, 1)) +
  guides(color = guide_legend(override.aes = list(size = 2))) + 
  labs(x='', y='Intra-person partial correlations\nwith global pain score')
p1_pain_pcor
p2_bfi_pcor <- ggplot(res_full[!rn %in% c('Global\nBFI')], aes(x=rn, y=bfi_total)) + 
  geom_boxplot(outliers = F) + 
  geom_jitter(width=.2, aes(color=highlight_id, alpha=highlight_id == 'All')) +
  scale_color_manual(values=c('All' = 'black','P3' = 'red',
                              'P5'='blue','P12' = 'green'),
                     breaks=c('P3', 'P5', 'P12')) +  
  scale_alpha_discrete(range=c(1, .2), guide='none')+
  scale_x_discrete(label = label_wrap(10)) + 
  geom_hline(aes(yintercept=-.3), color='red', linetype=2, alpha=.5) + 
  geom_hline(aes(yintercept=.3), color='red', linetype=2, alpha=.5) + 
  geom_hline(aes(yintercept=0), color='blue', linetype=2, alpha=.5) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=0, size=12),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.text = element_text(size=16),
        legend.title = element_blank()) + 
  guides(color = guide_legend(override.aes = list(size = 2))) + 
  ylim(c(-1,1)) +
  labs(x='', y='Intra-person partial correlations\nwith global BFI score')
p2_bfi_pcor


ggarrange(p1_paincor, p2_bficor, p1_pain_pcor,p2_bfi_pcor, ncol=1, common.legend = T)



##### Correlations by cycle  ######
dt[, nvals_cycle :=sum(!is.na(M10) & 
                          !is.na(pain_avg)), by=.(record_id,cycle)]
cor_dt <- dt[nvals_cycle > 20,
             lapply(.SD, function(x) {cor(x, bfi_total, use='pairwise.complete.obs', method='pearson')}),
             .SDcols = act_ls, by=.(record_id, cycle)] %>%
  merge(dt[nvals_cycle > 20
           , .(pain_std = sd(pain_total,na.rm=T),
               pain_mn = mean(pain_total, na.rm=T),
               bfi_std = sd(bfi_total,na.rm=T),
               bfi_mn = mean(bfi_total,na.rm=T),
               SRI_mn = mean(SleepRegularityIndex_GGIR,na.rm=T),
               sleep_mn = mean(SleepDurationInSpt_GGIR,na.rm=T),
               sleep_sd = sd(SleepDurationInSpt_GGIR,na.rm=T),
               M10_sd = sd(M10,na.rm=T),
               M10_mn = mean(M10, na.rm=T),
               gnrh_cycle = any(gnrh_taken == 1),
               nohorm_cycle = all(hormones_taken==0),
               hormones_taken = any(hormones_taken==1),
               postsurg_cycle = any(post_deep_surgery==T),
               surg_followup =  any(cycle > surg_cycle),
               bmi = first(bmi_base),
               age = first(age_base),
               spe_diag = first(spe_diag_base),
               deep_diag = first(die_diag_base) ==1 |any(deep_study_pat==T)), by=.(record_id,cycle)],
        by=c('record_id','cycle'))
cor_dt <- cor_dt %>% merge(combined_dt[, .(record_id,cycle,cycle_type,
                                           surgery_base_type)], by=c('record_id','cycle'))
cor_dt <- cor_dt %>% merge(id_mapping, by='record_id', all.x=T, all.y=F)

cor_dt[, highlight_id := 'All']
cor_dt[record_id_shifted == 5 |
         record_id_shifted == 3|
         record_id_shifted == 12 , highlight_id := paste0('P',record_id_shifted)]
cor_dt[, record_id_shifted := NULL]

cor_dt[, surg_base_type := as.character(NA)]
cor_dt[surgery_base_type == 'Surgical treatment\nin prev. 3 years', 
       surg_base_type :=  'Surgical treatment\nin prev. 3 years']

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

color_values <- c('No surgical treatment' = cbPalette[1],
                  "Surgical treatment\nin prev. 3 years"=cbPalette[2],
                  'Surgical treatment\nover 3 years ago'=cbPalette[4])

# main plots showing relationships between intra-person correlations and factors by cycle
p1 <- ggplot(cor_dt, aes(x=cycle_type,y=M10)) + 
  geom_boxplot(outliers=F) + 
  geom_jitter(width=.2, alpha=.4) + 
  scale_color_manual(values=color_values) + 
  scale_shape_manual(values=c(8), na.value=16,
                     breaks=c('Surgical treatment\nin prev. 3 years'), 
                     name='') + 
  scale_x_discrete(label = label_wrap(10)) +
  labs(x='', y='Intra-cycle correlation\nbetween M10 and global BFI')  +
  theme_minimal()
p2 <- ggplot(cor_dt, aes(x=cycle_type,y=SleepDurationInSpt_GGIR)) + 
  geom_boxplot(outliers=F) + 
  geom_jitter(width=.2, alpha=.4) + 
  scale_color_manual(values=color_values) + 
  scale_shape_manual(values=c(8), na.value=16,
                     breaks=c('Surgical treatment\nin prev. 3 years'), 
                     name='') + 
  scale_x_discrete(label = label_wrap(10)) +
  labs(x='', y='Intra-cycle correlation between\nsleep dur. (after) and global BFI')  +
  theme_minimal()
p3 <- ggplot(cor_dt, aes(x=cycle_type,y=SleepDurationInSpt_GGIR_minus1)) + 
  geom_boxplot(outliers=F) + 
  geom_jitter(width=.2, alpha=.4) + 
  scale_color_manual(values=color_values) + 
  scale_shape_manual(values=c(8), na.value=16,
                     breaks=c('Surgical treatment\nin prev. 3 years'), 
                     name='') + 
  scale_x_discrete(label = label_wrap(10)) +
  labs(x='', y='Intra-cycle correlation between\nsleep dur. (before) and global BFI')  +
  theme_minimal()
p4 <- ggplot(cor_dt, aes(x=cycle_type,y=SleepRegularityIndex_GGIR)) + 
geom_boxplot(outliers=F) + 
  geom_jitter(width=.2, alpha=.4) + 
  scale_color_manual(values=color_values) + 
  scale_shape_manual(values=c(8), na.value=16,
                     breaks=c('Surgical treatment\nin prev. 3 years'), 
                     name='') + 
  scale_x_discrete(label = label_wrap(10)) +
  labs(x='', y='Intra-cycle correlation between\nsleep regularity and global BFI')  +
  theme_minimal()

ggarrange(p1,p2,p3,p4, nrow=2, ncol=2, common.legend=T)


##### Associations with intra-person correlations #####
dt <- dt %>% merge(dt[is_dayoff == T, .(M10_dayoff = mean(M10,na.rm=T)), by=record_id], by='record_id')
dt <- dt %>% merge(dt[is_dayoff == F, .(M10_week = mean(M10,na.rm=T)), by=record_id], by='record_id')
dt[, M10_dayoff_diff := M10_dayoff - M10_week]
dt <- dt %>% merge(dt[is_dayoff == T, 
                      .(sleep_dayoff = mean(SleepDurationInSpt_GGIR_minus1,na.rm=T)), by=record_id], by='record_id')
dt <- dt %>% merge(dt[is_dayoff == F, 
                      .(sleep_week = mean(SleepDurationInSpt_GGIR_minus1,na.rm=T)), by=record_id], by='record_id')
dt[, sleep_dayoff_diff := sleep_dayoff - sleep_week]


act_ls <- c('M10', 'M10_minus1', 'SleepRegularityIndex_GGIR', 'SleepDurationInSpt_GGIR',
            'SleepDurationInSpt_GGIR_minus1','WASO_GGIR', 'sleep_efficiency_GGIR','WASO_GGIR_minus1')
cor_dt <- dt[nvalues > 20 & (is.na(surg_cycle)|cycle != surg_cycle),
             lapply(.SD, function(x) {cor(x, bfi_total, use='pairwise.complete.obs', method='pearson')}),
             .SDcols = act_ls, by=.(record_id)]  %>%
  merge(dt[nvalues > 20 &  (is.na(surg_cycle)|cycle != surg_cycle)
           , .(bfi_mn = mean(bfi_total,na.rm=T),
               pain_worst_mn = mean(pain_worst, na.rm=T),
               pain_avg_mn = mean(pain_avg, na.rm=T),
               pain_avg_sd = sd(pain_avg, na.rm=T),
               bfi_std = sd(bfi_total,na.rm=T),
               bfi_range = diff(range(bfi_total,na.rm=T)),
               M10_mn = mean(M10,na.rm=T),
               M10_sd = sd(M10,na.rm=T),
               SRI_mn = mean(SleepRegularityIndex_GGIR, na.rm=T),
               sleep_mn = mean(SleepDurationInSpt_GGIR, na.rm=T),
               sleep_sd = sd(SleepDurationInSpt_GGIR,na.rm=T),
               age = unique(age_base),
               bmi = unique(bmi_base),
               spe_only = unique(spe_only_base),
               surg_followup = any(cycle > surg_cycle),
               last_treat = unique(last_surg_treat_base)), by=record_id], by='record_id')


cor_dt <- cor_dt %>% merge(id_mapping, by='record_id', all.x=T, all.y=F)

cor_dt[, highlight_id := 'All']
cor_dt[record_id_shifted == 5 |
         record_id_shifted == 3|
         record_id_shifted == 12 , highlight_id := paste0('P',record_id_shifted)]
cor_dt[, record_id_shifted := NULL]


p1 <- ggplot(cor_dt, aes(x=SRI_mn, y=M10, color=highlight_id)) +
  geom_point() + stat_cor(method='spearman') + geom_smooth(method='lm') + 
  scale_color_manual(values=c('All' = 'black','P3' = 'red',
                              'P5'='blue','P12' = 'green'),
                     breaks=c('P3', 'P5', 'P12'), name='') +  
  theme_minimal() +
  labs(x='Mean sleep regularity', y='Intra-person correlation\nbetween M10 and BFI total')
p2 <- ggplot(cor_dt, aes(x=SRI_mn, y=SleepDurationInSpt_GGIR, color=highlight_id)) +
  geom_point() + stat_cor(method='spearman') + geom_smooth(method='lm') + 
  scale_color_manual(values=c('All' = 'black','P3' = 'red',
                              'P5'='blue','P12' = 'green'),
                     breaks=c('P3', 'P5', 'P12'), name='') +  
  theme_minimal() +
  labs(x='Mean sleep regularity',
       y='Intra-person correlation between\nsleep dur. (after) and BFI total')
p3 <- ggplot(cor_dt, aes(x=SRI_mn, y=SleepDurationInSpt_GGIR_minus1, color=highlight_id)) +
  geom_point() + stat_cor(method='spearman', label.y = -.4) + geom_smooth(method='lm') + 
  scale_color_manual(values=c('All' = 'black','P3' = 'red',
                              'P5'='blue','P12' = 'green'),
                     breaks=c('P3', 'P5', 'P12'), name='') +  
  theme_minimal() +
  labs(x='Mean sleep regularity',
       y='Intra-person correlation between\nsleep dur. (before) and BFI total')
p4 <- ggplot(cor_dt, aes(x=SRI_mn, y=sleep_efficiency_GGIR, color=highlight_id)) +
  geom_point() + stat_cor(method='spearman') + geom_smooth(method='lm') + 
  scale_color_manual(values=c('All' = 'black','P3' = 'red',
                              'P5'='blue','P12' = 'green'),
                     breaks=c('P3', 'P5', 'P12'), name='') +  
  theme_minimal() +
  labs(x='Mean sleep regularity',
       y='Intra-person correlation between\nsleep disturbance (WASO) and BFI total')

ggarrange(p1,p2,p3, nrow=2,ncol=2, common.legend = T)



##### Case studies #####

position_scales <- list(
  variable == 'pain_total' ~ scale_y_continuous(limits=c(1,10)),
  variable == 'bfi_total' ~scale_y_continuous(limits=c(0,10)))

var_labeller <- as_labeller(c(pain_total = 'Global pain score',
                              bfi_total = 'Global BFI score',
                              SleepDurationInSpt_GGIR = 'Sleep duration (after)',
                              SleepRegularityIndex_GGIR = 'Sleep regularity',
                              M10 = 'Physical activity (M10)'))

case_dt <- dt[record_id_shifted == 5 & cycle==2, .(record_date,is_period,is_weekend, 
                                                pain_total, bfi_total,
                                                SleepDurationInSpt_GGIR, SleepRegularityIndex_GGIR,
                                                M10)] %>% 
  melt(id.vars=c("record_date",'is_period','is_weekend')) %>% as.data.table()


p1 <- ggplot(case_dt, aes(x=record_date, y=value)) +
  facet_wrap(~variable,nrow=5, scales = 'free_y', labeller=var_labeller) +
  geom_line(data=na.omit(case_dt), linetype=2, color='grey') + 
  geom_line(data=case_dt) + 
  geom_point(aes(shape=is_weekend, size=is_weekend)) + 
  scale_shape_manual(values=c(16,4), labels=c('Weekday','Weekend'))+
  scale_size_manual(values=c(1,3))+
  geom_tile(data=case_dt[is_period==T],aes(y=0, height=Inf, fill=is_period),alpha=.2) +
  scale_fill_manual(values=c('red'),na.value = NA, labels=c( 'Menstruation')) + 
  theme_minimal() + labs(x='', y='', fill='', shape='') + 
  theme(legend.box.background = element_rect(color='black'),
        legend.spacing.y =unit(0,'pt'),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14)) + 
  guides(size='none')
p1

# new example
case_dt <- dt[record_id_shifted==3 & cycle==2, .(record_date,is_period,is_weekend, pain_total, bfi_total,
                                                SleepDurationInSpt_GGIR, SleepRegularityIndex_GGIR,
                                                M10)] %>% 
  melt(id.vars=c("record_date",'is_period','is_weekend')) %>% as.data.table()

p2 <- ggplot(case_dt, aes(x=record_date, y=value)) +
  facet_wrap(~variable,nrow=5, scales = 'free_y', labeller=var_labeller) +
  geom_line(data=na.omit(case_dt), linetype=2, color='grey') + 
  geom_line(data=case_dt) + 
  geom_point(aes(shape=is_weekend, size=is_weekend)) + 
  scale_shape_manual(values=c(16,4), labels=c('Weekday','Weekend'))+
  scale_size_manual(values=c(1,3))+
  geom_tile(data=case_dt[is_period==T],aes(y=0, height=Inf, fill=is_period),alpha=.2) +
  scale_fill_manual(values=c('red'),na.value = NA, labels=c( 'Menstruation')) + 
  theme_minimal() + labs(x='', y='', fill='', shape='') + 
  theme(legend.box.background = element_rect(color='black'),
        legend.spacing.y =unit(0,'pt'),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14)) +
  guides(size='none')

p2



### Hormone example:
case_dt <- dt[record_id_shifted==12 & cycle==3 &
                day_of_cycle <=30, .(record_date,is_weekend, pain_total, bfi_total,
                                                SleepDurationInSpt_GGIR, SleepRegularityIndex_GGIR,
                                                M10)] %>% 
  melt(id.vars=c("record_date",'is_weekend')) %>% as.data.table()
p3 <- ggplot(case_dt, aes(x=record_date, y=value)) +
  facet_wrap(~variable,nrow=5, scales = 'free_y', labeller=var_labeller) +
  geom_line(data=na.omit(case_dt), linetype=2, color='grey') + 
  geom_line(data=case_dt) + 
  geom_point(aes(shape=is_weekend, size=is_weekend)) + 
  scale_shape_manual(values=c(16,4), labels=c('Weekday','Weekend'))+
  scale_size_manual(values=c(1,3))+
  theme_minimal() + labs(x='', y='', fill='', shape='') + 
  theme(legend.box.background = element_rect(color='black'),
        legend.spacing.y =unit(0,'pt'),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  guides(size='none')
p3

position_scales <- list(
  variable == 'pain_total' ~ scale_y_continuous(limits=c(1,10)), #breaks=c(1,5,9)),
  variable == 'bfi_total' ~scale_y_continuous(limits=c(0,10)),# breaks=c(0,5,10)),
  variable == 'SleepDurationInSpt_GGIR'~scale_y_continuous(limits=c(3,15)),# breaks=c(1,4,7)),
  variable == 'SleepRegularityIndex_GGIR' ~scale_y_continuous(limits=c(0,80)), #breaks=c(0,30,60)),
  variable == 'M10' ~scale_y_continuous(limits=c(.05,.25))#c(0,90))#,)
)
ggarrange(p1 + facetted_pos_scales(y=position_scales),
          p2 + facetted_pos_scales(y=position_scales),
          p3 + facetted_pos_scales(y=position_scales), 
          ncol=3,nrow=1, common.legend = TRUE, legend="right")



##### GnRH trajectories #####
dt[,gnrh_change := sum(gnrh_taken ==1, na.rm=T)>0 & sum(gnrh_taken==0, na.rm=T) > 0, by=.(record_id)]
dt[, .(record_id,gnrh_change)] %>% unique()
dt[gnrh_change == 1, gnrh_status_start := first(gnrh_taken), by=record_id]
dt[gnrh_status_start == 0 & gnrh_taken == 1, gnrh_start_date := first(record_date), by=record_id]
dt[gnrh_status_start == 1 & gnrh_taken == 0, gnrh_stop_date := first(record_date), by=record_id]
dt[, `:=`(gnrh_start_date =first(gnrh_start_date[!is.na(gnrh_start_date)]),
          gnrh_stop_date = first(gnrh_stop_date[!is.na(gnrh_stop_date)])), by=record_id]
dt[, .(record_id, gnrh_status_start, gnrh_start_date, gnrh_stop_date)] %>% unique()


position_scales <- list(
  variable == 'pain_total' ~ scale_y_continuous(limits=c(1,10)), 
  variable == 'bfi_total' ~scale_y_continuous(limits=c(0,10)))


case_dt <- dt[record_id_shifted == 18, .(record_date,gnrh_start_date,is_weekend, pain_total,
                                     bfi_total,
                                     SleepDurationInSpt_GGIR, SleepRegularityIndex_GGIR,
                                     M10)] %>% 
  melt(id.vars=c("record_date",'gnrh_start_date','is_weekend')) %>% as.data.table()

p2_gnrh <- ggplot(case_dt, aes(x=record_date, y=value)) +
  facet_wrap(~variable,nrow=5, scales = 'free_y', labeller=var_labeller) +
  geom_vline(aes(xintercept=gnrh_start_date, color='gnrh_start'), lty = 1, linewidth=1) +
  scale_color_manual( values = c(gnrh_start="#56B4E9"), labels=c('Started GnRH agonist')) + 
  geom_line(data=na.omit(case_dt), linetype=2, color='grey') + 
  geom_line(data=case_dt) + 
  geom_point(aes(shape=is_weekend, size=is_weekend)) + 
  scale_shape_manual(values=c(16,4), labels=c('Weekday','Weekend'))+
  scale_size_manual(values=c(1,3)) + 
  theme_minimal() + labs(x='', y='', fill='', shape='') + 
  facetted_pos_scales(y=position_scales) + 
  theme(legend.box.background = element_rect(color='black'),
        legend.spacing.y =unit(0,'pt'),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14),
        legend.position = 'bottom',
        legend.title = element_blank()) + 
  guides(size='none')

p2_gnrh





##### example 1  #####

position_scales <- list(
  variable == 'pain_total' ~ scale_y_continuous(limits=c(1,10)), 
  variable == 'bfi_total' ~scale_y_continuous(limits=c(0,10)))
# variable=='SleepDurationInSpt_GGIR'~scale_y_continuous(limits=c(0,16)),
# variable=='SleepRegularityIndex_GGIR'~scale_y_continuous(limits=c(-50,75)),
# variable=='M10'~scale_y_continuous(limits=c(.05,.4)))


case_dt <- dt[record_id_shifted==54 & cycle < 3, .(record_date,is_weekend, 
                                                pain_total, bfi_total,
                                                SleepDurationInSpt_GGIR, SleepRegularityIndex_GGIR,
                                                M10)] %>% 
  melt(id.vars=c("record_date",'is_weekend')) %>% as.data.table()


case_dt[, is_weekend := as.character(is_weekend)]
case_dt[is_weekend=='TRUE', is_weekend := 'Weekend']
case_dt[is_weekend=='FALSE', is_weekend := 'Weekday']

p1 <- ggplot(case_dt, aes(x=record_date, y=value)) +
  facet_wrap(~variable,nrow=5, scales = 'free_y', labeller=var_labeller) +
  geom_line(data=na.omit(case_dt), linetype=2, color='grey') + 
  geom_line(data=case_dt) + 
  geom_point(aes(shape=is_weekend, size=is_weekend, color=highlight_day)) + 
  scale_shape_manual(values=c('Weekend'=4,'Weekday'=16,
                              'highlight' = 16),
                     breaks=c("Weekday",'Weekend'))+
  scale_size_manual(values=c('Weekend'=3,'Weekday' = 1,'highlight'=3), guide='none')+
  scale_color_manual(values=c('red'='red','blue'='blue'), na.value='black',
                     guide='none') + 
  theme_minimal() + labs(x='', y='', fill='', shape='') + 
  theme(legend.box.background = element_rect(color='black'),
        legend.spacing.y =unit(0,'pt'),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14),
        legend.position = 'top') + 
  facetted_pos_scales(y=position_scales)
p1



#### Actogram indicative example ####


prepare_actogram <- function(short_dt) {
  short_dt[, timestamp_10min :=floor_date(short_dt$timestamp, unit='10 minutes')]
  agg_activity <-  short_dt[, lapply(.SD, mean), by=timestamp_10min,
                            .SDcols = c('movement','movement_abs','activity',
                                        'ENMO','temperature', 'nonwear')]
  dates <- unique(date(agg_activity$timestamp_10min))
  agg_activity[, cal_date := date(agg_activity$timestamp_10min) %>% as.factor() %>% ordered()]
  agg_activity$timeofday = paste0(today(), 'T', 
                                  format(agg_activity$timestamp_10min,
                                         format='%H:%M:%S')) %>%
    as.POSIXct(format='%Y-%m-%dT%H:%M:%S', tz='UTC')
  
  agg_activity[, movement_nonwear := ifelse(nonwear > .5, NA, movement)]
  agg_activity[, dayofweek := weekdays(as.Date(timestamp_10min), abbreviate = T)]
  return(agg_activity)
}

filename <- file_dt[record_id_shifted == 3 & cycle==2]$filename
load(paste0(config$actigraphy_output_dir, filename, "/short_dt.rda"))
agg_activity <- prepare_actogram(short_dt)
date_ls <- unique(agg_activity$cal_date)
date_choices <- as.Date(date_ls[4:11]) 
dt[record_id_shifted == 3 & record_date %in% date_choices,
   .(pain_mn = mean(pain_total,na.rm=T),
     bfi_mn = mean(bfi_total,na.rm=T))]
weekday_ls <- date_choices %>% sapply(weekdays)
sub_dt <- agg_activity[cal_date %in% date_choices]
actogram1 <- ggplot(sub_dt, aes(x = timeofday, y = cal_date, fill= movement_nonwear)) + 
  geom_tile() + theme_minimal() +
  scale_fill_gradient(low='#AD0D1C', high='#FFFFC6', trans='log10', na.value = NA,
                      breaks=c(0.03,0.3), labels=c('Low','High'),
                      name='Activity level') + 
  scale_x_datetime(
    labels=date_format("%H:%M", tz = "GMT")) +
  scale_y_discrete(limits=rev, labels=rev(weekday_ls)) +
  labs(x = 'Time of day', y='')

actogram1 <- ggplot(sub_dt, aes(y = as.numeric(timeofday), x = as.POSIXct(cal_date, tz='UTC'),
                                fill= movement_nonwear)) + 
  geom_tile() + theme_minimal() +
  scale_fill_gradient(low='#AD0D1C', high='#FFFFC6', trans='log10', na.value = NA,
                      breaks=c(0.03,0.3), labels=c('Low','High'),
                      name='Activity level') + 
  scale_y_reverse(label=~ format(as.POSIXct(.x), '%H:%M'), 
                  breaks= as.numeric(seq(min(sub_dt$timeofday), max(sub_dt$timeofday) + 600, by='6 hours'))) + 
  scale_x_datetime(labels=date_format('%b %d\n(%a)'), date_breaks = '1 day') +
  labs(y = 'Time of day', x='') 
actogram1


filename <- file_dt[record_id_shifted == 12 & cycle==3]$filename
load(paste0(config$actigraphy_output_dir, filename, "/short_dt.rda"))
agg_activity <- prepare_actogram(short_dt)
date_ls <- unique(agg_activity$cal_date)
date_choices <- as.Date(date_ls[10:17])
dt[record_id_shifted == 12 & record_date %in% date_choices,
   .(pain_mn = mean(pain_total,na.rm=T),
     bfi_mn = mean(bfi_total,na.rm=T))]
weekday_ls <- date_choices %>% sapply(weekdays)
sub_dt <- agg_activity[cal_date %in% date_choices]
actogram2 <- ggplot(sub_dt, aes(x = timeofday, y = cal_date, fill= movement_nonwear)) + 
  geom_tile() + theme_minimal() +
  scale_fill_gradient(low='#AD0D1C', high='#FFFFC6', trans='log10', na.value = NA,
                      breaks=c(0.03,0.3), labels=c('Low','High'),name='Activity level') + 
  scale_x_datetime(
                   labels=date_format("%H:%M", tz = "GMT")) +
  scale_y_discrete(limits=rev, labels=rev(weekday_ls)) +
  labs(x = 'Time of day', y='') 
actogram2
actogram2 <- ggplot(sub_dt, aes(y = as.numeric(timeofday), x = as.POSIXct(cal_date, tz='UTC'),
                                fill= movement_nonwear)) + 
  geom_tile() + theme_minimal() +
  scale_fill_gradient(low='#AD0D1C', high='#FFFFC6', trans='log10', na.value = NA,
                      breaks=c(0.03,0.3), labels=c('Low','High'),
                      name='Activity level') + 
  scale_y_reverse(label=~ format(as.POSIXct(.x), '%H:%M'), 
                  breaks= as.numeric(seq(min(sub_dt$timeofday), max(sub_dt$timeofday) + 600, by='6 hours'))) + 
  scale_x_datetime(labels=date_format('%b %d\n(%a)'), date_breaks = '1 day') +
  labs(y = 'Time of day', x='') 


  
