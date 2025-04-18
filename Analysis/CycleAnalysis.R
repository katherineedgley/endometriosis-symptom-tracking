source('./Analysis/Functions/naming_cols.R')

#### Load in data #####

combined_symptom_dt <- fread(paste0(config$processed_output_dir, "symptom_cycle_dt.csv"),
                             colClasses = c("record_id" = 'character'))
combined_dt <- fread(paste0(config$processed_output_dir,"combined_cycle_dt.csv"),
                     colClasses = c("record_id" = 'character'))
pat_dt <- fread(paste0(config$processed_output_dir, "combined_pat_dt.csv"),
                         colClasses = c("record_id" = 'character'))
pat_excl_dt <- fread(paste0(config$processed_output_dir, "combined_pat_excl_deepsurgery.csv"),
                     colClasses = c('record_id' = 'character'))


dt <- fread(paste0(config$processed_output_dir, "/allmetrics_with_shifted.csv"),
            colClasses = c("record_id" = 'character'))



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

##### Plots by cycle type #####
p1 <- ggplot(combined_dt[!grepl('surg|Surg', cycle_type)], 
       aes(x=pain_total_mn, y=pain_total_sd, 
           color=cycle_type, shape=factor(hrt_label)), size=3, alpha=1) + 
  scale_shape_manual(values=c(8),labels=c('With add-back HRT'), na.value=19,
                     breaks=c(TRUE))+
  geom_point() + theme_minimal() +
  labs(x='Mean global pain', y='Std. of global pain')+
  theme(legend.title=element_blank(),
        legend.text=element_text(size=14),
        axis.title = element_text(size=14)) +
  guides(color = guide_legend(override.aes = list(size = 2.5)))
p2 <- ggplot(combined_dt[!grepl('surg|Surg', cycle_type)], 
             aes(x=bfi_total_mn, y=bfi_total_sd, color=cycle_type,
                 shape=factor(hrt_label)), size=3, alpha=1) + 
  scale_shape_manual(values=c(8),labels=c('With add-back HRT'), na.value=19,
                     breaks=c(TRUE))+
  geom_point() + theme_minimal() + 
  labs(x='Mean global BFI', y='Std. of global BFI')+
  theme(legend.title=element_blank(),
        legend.text=element_text(size=14),
        axis.title = element_text(size=14)) +
  guides(color = guide_legend(override.aes = list(size = 2.5)))

ggarrange(p1,p2, nrow=1, common.legend = T)


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
color_values <- c('No surgical treatment' = cbPalette[1],
                  "Surgical treatment\nin prev. 3 years"=cbPalette[2],
                  'Surgical treatment\nover 3 years ago'=cbPalette[4])

legend_color_size <- list(size=2.5)
legend_shape_size <- list(size=2.5)
p1 <- ggplot(combined_dt, aes(x=cycle_type, y=pain_total_mn)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=.1, alpha=.7, size=1,aes(shape=factor(hrt_label),  
                                     color=surgery_base_type)) +
  scale_shape_manual(values=c(8),breaks=TRUE,labels=c("With add-back HRT"),
                     na.value=19,name='') +
  scale_color_manual(values=color_values,
                     name='')+
  scale_x_discrete(labels=label_wrap(10)) +
  theme_minimal() +
  labs(x='', y='Mean global pain') +
  theme(legend.text = element_text(size=12)) +
  guides(color = guide_legend(override.aes = legend_color_size),
         shape= guide_legend(override.aes = legend_shape_size))


p2 <- ggplot(combined_dt, aes(x=cycle_type, y=bfi_total_mn)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=.1, alpha=.7, size=1,
              aes(shape=factor(hrt_label),  color=surgery_base_type)) +
  scale_shape_manual(values=c(8),breaks=TRUE,labels=c("With add-back HRT"),
                     na.value=19,name='') +
  scale_color_manual(values=color_values,
                     name='')+
  scale_x_discrete(labels=label_wrap(10)) +
  theme_minimal() +
  labs(x='', y='Mean global BFI') +
  theme(legend.text = element_text(size=12)) +
  guides(color = guide_legend(override.aes = legend_color_size),
         shape= guide_legend(override.aes = legend_shape_size))


p3 <- ggplot(combined_dt, aes(x=cycle_type, y=pain_total_sd)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=.1, alpha=.7,size=1,
              aes(shape=factor(hrt_label),  color=surgery_base_type)) +
  scale_shape_manual(values=c(8),breaks=TRUE,labels=c("With add-back HRT"),
                     na.value=19,name='') +
  scale_color_manual(values=color_values,
                     name='')+
  scale_x_discrete(labels=label_wrap(10)) +
  theme_minimal() +
  labs(x='', y='Std. of global pain') +
  theme(legend.text = element_text(size=12)) + 
  guides(color = guide_legend(override.aes = legend_color_size),
         shape= guide_legend(override.aes = legend_shape_size))

p4 <- ggplot(combined_dt, aes(x=cycle_type, y=bfi_total_sd)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=.1, alpha=.7,size=1,
              aes(shape=factor(hrt_label),  color=surgery_base_type)) +
  scale_shape_manual(values=c(8),breaks=TRUE,labels=c("With add-back HRT"),
                     na.value=19,name='') +
  scale_color_manual(values=color_values,
                     name='')+
  scale_x_discrete(labels=label_wrap(10)) +
  theme_minimal() +
  labs(x='', y='Std. of global BFI') +
  theme(legend.text = element_text(size=12)) + 
  guides(color = guide_legend(override.aes = legend_color_size),
         shape= guide_legend(override.aes = legend_shape_size))


ggarrange(p1,p2,p3,p4, nrow=2, ncol=2, common.legend=T,
          legend='top')

combined_dt[, surg_within_3yrs := (!is.na(last_surg_treat_timediff_base) &  
                                     last_surg_treat_timediff_base <= 3) |
              (!is.na(surg_cycle) & cycle > surg_cycle)]
combined_dt[is.na(surg_within_3yrs), surg_within_3yrs := FALSE]



# Same cycle comparison for actigraphy measures
p1 <- ggplot(combined_dt, aes(x=cycle_type, y=SleepDurationInSpt_GGIR_sd)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=.1, alpha=.7,size=1,
              aes(shape=factor(hrt_label),  color=surgery_base_type)) +
  scale_shape_manual(values=c(8),breaks=TRUE,labels=c("With add-back HRT"),
                     na.value=19,name='') +
  scale_color_manual(values=color_values,
                     name='')+
  theme_minimal() +
  labs(x='', y='Std. of sleep duration') +
  scale_x_discrete(labels=label_wrap(10)) +
  theme(legend.text = element_text(size=12)) + 
  guides(color = guide_legend(override.aes = legend_color_size),
         shape= guide_legend(override.aes = legend_shape_size))

p2 <- ggplot(combined_dt, aes(x=cycle_type, y=SleepRegularityIndex_GGIR_mn)) +   
  geom_boxplot(outliers = F) +
  geom_jitter(width=.1, alpha=.7,size=1,
              aes(shape=factor(hrt_label),
                                     color=surgery_base_type)) +
  scale_shape_manual(values=c(8),breaks=TRUE,labels=c("With add-back HRT"),
                     na.value=19,name='') +
  scale_color_manual(values=color_values,
                     name='')+
  theme_minimal() +
  labs(x='', y='Mean sleep regularity') +
  scale_x_discrete(labels=label_wrap(10)) +
  theme(legend.text = element_text(size=12)) + 
  guides(color = guide_legend(override.aes = legend_color_size),
         shape= guide_legend(override.aes = legend_shape_size))

p3 <- ggplot(combined_dt, aes(x=cycle_type, y=M10_self_mn)) + 
  geom_boxplot(outliers = F) +
  geom_jitter(width=.1, alpha=.7,size=1,
              aes(shape=factor(hrt_label),
                                     color=surgery_base_type)) +
  scale_shape_manual(values=c(8),breaks=TRUE,labels=c("With add-back HRT"),
                     na.value=19,name='') +
  scale_color_manual(values=color_values,
                     name='')+
  theme_minimal() +
  labs(x='', y='Mean M10') +
  scale_x_discrete(labels=label_wrap(10)) +
  theme(legend.text = element_text(size=12)) + 
  guides(color = guide_legend(override.aes = legend_color_size),
         shape= guide_legend(override.aes = legend_shape_size))

p4 <- ggplot(combined_dt, aes(x=cycle_type, y=WASO_GGIR_mn)) + 
  geom_boxplot(outliers = F) +
  geom_jitter(width=.1, alpha=.7,size=1,
              aes(shape=factor(hrt_label),
                                     color=surgery_base_type)) +
  scale_shape_manual(values=c(8),breaks=TRUE,labels=c("With add-back HRT"),
                     na.value=19,name='') +
  scale_color_manual(values=color_values,
                     name='')+
  theme_minimal() +
  labs(x='', y='Mean sleep disturbance (WASO)') +
  scale_x_discrete(labels=label_wrap(10)) + 
  theme(legend.text = element_text(size=12)) + 
  guides(color = guide_legend(override.aes = legend_color_size),
         shape= guide_legend(override.aes = legend_shape_size))

ggarrange(p1,p2,p3,p4, nrow=2, ncol=2, common.legend=T,
          legend='bottom')

#### Correlations using only symptoms ####


combined_symptom_dt %>% colnames()
summary_cols <- combined_symptom_dt[, 4:107] %>% colnames()
cycle_cols <- combined_symptom_dt[, .(ehp30_overall, pain_ehp30,control_ehp30, self_ehp30,
                                      emotion_ehp30, social_ehp30)] %>% colnames()
ehp30_cols <- cycle_cols[grepl('ehp30', cycle_cols)]
cor_mat <- combined_symptom_dt[, mget(c(summary_cols, cycle_cols))] %>%
  cor(method='spearman', use='pairwise.complete.obs')
cor_mat <- get_cor_mat(combined_symptom_dt,
            summary_cols,cycle_cols, cor_method='spearman')
cor_dt <- cor_mat[[1]]
df_dt <- cor_mat[[2]]
chosen_cols <- summary_cols[grepl('mn',summary_cols) & !grepl('upper|lower|sd', summary_cols)]
cor_sub <- cor_dt[rowname %in% chosen_cols, mget(c('rowname',cycle_cols))]
cor_sub[, strong_cor := apply(.SD, MARGIN=1, FUN=function(x){max(abs(x)) > .3}), .SDcols=ehp30_cols]
cor_sub <- cor_sub[strong_cor == T, !c('strong_cor')]
cor_sub <- cor_sub %>% plyr::rename(replace=ehp30_name_mapping)
cor_mat <- as.matrix(cor_sub, rownames='rowname')

rownames(cor_mat) <- cor_colnames$cor_colnames[match(rownames(cor_mat), cor_colnames$orig)]
strong_mat <- abs(cor_mat) < .3
png(filename = paste0(config$plots_output_dir, 'quest_all_corrs.png'),
    width=5, height=4, units='in',res=300)
t(cor_mat) %>% corrplot(tl.cex=1, tl.col='black', p.mat=t(strong_mat), insig = 'blank',
                        method='number', number.cex=.6)
dev.off()


chosen_cols <- summary_cols[grepl('bfi|pain_total',summary_cols)]
cor_sub <- cor_dt[rowname %in% chosen_cols, mget(c('rowname',cycle_cols))]
cor_sub[, strong_cor := apply(.SD, MARGIN=1, FUN=function(x){max(abs(x)) > .3}), .SDcols=ehp30_cols]
cor_sub <- cor_sub[strong_cor == T, !c('strong_cor')]
cor_sub <- cor_sub[grepl('pain', rowname),] %>% rbind(cor_sub[grepl('bfi', rowname),])
cor_sub <- cor_sub %>% plyr::rename(replace=ehp30_name_mapping)
cor_sub$rowname <- transform_summary_cols(cor_sub$rowname)
cor_mat <- as.matrix(cor_sub, rownames='rowname')
strong_mat <- abs(cor_mat) < .3
t(cor_mat) %>% corrplot(tl.cex=1, tl.col='black', p.mat=t(strong_mat), insig = 'blank',
                        cl.ratio=.15,cl.cex=.6,
                        method='number', number.cex=.5)# method='number', number.cex=.5)


#### Plot relationship between daily and end-of-cycle symptoms ####

melted_dt <- combined_symptom_dt[, .(pain_avg_mn, pain_worst_mn, pain_worst_upper_mn,
                                     pain_ehp30)] 
colnames(melted_dt)[1:3] <- c("Avg. pain","Worst pain",
                              "Upper worst pain" )
melted_dt <- melted_dt %>% melt(id.vars=c("pain_ehp30")) %>% as.data.table()

ggplot(melted_dt, aes(x=value, y=pain_ehp30, group=variable, color=variable)) +
  geom_point(alpha=.6) + 
  geom_smooth(method='lm',formula=y~x+I(x^2)) + theme_minimal()

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p1 <- ggplot(combined_symptom_dt, aes(x=pain_avg_mn, y=pain_ehp30)) + 
  geom_point(color=cbPalette[2]) + 
  geom_smooth(method='lm',formula=y~x+I(x^2), #fill=cbPalette[2],
              color=cbPalette[2]) + stat_cor(method='spearman',label.x=7.5,
                                             label.y=20, aes(label=..r.label..)) + 
  theme_minimal() + labs(x="Mean of 'average pain'\n(Avg. pain)", y='EHP-30 pain')+
  xlim(1,10)
p2 <- ggplot(combined_symptom_dt, aes(x=pain_worst_mn, y=pain_ehp30)) + 
  geom_point(color=cbPalette[4]) + 
  geom_smooth(method='lm',formula=y~x+I(x^2), #fill=cbPalette[4],
              color=cbPalette[4]) + stat_cor(method='spearman',label.x=7.5,
                                             label.y=20, aes(label=..r.label..)) + 
  theme_minimal() + labs(x="Mean of 'worst pain'\n(Worst pain)", y='EHP-30 pain') +
  xlim(1,10)
p3 <- ggplot(combined_symptom_dt, aes(x=pain_worst_upper_mn, y=pain_ehp30)) + 
  geom_point(color=cbPalette[3]) + 
  geom_smooth(method='lm',formula=y~x+I(x^2), #fill=cbPalette[3],
              color=cbPalette[3]) + stat_cor(method='spearman',label.x=7.5,
                                             label.y=20, aes(label=..r.label..)) + 
  theme_minimal() + labs(x="Mean of upper 25% 'worst pain' days\n(Upper worst pain)", y='EHP-30 pain') +
  xlim(1,10)
color_map <- c("Upper worst pain"=cbPalette[3],
               "Worst pain"=cbPalette[4],
               "Avg. pain"=cbPalette[2])
p4 <- ggplot(melted_dt, aes(x=value, y=pain_ehp30, color=variable)) + 
  geom_smooth(method='lm',formula=y~x+I(x^2)) + stat_cor(method='spearman',
                                                         aes(label=..r.label..),
                                                         show.legend = F) + 
  scale_color_manual(values=color_map) + theme_minimal()+
  labs(x='Daily pain summary measure', y='EHP-30 pain')+
  theme(legend.position  = c(.73,.25),
        legend.title = element_blank()) +
  xlim(1,10)
p4
grid.arrange(p1,p2,p3,p4, nrow=2, ncol=2)





#### Relationships with age and BMI - confounders ####


combined_dt[, 
            .(dur_day_total_IN_min_GGIR_mn, dur_day_total_LIG_min_GGIR_mn, dur_day_total_MOD_min_GGIR_mn,
              dur_day_total_VIG_min_GGIR_mn, bfi_total_mn, pain_total_mn, ehp30_overall, 
              record_id, cycle, age_base, bmi_base, start_date, post_deep_surgery, M10_self_mn,
              watch_dom_wrist)]
combined_dt[, .N, by=.(watch_dom_wrist)]
combined_dt[, ]
# M10 comparisons
combined_dt[, watch_wrist_label := ifelse(watch_dom_wrist == T, 'Dominant\nwrist', 'Non-dominant\nwrist')]
combined_dt[is.na(watch_dom_wrist), watch_wrist_label := NA]
p1 <- ggplot(combined_dt[!is.na(watch_dom_wrist)], aes(x = watch_wrist_label, y=M10_self_mn)) + 
  geom_boxplot() + theme_minimal() +
  labs(x='', y='M10') +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16))
p1
p2 <- ggplot(combined_dt[!is.na(watch_dom_wrist)], aes(x = watch_wrist_label, y=RA_self_mn)) + 
  geom_boxplot() + theme_minimal() +
  labs(x='', y='Relative amplitude\n(RA)') +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16))
p2
ggplot(combined_dt, aes(x = factor(watch_dom_wrist), y=M5VALUE_GGIR_mn)) + 
  geom_boxplot()
# MVPA comparisons:
p3 <- ggplot(combined_dt[!is.na(watch_dom_wrist)], 
             aes(x = watch_wrist_label, y=ACC_day_total_MOD_mg_GGIR_mn)) + 
  geom_boxplot() + theme_minimal() +
  labs(x='', y='Acceleration in\nmoderate activity') +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14))
p3

p4 <- ggplot(combined_dt[!is.na(watch_dom_wrist)], 
             aes(x = watch_wrist_label, y=ACC_day_MVPA_bts_1_5_mg_GGIR_mn)) + 
  geom_boxplot() + theme_minimal() +
  labs(x='', y='Acceleration in MVPA\n(bouts 1-5 min)') +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14))
p5 <- ggplot(combined_dt[!is.na(watch_dom_wrist)], 
             aes(x = watch_wrist_label, y=ACC_day_MVPA_bts_5_10_mg_GGIR_mn)) + 
  geom_boxplot() + theme_minimal() +
  labs(x='', y='Acceleration in MVPA\n(bouts 5-10 min)') +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14))
p5
p6 <- ggplot(combined_dt[!is.na(watch_dom_wrist)], 
             aes(x = watch_wrist_label, y=ACC_day_MVPA_bts_10_mg_GGIR_mn)) + 
  geom_boxplot() + theme_minimal() +
  labs(x='', y='Acceleration in MVPA\n(bouts > 10 min)') +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14))
p6


ggarrange(p1,p2,p3,p4, p5, p6, nrow=2, ncol=3)


##### Look at age/BMI correlations and confounding factors #####
summary_cols <- colnames(pat_dt)[2:ncol(pat_dt)]
summary_cols <- summary_cols[!grepl("_base|record_id", summary_cols)]
nonnum_cols <- summary_cols[pat_dt[, mget(summary_cols)] %>% lapply(class) %>% unlist() != 'numeric']
novar_cols <- summary_cols[pat_dt[, lapply(.SD, sd, na.rm=T) == 0, .SDcols=summary_cols]]
summary_cols <- summary_cols[!summary_cols %in% c(novar_cols, nonnum_cols)]
symptom_summary_cols <- summary_cols[summary_cols %in% colnames(combined_symptom_dt)]


cor_res <- get_cor_mat(pat_dt, summary_cols,  c('bmi_base','age_base'))
cor_dt <- cor_res[[1]]

cor_dt[, strong_cor := apply(.SD, MARGIN=1, FUN=function(x){max(abs(x)) > .3}), .SDcols=c("bmi_base","age_base")]
cor_dt[abs(bmi_base) > .3][order(-abs(bmi_base))][1:30]
cor_dt[abs(bmi_base) > .3 & rowname %in% symptom_summary_cols]
cor_dt[abs(age_base) > .3][order(-abs(age_base))][1:50]
cor_dt[abs(age_base) > .3 & rowname %in% symptom_summary_cols]
cor_dt[abs(age_base) > .3 & abs(bmi_base) > .3][order(-abs(age_base))]

p1 <- ggplot(pat_dt, aes(y=ACC_day_total_VIG_mg_GGIR_mn, x=bmi_base)) + 
  geom_point() + stat_smooth(method='lm') + 
  stat_cor(method='spearman',label.y=350,
                          aes(label=..r.label..)) + theme_minimal() + 
  labs(x='BMI', y='Mean vigorous PA (day)')
p2 <- ggplot(pat_dt, aes(y=sleep_zenith_temp_self_mn, x=bmi_base)) + 
  geom_point() + stat_smooth(method='lm') + 
  stat_cor(method='spearman', label.y=33.5,
                          aes(label=..r.label..)) + theme_minimal() + 
  labs(x='BMI', y='Mean max. sleep temp.')
p3 <- ggplot(pat_dt, aes(x=age_base, y=ehp30_overall, label=record_id))+ 
  geom_point() + stat_smooth(method='lm') + 
  stat_cor(method='spearman', label.y = 25,
                          aes(label=..r.label..)) + theme_minimal() + 
  labs(x='Age', y='EHP-30 total')
p4 <- ggplot(pat_dt, aes(x=age_base, y=Nblocks_spt_sleep_GGIR_med, label=record_id))+ 
  geom_point() + stat_smooth(method='lm') + 
  stat_cor(method='spearman', label.y=12,
                          aes(label=..r.label..)) + theme_minimal() + 
  labs(x='Age', y="Med. no. sleep bouts")
p4

ggarrange(p1,p2,p3,p4, nrow=2, ncol=2)




#### Correlations using all act variables ####

summary_cols <- combined_symptom_dt[, 4:104] %>% colnames()
cycle_cols <- combined_dt[, .(ehp30_overall, pain_ehp30,control_ehp30, self_ehp30,
                              emotion_ehp30, social_ehp30
                             )] %>% colnames()

report_cols <- c(summary_cols,cycle_cols)

combined_dt %>% colnames()
ggir_cols <- colnames(combined_dt)[grepl("GGIR", colnames(combined_dt))]
ggir_cols <- ggir_cols[lapply(combined_dt[, mget(ggir_cols)], class) == 'numeric']
self_cols <- colnames(combined_dt)[grepl('self', colnames(combined_dt)) &
                                     !grepl('eq5d|ehp30', colnames(combined_dt))]

act_cols <- c(ggir_cols, self_cols)
cr <- cor(combined_dt[, mget(act_cols)],
          combined_dt[, mget(report_cols)], use='pairwise.complete.obs',
    method='spearman')
cr[upper.tri(cr, diag=TRUE)] <- NA
cors_cycle <- reshape2::melt(cr, na.rm=TRUE, value.name="cor") %>% as.data.table()
rm(cr)
cors_cycle <- cors_cycle[abs(cor) > .3]
cors_cycle[order(-abs(cor))][1:99]
cors_cycle[grepl('mn', Var1) & grepl('mn', Var2) &
             !grepl('upper|lower', Var1)][order(-abs(cor))][1:90]


combined_dt[, MVPA_GGIR_total := dur_day_total_MOD_min_GGIR_mn + dur_day_total_VIG_min_GGIR_mn]


R1 <- combined_dt[, .(ACC_day_MVPA_bts_10_mg_GGIR_mn, pain_total_upper_mn, watch_dom_wrist,
                     age_base, bmi_base)] %>%
  na.omit %>% pcor(method='spearman') 
p1 <- ggplot(combined_dt, aes(x=ACC_day_MVPA_bts_10_mg_GGIR_mn, y=pain_total_upper_mn)) + 
  geom_point() + 
  annotate("text", label = paste0("R = ", round(R1[[1]][1,2], 2)),
                                  #"\n(N=", R1$n, ")"), 
           x = 200, y=9) +
#stat_cor(method='spearman', label.x = 170, aes(label=..r.label..)) +
  stat_smooth(method='lm') + 
  theme_minimal() + 
  labs(x='Moderate-to-vigorous\nactivity (daytime) - mean',
       y='Global pain - mean of top 25%')
p1
R2 <- combined_dt[, .(dur_spt_wake_LIG_min_GGIR_upper, bfi_total_upper_mn, 
                     watch_dom_wrist,
                     age_base, bmi_base)] %>%
  na.omit %>% pcor(method='spearman') 
p2 <- ggplot(combined_dt, aes(x=dur_spt_wake_LIG_min_GGIR_upper,
                              y=bfi_total_upper_mn)) + 
  geom_point() + 
  annotate("text", label = paste0("R = ", round(R2[[1]][1,2], 2)),
                                  #"\n(N=", R2$n, ")"), 
           x=11, y=1) +
  # stat_cor(method='spearman', label.y=1, label.x=11,
  #                         aes(label=..r.label..)) + 
  stat_smooth(method='lm') + 
  theme_minimal() + 
  labs(x='Dur. of wake (light activity) in sleep period -\nupper quartile', 
       y='Global BFI - mean of top 25%')
p2
R3 <- combined_dt[, .(WASO_GGIR_sd, fatigue_24hr_rmssd, 
                     watch_dom_wrist,
                     age_base, bmi_base)] %>%
  na.omit %>% pcor(method='spearman')
p3 <- ggplot(combined_dt, 
             aes(x=WASO_GGIR_sd, y=fatigue_24hr_rmssd, label=record_id)) + 
  geom_point() + 
  annotate("text", label = paste0("R = ", round(R3[[1]][1,2], 2)),
                                  #"\n(N=", R3$n, ")"),
                                  x=1.0, y=3.5) +
  # stat_cor(method='spearman', label.y=3.5,
  #                         aes(label=..r.label..)) + 
  stat_smooth(method='lm') +
  theme_minimal() + 
  labs(x='WASO - std.', y='Avg. fatigue - std.')
p3
R4 <- combined_dt[, .(IV2_self_upper, fatigue_walking_sd,
                     watch_dom_wrist,
                     age_base, bmi_base)] %>%
  na.omit %>% pcor(method='spearman') #%>% `[[`(1)
p4 <- ggplot(combined_dt, 
             aes(x=IV2_self_upper, y=fatigue_walking_sd, label=record_id)) + 
  geom_point() + 
  annotate("text", label = paste0("R = ", round(R4[[1]][1,2], 2)),
                                  #"\n(N=", R4$n, ")"),
           x=0.35, y=3.2) +
  # stat_cor(method='spearman', aes(label=..r.label..)) +
  stat_smooth(method='lm') +
  theme_minimal() + 
  labs(x='IV2 - upper quartile',
       y='BFI Q4c (walking interference) - std.')
p4

R5 <- combined_dt[, .(guider_onset_GGIR_sd, bfi_total_sd,
                     watch_dom_wrist,
                     age_base, bmi_base)] %>%
  na.omit %>% pcor(method='spearman') # %>% `[[`(1)
p5 <- ggplot(combined_dt, aes(x=guider_onset_GGIR_sd, y=bfi_total_sd)) + 
  geom_point() + 
  annotate("text", label = paste0("R = ", round(R5[[1]][1,2], 2)),
                                  #"\n(N=", R5$n, ")"),
           x=4.4, y=3.1) +
  # stat_cor(method='spearman', label.x=4.4, label.y=3.1,
  #                         aes(label=..r.label..)) +
  stat_smooth(method='lm') +
  theme_minimal() + 
  labs(x='Time of sleep onset - std.',
       y='Global BFI - std.')
p5

R6 <- combined_dt[, .(wake_actrange_TKEOratio_self_iqr, 
                      fatigue_activity_iqr,
                      watch_dom_wrist,
                      age_base, bmi_base)] %>%
  na.omit %>% pcor(method='spearman') #%>% `[[`(1)
p6 <- ggplot(combined_dt, aes(x=wake_actrange_TKEOratio_self_iqr, 
                              y=fatigue_activity_iqr)) + 
  geom_point() + 
  annotate("text", label = paste0("R = ", round(R6[[1]][1,2], 2)),
                                  #"\n(N=", R6$n, ")"),
           x=0.05, y=5.5) +
  # stat_cor(method='spearman', aes(label=..r.label..)) + 
  stat_smooth(method='lm') +
  theme_minimal() + 
  labs(x='Daytime proportion\nof activity TKEO - IQR',
       y='BFI Q4a (activity interference) - IQR')
p6

ggarrange(p1,p2,p3,p4,p5,p6, nrow=2, ncol=3)






