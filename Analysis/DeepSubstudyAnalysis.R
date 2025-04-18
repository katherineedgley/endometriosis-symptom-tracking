
#### Load main files/dirs ####

config <- read.config(file="./processing_config.yaml")

diary_dt <- fread(paste0(config$processed_output_dir, "diary_dt.csv"),
                  colClasses = c('record_id'='character'))

file_dt <- fread(paste0(config$source_data_dir, "file_dt.csv"),
                 colClasses = c('record_id'='character',
                                'start_date'='IDate',
                                'end_date' = 'IDate'))

dt <- fread(paste0(config$processed_output_dir, "allmetrics_dt.csv"),
            colClasses = c('record_id' = "character"))

combined_symptom_dt <- fread(paste0(config$processed_output_dir,"symptom_cycle_dt.csv"),
                             colClasses = c("record_id" = 'character'))
combined_dt <- fread(paste0(config$processed_output_dir,"combined_cycle_dt.csv"),
                     colClasses = c("record_id" = 'character'))

# fill missing nightshifts to 0 (checked that all 0 at baseline)
combined_dt[is.na(yn_night_shifts) & cycle != 0, yn_night_shifts := 0]

base_dt <- fread(paste0(config$processed_output_dir,"baseline_dt.csv"),
                 colClasses = c("record_id"='character'))
deep_dt <- fread(paste0(config$processed_output_dir,"deep_dt.csv"),
                 colClasses = c('record_id'='character'))

post_surg_pats <- diary_dt[post_deep_surgery == T]$record_id %>% unique()
deep_dt[, post_surg := record_id %in% post_surg_pats]
deep_dt[deep_diag == T] %>% nrow() # Number of participants with deep/ovarian diag

# only look at those that actually had deep endo diagnosis
deep_dt <- deep_dt[deep_diag == T & post_surg == T]
deep_dt %>% nrow() #  Participants with deep/ovarian diag and cycles post surgery
deep_study_pats <- deep_dt$record_id

deep_dt[, TLH := grepl('TLH|hysterectomy|Hysterectomy|Total Laparoscopic Hysterectomy|TAH', crf_v3_surg_other_detials)]
deep_dt[, BSO := grepl('BSO|ooph', crf_v3_surg_other_detials)]
deep_dt[, .N, by=TLH]
deep_dt[, .N, by=BSO]

deep_dt[, .(record_id, TLH, BSO)]
diary_dt[record_id %in% deep_study_pats, .(start_date = first(record_date)),
         by=.(record_id,cycle)][, diff(start_date)[2], by=record_id]$V1 %>% mean(na.rm=T)
diary_dt[record_id %in% deep_study_pats, .(start_date = first(record_date)),
         by=.(record_id,cycle)][, diff(start_date)[2], by=record_id]$V1 %>% min(na.rm=T)
diary_dt[record_id %in% deep_study_pats, .(start_date = first(record_date)),
         by=.(record_id,cycle)][, diff(start_date)[2], by=record_id]$V1 %>% max(na.rm=T)

complete_deep_pats <- dt[record_id %in% deep_study_pats & cycle==3]$record_id %>% unique()
sub_dt <- dt[record_id %in% deep_study_pats]
sub_diary <- diary_dt[record_id %in% deep_study_pats]
deep_dt <- deep_dt %>% 
  merge(sub_dt[cycle==3, .(hrt_taken_followup = sum(hrt_taken == 1) > 0),
               by=record_id],by='record_id', all=T) %>%
  merge(sub_dt[cycle==1, .(gnrh_taken_cycle1 = sum(gnrh_taken)>0,
                           hrt_taken_cycle1 = sum(hrt_taken) > 0), by=record_id],
        by='record_id',all=T) %>%
  merge(sub_dt[, .(gnrh_taken_base = unique(gnrh_taken_base)), by=record_id], 
        by='record_id', all=T)

# check that no differences between baseline GnRH and first cycle
stopifnot(deep_dt$gnrh_taken_base == deep_dt$gnrh_taken_cycle1)
deep_dt[, .(record_id, TLH, BSO, gnrh_taken_cycle1, hrt_taken_cycle1,
            hrt_taken_followup)]
sub_dt[, `:=`(ndays_diaries = sum(!is.na(pain_avg)),
              ndays_act_valid = sum(!is.na(M10)),
              ndays_SRI_valid = sum(!is.na(SleepRegularityIndex_GGIR)),
              ndays_sleep_valid = sum(!is.na(SleepDurationInSpt_GGIR))), by=.(record_id,cycle)]
sub_diary[, `:=`(ndays_diaries = sum(!is.na(pain_avg))), by=.(record_id,cycle)]


valid_dt <- unique(sub_dt[, .(record_id,cycle,ndays_act_valid, 
                              ndays_SRI_valid, ndays_sleep_valid)]) %>%
  merge(unique(sub_diary[, .(record_id,cycle, ndays_diaries)]), by=c('record_id','cycle'))

combined_dt <- combined_dt %>% merge(valid_dt, by=c('record_id','cycle'), all.x=T, all.y=T)
combined_dt <- combined_dt %>% merge(deep_dt[, .(record_id,TLH, BSO, hrt_taken_followup, 
                                                 gnrh_taken_cycle1, hrt_taken_cycle1)],
                                     by='record_id', all.x=T, all.y=F)

combined_dt$postsurg_cat %>% table()

##### Combined cycle analysis #####


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
combined_dt[!is.na(surg_cycle) & cycle>surg_cycle, cycle_type := 'Surgery followup']

combined_dt[!is.na(surg_cycle) & cycle < surg_cycle, surg_stage_num := 1]
combined_dt[!is.na(surg_cycle) & cycle == surg_cycle, surg_stage_num := 2]
combined_dt[!is.na(surg_cycle) & cycle > surg_cycle, surg_stage_num := 3]

combined_dt[, cycle_stage :=ordered(surg_stage_num, levels=c(1,2,3),labels=c('Baseline','Immediately post-surgery',
                                                                             'Final follow-up'))]

combined_dt[!is.na(cycle_stage)]$deep_pat
combined_dt <- combined_dt %>% 
  merge(combined_dt[deep_pat==T & cycle==3,
                    .(complete_deeppat_sleep_mn =!is.na(SleepDurationInSpt_GGIR_mn),
                      complete_deeppat_sleep_sd = !is.na(SleepDurationInSpt_GGIR_sd),
                      complete_deeppat_SRI = !is.na(SleepRegularityIndex_GGIR_mn),
                      complete_deeppat_act_mn  = !is.na(M10_self_mn),
                      complete_deeppat_act_sd  = !is.na(M10_self_sd),
                      complete_deeppat_diary_mn = !is.na(pain_avg_mn),
                      complete_deeppat_diary_sd  = !is.na(pain_avg_sd)), 
                    by=.(record_id)], by='record_id', all.x=T)

sub_combined_dt <- combined_dt[!is.na(surg_stage_num)]

rm(combined_dt)
rm(combined_symptom_dt)

#### Set day of surgery variable and plot by day after #####

dates_dt <- sub_dt[, .(start_date = first(record_date)), by=.(record_id, cycle)] %>%
  merge(deep_dt[, !c('crf_v3_surg_other_detials')], by='record_id')
dates_dt <- dates_dt[cycle==2]
dates_dt[, surg_dayofcycle := difftime(desf_operation_date,start_date, units='day')]

sub_dt[cycle==2, .(sum(!is.na(pain_avg)|!is.na(M10))), by=record_id]
pats2use<- sub_dt[, .(ncycles = uniqueN(cycle)), by=record_id][ncycles>1]$record_id
sub_dt <- sub_dt[record_id %in% pats2use]
sub_dt <- sub_dt %>% merge(dates_dt[, .(record_id, cycle, surg_dayofcycle)], by=c("record_id","cycle"), all=T)

diary_pats2use <- diary_dt[(record_id %in% deep_study_pats)][,(uniqueN(cycle) > 1), by=record_id][V1 == TRUE]$record_id
sub_diary <- diary_dt[record_id %in% diary_pats2use]
dates_dt <- sub_diary[, .(start_date = first(record_date)), by=.(record_id, cycle)] %>%
  merge(deep_dt, by='record_id')
dates_dt <- dates_dt[cycle==2]
dates_dt[, surg_dayofcycle := difftime(desf_operation_date,start_date, units='day')]
sub_diary <- sub_diary %>%
  merge(dates_dt[, .(record_id, cycle, surg_dayofcycle)], by=c("record_id","cycle"), all=T)

sub_dt[, .(firstday = first(record_date)), by=.(record_id,cycle)][, diff(firstday), by=.(record_id)]
sub_dt[day_of_cycle==0, .(ncycle=.N, days = diff(day_of_course)), by=.(record_id)]
sub_diary[, .(firstday = first(record_date)), by=.(record_id,cycle)][, diff(firstday), by=.(record_id)]

sub_dt[, ndays_act_valid := sum(!is.na(M10)), by=.(record_id, cycle)]
sub_dt[, .(ndays_act_valid, record_id, cycle)] %>% unique()
sub_dt[, ncycles := uniqueN(cycle), by=record_id]
sub_dt <- sub_dt[ncycles > 1]

sub_diary[, ndays_diary := sum(!is.na(bfi_total)), by=.(record_id, cycle)]
sub_diary[, .(ndays_diary, record_id, cycle)] %>% unique()
sub_diary <- sub_diary[ndays_diary >= 10] %>% unique()
sub_diary[, ncycles := uniqueN(cycle), by=record_id]
sub_diary <- sub_diary[ncycles > 1]


sub_dt[, dayofweek := weekdays(record_date, abbreviate = T)]
sub_dt[, is_weekend := dayofweek %in% c("Sat","Sun")]
sub_dt[, is_dayoff := is_weekend | holiday==1]



sub_combined_dt[TLH==T & BSO==F, surg_type:= 'Hysterectomy']
sub_combined_dt[TLH==T & BSO==T, surg_type := 'Hysterectomy &\noopherectomy']
sub_combined_dt[TLH==F & BSO==F, surg_type := 'No hysterectomy or\noopherectomy']

sub_combined_dt[yn_night_shifts == 1, surg_with_shiftwork := 'Night shift work']
sub_combined_dt[, shiftwork_gnrh := ifelse(yn_night_shifts == 1, 'Night shift work', NA)]
sub_combined_dt[, shiftwork_gnrh := ifelse(gnrh_taken == 1, 'GnRH agonist', shiftwork_gnrh)]

sub_combined_dt[, hrt_labels := ifelse(hrt_taken_followup, 'HRT taken at follow-up',
                                       NA)]



#### Change in summary measures plots #####


sub_combined_dt[, .(yn_night_shifts, gnrh_taken, cycle, record_id)]

sub_combined_dt[, .(record_id,cycle,TLH, BSO, hrt_taken_followup, gnrh_taken)] %>% 
  unique()

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sub_combined_dt[complete_deeppat_diary_mn == T & cycle!=2, 
                .(pain_total_mn, bfi_total_mn, record_id,cycle_stage)][
                  ,.(diff(bfi_total_mn), diff(pain_total_mn)), by=record_id]
sub_combined_dt[complete_deeppat_act_mn == T & cycle!=2, 
                .(M10_self_mn,WASO_GGIR_mn, SleepRegularityIndex_GGIR_mn,
                  SleepDurationInSpt_GGIR_sd, record_id,cycle_stage)][
                    ,.(diff(M10_self_mn)>0,
                       diff(WASO_GGIR_mn)<0,
                       diff(SleepRegularityIndex_GGIR_mn)>0,
                       diff(SleepDurationInSpt_GGIR_sd)>0), by=record_id]

plot_surgchange <- function(col, colname, filter_col, lims) {
  p1 <- ggplot(sub_combined_dt[get(filter_col) == T & !is.na(get(col))], 
               aes(x=cycle_stage, y=.data[[col]])) + 
    geom_line(aes(x=surg_stage_num, y=.data[[col]], group=record_id,
                  color=surg_type, linetype=hrt_labels),linewidth=.8, alpha=.8) + 
    geom_point(aes(color=surg_type, shape=shiftwork_gnrh,
                   size=shiftwork_gnrh))+
    scale_x_discrete(labels=label_wrap(10)) + 
    scale_shape_manual(values=c('Night shift work'=4,
                                'GnRH agonist' = 8), na.value=16,
                       breaks=c('Night shift work','GnRH agonist')) +
    scale_linetype_manual(values=c('HRT taken at follow-up' = 2), na.value=1,
                          breaks=c('HRT taken at follow-up')) + 
    scale_color_manual(values=c('Hysterectomy'="#D55E00",
                                'Hysterectomy &\noopherectomy' = "#56B4E9",
                                'No hysterectomy or\noopherectomy' = "#999999"))+
    scale_size_manual(values=c('Night shift work' = 3,
                               'GnRH agonist'=3),na.value=2, guide='none')+
    scale_y_continuous(limits=lims)+
    labs(x="", y=colname) + theme_minimal() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          legend.key.width = unit(4, "line"),
          legend.title = element_blank(),
          legend.text = element_text(size=14)) +
    guides(color=guide_legend(override.aes=list(size=5), nrow=2),
           shape = guide_legend(override.aes = list(size=5), nrow=2)) 
  return(p1)
} 


p1 <- plot_surgchange('pain_total_mn', 'Mean global pain',
                      'complete_deeppat_diary_mn', lims=c(1,10))
p2 <- plot_surgchange('bfi_total_mn', 'Mean global BFI',
                      'complete_deeppat_diary_mn', lims=c(0,10))
p3 <- plot_surgchange('M10_self_mn', 'Mean M10',
                      'complete_deeppat_act_mn', lims=NULL)
p4 <- plot_surgchange('SleepDurationInSpt_GGIR_mn', 'Mean sleep duration',
                      'complete_deeppat_act_mn', lims=NULL)
# p5 <- plot_surgchange('SleepDurationInSpt_GGIR_sd', 'Std. of sleep duration',
#                       'complete_deeppat_act_sd', lims=NULL)
p5 <- plot_surgchange('SleepRegularityIndex_GGIR_mn', 'Sleep regularity',
                      'complete_deeppat_act_sd', lims=NULL)
p6 <- plot_surgchange('WASO_GGIR_mn', 'Mean wake after sleep onset (WASO)',
                      'complete_deeppat_act_mn', lims=NULL)

ggarrange(p1,p2,p3,p4,p5, p6,nrow=2, ncol=3, common.legend = T)


#### Change post-operatively ####


sub_combined_dt[, cycle_sleep := ifelse(is.na(SleepDurationInSpt_GGIR_mn), 
                                        NA, cycle)]
sub_combined_dt[, has_2cycles := sum(cycle_sleep==1,na.rm=T)> 0 & sum(cycle_sleep==2,na.rm=T) > 0,
                by=record_id]
sub_combined_dt[, .(record_id,cycle, cycle_sleep, has_2cycles)] %>% unique()
sub_combined_dt[has_2cycles == T, .(record_id)] %>% unique()

sub_combined <- sub_combined_dt[has_2cycles == T & cycle %in% c(1,2)]
sub_combined[, .(record_id,cycle, cycle_sleep, has_2cycles)] %>% unique()
postsurg_ls <- sub_combined$unq_cycle %>% unique()


# adjust day of cycle in 2nd cycle to start 0 as day of surgery
sub_dt[cycle != 2, day_of_cycle_surg := day_of_cycle]
sub_dt[cycle == 2, day_of_cycle_surg := day_of_cycle - surg_dayofcycle]
sub_dt[, unq_cycle := paste0(record_id, '_', cycle)]

sub_postsurg_dt <- sub_dt[unq_cycle %in% postsurg_ls]

col_ls <- c("SleepDurationInSpt_GGIR",'M10','SleepRegularityIndex_GGIR',
            'WASO_GGIR','sleep_efficiency_GGIR','pain_total','bfi_total')
col_names <- c('Sleep dur.', 'M10', 'Sleep regularity',
               'WASO', 'Sleep eff.', 'Global pain','Global fatigue')
sub_postsurg_dt[, (col_names) := lapply(.SD, 
                                        function(x){
                                          (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}),
                .SDcols=col_ls]

postsurg_dt <- sub_postsurg_dt[cycle==1, lapply(.SD, mean, na.rm=T), 
                               .SDcols=col_names, by=record_id] %>%
  merge(sub_postsurg_dt[cycle==1, 
                        .(num_valid = sum(!is.na(SleepRegularityIndex_GGIR)),
                          cycle = 'Baseline'), by=record_id], by='record_id') %>%
  rbind(sub_postsurg_dt[cycle==2 & day_of_cycle_surg <= 10, lapply(.SD, mean, na.rm=T), 
                        .SDcols=col_names, by=record_id] %>%
          merge(sub_postsurg_dt[cycle==2 & day_of_cycle_surg <=10, 
                                .(num_valid = sum(!is.na(SleepRegularityIndex_GGIR)),
                                  cycle = 'Week post-surgery'), by=record_id],
                by='record_id'))

postsurg_dt <- postsurg_dt[order(record_id)]

postsurg_change <- 
  postsurg_dt[, lapply(.SD, diff), .SDcols=col_names,
              by=record_id] %>%
  merge(unique(sub_combined[, .(surg_type, hrt_labels, gnrh_taken_cycle1,record_id)]),
        by='record_id')
postsurg_melted <- postsurg_change %>% melt(id.vars=c('record_id',
                                                      'surg_type','hrt_labels',
                                                      'gnrh_taken_cycle1')) %>%
  as.data.table()
postsurg_melted[, ID := factor(record_id,labels = 1:13)]

ggplot(postsurg_melted,
       aes(x=variable, y=value, label=ID)) +
  geom_boxplot(alpha=.3) + 
  geom_point(aes(color=surg_type, shape=hrt_labels))+
  scale_shape_manual(values = c('HRT taken at follow-up'=17), na.value=16,
                     breaks=c("HRT taken at follow-up")) + 
  geom_text_repel(aes(color=surg_type), max.overlaps = 30,size=3,
                  show.legend=F) +
  geom_hline(aes(yintercept=0), color='red', linetype=2, alpha=.2) + 
  theme_minimal() + 
  labs(x='', y='Change in scaled mean value\nfrom baseline to 10 days post-surgery') + 
  theme(legend.position = 'top',
        legend.title=element_blank(),
        axis.text=element_text(size=12),
        axis.title = element_text(size=14),
        legend.text = element_text(size=12)) + 
  guides(color=guide_legend(override.aes=list(size=4)),
         shape = guide_legend(override.aes = list(size=4))) 


##### Plot activity trajectories summarised ######


sub_dt[, sum(!is.na(M10)), by=.(record_id, cycle)]
sub_dt[, uniqueN(record_id), by=cycle]

# adjust day of cycle in 2nd cycle to start 0 as day of surgery
sub_dt[cycle != 2, day_of_cycle_surg := day_of_cycle]
sub_dt[cycle == 2, day_of_cycle_surg := day_of_cycle - surg_dayofcycle]

sub_dt[cycle==2, sum(!is.na(M10)), by=.( day_of_cycle_surg)]
sub_dt[, cycle_total := uniqueN(record_id), by=.(cycle)]
sub_dt[, keep_day := (sum(!is.na(M10))/cycle_total)>=.5, by=.(cycle, day_of_cycle_surg)]

sub_diary[cycle != 2, day_of_cycle_surg := day_of_cycle]
sub_diary[cycle == 2, day_of_cycle_surg := day_of_cycle - surg_dayofcycle]

sub_diary[, cycle_total := uniqueN(record_id), by=.(cycle)]
sub_diary[, keep_day := (sum(!is.na(bfi_total))/cycle_total)>=.8, by=.(cycle, day_of_cycle_surg)]


sub_dt$cycle_name <- factor(sub_dt$cycle, levels = c(1, 2, 3), 
                            labels = c("Baseline","Immediately\npost-op","6-months post-op"),
                            ordered = TRUE)
sub_diary$cycle_name <- factor(sub_diary$cycle, levels = c(1, 2, 3), 
                               labels = c("Baseline","Post-op","6-months post-op"),
                               ordered = TRUE)
sub_dt[, cycle_has_data := sum(!is.na(M10)) > 0, by=.(record_id,cycle)]
sub_dt[, .(record_id,cycle, cycle_has_data)] %>% unique()
sub_diary[, cycle_has_data := sum(!is.na(pain_avg)) > 0, by=.(record_id,cycle)]
sub_diary[, .(record_id,cycle, cycle_has_data)] %>% unique()

sub_dt[keep_day==T & cycle <=2, prepost_surg := all(cycle_has_data, na.rm=T)==T , by=.(record_id)]
sub_diary[keep_day==T & cycle <=2, prepost_surg := all(cycle_has_data, na.rm=T)==T, by=.(record_id)]

color_ls <- brewer_pal(5, "Set2")


compute_se <- function(x) {
  return(sd(x)/sqrt(length(x)))
}

col <- 'M10'
plot_traj_frombase <- function(sub_dt, col, colname) {
  summary_stats <- Rmisc::summarySE(sub_dt[keep_day==T & cycle<=2 & prepost_surg==T],
                                    measurevar=col, groupvars=c("cycle_name",'day_of_cycle_surg'),
                                    na.rm=T) %>% as.data.table()
  colnames(summary_stats)[4] <- 'mean_val'
  
  baseline_mn <- summary_stats[cycle_name == 'Baseline']$mean_val %>% mean()
  baseline_ci <- summary_stats[cycle_name == 'Baseline']$mean_val %>% compute_se()
  
  summary_stats <- summary_stats[cycle_name == 'Immediately\npost-op']
  summary_stats$baseline_mn <- baseline_mn
  summary_stats$baseline_ci <- baseline_ci
  
  p <- ggplot() + 
    geom_line(data=sub_dt[cycle==2], 
              aes(x = day_of_cycle_surg, y = get(col), group = record_id, color = record_id), alpha=.4) +
    facet_wrap(~cycle_name) + # Add transparent lines for individual trajectories
    geom_line(data = summary_stats, 
              aes(x = day_of_cycle_surg, y = mean_val), 
              color = "black", size = 1) +
    geom_ribbon( data=summary_stats,
                 aes(x = day_of_cycle_surg,
                     ymin = mean_val-ci, ymax = mean_val+ci), 
                 alpha = 0.2) +
    geom_hline(yintercept = baseline_mn, linetype=2, color='red', size=1) + 
    scale_x_continuous(limits = c(0,28)) + 
    # geom_ribbon(data=summary_stats,aes(x=day_of_cycle_surg,
    #             ymin=baseline_mn - baseline_ci, ymax=baseline_mn + baseline_ci),
    #             alpha=.2, fill='red') + 
    labs(title='',
         x = "Days since surgery",
         y = colname) +
    theme_minimal() +
    theme(legend.position = 'none',
          strip.text = element_blank(),
          axis.title.y = element_text(size=16),
          axis.title.x = element_text(size=18),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=14))
  return(p)
}

# PLOT M10 trajectories post-surgery
sub_dt[keep_day==T & cycle<=2 & prepost_surg==T]$record_id %>% uniqueN()
plot_traj_frombase(sub_dt, 'M10','M10')





