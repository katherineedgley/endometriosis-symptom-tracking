# Assess adherence and available data
sapply(paste0("./Processing/Functions/", list.files("./Processing/Functions")),
       source)

##### Load in data #####
config <- read.config(file="./processing_config.yaml")

base_dt <- fread(paste0(config$processed_output_dir, "baseline_dt.csv"),
                  colClasses = c('record_id'='character'))

file_dt <- fread(paste0(config$source_data_dir, "file_dt.csv"),
                 colClasses = c('record_id'='character',
                                'start_date'='IDate',
                                'end_date' = 'IDate'))

alldata <- fread(paste0(config$source_data_dir, "alldata_processed.csv"),
                 colClasses=c('record_id'='character'))
fulldiary_dt <- fread(paste0(config$processed_output_dir, "diary_dt.csv"))


dt <- fread(paste0(config$processed_output_dir, "allmetrics_dt.csv"),
            colClasses = c('record_id' = "character"))

combined_symptom_dt <- fread(paste0(config$processed_output_dir,"symptom_cycle_dt.csv"),
                             colClasses = c("record_id" = 'character'))
combined_dt <- fread(paste0(config$processed_output_dir,"combined_cycle_dt.csv"),
                     colClasses = c("record_id" = 'character'))

diary_dt <- prepare_adherence(alldata, dt)



# initial data processing ----------------------------------------------------

diary_dt[cycle==3]$record_id %>% uniqueN()
dropped_pats <- base_dt[dropped_pat == T]$record_id
diary_dt[, dropped_pat := record_id %in% dropped_pats]

diary_dt[, ncycles := uniqueN(cycle), by=record_id]

diary_dt[has_watch_data==F, .(record_id,cycle, perc_wear, dropped_pat, ncycles)] %>% unique()

dt[, dropped_pat := record_id %in% dropped_pats]
dt[,sum(!is.na(pain_avg)), by=record_id]$V1 %>% mean()
dt[,sum(!is.na(pain_avg)), by=record_id]$V1 %>% max()

diary_dt[, sum(is.na(perc_wear)), by=record_id]

dt[,sum(!is.na(pain_avg) & !is.na(M10)), by=record_id]$V1 %>% mean()
dt[, .N, by=record_id]$N %>% mean()
dt[dropped_pat == F, .N, by=record_id]$N %>% mean()
dt[, .N, by=record_id]
dt[!(is.na(perc_wear) & day_of_cycle > 42)
   , .N, by=record_id]
dt$perc_wear %>% mean(na.rm=T)


#### Summary #####
count_cycle_dt <- diary_dt[, .(diary_responses = sum(diary_missing == F),
                         has_watch_data = unique(has_watch_data),
                         suff_wear = sum(perc_wear > 0.66)/.N,
                         cycle_length  = .N), 
                     by=c("record_id","cycle")]
count_dt <- count_cycle_dt[, .(total_length = sum(cycle_length),
                         total_responses = sum(diary_responses),
                         ncycles = uniqueN(cycle)), by='record_id']
count_dt[, dropped_pat := record_id %in% dropped_pats]
count_dt[, perc_responded := total_responses/total_length]

count_dt$perc_responded %>% mean()
count_dt$perc_responded %>% sd()

count_dt[dropped_pat == F]$perc_responded %>% mean()
count_dt[dropped_pat == F]$perc_responded %>% sd()

count_dt
count_cycle_dt[has_watch_data==F]

# watch data:
diary_dt <- diary_dt %>% merge(count_dt[, .(record_id,perc_responded)], by='record_id')
diary_dt[is.na(perc_wear), .(record_id, cycle)] %>% unique()

count_dt <- merge(count_dt, 
                  diary_dt[, .(perc_wear_mn = mean(perc_wear, na.rm=T),
                               nonwear_perc = mean(nonwear_perc_day_GGIR,na.rm=T),
                               suffwear_perc = mean(perc_wear > .66, na.rm=T)), 
                     by=c("record_id")], by='record_id')
count_dt$perc_wear_mn %>% mean(na.rm=T)

count_dt$perc_wear_mn %>% sd(na.rm=T)
count_dt$suffwear_perc %>% mean(na.rm=T)
count_dt$suffwear_perc %>% sd(na.rm=T)
count_dt$perc_responded %>% mean(na.rm=T)
count_dt$perc_responded %>% sd(na.rm=T)

count_dt[dropped_pat == F]$perc_wear_mn %>% mean(na.rm=T)
count_dt[dropped_pat == F]$perc_wear_mn %>% sd(na.rm=T)

count_dt[dropped_pat == F]$suffwear_perc %>% mean(na.rm=T)
count_dt[dropped_pat == F]$suffwear_perc %>% sd(na.rm=T)

count_dt[dropped_pat == F]$perc_responded %>% mean(na.rm=T)
count_dt[dropped_pat == F]$perc_responded %>% sd(na.rm=T)

count_dt$ncycles %>% mean()

count_dt$perc_responded %>% mean(na.rm=T)
count_dt$perc_wear_mn %>% mean(na.rm=T)



# Cycle count_dt

cycle_count_dt <- diary_dt[, .(diary_responses = sum(diary_missing == F)/28,
                         has_watch_data = unique(has_watch_data),
                         perc_wear=mean(perc_wear,na.rm=T),
                         cycle_length  = .N), 
                     by=c("record_id","cycle")]


#### Acceptability data ######

acc_dt <- fread(paste0(config$source_data_dir, "acceptability.csv"), 
                       colClasses=c("record_id"='character'))
acc_dt$watch_comfort_issue %>% table()
acc_dt$crf_v4_acceptability_q1 %>% table()
acc_dt[!is.na(crf_v4_acceptability_q1)]
acc_dt$crf_v4_acceptability_q7 %>% table()
acc_dt$crf_v4_acceptability_q7 %>% mean(na.rm=T) 

acc_dt$crf_v4_acceptability_q5 %>% mean(na.rm=T) 
acc_dt$crf_v4_acceptability_q6 %>% mean(na.rm=T)


dropouts_dt <- fread(paste0(config$source_data_dir, "withdrawals.csv"),
                     colClasses=c('record_id'='character'))

dropouts_dt$reason %>% table()

dropouts_dt[ncycles_participated == 1, .N, by= .(reason, when_cycle, details)]
dropouts_dt[ncycles_participated == 2, .N, by= .(reason, when_cycle, details)]
dropouts_dt[ncycles_participated == 3, .N, by= .(reason, when_cycle, details)]


dropouts_dt[ncycles_participated == 1]$when_cycle %>% table()
dropouts_dt[ncycles_participated == 2]$when_cycle %>% table()
dropouts_dt[ncycles_participated == 3]$when_cycle %>% table()

count_dt[ncycles == 2 | ncycles ==3]
count_dt[ncycles ==3]


#### Data flow #####
file_dt[, has_watch_data := TRUE]
file_dt[, num_watches_cycle := .N, by=.(record_id,cycle)]
# where there are two files for a cycle, set end date to second file and remove extra row
file_dt[, end_date := last(end_date), by=.(record_id,cycle)]
file_dt<- file_dt[, .SD[1], by=.(record_id,cycle)]
num_cycles_dt <- file_dt[, .N, by=record_id]
# The number of participants that completed 1,2,3 cycles:
num_cycles_dt[, .N, by=N]

cycle_wear_dt <- dt[, .(suff_wear_days  = sum(perc_wear > 0.75, na.rm=T)), by=.(record_id,cycle)] 
cycle_wear_dt[suff_wear_days < 2]
cycle_wear_dt[, valid_watchdata := TRUE]
cycle_wear_dt[suff_wear_days < 2, valid_watchdata := FALSE]
file_dt <- file_dt %>% merge(cycle_wear_dt, by=c('record_id','cycle'))
file_dt[valid_watchdata == F]
num_cycles_dt <- file_dt[valid_watchdata == T, .N, by=record_id]
num_cycles_dt[, .N, by=N]


diary_dt[, has_any_watchdata_cycle := sum(has_watch_data) > 0, by=.(record_id,cycle)]

cycle_count_dt <- cycle_count_dt %>% merge(unique(diary_dt[, .(has_any_watchdata_cycle, record_id,cycle)]),
                                           by=c('record_id','cycle'))
missing_pats <- cycle_count_dt[has_watch_data ==F]$record_id %>% unique()


# Number of people with or without data for each cycle:
unique(diary_dt[, .(has_watch_data, record_id,cycle)])[, .N, by=.(cycle, has_watch_data)][order(cycle)]
cycle_count_dt <- diary_dt[, .(record_id,cycle, num_diaries, has_any_watchdata_cycle, dropped_pat)] %>% unique()
cycle_count_dt[, .N, by=.(cycle,has_any_watchdata_cycle)][order(cycle)]
cycle_count_dt[has_any_watchdata_cycle == F]
cycle_count_dt[, ncycles_watch := sum(has_any_watchdata_cycle), by=record_id]

unique(cycle_count_dt[, .(record_id,ncycles_watch)])[, .N, by=ncycles_watch]

cycle_count_dt[, ncycles_diary := sum(num_diaries > 1), by=record_id]
unique(cycle_count_dt[, .(record_id,ncycles_diary)])[, .N, by=ncycles_diary]
cycle_count_dt[, completed_pat := ncycles_diary == 3]
cycle_count_dt[completed_pat == T, total_watches_complete := sum(has_any_watchdata_cycle), by=cycle]

diary_dt <- diary_dt %>% merge(unique(cycle_count_dt[, .(record_id,ncycles_diary, ncycles_watch)]),
                   by='record_id')
diary_dt <- diary_dt %>% 
  merge(unique(cycle_count_dt[!is.na(total_watches_complete), .(cycle, total_watches_complete)]),
                               by='cycle')

diary_dt[ncycles_diary == 3]$record_id %>% uniqueN()
diary_dt[ncycles_watch == 3]$record_id %>% uniqueN()
diary_dt[ncycles_watch == 3 & ncycles_diary==3]$record_id %>% uniqueN()



##### Look at retrospective and paper entries - weekly adherence plots ####


diary_dt[, week_num := cut(as.numeric(day_of_cycle), 4, labels=1:4)]
week_dt <- diary_dt[, .(day_of_cycle, week_num)] %>% unique()
stopifnot(all(week_dt[, .N, by=week_num]$N == 7))


diary_dt[, after_watchend := record_date > watch_end_date]

watch_count <- unique(diary_dt[, .(record_id,cycle, has_any_watchdata_cycle)])
stopifnot(all(watch_count[, .N, by=.(record_id,cycle)]$N == 1)) # check no duplicates
diary_dt <- diary_dt %>% merge(watch_count[, .(total_watches = sum(has_any_watchdata_cycle)), by=cycle],
                   by=c('cycle'))

week_response_dt <- diary_dt[, .(
                               perc_feasible = sum(response_type == 'correct' | 
                                                     response_type == 'feasible')/.N,
                               perc_paper = sum(response_type=='paper')/.N,
                               perc_other = sum(response_type == 'other')/.N,
                               total_enrolled = .N/7,
                               total_watches = unique(total_watches),
                               perc_wear_mn = mean(perc_wear, na.rm=T)), 
                             by=.(cycle, week_num)] %>% unique()
week_response_dt[, cycle_label := paste0('Smartwatch cycle ', cycle, '\nN=', total_enrolled, 
                                         ' / N=', total_watches)]
week_response_dt <- week_response_dt[, !c("total_enrolled",'total_watches')] %>%
  melt(id.vars = c("cycle","week_num", "cycle_label",'perc_wear_mn'))


ggplot(week_response_dt, aes(x=week_num, y=value*100, fill=variable)) + 
  geom_bar(stat='identity', position=position_stack(reverse=T)) +
  facet_wrap(~cycle_label) +
  scale_fill_manual(values=c("#4AA489", "#7DBE5C",  "#D85941"),
                    labels=c("Completed on day\nor following day",
                             "Paper (no timestamp available)",
                             "Completed retrospectively\n(at later time)"))  +
  scale_y_continuous(limits=c(0,100))  + labs(x="Week", y="Adherence (%)") + 
  guides(fill=guide_legend(override.aes = list(shape = NA), byrow=TRUE),
         point = guide_legend()) +
  geom_line(aes(x=as.numeric(week_num), y=perc_wear_mn*100), linetype='dotted', linewidth=1) +
  geom_point(aes(x=as.numeric(week_num), y=perc_wear_mn*100), size=4) +
  theme_minimal() +
  theme(axis.title = element_text(size=24),
        axis.text = element_text(size=18),
        strip.text = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_blank(),
        legend.position = 'right',
        legend.key.size=unit(1, "cm"),
        legend.spacing.y = unit(.7, "cm"))




#### Adherence with only those completing 3 cycles ####


week_response_dt <- diary_dt[ ncycles_diary == 3, .(
                                 perc_feasible = sum(response_type == 'correct' |
                                                       response_type == 'feasible')/.N,
                                 perc_paper = sum(response_type=='paper')/.N,
                                 perc_other = sum(response_type == 'other')/.N,
                                 perc_wear_mn = mean(perc_wear, na.rm=T),
                                 total_watches = unique(total_watches_complete),
                                 total_enrolled = .N/7), 
                             by=.(cycle, week_num)] %>% unique()
week_response_dt[, cycle_label := paste0('Smartwatch cycle ', cycle, '\nN=', total_enrolled, 
                                         ' / N=', total_watches)]
week_response_dt <- week_response_dt[, !c("total_enrolled",'total_watches')] %>%
  melt(id.vars = c("cycle","week_num", "cycle_label",'perc_wear_mn'))
ggplot(week_response_dt, aes(x=week_num, y=value*100, fill=variable)) + 
  geom_bar(stat='identity', position=position_stack(reverse=T)) +
  facet_wrap(~cycle_label) +
  scale_fill_manual(values=c("#4AA489", "#7DBE5C", "#D85941"),
                    labels=c("Completed on day\nor following day",
                             "Paper (no timestamp available)",
                             "Completed retrospectively\n(at later time)"))  +
  scale_y_continuous(limits=c(0,100))  + labs(x="Week", y="Adherence (%)") + 
  guides(fill=guide_legend(override.aes = list(shape = NA), byrow=TRUE),
         point = guide_legend()) +
  geom_line(aes(x=as.numeric(week_num), y=perc_wear_mn*100), linetype='dotted', linewidth=1) +
  geom_point(aes(x=as.numeric(week_num), y=perc_wear_mn*100), size=4) +
  theme_minimal() +
  theme(axis.title = element_text(size=24),
        axis.text = element_text(size=18),
        strip.text = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_blank(),
        legend.position = 'right',
        legend.key.size=unit(1, "cm"),
        legend.spacing.y = unit(.7, "cm"))



#### Plot by participant #####

print(diary_dt[dropped_pat==T & cycle==ncycles][, .(record_id, ncycles, cycle)] %>% unique())


count_dt <- count_cycle_dt[, .(total_length = sum(cycle_length),
                               total_responses = sum(diary_responses),
                               ncycles = uniqueN(cycle)), by='record_id']
count_dt[, dropped_pat := record_id %in% dropped_pats]
count_dt[, perc_responded := total_responses/total_length]

count_dt <- merge(count_dt, 
                  diary_dt[, .(perc_wear_mn = mean(perc_wear, na.rm=T)),
                           by=c("record_id")], by='record_id')
count_dt <- count_dt %>% merge(id_mapping, by='record_id', all.x=T, all.y=F)
count_dt[, pat_order := perc_wear_mn]
count_dt[is.na(pat_order), pat_order := 0]
count_dt <- count_dt[order(-dropped_pat, pat_order)]
count_dt[, record_id_label := factor(record_id, levels=count_dt$record_id, labels = count_dt$record_id_shifted)]


ggplot(count_dt) +   
  geom_point( aes(x=factor(record_id_label), y=perc_responded*100), color="#D85941", size=4,alpha=.7) +
  
  geom_segment( aes(x=factor(record_id_label), xend=factor(record_id_label),
                    y=perc_wear_mn*100, yend=perc_responded*100), color="black", alpha=.5) +
  geom_point( aes(x=factor(record_id_label), y=perc_wear_mn*100), color="#4AA489", size=4, alpha=.7 ) +
  annotate("rect", xmin = .5, xmax = 18.5, ymin = 10, ymax = 104,
           alpha = 0, color= "#CA6148") + 
  theme_minimal()+
  theme(
    axis.title.y = element_text(size=24),
    axis.title.x = element_text(size=24),
    axis.text.y = element_text(size=20),
    axis.text.x = element_text(size=12, angle= 90)
  ) + 
  xlab("Participant ID") +
  ylab("Adherence (%)")



