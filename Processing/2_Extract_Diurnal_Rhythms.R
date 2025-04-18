

file_dt <- fread(paste0(config$source_data_dir, "file_dt.csv"),
                 colClasses=c('record_id'='character'))

pat_dt <- file_dt[, .(record_id,cycle)] %>% unique()

if (file.exists(paste0(config$processed_output_dir, "patient_rhythms.csv"))) {
  pat_rhythms_dt <- fread(paste0(config$processed_output_dir, "patient_rhythms.csv"),
                          colClasses=c("record_id"='character'))
  pat_rhythms_dt[, processed := TRUE]
  daily_rhythms_dt <- fread(paste0(config$processed_output_dir, "daily_rhythms.csv"),
                          colClasses=c("record_id"='character'))
  pat_dt <- pat_dt %>% merge(pat_rhythms_dt[, .(record_id,cycle, processed)], 
                             by=c("record_id",'cycle'), all=T)
  pat_dt <- pat_dt[is.na(processed)]
  pat_rhythms_dt[, processed := NULL]
} else {
  pat_rhythms_dt <- data.table()
  daily_rhythms_dt <- data.table()
}


for (i in 1:nrow(pat_dt)) {
  record <- pat_dt$record_id[i]
  cycle_num <- pat_dt$cycle[i]
  filename <- file_dt[record_id == record & cycle==cycle_num]$filename
  print(filename)
  print(paste0("Record ", record, " cycle ", cycle_num))
  short_combined <- data.table()
  for (j in 1:length(filename)) {
    load(paste0(config$actigraphy_output_dir, filename[j], "/short_dt.rda"))
    short_combined <- rbind(short_combined, short_dt)
  }
  short_combined[, time_date := as.Date(timestamp)]
  valid_days <- short_combined[, sum(nonwear==0) > 1440*.66, by=time_date]$V1 %>% sum()
  if (valid_days > 5) {
    diurnal_ls <- extract_diurnal_variability(short_combined)
    rhythm_dt <- diurnal_ls[[1]]
    rhythm_dt$record_id[1] <- record
    rhythm_dt$cycle[1] <- cycle_num
    pat_rhythms_dt <- rbind(pat_rhythms_dt, rhythm_dt)
    
    daily_cosinor_dt <- diurnal_ls[[2]]
    daily_cosinor_dt[, record_id := record]
    daily_cosinor_dt[, cycle := cycle_num]
    daily_rhythms_dt <- rbind(daily_rhythms_dt, daily_cosinor_dt)
  } else {
    print("Not enough valid days to process")
  }
}

write.csv(pat_rhythms_dt,
          paste0(config$processed_output_dir, "patient_rhythms.csv"), row.names=F)
write.csv(daily_rhythms_dt, 
          paste0(config$processed_output_dir, "daily_rhythms.csv"), row.names=F)


