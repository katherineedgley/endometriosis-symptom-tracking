
#functions for diurnal analysis
extract_diurnal_variability <- function(short_dt, plot=FALSE) {
  short_dt[nonwear == 1, `:=`(ENMO = NA, movement = NA,x=NA,
                              y=NA,z=NA)]
  
  short_dt[, time_date := as.Date(timestamp)]
  date_ls <- unique(short_dt$time_date)
  # remove first and last day
  short_dt <- short_dt[time_date %in% date_ls[2:(length(date_ls)-1)]]
  short_dt <- short_dt[721:(nrow(short_dt)-720)] # remove first 12 and last 12 hours
  if (length(unique(short_dt$filename)) > 1) {
    filename_ls <- unique(short_dt$filename)
    time_seq1 <- data.table(timestamp=seq(first(short_dt[filename==filename_ls[1]]$timestamp), 
                                          by=60,to=first(short_dt[filename==filename_ls[2]]$timestamp)))
    time_seq2 <- short_dt[filename==filename_ls[2], .(timestamp)]
    time_seq <- rbind(time_seq1[1:(nrow(time_seq1)-1)], time_seq2)
    short_dt <- merge(short_dt, time_seq, by='timestamp', all=T)
    
  }
  short_dt$timepoint <- rep(1:1440, length(date_ls)-3)/60
  dates_final <- as.Date(short_dt$timestamp) %>% unique()
  short_dt[, day := rep(1:(length(dates_final)-1),
                        each = 1440,
                        length.out = nrow(short_dt))]
  day_wear <- short_dt[, .(nonwear_perc = sum(nonwear)/1440), by=day]
  suff_wear_days <- day_wear[nonwear_perc <= .1]$day  # take only days with less than 10% nonwear
  
  short_dt[, ENMO_center := ENMO - mean(ENMO, na.rm=T)]
  short_dt[, ENMO_integrated := ENMO_center %>% moving_summary(w=360, fun=sum)]
  
  short_dt <- short_dt[day %in% suff_wear_days]
  
  fit_alldays <-
    cosinor.lm(ENMO_integrated ~ time(timepoint) + 1,
               data = short_dt,
               period = 24)
  mesor_alldays <- fit_alldays$coefficients[1]
  amp_alldays <- fit_alldays$coefficients[2]
  acr_alldays <-
    correct.acrophase(fit_alldays)*(180/pi)/360*24*-1
  print(paste("Phi all days:",acr_alldays))
  fittedvals <- fit_alldays$fit$fitted.values
  min_val <- min(fittedvals)
  val_diffs <- abs(fittedvals - min_val)
  mintime_alldays <- (24*which(val_diffs < 1e-10)[1]/1440) # minimum activity time from 12pm onwards
  
  if (plot) {
    pred_dt <- copy(short_dt)
    pred_dt[, fittedvals := fit_alldays$fit$fitted.values]
    pred_dt[, id := 1:nrow(pred_dt)]
    pred_dt <- pred_dt %>% melt(measure.vars = c("ENMO_integrated", "fittedvals")) %>%
      as.data.table()
    
    pred_dt[, end_of_day := timepoint==1]
    ggplot(pred_dt[id %in% (1440*4 +1):(1440*10)],aes(x=id, y=value, colour=variable)) + 
      geom_line() + 
      geom_vline(xintercept=pred_dt[(id %in% (1440*4 +1):(1440*10)) & 
                                          end_of_day==TRUE]$id)
    
  }
  
  
  # fit models for each day ----
  daily_cosinor_dt <- data.table(day = 1:length(suff_wear_days), 
                                 time_date = as.IDate(NA),
                                 phi = NA, mesor = NA, amp = NA,
                                 min_time = NA)
  fitted_dt <- data.table()
  for (i in 1:length(suff_wear_days)){
    day_num <- suff_wear_days[i]
    sub_dt <- short_dt[day == day_num]
    daily_cosinor_dt$time_date[i] <- unique(short_dt[day == day_num]$time_date)[1]
    cosinor_fit <-
      cosinor.lm(ENMO_integrated ~ time(timepoint),
                 data = sub_dt,
                 period = 24)
    fitted_day <- copy(sub_dt[, .(timepoint, timestamp,ENMO_integrated)])
    fitted_day[, fittedvals := cosinor_fit$fit$fitted.values]
    fitted_day[, day := day_num]
    fitted_dt <- rbind(fitted_dt, fitted_day)
    daily_cosinor_dt$phi[i] <-
      -correct.acrophase(cosinor_fit)*(180/pi)/360*24
    daily_cosinor_dt$mesor[i] <- cosinor_fit$coefficients[1]
    daily_cosinor_dt$amp[i] <- cosinor_fit$coefficients[2]
    
    fittedvals <- cosinor_fit$fit$fitted.values
    val_diffs <- abs(fittedvals - min(fittedvals))
    min_time <- (24*which(val_diffs < 1e-10)[1]/1440) # minimum activity time from 12pm onwards
    daily_cosinor_dt$min_time[i] <- min_time
    
    fitted_day <- fitted_day %>%
      melt(measure.vars = c("ENMO_integrated","fittedvals")) %>% as.data.table()
    

  }
  
  if (plot) {
    fitted_melted <- fitted_dt %>% 
      melt(measure.vars = c("ENMO_integrated","fittedvals")) %>% as.data.table()
    
    ggplot(fitted_melted,
           aes(x=timestamp, y=value, colour=variable)) + geom_line()
    
  }
  
  # compute variability in rhythms ---
  rhythm_dt <- data.table(mintime_diff_mn = NA,
                          mesor_diff_mn = NA,
                          phi_diff_mn = NA,
                          amp_diff_mn = NA,
                          mintime_sd = NA,
                          mesor_sd = NA,
                          amp_sd = NA,
                          phi_sd = NA,
                          amp_mn = NA,
                          phi_mn=NA,
                          mesor_mn=NA,
                          mintime_mn = NA)
  
  daily_cosinor_dt[, mesor_alldays := mesor_alldays]
  daily_cosinor_dt[, amp_alldays := amp_alldays]
  daily_cosinor_dt[, acr_alldays := acr_alldays]
  daily_cosinor_dt[, mintime_alldays := mintime_alldays]
  
  daily_cosinor_dt[, mintime_diff := min_time - mintime_alldays]
  rhythm_dt$mintime_diff_mn[1] <- daily_cosinor_dt$mintime_diff %>% mean()

  daily_cosinor_dt[, mesor_diff := mesor - mesor_alldays]
  rhythm_dt$mesor_diff_mn[1] <- daily_cosinor_dt$mesor_diff %>% mean()

  daily_cosinor_dt[, phi_diff := phi - acr_alldays]
  rhythm_dt$phi_diff_mn[1] <- daily_cosinor_dt$phi_diff %>% mean()

  daily_cosinor_dt[, amp_diff := amp - amp_alldays]
  rhythm_dt$amp_diff_mn[1] <- daily_cosinor_dt$amp_diff %>% mean()

  rhythm_dt$amp_mn[1] <- amp_alldays
  rhythm_dt$mesor_mn[1] <- mesor_alldays
  rhythm_dt$phi_mn[1] <- acr_alldays
  rhythm_dt$mintime_mn[1] <- mintime_alldays
  
  rhythm_dt$amp_sd[1] <- daily_cosinor_dt$amp %>% sd()
  rhythm_dt$phi_sd[1] <- daily_cosinor_dt$phi %>% sd()
  rhythm_dt$mintime_sd[1] <- daily_cosinor_dt$min_time %>% sd()
  rhythm_dt$mesor_sd[1] <- daily_cosinor_dt$mesor %>% sd()
  
  
  
  return(list(rhythm_dt, daily_cosinor_dt))
}






# Helper functions  ------------------------------------------------------------
# function to compute the moving average, including the end points
# INPUTS: x - vector to compute average over
#         w - size of window
moving_summary = function(x, w, fun=mean, overlap = NA,
                          rmv_na = TRUE, type='centered') {
  if (is.na(overlap)) {overlap <- w-1}
  
  if (type == 'centered') {
    xfill <- c(rep(NA, floor(w/2)), x, rep(NA, floor(w/2))) 
  } else if (type == 'start') {
    xfill <- c(x, rep(NA, w))  # should this be w-1??
  }
  if (overlap == w-1) {
    meanvec <- rep(NA, length(x))
    for (i in 1:length(x)) {
      meanvec[i] <- fun(shift(xfill, n=i-1, type='lead')[1:w], na.rm=rmv_na)
    }
  } else {
    overlap_diff <- w - overlap
    meanvec <- rep(NA, ceiling(length(x)/overlap_diff)+1)
    k <- 1
    for (i in seq(1, length(x), overlap_diff)) {
      meanvec[k] <- fun(shift(xfill, n=i-1, type='lead')[1:w], na.rm=rmv_na)
      k <- k + 1
    }
  }
  return (meanvec)
}
