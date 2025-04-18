# Day-level correlation analysis
source("./Analysis/Functions/cor_functions.R")
source("./Analysis/Functions/naming_cols.R")

config <- read.config(file="./processing_config.yaml")


dt <- fread(paste0(config$processed_output_dir, "allmetrics_with_shifted.csv"),
                   colClasses=c('record_id'='character'))


dt %>% dim()

ggir_cols <- colnames(dt)[grepl('GGIR', colnames(dt)) & !grepl('minus', colnames(dt)) &
                            !grepl('prev5days', colnames(dt))]
dt[, mget(ggir_cols)] %>% lapply(class) %>% unlist() %>% table()
self_act_cols <-  colnames(dt)[colnames(dt) %in% c(config$actigraphy_cols_self,
                                                   config$actigraphy_cols_diurnal)]

act_cols <- c(ggir_cols, self_act_cols)
act_cols <- act_cols[!act_cols %in% config$nonwear_cols]
fatigue_cols <- c("fatigue_now","fatigue_24hr",
                  "fatigue_worst", "fatigue_activity", "fatigue_mood",
                  "fatigue_enjoyment", "fatigue_walking",
                  "fatigue_work", "fatigue_relations")

symptom_cols <- c( "pain_total","pain_avg","pain_worst", "bfi_total",
                   fatigue_cols)

dt[, (act_cols) := lapply(.SD, as.numeric), .SDcols=act_cols]

cols2drop <- c( 'daysleeper_GGIR','number_wakeups')
act_cols <- act_cols[!act_cols %in% cols2drop]
dt[, lapply(.SD, uniqueN), .SDcols=act_cols]


rmcorr_mat <- get_rmcorr_mat(dt, symptom_cols, symptom_cols, 'record_id')
cor_dt <- rmcorr_mat[[1]] 
pval_dt <- rmcorr_mat[[2]]
df_dt <- rmcorr_mat[[3]]
cor_mat <- as.matrix(cor_dt, rownames='rowname')
rownames(cor_mat) <- cor_colnames$cor_colnames[match(rownames(cor_mat), cor_colnames$orig)]
colnames(cor_mat) <- cor_colnames$cor_colnames[match(colnames(cor_mat), cor_colnames$orig)]

png(paste0(config$plots_output_dir, "within_symptom_corrs.png"),
    width=6, height=6, res=300, units='in')
corrplot(cor_mat, tl.col='black', tl.cex = 1.2, method='number', number.cex = .8)
dev.off()



###### Symptoms and actigraphy cols correlations #####


rmcorr_mat <- get_rmcorr_mat(dt, symptom_cols, act_cols, 'record_id')
cor_dt <- rmcorr_mat[[1]] 
pval_dt <- rmcorr_mat[[2]]
df_dt <- rmcorr_mat[[3]]
cor_mat <- as.matrix(cor_dt, rownames='rowname') %>% t() %>% as.data.table(keep.rownames=T)
p_mat <- as.matrix(pval_dt, rownames='rowname') %>% t() %>% as.data.table(keep.rownames=T)

colnames(cor_mat) <- day_colnames_excel$day_colnames_excel[match(colnames(cor_mat), day_colnames_excel$orig)]


# save as xlsx
wb <- createWorkbook()
addWorksheet(wb, sheetName = "Repeated-measures correlations", gridLines = FALSE)
addWorksheet(wb, sheetName = "P-values", gridLines = FALSE)
writeDataTable(wb, sheet = 1, x = cor_mat, colNames = TRUE, rowNames = FALSE) #tableStyle = "TableStyleLight9")
writeDataTable(wb, sheet = 2, x = p_mat, colNames = TRUE, rowNames = FALSE)# tableStyle = "TableStyleLight9")
saveWorkbook(wb, paste0(config$plots_output_dir, 'rmcorrs_all.xlsx'),
             overwrite = TRUE)  ## save to working directory



