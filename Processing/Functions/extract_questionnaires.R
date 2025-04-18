# extract end-of-cycle questionnaires
#' Function to extract end-of-cycle questionnaires
#' 
#' @param alldata data from REDCap
#' @return Table with questionnaires at all cycles (0-3)
#'
extract_questionnaires <- function(alldata) {
  
  baseline_data <- extract_quest_cycle(alldata, 0)
  baseline_data[, cycle := 0]
  baseline_colnames <- colnames(baseline_data)
  
  cycle1_data <- extract_quest_cycle(alldata, 1)
  cycle1_data[, cycle := 1]
  cycle1_colnames <- colnames(cycle1_data)
  cycle1_data <- fix_cycle_colnames(cycle1_data, colnames(baseline_data))
  
  
  cycle2_data <- extract_quest_cycle(alldata, 2)
  cycle2_data[, cycle := 2]
  cycle2_data <- fix_cycle_colnames(cycle2_data, colnames(baseline_data))

  cycle3_data <- extract_quest_cycle(alldata, 3)
  cycle3_data[, cycle := 3]
  cycle3_data <- fix_cycle_colnames(cycle3_data, colnames(baseline_data))
  
  
  allcycle_data <- rbind(baseline_data, cycle1_data, cycle2_data, cycle3_data)
  allcycle_data <- allcycle_data[, !grepl("calc_", colnames(allcycle_data)), with=F]
  allcycle_data <- allcycle_data[, mget(c("record_id", "cycle",
                         colnames(allcycle_data)[!colnames(allcycle_data) %in%
                                                   c("record_id", "cycle")]))]
  
  eq5d_cols <- colnames(allcycle_data)[grepl('eq5d', colnames(allcycle_data))][1:5]
  allcycle_data[, eq5d_total := apply(.SD, MARGIN=1, FUN=sum), .SDcols=eq5d_cols]
  
  return(allcycle_data)
}

#' Function to input cycle2 and cycle3 questionnaire data, and fix the 
#' EHP-30 "relation status" variable which is different from cycle1 (changes
#' from multiple choice to checkmarks)
#'
#' @input cycle_data - cycle 2 or 3 processed data from "extract_quest_cycle"
#' @input final_colnames - list of colnames that should be output (same as cycle1)
#'
fix_cycle_colnames <- function(cycle_data, final_colnames) {
  relation_vars <- colnames(cycle_data)[grepl("relation_status", colnames(cycle_data))]
  relation_dt <- cycle_data[, mget(c("record_id", relation_vars))]
  zero_inds <- which(rowSums(relation_dt[, mget(relation_vars)]) == 0)
  
  cycle_data$ehp30_3a_relation_status <-max.col(relation_dt[, mget(relation_vars)])
  cycle_data$ehp30_3a_relation_status[zero_inds] <- NA
  cycle_data <- cycle_data[, mget(final_colnames)]
  
  return(cycle_data)
}

extract_quest_cycle <- function(alldata, cycle_num) {
  redcap_event <- paste0("visit_", cycle_num+1, "_arm_1")
  quest_cols <- colnames(alldata)[grepl("ehp30|bfi|eq5d|e15d|gpq", colnames(alldata))]
  quest_cols <- quest_cols[grepl(paste0("v", cycle_num+1), quest_cols)]
  quest_data <- alldata[redcap_event_name == redcap_event & redcap_repeat_instrument == "",
                             mget(c("record_id", quest_cols))]
  quest_final_cols <- gsub(paste0("_v", cycle_num+1), "", quest_cols)
  quest_final_cols[quest_final_cols == "e15d_selfcare"] <- "eq5d_selfcare"
  colnames(quest_data) <- c("record_id", quest_final_cols)
  return(quest_data)
}


