# Function to compute vector of repeated measures correlations
get_all_rmcorrs <- function(dt, var1, col_ls) {
  cor_dt <- data.table()
  rmcorr_dt <- data.table()
  for (var2 in col_ls) {
    print(var2)
    res_ls <- cor_LOPO(dt, var1, var2)
    cors_lopo <- res_ls[[1]] %>% as.data.table()
    colnames(cors_lopo) <- var2
    rmcorrs_lopo <- res_ls[[2]] %>% as.data.table()
    colnames(rmcorrs_lopo) <- var2
    cor_dt <- cbind(cor_dt, cors_lopo)
    rmcorr_dt <- cbind(rmcorr_dt, rmcorrs_lopo)
  }
  return(list(cor_dt, rmcorr_dt))
}


# Function to compute matrix of repeated-measures correlations
#'
#' 
#'
get_rmcorr_mat <- function(dt, ls1, ls2, pat_var) {
  cor_dt <- data.table(rowname = ls1)
  cor_dt[, (ls2):=as.numeric(NA)]
  df_dt <- data.table()
  pval_dt <- cor_dt %>% copy()
  for (var1 in ls1) {
    for (var2 in ls2) {
      res <- rmcorr(pat_var, var1, var2, dt)
      cor_dt[rowname==var1, var2] <- res$r
      pval_dt[rowname == var1, var2] <- res$p
      num_pats <- dt[, sum(!is.na(get(var1)) & !is.na(get(var2))) > 1,
                     by=get(pat_var)][V1 ==T, .N]
      df_sample <- data.table(v1 = var1, v2 = var2, df = res$df, pats = num_pats)
      df_dt <- rbind(df_dt, df_sample)
    }
  }
  return(list(cor_dt, pval_dt, df_dt))
}

# Function to compute matrix of correlations:
get_cor_mat <- function(dt, ls1, ls2, cor_method='spearman', fast = TRUE) {
  cor_dt <- data.table(rowname = ls1)
  cor_dt[, (ls2):=as.numeric(NA)]
  df_dt <- data.table()
  total_combs <- length(ls1)*length(ls2)
  print(paste0("Total combinations: ", total_combs))
  i <- 0
  for (var1 in ls1) {
    for (var2 in ls2) {
      if (i %% 500 == 0) {
        print(i)
      }
      i <- i + 1
      res <- cor(dt[[var1]], dt[[var2]], use='pairwise.complete.obs', method=cor_method)
      cor_dt[rowname==var1, var2] <- res
      
      df_sample <- data.table(v1 = var1, v2 = var2, df = nrow(dt[!is.na(get(var1)) & !is.na(get(var2))]))
      df_dt <- rbind(df_dt, df_sample)
    }
  }
  return(list(cor_dt, df_dt))
}

