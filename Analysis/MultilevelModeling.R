#### Multilevel modeling #####
config <- read.config(file="./processing_config.yaml")

dt <- fread(paste0(config$processed_output_dir, "allmetrics_with_shifted.csv"),
            colClasses=c("record_id"='character','record_date' = 'IDate'))

# Exclude surgical cycles and set number of valid values
dt[is.na(surg_cycle) | cycle != surg_cycle,
   `:=` (nvalues = sum(!is.na(SleepRegularityIndex_GGIR) & !is.na(pain_total) & 
                         !is.na(M10) & !is.na(M10_minus1))),
   by=record_id]



## Function to standardise 
#' @param model_formula regression formula
#' @param orig_data input data to standardise (mean-std.)
#' @param outcome_is_factor Whether the outcome variable is binary
#' @param standard_outcome Whether the outcome variable should also be standardised
#' @param random_var The participant variable used as random effect
#' @param cor_var day variable to be used in cor
#'
normalize_vars <- function(model_formula, orig_data, outcome_is_factor=FALSE,
                           standard_outcome = FALSE,
                           random_var = 'record_id') {
  data <- orig_data %>% copy()
  
  predictor_ls <- all.vars(model_formula)
  factor_vars <- predictor_ls[data[, mget(predictor_ls)] %>% sapply(uniqueN) <=3]
  print(paste0("Factor variables: ", factor_vars))
  if (random_var %in% predictor_ls) {
    factor_vars <- c(factor_vars, random_var)
  }
  outcome_var <- as.character(model_formula[[2]])
  if (outcome_is_factor) {
    factor_vars <- c(factor_vars, outcome_var)
    print("Outcome is factor")
  }
  num_vars <- predictor_ls[!(predictor_ls %in% factor_vars)]
  if (standard_outcome == F) {
    num_vars <- num_vars[!num_vars %in% c(outcome_var)]
  }
  
  print(paste0('Numeric variables: ', num_vars))
  data[, (factor_vars) := lapply(.SD, as.factor), .SDcols=factor_vars]
  data[, (num_vars) := lapply(.SD, as.numeric), .SDcols=num_vars]
  # Standardise:
  data[, (num_vars) := lapply(.SD, function(x){(x-mean(x, na.rm=T))/sd(x, na.rm=T)}),
       .SDcols=num_vars]

  return(data)
}



##### Predict BFI total #####


form.1 <- formula(bfi_total~ pain_total + 
                    M10+
                    M10_minus1 +
                    SleepDurationInSpt_GGIR +
                    SleepDurationInSpt_GGIR_minus1 +
                    WASO_GGIR +
                    WASO_GGIR_minus1 + 
                    SleepRegularityIndex_GGIR
                  )


normed_data <- normalize_vars(form.1, dt[nvalues >= 20],
                              standard_outcome = TRUE)
lme_model <- lme(form.1, random=~1|record_id, 
                 data=normed_data, 
                  correlation=corCAR1(form=~ day_of_course|record_id),
                 na.action=na.exclude)
plot(lme_model)
model_summary <- summary(lme_model)
model_summary




normed_data[, bfi_preds := predict(lme_model)]
ggplot(normed_data, aes(x=bfi_total, y=bfi_preds)) + geom_point()
residuals <- resid(lme_model)
qqnorm(residuals)
qqline(residuals)


### Modeling of pain

form.2 <- formula(pain_total ~ bfi_total + M10 +
                    M10_minus1 + 
                    SleepDurationInSpt_GGIR + 
                    SleepDurationInSpt_GGIR_minus1 + 
                    WASO_GGIR + 
                    WASO_GGIR_minus1 +
                    SleepRegularityIndex_GGIR 
)


normed_data <- normalize_vars(form.2, dt[nvalues >= 20],
                              standard_outcome = TRUE,impute=FALSE)
lme_model <- lme(form.2, random=~1|record_id, 
                 data=normed_data, 
                 correlation=corCAR1(form=~ day_of_course|record_id),
                 na.action=na.exclude)

plot(lme_model)
model_summary <- summary(lme_model)
model_summary

normed_data[, bfi_preds := predict(lme_model)]
ggplot(normed_data, aes(x=bfi_total, y=bfi_preds)) + geom_point()
residuals <- resid(lme_model)
qqnorm(residuals)
qqline(residuals)


