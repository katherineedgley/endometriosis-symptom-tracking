ehp30_name_mapping <- c('ehp30_overall' = 'Global EHP-30',
                        'pain_ehp30' = 'EHP-30 pain',
                        'control_ehp30' = 'EHP-30 control',
                        'self_ehp30' = 'EHP-30 self-image',
                        'emotion_ehp30' = 'EHP-30 emotional',
                        'social_ehp30' = 'EHP-30 social')





## daily cols:
day_colnames_excel <- 
c('pain_avg' = 'Avg. pain',
  'pain_worst' = 'Worst pain',
  'pain_total' = 'Global pain',
  'bfi_total' = 'Global BFI',
  'fatigue_now' = 'BFI Q1 (fatigue now)',
  'fatigue_24hr' = 'BFI Q2 (avg. fatigue)',
  'fatigue_worst' = 'BFI Q3 (worst fatigue)',
  'fatigue_activity' = 'BFI Q4a (activity interference)',
  'fatigue_walking' = 'BFI Q4c (walking interference)',
  'fatigue_enjoyment' = 'BFI Q4f (enjoyment interference)',
  'fatigue_mood' = 'BFI Q4b (mood interference)',
  'fatigue_work' = 'BFI Q4d (work interference)',
  'fatigue_relations' = 'BFI Q4e (relations interference)'
)
day_colnames_excel <- as.data.table(day_colnames_excel, keep.rownames = 'orig')


cor_colnames <- 
  c('pain_avg_mn'='Mean avg. pain',
    'pain_worst_mn'='Mean worst pain',
  'pain_total_mn' = 'Mean global pain',
  'bfi_total_mn' = 'Mean global BFI',
  'fatigue_now_mn' = 'Fatigue now',
  'fatigue_24hr_mn' = 'Avg. fatigue',
  'fatigue_worst_mn' = 'Worst fatigue',
  'fatigue_activity_mn' = 'Activity',
  'fatigue_walking_mn' = 'Walking',
  'fatigue_enjoyment_mn' = 'Enjoyment',
  'fatigue_work_mn' = 'Work',
  'fatigue_mood_mn' = 'Mood',
  'fatigue_relations_mn' = 'Relations',
  'pain_total_sd' = 'Std. of global pain',
  'pain_total_upper_mn' = 'Upper 25% global pain mean',
  'pain_total_lower_mn' = 'Lower 25% global pain mean',
  'pain_total_iqr' = 'IQR of global pain',
  'pain_total_skew' = 'Skew of global pain',
  'pain_total_rmssd' = 'RMSSD of global pain',
  'pain_total_tkeo' = 'TKEO of global pain',
  ## daily cols:
  'pain_avg' = 'Avg. pain',
  'pain_worst' = 'Worst pain',
  'pain_total' = 'Global pain',
  'bfi_total' = 'Global BFI',
  'fatigue_now' = 'Fatigue now',
  'fatigue_24hr' = 'Avg. fatigue',
  'fatigue_worst' = 'Worst fatigue',
  'fatigue_activity' = 'Activity',
  'fatigue_walking' = 'Walking',
  'fatigue_enjoyment' = 'Enjoyment',
  'fatigue_mood' = 'Mood',
  'fatigue_work' = 'Work',
  'fatigue_relations' = 'Relations'
  )

cor_colnames <- as.data.table(cor_colnames, keep.rownames = 'orig')


transform_summary_cols <- function(col_ls) {
  col_ls[grepl('iqr', col_ls)] <- 'IQR'
  col_ls[grepl('tkeo', col_ls)] <- 'TKEO'
  col_ls[grepl('rmssd', col_ls)] <- 'RMSSD'
  col_ls[grepl('skew', col_ls)] <- 'Skew'
  col_ls[grepl('upper_mn', col_ls)] <- 'Upper 25% mean'
  col_ls[grepl('lower_mn', col_ls)] <- 'Lower 25% mean'
  col_ls[grepl('mn', col_ls)] <- 'Mean'
  col_ls[grepl('sd', col_ls)] <- 'Std. dev.'
  return(col_ls)
}






