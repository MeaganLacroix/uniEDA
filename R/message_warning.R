uniEDA <- function(data, cv_flag = 30,
                   missing_flag = 30,
                   skewness_flag = 2,
                   kurtosis_flag = 2,
                   outlier_flag = 5) {
  
  library(dplyr)
  library(tidyr)
  library(kableExtra)
  library(e1071)
  
  # Filter numeric columns
  numeric_data <- data %>% select(where(is.numeric))
  
  # Calculate summary statistics
  summary_table <- numeric_data %>%
    summarise(across(everything(), list(
      N = ~sum(!is.na(.)),
      Missing = ~sum(is.na(.)),
      Missingpercent = ~mean(is.na(.)) * 100,
      Mean = ~mean(., na.rm = TRUE),
      Median = ~median(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      Min = ~min(., na.rm = TRUE),
      Max = ~max(., na.rm = TRUE),
      Q1 = ~quantile(., 0.25, na.rm = TRUE),
      Q3 = ~quantile(., 0.75, na.rm = TRUE),
      IQR = ~IQR(., na.rm = TRUE),
      Skewness = ~skewness(., na.rm = TRUE),
      Kurtosis = ~kurtosis(., na.rm = TRUE),
      NOutliers = ~sum(. < quantile(., 0.25, na.rm = TRUE) - 1.5 * IQR(., na.rm = TRUE) |
                         . > quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(., na.rm = TRUE), na.rm = TRUE),
      PercentOutliers = ~sum(. < quantile(., 0.25, na.rm = TRUE) - 1.5 * IQR(., na.rm = TRUE) |
                               . > quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(., na.rm = TRUE), na.rm = TRUE) / sum(!is.na(.)) * 100),
      .names = "{col}_{fn}")) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("Variable", "Metric"),
      names_pattern = "(.*)_(.*)") %>%
    pivot_wider(names_from = Metric, values_from = value)
  
  # Add CV
  summary_table <- summary_table %>%
    mutate(CV = abs(SD / Mean * 100))
  
  # Round numeric columns
  summary_table <- summary_table %>%
    mutate(across(c(Missingpercent, Mean, Median, SD, Min, Max, Q1, Q3, IQR, Skewness, Kurtosis, CV, PercentOutliers), ~round(., 2)))
  
  ###
  # Generate warnings NUM 
  warnings <- summary_table %>%
    mutate(
      Warning = case_when(
        abs(Skewness) > skewness_flag ~ paste0(Variable, " is highly skewed (skewness = ", Skewness, "). Consider applying a log or square root transformation."),
        abs(Kurtosis - 3) > kurtosis_flag ~ paste0(Variable, " has a high kurtosis (", Kurtosis, "). This may indicate heavy tails. Consider normalizing the distribution."),
        Missingpercent > missing_flag ~ paste0(Variable, " is missing ", Missingpercent, "% of values. Imputation or removal is recommended."),
        CV > cv_flag ~ paste0(Variable, " has high variability (CV = ", CV, "%). Consider applying a transformation (e.g., logarithmic), using robust statistical measures."),
        PercentOutliers > outlier_flag ~ paste0(Variable, " has ", PercentOutliers, "% outliers. Review the data points or consider robust statistical methods."),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Warning)) %>%
    select(Warning)
  
  
  
  ### 
  # Generate warnings CAT  
  warnings <- bind_rows(
    cat_summary %>%
      filter(Missingpercent > missing_flag) %>%
      mutate(Warning = paste0(Variable, " is missing ", Missingpercent, "% data. Consider imputation or removal.")),
    smd_df %>%
      filter(abs(SMD) > smd_flag) %>%
      mutate(Warning = paste0(Variable, " level '", Level, "' exceeds SMD threshold (", round(SMD, 2), "). Consider collapsing categories."))
  )

  # Create kable table with specified flags
  kable_table <- summary_table %>%
    kbl(col.names = c("Variable", "N", "Missing", "Missing %", "Mean", "Median", "SD", "Min", "Max", "Q1", "Q3", "IQR", "Skewness", "Kurtosis", "N Outliers", "Outliers %", "CV"),
        align = "c") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = F) %>%
    column_spec(1:17, color = "black") %>%
    column_spec(4, color = ifelse(summary_table$Missingpercent >= missing_flag, "red", "black")) %>%
    column_spec(13, color = ifelse(abs(summary_table$Skewness) > skewness_flag, "red", "black")) %>%
    column_spec(14, color = ifelse(abs(summary_table$Kurtosis - 3) > kurtosis_flag, "red", "black")) %>%
    column_spec(16, color = ifelse(summary_table$PercentOutliers >= outlier_flag, "red", "black")) %>%
    column_spec(17, color = ifelse(summary_table$CV > cv_flag, "red", "black"))
  
  # Combine table and warnings
  result <- list(
    table = kable_table,
    warnings = warnings
  )
  
  return(result)
}

