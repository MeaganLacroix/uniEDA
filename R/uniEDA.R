
#' Title
#'
#' @param data
#' @param cv_flag
#' @param missing_flag
#' @param skewness_flag
#' @param kurtosis_flag
#' @param outlier_flag
#'
#' @return
#' @export
#'
#' @examples
#'
#'
uniEDA <- function(data, cv_flag = 30,
                                  missing_flag = 5,
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

  #Add CV
  summary_table <- summary_table %>%
    mutate(CV = abs(SD / Mean * 100))

  # Round numeric columns
  summary_table <- summary_table %>%
    mutate(across(c(Missingpercent, Mean, Median, SD, Min, Max, Q1, Q3, IQR, Skewness, Kurtosis, CV,       PercentOutliers), ~round(., 2)))


  ## Create kable table with specified flags
  kable_table <- summary_table %>%
    kbl(col.names = c("Variable", "N", "Missing", "Missing %", "Mean", "Median", "SD", "Min", "Max",       "Q1", "Q3", "IQR", "Skewness", "Kurtosis", "N Outliers", "Outliers %", "CV"),
        align = "c") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = F) %>%
    column_spec(1:17, color = "black") %>%
    column_spec(4, color = ifelse(summary_table$Missingpercent >= missing_flag, "red", "black")) %>%
    column_spec(13, color = ifelse(abs(summary_table$Skewness) > skewness_flag, "red", "black")) %>%
    column_spec(14, color = ifelse(abs(summary_table$Kurtosis)-3 > kurtosis_flag, "red", "black")) %>%
    column_spec(16, color = ifelse(abs(summary_table$PercentOutliers) >= outlier_flag, "red", "black")) %>%
    column_spec(17, color = ifelse(summary_table$CV > cv_flag, "red", "black" ))

  return(kable_table)

}



