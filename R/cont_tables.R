

#' @title a function to generate a summary table for continuous variables
#'
#' @param data a dataframe
#' @param cv_flag numeric flag for coefficient of variation
#' @param missing_flag numeric flag for percent of missing data
#' @param skewness_flag numeric flag for skewness level
#' @param kurtosis_flag numeric flag for kurtosis level
#' @param outlier_flag numeric flag for number of outliers
#' @param cont_raw_output boolean, if TRUE, will return raw output, if FALSE, will return kable table
#'
#' @return kable_table
#' @export
#'
#'
summarize_cont <- function(data,
                           cv_flag = 30,
                           missing_flag = 5,
                           skewness_flag = 2,
                           kurtosis_flag = 2,
                           outlier_flag = 5,
                           cont_raw_output = FALSE) {

  library(dplyr)
  library(tidyr)
  library(kableExtra)
  library(e1071)

  # Check if the dataframe is empty
  if (nrow(data) == 0) {
    stop("The input dataframe is empty. Please provide a non-empty dataframe.")
  }

  if (all(sapply(data, function(col) all(is.na(col))))) {
    stop("The input dataset contains only NA values in all columns. Please provide a valid dataset.")
  }
  summary_table <- data %>%
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

    pivot_wider(names_from = Metric, values_from = value) %>%

    # Add CV
    mutate(CV = abs(SD / Mean * 100)) %>%


    # Round numeric columns
    mutate(across(c(Missingpercent, Mean, Median, SD, Min, Max, Q1, Q3,
                    IQR, Skewness, Kurtosis, CV, PercentOutliers),
                  ~round(., 2))) %>%

    # Add message columns for flagged variables
    mutate(
      SkewnessFlag = ifelse(abs(Skewness) > skewness_flag,
                            paste0("Highly skewed (", round(Skewness, 2),
                                   "). Consider transformation."), ""),
      KurtosisFlag = ifelse(abs(Kurtosis - 3) > kurtosis_flag,
                            paste0("Excess kurtosis (", round(Kurtosis, 2),
                                   " -3). Consider normalization."), ""),
      MissingFlag = ifelse(Missingpercent > missing_flag,
                           paste0("Missing ", round(Missingpercent, 2),
                                  "% data. Consider imputation."), ""),
      CVFlag = ifelse(CV > cv_flag,
                      paste0("High variability (CV ", round(CV, 2),
                             "). Consider transformation."), ""),
      OutlierFlag = ifelse(PercentOutliers > outlier_flag,
                           paste0("Outliers ", round(PercentOutliers, 2),
                                  "% present. Review data."), ""))



  # Create kable table with specified flags
  kable_table <- summary_table %>%
    kbl(col.names = c("Variable", "N", "Missing", "Missing %", "Mean",
                      "Median", "SD", "Min", "Max", "Q1", "Q3", "IQR",
                      "Skewness", "Kurtosis", "N Outliers", "Outliers %",
                      "CV", "Skewness Message", "Excess Kurtosis Message",
                      "Missing Message", "CV Message", "Outlier Message"),
        align = "c") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = F) %>%
    column_spec(1:17, color = "black") %>%
    column_spec(4, color = ifelse(summary_table$Missingpercent >= missing_flag, "red", "black")) %>%
    column_spec(13, color = ifelse(abs(summary_table$Skewness) > skewness_flag | summary_table$SkewnessFlag != "", "red", "black")) %>%
    column_spec(14, color = ifelse(abs(summary_table$Kurtosis) - 3 > kurtosis_flag | summary_table$KurtosisFlag != "", "red", "black")) %>%
    column_spec(16, color = ifelse(summary_table$PercentOutliers >= outlier_flag | summary_table$OutlierFlag != "", "red", "black")) %>%
    column_spec(17, color = ifelse(summary_table$CV > cv_flag | summary_table$CVFlag != "", "red", "black")) %>%
    column_spec(18:22, color = "royalblue3")

  if (cont_raw_output) {
    return(summary_table)
  }

  return(kable_table)
}
