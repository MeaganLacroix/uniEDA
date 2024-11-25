library(dplyr)
library(tidyr)
library(kableExtra)

summarize_cat <- function(data,
                          percentage_flag = 50,
                          SMD_flag = 0.2) {
  # Initialize an empty data frame for the results
  result <- data.frame(
    Variable = character(),
    Level = character(),
    Frequency = numeric(),
    Percent = numeric(),
    SMD = numeric(),
    stringsAsFactors = FALSE
  )

  for (var in names(data)) {
    var_data <- data[[var]]

    # Skip variables that are entirely NA
    if (is.null(var_data) || all(is.na(var_data))) {
      next
    }

    # Calculate basic statistics
    freq <- table(var_data, useNA = "ifany")
    total <- sum(freq, na.rm = TRUE)
    percent <- round(100 * as.numeric(freq) / total, 2)

    # Calculate Standardized Mean Difference (SMD) for non-missing levels
    non_missing_levels <- unique(as.character(var_data[!is.na(var_data)]))
    smd <- sapply(non_missing_levels, function(level) {
      in_level <- var_data == level
      other <- !is.na(var_data) & !in_level
      abs(mean(in_level) - mean(other))
    })

    # Include "Missing" level explicitly
    missing_count <- sum(is.na(var_data))
    missing_percent <- round(100 * missing_count / nrow(data), 2)

    # Combine results for all levels, including "Missing"
    valid_levels <- c(non_missing_levels, "Missing")
    valid_freq <- c(as.numeric(freq[non_missing_levels]), missing_count)
    valid_percent <- c(percent[non_missing_levels], missing_percent)
    valid_smd <- c(smd, NA)  # SMD for "Missing" is NA

    # Debug: Check valid_levels and valid_percent
    #print("DEBUG: Valid Levels and Percentages")
    #print(valid_levels)
    #print(valid_percent)

    # Combine results for the variable
    var_result <- data.frame(
      Variable = var,
      Level = valid_levels,
      Frequency = valid_freq,
      Percent = valid_percent, # Explicitly name Percent column
      SMD = valid_smd
    )

    # Append to the final result
    result <- rbind(result, var_result)
  }


  # Debug: Check the structure of result
  #print("DEBUG: Result before replacing NA values")
  #print(head(result))
  #print(colnames(result)) # Confirm column names
  #print(dim(result))

  # Ensure no NA values in Percent or SMD (except for "Missing")
  result$Percent[is.na(result$Percent)] <- 0
  result$SMD[is.na(result$SMD) & result$Level != "Missing"] <- 0

  # Stop if result is empty
  if (nrow(result) == 0) {
    stop("DEBUG: Result data frame is empty. Check input data or processing logic.")
  }

  # Create a well-styled summary table using kableExtra
  kable_table <- result %>%
    kbl(
      col.names = colnames(result), # Automatically match column names
      align = "c",
      row.names = FALSE # Suppress row numbers
    ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = FALSE) %>%
    column_spec(4, color = ifelse(!is.na(result$Percent) & result$Percent > percentage_flag,
                                  "red", "black")) %>%
    column_spec(5, color = ifelse(!is.na(result$SMD) & result$SMD > SMD_flag,
                                  "red", "black"))

  return(kable_table)
}
