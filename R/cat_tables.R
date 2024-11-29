summarize_cat <- function(data,
                          var_names,
                          percentage_flag = 30,
                          SMD_flag = 0.2,
                          cat_raw_output = FALSE) {
  result <- data.frame(
    Variable = character(),
    Level = character(),
    Frequency = numeric(),
    Percent = numeric(),
    SMD = numeric(),
    Message = character(),
    stringsAsFactors = FALSE
  )

  for (var in var_names) {
    var_data <- data[[var]]

    if (is.null(var_data) || all(is.na(var_data))) {
      next
    }

    freq <- table(var_data, useNA = "ifany")
    total <- sum(freq, na.rm = TRUE)
    percent <- round(100 * freq / total, 2)

    non_missing_levels <- levels(factor(var_data, exclude = NULL))
    smd <- sapply(non_missing_levels, function(level) {
      p1 <- sum(var_data == level, na.rm = TRUE) / total
      p2 <- sum(var_data != level, na.rm = TRUE) / (total * (length(non_missing_levels) - 1))
      sqrt_pooled_var <- sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
      if (sqrt_pooled_var == 0) return(0)
      abs(p1 - p2) / sqrt_pooled_var
    })

    missing_count <- sum(is.na(var_data))
    missing_percent <- round(100 * missing_count / nrow(data), 2)

    valid_levels <- c(non_missing_levels, "Missing")
    valid_freq <- c(as.numeric(freq), missing_count)
    valid_percent <- c(percent, missing_percent)
    valid_smd <- c(smd, NA)

    messages <- ifelse(valid_smd > SMD_flag & !is.na(valid_smd),
                       paste(var, "level exceeds SMD threshold. Consider collapsing categories."),
                       "")
    if (missing_percent > percentage_flag) {
      messages[length(messages)] <- paste(var, "is missing", missing_percent, "% data. Consider imputation or removal.")
    }

    var_result <- data.frame(
      Variable = var,
      Level = valid_levels,
      Frequency = valid_freq,
      Percent = valid_percent,
      SMD = valid_smd,
      Message = messages
    )

    result <- rbind(result, var_result)
  }

  # Round SMD values to 2 decimal places
  result$SMD <- round(result$SMD, 2)

  # Create a well-styled summary table using kableExtra
  kable_table <- result %>%
    kbl(
      col.names = c("Variable", "Level", "Frequency", "Percent", "SMD", "Message"),
      align = c("l", "l", "r", "r", "r", "l"),
      row.names = FALSE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE
    ) %>%
    column_spec(4, color = ifelse(!is.na(result$Percent) & result$Percent > percentage_flag,
                                  "red", "black")) %>%
    column_spec(5, color = ifelse(!is.na(result$SMD) & result$SMD > SMD_flag,
                                  "red", "black")) %>%
    column_spec(6, color = "royalblue3")  # Set Message column color to royalblue3

  if (cat_raw_output) {
    return(result)
  }

  return(kable_table)
}


