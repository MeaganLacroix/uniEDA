summarize_cat <- function(data,
                          percentage_missing = 30,
                          SMD_flag = 0.2,
                          cat_raw_output = FALSE) {
  # Check for empty dataframe
  if (nrow(data) == 0) {
    stop("The input dataframe is empty. Please provide a non-empty dataframe.")
  }

  # Check for datasets with all NA columns
  if (all(sapply(data, function(col) all(is.na(col))))) {
    stop("The input dataset contains only NA values in all columns. Please provide a valid dataset.")
  }

  # Check for non-data frame input
  if (!is.data.frame(data)) {
    stop("The input must be a data frame.")
  }

  result <- data.frame(
    Variable = character(),
    Level = character(),
    Frequency = numeric(),
    Percent = numeric(),
    SMD = numeric(),
    Message = character(),
    stringsAsFactors = FALSE
  )

  for (var in names(data)) {
    var_data <- data[[var]]

    # Skip variables with all NA or a single unique value
    if (is.null(var_data) || all(is.na(var_data)) || length(unique(var_data[!is.na(var_data)])) <= 1) {
      next
    }

    # Replace NA with "Missing" for consistent handling
    var_data <- as.character(var_data)
    var_data[is.na(var_data)] <- "Missing"

    # Calculate basic statistics
    freq <- table(var_data)
    total <- sum(freq)
    percent <- round(100 * freq / total, 2)

    # Calculate SMD
    non_missing_levels <- setdiff(names(freq), "Missing")
    smd <- sapply(non_missing_levels, function(level) {
      p1 <- sum(var_data == level) / total
      p2 <- sum(var_data != level) / (total * (length(non_missing_levels) - 1))
      sqrt_pooled_var <- sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
      if (sqrt_pooled_var == 0) return(0)
      abs(p1 - p2) / sqrt_pooled_var
    })

    # Add NA for "Missing" in SMD
    valid_levels <- names(freq)
    valid_freq <- as.numeric(freq)
    valid_percent <- percent
    valid_smd <- c(smd, NA)[match(valid_levels, c(non_missing_levels, "Missing"))]

    # Create Messages
    messages <- ifelse(valid_smd > SMD_flag & !is.na(valid_smd),
                       paste(var, "level exceeds SMD threshold. Consider collapsing categories."),
                       "")
    if ("Missing" %in% valid_levels) {
      missing_percent <- valid_percent[valid_levels == "Missing"]
      if (missing_percent > percentage_missing) {
        messages[valid_levels == "Missing"] <- paste(var, "is missing", missing_percent, "% data. Consider imputation or removal.")
      }
    }

    # Align vector lengths
    max_length <- max(length(valid_levels), length(valid_freq), length(valid_percent), length(valid_smd), length(messages))
    valid_levels <- rep(valid_levels, length.out = max_length)
    valid_freq <- rep(valid_freq, length.out = max_length)
    valid_percent <- rep(valid_percent, length.out = max_length)
    valid_smd <- rep(valid_smd, length.out = max_length)
    messages <- rep(messages, length.out = max_length)

    # Create the variable's result data frame
    var_result <- data.frame(
      Variable = var,
      Level = valid_levels,
      Frequency = valid_freq,
      Percent = valid_percent,
      SMD = valid_smd,
      Message = messages,
      stringsAsFactors = FALSE
    )

    result <- rbind(result, var_result)
  }

  # Round SMD values to 2 decimal places
  result$SMD <- round(result$SMD, 2)

  if (cat_raw_output) {
    return(result)
  }

  # Create kableExtra summary table
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
    column_spec(6, color = "royalblue3")

  return(kable_table)
}


