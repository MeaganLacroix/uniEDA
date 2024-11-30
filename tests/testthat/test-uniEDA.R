
################ test continuous tables #####################################################################
test_that("summarize_cont works with a standard dataset", {
  data <- data.frame(
    Var1 = rnorm(100, mean = 50, sd = 10),
    Var2 = runif(100, min = 0, max = 100),
    Var3 = c(rep(NA, 10), rpois(90, lambda = 5))
  )

  result <- summarize_cont(data)
  expect_s3_class(result, "kableExtra")
})

test_that("summarize_cont handles empty datasets gracefully", {
  data <- data.frame()
  expect_error(
    summarize_cont(data),
    "The input dataframe is empty. Please provide a non-empty dataframe.")
})

test_that("summarize_cont handles NA datasets gracefully", {
  data <- data.frame(Var1 = rep(NA, 100))
  expect_error(
    summarize_cont(data),
    "The input dataset contains only NA values in all columns. Please provide a valid dataset.")
})

test_that("summarize_cont works with a single-column dataset", {
  data <- data.frame(Var1 = rnorm(100, mean = 50, sd = 10))
  result <- summarize_cont(data)
  expect_s3_class(result, "kableExtra")
})


test_that("summarize_cont errors for non-data frame input", {
  expect_error(summarize_cont(matrix(1:10, nrow = 2)))
})


test_that("summarize_cont handles NULL input", {
  expect_error(summarize_cont(NULL))
})

test_that("summarize_cont handles large datasets", {
  data <- data.frame(matrix(rnorm(1e6), ncol = 10))
  result <- summarize_cont(data)
  expect_s3_class(result, "kableExtra")
})

test_that("summarize_cont calculates all metrics correctly", {
  # Controlled dataset
  test_data <- data.frame(
    A = c(1, 2, 3, 4, 5),
    B = c(10, 20, NA, 40, 50),
    C = c(100, 200, 300, 400, NA)
  )

  # Run summarize_cont with raw_output = TRUE
  result <- summarize_cont(test_data, cont_raw_output = TRUE)

  # Manually calculate expected results
  expected_results <- data.frame(
    Variable = c("A", "B", "C"),
    Mean = c(round(mean(1:5), 2), round(mean(c(10, 20, 40, 50), na.rm = TRUE), 2), round(mean(c(100, 200, 300, 400), na.rm = TRUE), 2)),
    SD = c(round(sd(1:5), 2), round(sd(c(10, 20, 40, 50), na.rm = TRUE), 2), round(sd(c(100, 200, 300, 400), na.rm = TRUE), 2)),
    Missingpercent = c(0, 20, 20),
    Min = c(1, 10, 100),
    Max = c(5, 50, 400),
    Q1 =  c(
      quantile(c(1, 2, 3, 4, 5), 0.25, type = 7),
      quantile(c(10, 20, 40, 50), 0.25, na.rm = TRUE),
      quantile(c(100, 200, 300, 400), 0.25, na.rm = TRUE)),
    Q3 = c(
      quantile(c(1, 2, 3, 4, 5), 0.75, type = 7),
      quantile(c(10, 20, 40, 50), 0.75, na.rm = TRUE, type = 7),
      quantile(c(100, 200, 300, 400), 0.75, na.rm = TRUE, type = 7)),
    Skewness = c(round(e1071::skewness(1:5), 2), round(e1071::skewness(c(10, 20, 40, 50), na.rm = TRUE), 2), round(e1071::skewness(c(100, 200, 300, 400), na.rm = TRUE), 2)),
    Kurtosis = c(round(e1071::kurtosis(1:5), 2), round(e1071::kurtosis(c(10, 20, 40, 50), na.rm = TRUE), 2), round(e1071::kurtosis(c(100, 200, 300, 400), na.rm = TRUE), 2)),
    NOutliers = c(0, 0, 0),  # No outliers in this dataset
    PercentOutliers = c(0, 0, 0)  # No outliers as percentage
  )

  # Check that the result matches the expected results for all metrics
  for (col_name in names(expected_results)) {
    expect_equal(result[[col_name]], expected_results[[col_name]], info = paste("Mismatch in column:", col_name))
  }
})

test_that("summarize_cont flags work correctly", {
  # 1. SkewnessFlag
  skewed_data <- data.frame(Var1 = c(1, 1, 1, 1, 10, 100, 900000))
  result <- summarize_cont(skewed_data, skewness_flag = 1.5, cont_raw_output = TRUE)
  expect_true(any(grepl("Highly skewed", result$SkewnessFlag)))

  # 2. KurtosisFlag
  kurtotic_data <- data.frame(Var1 = c(1, 2, 2, 2, 1000))
  result <- summarize_cont(kurtotic_data, cont_raw_output = TRUE)
  expect_true(any(grepl("Excess kurtosis", result$KurtosisFlag)))

  # 3. MissingFlag
  missing_data <- data.frame(Var1 = c(NA, NA, 1, 2, 3))
  result <- summarize_cont(missing_data, cont_raw_output = TRUE)
  expect_true(any(grepl("Missing", result$MissingFlag)))

  # 4. CVFlag
  high_variability_data <- data.frame(Var1 = c(1, 100, 200, 300, 400))
  result <- summarize_cont(high_variability_data, cont_raw_output = TRUE)
  expect_true(any(grepl("High variability", result$CVFlag)))

  # 5. OutlierFlag
  outlier_data <- data.frame(Var1 = c(1, 2, 3, 4, 100, 1000))
  result <- summarize_cont(outlier_data, cont_raw_output = TRUE)
  expect_true(any(grepl("Outliers", result$OutlierFlag)))
})


##################### test continuous plots ####################################
test_that("generate_boxplots works correctly", {
  data("iris")
  expect_silent(generate_boxplots(iris)) # No errors

  data("esoph")
  expect_silent(generate_boxplots(esoph))
})

test_that("generate_density_plots works correctly", {
  data("iris")
  expect_silent(generate_density_plots(iris)) # No errors

  data("esoph")
  expect_silent(generate_density_plots(esoph))
})

################ test categorical table #####################################################################

#### Test: Output is a kableExtra table ####

test_that("summarize_cat returns a kableExtra object", {
  data("iris")
  expect_silent(summarize_cat(iris)) # No errors

  data("esoph")
  expect_silent(summarize_cat(esoph))
}) # test passed

test_that("summarize_cat returns a kableExtra object", {
  data <- data.frame(
    var1 = c("A", "B", "A", "C", "B", "A", NA),
    var2 = c(1, 2, 1, 2, 1, 2, 1)
  )
  result <- summarize_cat(data)
  expect_s3_class(result, "kableExtra")
}) # test passed

#### Test: With a Standard Dataset ####

test_that("summarize_cat works with a standard dataset", {
  data <- data.frame(
    var1 = c("A", "B", "A", NA, "B", "A", NA),
    var2 = c("X", "Y", "X", "Z", NA, "X", "Y")
  )

  result <- summarize_cat(data, cat_raw_output = TRUE)
  expect_true("Missing" %in% result$Level)
  expect_true("A" %in% result$Level)
  expect_s3_class(result, "data.frame")
}) #test passed

#### Test: Handling of Empty Datasets ####

test_that("summarize_cat handles empty datasets gracefully", {
  data <- data.frame()
  expect_error(
    summarize_cat(data, cat_raw_output = TRUE),
    "The input dataframe is empty. Please provide a non-empty dataframe."
  )
}) #test passed


#### Test: NA-Only Dataset ####

test_that("summarize_cat handles NA datasets gracefully", {
  data <- data.frame(var1 = rep(NA, 10))
  expect_error(
    summarize_cat(data, cat_raw_output = TRUE),
    "The input dataset contains only NA values in all columns. Please provide a valid dataset."
  )
}) #test passed

#### Test: Single-Column Dataset ####

test_that("summarize_cat works with a single-column dataset", {
  data <- data.frame(var1 = c("A", "B", "A", "C", NA, "B", "A"))
  result <- summarize_cat(data, cat_raw_output = TRUE)
  expect_true("Missing" %in% result$Level)
  expect_s3_class(result, "data.frame")
}) #test passed


#### Test: Non-Data Frame Input ####

test_that("summarize_cat errors for non-data frame input", {
  expect_error(summarize_cat(matrix(1:10, nrow = 2)))
}) #test passed


#### Test: Large Datasets ####

test_that("summarize_cat handles large datasets", {
  data <- data.frame(matrix(sample(letters, 1e6, replace = TRUE), ncol = 10))
  result <- summarize_cat(data, cat_raw_output = TRUE)
  expect_s3_class(result, "data.frame")
}) # test passed

#### Test: Calculations ####

test_that("summarize_cat calculates metrics correctly", {
  data <- data.frame(
    var1 = c("A", "B", "A", NA, "B", "A", NA)
  )

  result <- summarize_cat(data, cat_raw_output = TRUE)

  # Manually calculate expected values
  expected_freq <- c(3, 2, 2)  # "A", "B", "Missing"
  expected_percent <- round(100 * expected_freq / sum(expected_freq), 2)
  expected_levels <- c("A", "B", "Missing")

  # Validate frequencies, percentages, and levels
  expect_equal(result$Frequency, expected_freq)
  expect_equal(result$Percent, expected_percent)
  expect_equal(result$Level, expected_levels)
}) #test passed


#### Test: Flags ####

test_that("summarize_cat flags work correctly", {
  data <- data.frame(
    var1 = c("A", "B", "A", "C", "B", "A", NA, NA, NA, NA)
  )

  result <- summarize_cat(data, percentage_flag = 30, SMD_flag = 0.2, cat_raw_output = TRUE)

  # Print the result for manual inspection
  print(result)

  # Check for SMD threshold message
  expect_true(any(grepl("level exceeds SMD threshold", result$Message, fixed = TRUE)))

  # Check for missing data percentage message
  expect_true(any(grepl("is missing", result$Message, fixed = TRUE)))
}) #test passed


#### Test: Multiple Columns ####

test_that("summarize_cat works with multiple categorical variables", {
  data <- data.frame(
    var1 = c("A", "B", "A", NA, "B", "A", NA),
    var2 = c("X", "Y", "X", "Z", NA, "X", "Y")
  )

  result <- summarize_cat(data, cat_raw_output = TRUE)

  # Validate output includes levels and missing data for all variables
  expect_true(all(c("A", "B", "Missing") %in% result$Level))
  expect_true(all(c("X", "Y", "Z", "Missing") %in% result$Level))
}) #test passed

#### Test: Missing Values ####
test_that("summarize_cat handles missing values correctly", {
  data <- data.frame(
    var1 = c("A", "B", "A", NA, "B", "A", NA)
  )

  result <- summarize_cat(data, percentage_flag = 50, SMD_flag = 0.2, cat_raw_output = TRUE)

  # Check that "Missing" is included as a level
  expect_true("Missing" %in% result$Level)

  # Check that the percentage for "Missing" is correct
  missing_row <- result[result$Level == "Missing", ]
  expect_equal(missing_row$Percent, round(2 / 7 * 100, 2))
}) # test passed

#### Test: All NA Columns ####

test_that("summarize_cat stops when all columns are NA", {
  data <- data.frame(
    var1 = rep(NA, 10)
  )

  expect_error(
    summarize_cat(data, percentage_flag = 50, SMD_flag = 0.2, cat_raw_output = TRUE),
    "The input dataset contains only NA values in all columns. Please provide a valid dataset."
  )
}) #test passed


#### Test: No Missing Values ####

test_that("summarize_cat works without missing values", {
  data <- data.frame(
    var1 = c("A", "B", "A", "B", "C")
  )

  result <- summarize_cat(data, percentage_flag = 50, SMD_flag = 0.2, cat_raw_output = TRUE)

  # Check that "Missing" is not included as a level
  expect_false("Missing" %in% result$Level)
})


#### Test: Large datasets with Missing Values ####

test_that("summarize_cat handles large datasets with missing values", {
  data <- data.frame(
    var1 = c(sample(letters[1:5], 1e4, replace = TRUE), rep(NA, 500))
  )

  result <- summarize_cat(data, percentage_flag = 10, SMD_flag = 0.2, cat_raw_output = TRUE)

  # Check that "Missing" is included
  expect_true("Missing" %in% result$Level)
}) # test passed


##################### test categorical bar charts ###################################################
test_that("generate_bar_charts works correctly", {
  data("iris")
  expect_silent(generate_bar_charts(iris)) # No errors

  data("esoph")
  expect_silent(generate_bar_charts(esoph))
})

######################## test uniEDA function #########################################################

test_that("uniEDA works with a standard dataset", {
  data <- data.frame(
    Var1 = rnorm(100, mean = 50, sd = 10),
    Var2 = runif(100, min = 0, max = 100),
    Var3 = c(rep(NA, 10), rpois(90, lambda = 5))
  )

  result <- uniEDA(data)
  expect_s3_class(result, "kableExtra")
})

test_that("uniEDA handles empty datasets gracefully", {
  data <- data.frame()
  expect_error(
    uniEDA(data),
    "The input dataframe is empty. Please provide a non-empty dataframe.")
})

test_that("uniEDA handles NA datasets gracefully", {
  data <- data.frame(Var1 = rep(NA, 100))
  expect_error(
    uniEDA(data),
    "The input dataset contains only NA values in all columns. Please provide a valid dataset.")
})

test_that("uniEDA works with a single-column dataset", {
  data <- data.frame(Var1 = rnorm(100, mean = 50, sd = 10))
  result <- uniEDA(data)
  expect_s3_class(result, "kableExtra")
})


test_that("uniEDA errors for non-data frame input", {
  expect_error(uniEDA(matrix(1:10, nrow = 2)))
})


test_that("uniEDA handles NULL input", {
  expect_error(uniEDA(NULL))
})

test_that("uniEDA handles large datasets", {
  data <- data.frame(matrix(rnorm(1e6), ncol = 10))
  result <- uniEDA(data)
  expect_s3_class(result, "kableExtra")
})

test_that("uniEDA throws an error for non-numeric flags", {
  data <- data.frame(A = rnorm(10), B = runif(10))
  expect_error(uniEDA(data, cv_flag = "non-numeric"), "Argument must be numeric")
  expect_error(uniEDA(data, missing_flag = "non-numeric"), "Argument must be numeric")
  expect_error(uniEDA(data, skewness_flag = "non-numeric"), "Argument must be numeric")
  expect_error(uniEDA(data, kurtosis_flag = "non-numeric"), "Argument must be numeric")
  expect_error(uniEDA(data, outlier_flag = "non-numeric"), "Argument must be numeric")
  expect_error(uniEDA(data, min_category = "non-numeric"), "Argument must be numeric")
  expect_error(uniEDA(data, percentage_flag = "non-numeric"), "Argument must be numeric")
  expect_error(uniEDA(data, SMD_flag = "non-numeric"), "Argument must be numeric")
})

test_that("uniEDA throws an error for non-logical boolean parameters", {
  data <- data.frame(A = rnorm(10), B = runif(10))
  expect_error(uniEDA(data, cont_boxplots = "non-logical"), "Arugment must be logic")
  expect_error(uniEDA(data, cont_densplots = 5), "Arugment must be logic")
  expect_error(uniEDA(data, cat_barcharts = "non-logical"), "Arugment must be logic")
  expect_error(uniEDA(data, cat_raw_output = 1), "Arugment must be logic")
  expect_error(uniEDA(data, cont_raw_output = "non-logical"), "Arugment must be logic")
})

test_that("uniEDA handles invalid exclud_vars correctly", {
  data <- data.frame(A = rnorm(10), B = runif(10))
  expect_error(uniEDA(data, exclud_vars = "non-existent-column"),
               "Error in `select()`") # Adjust based on the actual error
})

test_that("uniEDA throws an error for mixed invalid inputs", {
  data <- data.frame(A = rnorm(10), B = runif(10))
  expect_error(uniEDA(data, cv_flag = "invalid", cont_boxplots = 5),
               "Argument must be numeric") # Adjust if multiple errors cascade
})


