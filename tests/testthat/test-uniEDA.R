################test continuous tables#####################################################################
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

########################test uniEDA function#########################################################

#####################test categorical bar charts ###################################################
test_that("generate_bar_charts works correctly", {
  data("iris")
  expect_silent(generate_bar_charts(iris)) # No errors

  data("esoph")
  expect_silent(generate_bar_charts(esoph))
})
