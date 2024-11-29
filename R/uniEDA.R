#' @title  univariate exploratory data analysis
#' @description a function that returns descriptive tables and figures for continuous and categorical data
#' @param data data frame, any dataset containing continuous and/or categorical data
#' @param cv_flag numeric, coefficient of variation flag in continuous table, calculated as SD/mean*100, default set to 30
#' @param missing_flag numeric, flag for percentage of missing data in continuous table, default set to 5%
#' @param skewness_flag numeric, flag for skewness in continuous table, default set to 2
#' @param kurtosis_flag numeric, flag for excess kurtosis (kurtosis - 3) in continuous table, default set to 2
#' @param outlier_flag numeric, flag for percentage of data that are outliers in continuous table, default set to 5%
#' @param min_category numeric, minimum number of categories to be considered as categorical variables, default set to 3
#' @param percentage_flag numeric, flag for frequency percent in categorical table, default set to 50
#' @param SMD_flag numeric, flag for standardized mean difference in categorical table, default set to .2
#' @param cont_boxplots logical, include continuous box plots TRUE or FALSE
#' @param cont_densplots logical, include continuous density plots TRUE or FALSE
#' @param cat_barcharts logical, include categorical bar charts TRUE or FALSE
#' @param cont_raw_output logical, if TRUE, returns continuous summary table as raw data, if FALSE, returns kable table
#' @param exclud_vars character, variables in dataset to be excluded
#'
#' @author Meagan Lacroix, Rebecca Raj, Syeda Aiman Fatima, Xinze Yu, Xingchen Hu
#' @examples
#' uniEDA(armed_conf, cont_boxplots = TRUE, cont_denseplots = TRUE, cat_barcharts = TRUE, exclud_vars = "ISO")
#'
#' @return
#' Continuous summary table: A tibble (if `cont_raw_output = TRUE`) or a `kableExtra` object (if `cont_raw_output = FALSE`):
#'
#' - **Variable**: Name of the variable.
#' - **N**: Number of non-missing values.
#' - **Missing**: Number of missing values.
#' - **Missingpercent**: Percentage of missing values.
#' - **Mean**: Mean of the variable.
#' - **Median**: Median of the variable.
#' - **SD**: Standard deviation.
#' - **Min**: Minimum value.
#' - **Max**: Maximum value.
#' - **Q1**: First quartile.
#' - **Q3**: Third quartile.
#' - **IQR**: Interquartile range.
#' - **Skewness**: Skewness of the variable.
#' - **Kurtosis**: Kurtosis of the variable.
#' - **NOutliers**: Number of outliers.
#' - **PercentOutliers**: Percentage of outliers.
#' - **Flags**: Columns for flagged variables, such as `SkewnessFlag`, `KurtosisFlag`, `MissingFlag`, `CVFlag`, and `OutlierFlag`.
#'
#' Categorical summary table: A tibble (if `cat_raw_output = TRUE`) or a `kableExtra` object (if `cat_raw_output = FALSE`):
#'
#'    - **Variable**: Name of the variable.
#'    - **Level**: Category levels.
#'    - **Frequency**: Frequency of each category.
#'    - **Percent**: Frequency percentage of each category.
#'    - **SMD**: Standardized mean difference between levels
#'
#' Boxplots for Continuous Variables (if cont_boxplots = TRUE)
#'    A `ggplot2` object showing boxplots of continuous variables.
#'
#' Density Plots for Continuous Variables (if cont_densplots = TRUE)
#'    A `ggplot2` object showing density plots for continuous variables.
#'
#' Bar Charts for Categorical Variables (if cat_barcharts = TRUE)
#'    A `ggplot2` object showing bar charts for categorical variables
#'
#' @export
#'
#'
uniEDA <- function(data,
                   cv_flag = 30,             # continuous criteria
                   missing_flag = 5,         # continuous criteria
                   skewness_flag = 2,        # continuous criteria
                   kurtosis_flag = 2,        # continuous criteria
                   outlier_flag = 5,         # continuous criteria
                   min_category = 3,         # minimum number of categories to be considered as categorical variables
                   percentage_flag = 50,     # categorical criteria
                   SMD_flag = 0.2,           # categorical criteria
                   cont_boxplots = FALSE,    # continuous plots
                   cont_densplots = FALSE,   # continuous plots
                   cat_barcharts = FALSE,    # categorical plots
                   cont_raw_output = FALSE,  # produce raw output in summary table
                   exclud_vars = NULL        # variables to be excluded, such as IDs
) {

  # Check if the dataframe is empty
  if (nrow(data) == 0) {
    stop("The input dataframe is empty. Please provide a non-empty dataframe.")
  }
  # Check if the dataframe is NA
  if (all(sapply(data, function(col) all(is.na(col))))) {
    stop("The input dataset contains only NA values in all columns. Please provide a valid dataset.")
  }

  if (!is.numeric(cv_flag) || !is.numeric(missing_flag) || !is.numeric(skewness_flag) || !is.numeric(kurtosis_flag)
      || !is.numeric(outlier_flag) || !is.numeric(min_category) || !is.numeric(percentage_flag) || !is.numeric(SMD_flag)) {
    stop("Argument must be numeric")
  }

  if(!is.logical(cont_boxplots) || !is.logical(cont_densplots) || !is.logical(cat_barcharts)) {
    stop("Arugment must be logic")
  }


  library(here())
  source(here("R", "cont_tables.R"))
  source(here("R", "cont_plots.R"))
  source(here("R", "cat_tables.R"))
  source(here("R", "cat_barcharts.R"))

  # Remove specified variables
  data <- data %>% select(setdiff(names(data), exclud_vars))

  # Functions to filter continuous variables
  identify_continuous_vars <- function(data, min_category = 3) {
    continuous_vars <- sapply(data, function(col) {
      is.numeric(col) && length(unique(col)) > min_category
    })
    return(names(continuous_vars[continuous_vars]))
    }
  select_numerical_data <- function(data, min_category = 3) {
    continuous_vars <- identify_continuous_vars(data, min_category)
    numeric_data <- data[, continuous_vars, drop = FALSE]
    return(numeric_data)
    }

  # Separate continuous and categorical variables
  numeric_data_ind <- identify_continuous_vars(data, min_category=min_category)
  numeric_data <- select_numerical_data(data, min_category=min_category)
  cat_data <- data %>% select(setdiff(names(data), numeric_data_ind))

  # find whether continuous and categorical variables are empty
  is.na.num <- length(numeric_data) == 0
  is.na.cat <- length(cat_data) == 0

  if(!is.na.num) {
    # Create summary table for continuous variables
    readline("Press Enter to proceed to continuous summary table...")
    print(summarize_cont(numeric_data,
                         cv_flag = cv_flag,
                         missing_flag = missing_flag,
                         skewness_flag = skewness_flag,
                         kurtosis_flag = kurtosis_flag,
                         outlier_flag = outlier_flag,
                         cont_raw_output = cont_raw_output))

    # Create boxplots and density plots for continuous variables
    if(cont_boxplots){
      readline("Press Enter to proceed to boxplots...")
      generate_boxplots(numeric_data)
    }
    if(cont_densplots){
      readline("Press Enter to proceed to density plots...")
      generate_density_plots(numeric_data)
    }
  } else {print("No suitable continuous variables found in the dataset!")}


  if(!is.na.cat) {
    # Create summary table for categorical variables
    readline("Press Enter to proceed to categorical summary table...")
    print(summarize_cat(cat_data,
                        percentage_flag = percentage_flag,
                        SMD_flag = SMD_flag))

    # Create bar charts for categorical variables
    if (cat_barcharts){
      readline("Press Enter to proceed to bar charts...")
      generate_bar_charts(cat_data)
    }
  } else {print("No suitable categorical variables found in the dataset!")}
}
