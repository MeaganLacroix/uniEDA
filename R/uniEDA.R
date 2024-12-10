#' @title  Univariate exploratory data analysis
#' @description A function that returns descriptive tables and figures for continuous and categorical data.
#'
#' @import dplyr
#'
#' @param data Data frame, any dataset containing continuous and/or categorical data.
#' @param cv_flag Numeric, coefficient of variation flag in continuous table, calculated as SD/mean*100, default set to 30.
#' @param missing_flag Numeric, flag for percentage of missing data in continuous table, default set to 5\%.
#' @param skewness_flag Numeric, flag for skewness in continuous table, default set to 2.
#' @param kurtosis_flag Numeric, flag for excess kurtosis (kurtosis - 3) in continuous table, default set to 2.
#' @param outlier_flag Numeric, flag for percentage of data that are outliers in continuous table, default set to 5\%.
#' @param min_category Numeric, minimum number of categories to be considered as categorical variables, default set to 3.
#' @param percentage_missing Numeric, flag for missing percent in categorical table, default set to 30\%.
#' @param SMD_flag Numeric, flag for standardized mean difference in categorical table, default set to .2.
#' @param cont_boxplots Logical, include continuous box plots TRUE or FALSE.
#' @param cont_densplots Logical, include continuous density plots TRUE or FALSE.
#' @param cat_barcharts Logical, include categorical bar charts TRUE or FALSE.
#' @param cont_raw_output Logical, if TRUE, returns continuous summary table as raw data, if FALSE, returns kable table.
#' @param cat_raw_output Logical, if TRUE, returns categorical summary table as raw data, if FALSE, returns kable table.
#' @param exclude_vars Character, variables in dataset to be excluded.
#'
#' @author Meagan Lacroix, Rebecca Raj, Syeda Aiman Fatima, Xinze Yu, Xingchen Hu.
#'
#' @examples
#' uniEDA(armed_conf, cont_boxplots = TRUE, cont_densplots = TRUE, cat_barcharts = TRUE, exclude_vars = "ISO")
#'
#' @references
#' Jawad M, Hone T, Vamos EP, Cetorelli V, Millett C.
#' Implications of armed conflict for maternal and child health: A regression analysis of data from 181 countries for 2000-2019.
#' PLoS Med. 2021 Sep 28;18(9):e1003810. doi: 10.1371/journal.pmed.1003810. PMID: 34582455; PMCID: PMC8478221.
#'
#' @return
#' \describe{
#'   \item{Continuous summary table}{
#'     A tibble (if \code{cont_raw_output = TRUE}) or a \code{kableExtra} object (if \code{cont_raw_output = FALSE}):
#'     \itemize{
#'       \item \strong{Variable}: Name of the variable.
#'       \item \strong{N}: Number of non-missing values.
#'       \item \strong{Missing}: Number of missing values.
#'       \item \strong{Missingpercent}: Percentage of missing values.
#'       \item \strong{Mean}: Mean of the variable.
#'       \item \strong{Median}: Median of the variable.
#'       \item \strong{SD}: Standard deviation.
#'       \item \strong{Min}: Minimum value.
#'       \item \strong{Max}: Maximum value.
#'       \item \strong{Q1}: First quartile.
#'       \item \strong{Q3}: Third quartile.
#'       \item \strong{IQR}: Interquartile range.
#'       \item \strong{Skewness}: Skewness of the variable.
#'       \item \strong{Kurtosis}: Kurtosis of the variable.
#'       \item \strong{NOutliers}: Number of outliers.
#'       \item \strong{PercentOutliers}: Percentage of outliers.
#'       \item \strong{Flags}: Columns for flagged variables, such as \code{SkewnessFlag}, \code{KurtosisFlag}, \code{MissingFlag}, \code{CVFlag}, and \code{OutlierFlag}.
#'     }
#'   }
#'   \item{Categorical summary table}{
#'     A tibble (if \code{cat_raw_output = TRUE}) or a \code{kableExtra} object (if \code{cat_raw_output = FALSE}):
#'     \itemize{
#'       \item \strong{Variable}: Name of the variable.
#'       \item \strong{Level}: Category levels.
#'       \item \strong{Frequency}: Frequency of each category.
#'       \item \strong{Percent}: Frequency percentage of each category.
#'       \item \strong{SMD}: Standardized mean difference between levels.
#'     }
#'   }
#'   \item{Boxplots for Continuous Variables (if cont_boxplots = TRUE)}{
#'     A \code{ggplot2} object showing boxplots of continuous variables.
#'   }
#'   \item{Density Plots for Continuous Variables (if cont_densplots = TRUE)}{
#'     A \code{ggplot2} object showing density plots for continuous variables.
#'   }
#'   \item{Bar Charts for Categorical Variables (if cat_barcharts = TRUE)}{
#'     A \code{ggplot2} object showing bar charts for categorical variables.
#'   }
#' }
#' @export

uniEDA <- function(data,
                   cv_flag = 30,             # continuous criteria
                   missing_flag = 5,         # continuous criteria
                   skewness_flag = 2,        # continuous criteria
                   kurtosis_flag = 2,        # continuous criteria
                   outlier_flag = 5,         # continuous criteria
                   min_category = 3,         # minimum number of categories to be considered as categorical variables
                   percentage_missing = 30,     # categorical criteria
                   SMD_flag = 0.2,           # categorical criteria
                   cont_boxplots = FALSE,    # continuous plots
                   cont_densplots = FALSE,   # continuous plots
                   cat_barcharts = FALSE,    # categorical plots
                   cont_raw_output = FALSE,  # produce raw output in summary table
                   cat_raw_output = FALSE,   # produce raw output in summary table
                   exclude_vars = NULL        # variables to be excluded, such as IDs
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
      || !is.numeric(outlier_flag) || !is.numeric(min_category) || !is.numeric(percentage_missing) || !is.numeric(SMD_flag)) {
    stop("Argument must be numeric")
  }

  if(!is.logical(cont_boxplots) || !is.logical(cont_densplots) || !is.logical(cat_barcharts) || !is.logical(cont_raw_output) || !is.logical(cat_raw_output)) {
    stop("Argument must be logic")
  }


  # Remove specified variables
  data <- data %>% select(setdiff(names(data), exclude_vars))

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
                        percentage_missing = percentage_missing,
                        SMD_flag = SMD_flag,
                        cat_raw_output = cat_raw_output))

    # Create bar charts for categorical variables
    if (cat_barcharts){
      readline("Press Enter to proceed to bar charts...")
      generate_bar_charts(cat_data)
    }
  } else {print("No suitable categorical variables found in the dataset!")}
}
