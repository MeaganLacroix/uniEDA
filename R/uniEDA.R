
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
                   exclud_vars = NULL        # varibles to be excluded, such as IDs
                   ) {

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
                         outlier_flag = outlier_flag))

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
