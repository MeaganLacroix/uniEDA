#' Example Dataset for the Package
#'
#' @description A dataset recreated from the paper "Implications of armed conflict for maternal and child health: A regression analysis of data from 181 countries for 2000-2019"
#'              by Jawad(2021). All armed conflict variables are lagged by a year.
#'
#' @format A data frame with 3720 rows and 21 variables:
#' \describe{
#'   \item{country_name}{A character variable containing country names}
#'   \item{ISO}{A character variable representing the names of countries}
#'   \item{region}{A character variable naming the region of the country}
#'   \item{year}{A numeric variable representing the year of data collection, ranging from 2000 to 2020}
#'   \item{gdp1000}{A numeric variable representing GDP per capita of the country}
#'   \item{OECD}{A binary numeric variable representing country membership in the Organisation for Economic Co-operation and Development, 0 = No, 1 = Yes}
#'   \item{OECD2023}{A binary numeric variable representing country membership in the Organisation for Economic Co-operation and Development as of 2023, 0 = No, 1 = Yes}
#'   \item{popdens}{A numeric variable representing population density of the country}
#'   \item{urban}{A numeric variable representing the percentage of the population living in urban areas}
#'   \item{agedep}{A numeric variable representing the age dependency ratio, which is the proportion of dependents aged <15 years or over 64 years per 100 working age individuals}
#'   \item{male_edu}{A numeric variable representing years of male education per capita (age standardized)}
#'   \item{temp}{A numeric variable representing mean population-weighted annual temperature in degrees Celsius}
#'   \item{rainfall1000}{A numeric variable representing mean population-weighted annual rainfall in millimetres per year, scaled down by 1000}
#'   \item{totdeath}{A numeric variable representing total number of battle-related deaths}
#'   \item{armconf1}{A binary numeric variable representing armed conflict, 0 = No, < 25 battle-related deaths, 1 = Yes, >=25 battle-related deaths}
#'   \item{drought}{A binary numeric variable representing drought, 0 = no drought, 1 = drought}
#'   \item{earthquake}{A binary numeric variable representing earthquake, 0 = no earthquake, 1 = earthquake}
#'   \item{Mor_inf}{A numeric variable representing infant mortality rate per 1000 live births}
#'   \item{Mor_neonat}{A numeric variable representing neonatal mortality rate per 1000 live births}
#'   \item{Mor_under5}{A numeric variable representing children under 5 mortality rate per 1000 live births}
#'   \item{Mor_mat}{A numeric variable representing maternal mortality rate per 100,000 live births}
#' }
#'
#' @details
#' This dataset is provided for illustrative purposes and showcases the usage of parameter specifications in the UniEDA function
#'
#' @examples
#' # Load the dataset
#' data(armed_conf)
#'
#' # Display the first few rows
#' head(armed_conf)
#'
#' # Example usage with a package function
#' uniEDA(
#' armed_conf,
#' cont_boxplots = TRUE,
#' cont_densplots = TRUE,
#' cat_barcharts = TRUE,
#' exclude_vars = "ISO")
#'
#' @source Jawad M, Hone T, Vamos EP, Cetorelli V, Millett C.
#' Implications of armed conflict for maternal and child health: A regression analysis of data from 181 countries for 2000-2019.
#' PLoS Med. 2021 Sep 28;18(9):e1003810. doi: 10.1371/journal.pmed.1003810. PMID: 34582455; PMCID: PMC8478221.
#'
"armed_conf"

