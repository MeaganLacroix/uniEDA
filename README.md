uniEDA streamlines univariate exploratory data analysis, offering detailed summaries and visual diagnostics for continuous and categorical variables. The package flags potential issues (e.g., skewness, kurtosis, outliers, missing data) using customizable thresholds. It generates intuitive plots like boxplots and bar charts and is ideal for data diagnostics and transformation recommendations.

## Installation Instructions

```r
install.packages("devtools")
library(devtools)
devtools::install_github("MeaganLacroix/uniEDA")
library(uniEDA)
```

## Example

In this example, we are using the armed conflict data (armed_conf) which was recreated from the paper "Implications of armed conflict for maternal and child health: A regression analysis of data from 181 countries for 2000-2019" by Jawad(2021). This code will produce a continuous summary table, categorical summary table, boxplots, density plots, and bar charts. 
```r
uniEDA(
armed_conf,
cv_flag = 30,                 # continuous criteria
missing_flag = 5,             # continuous criteria
skewness_flag = 2,            # continuous criteria
kurtosis_flag = 2,            # continuous criteria
outlier_flag = 5,             # continuous criteria
min_category = 3,             # minimum number of categories to be considered as categorical variables
percentage_missing = 30,      # categorical criteria
SMD_flag = 0.2,               # categorical criteria
cont_boxplots = TRUE,         # continuous plots
cont_densplots = TRUE,        # continuous plots
cat_barcharts = TRUE,         # categorical plots
cont_raw_output = FALSE,      # produce raw output in summary table
cat_raw_output = FALSE,       # produce raw output in summary table
exclude_vars = c("country_name", "region")    # variables to be excluded
)
```

 
