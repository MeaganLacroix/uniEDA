library(here)
library(ggplot2)
library(dplyr)

data <- read.csv(here("Data", "finaldata.csv"), header = TRUE)

# Function to generate bar charts for categorical variables
generate_bar_charts <- function(data, exclude_vars = NULL) {
  # Identify categorical variables (factors or character)
  cat_vars <- names(data)[sapply(data, function(col) is.character(col) || is.factor(col) || length(unique(col)) < 20)]
  
  # Exclude specified variables
  if (!is.null(exclude_vars)) {
    cat_vars <- setdiff(cat_vars, exclude_vars)
  }
  
  if (length(cat_vars) == 0) {
    stop("No suitable categorical variables found in the dataset.")
  }
  
  # Generate and display bar charts
  for (var in cat_vars) {
    # Get unique values of the variable
    unique_vals <- unique(na.omit(data[[var]]))
    is_binary <- length(unique_vals) == 2
    
    plot <- ggplot(data, aes(x = factor(.data[[var]], levels = unique_vals))) +
      geom_bar(aes(y = (..count..) / sum(..count..) * 100), fill = "skyblue", color = "black") +
      geom_text(stat = "count", aes(y = (..count..) / sum(..count..) * 100, 
                                    label = sprintf("%.1f%%", (..count..) / sum(..count..) * 100)), 
                vjust = -0.5) +
      labs(title = paste("Bar Chart of", var),
           x = var,
           y = "Percentage") +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) # Ensure y-axis is labeled in percentages
    
    # Adjust x-axis for binary and non-binary variables
    if (is_binary) {
      plot <- plot + scale_x_discrete(labels = unique_vals)
    } else {
      plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    print(plot)  # Display plot
  }
}

# Example usage: Exclude country_name and ISO
generate_bar_charts(data, exclude_vars = c("country_name", "ISO"))

