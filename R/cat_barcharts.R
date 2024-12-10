


#' @title a function to generate bar charts for categorical data
#'
#' @param data a dataframe
#'
#' @return plot
#' @export
#'
#'
#'
generate_bar_charts <- function(data) {
  library(here)
  library(ggplot2)
  library(dplyr)

  # Generate and display bar charts
  for (var in names(data)) {
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
