

#' @title a function to generate boxplots and density plots for continuous data
#'
#' @param data a dataframe
#' @param save_plots boolean, if TRUE will save plots in specified directory
#' @param output_dir directory for saving plots
#'
#' @return p
#' @export
#'
#'
#'
#'
generate_boxplots <- function(data, save_plots = FALSE, output_dir = "boxplots") {
  # load required package
  library(ggplot2)

  # Create directory for saving plots, if needed
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  # Loop through each numeric variable
  for (var in names(data)) {
    # Generate the plot
    p <- ggplot(data, aes_string(y = var)) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", var), y = "Value") +
      theme_minimal()

    # Print the plot
    print(p)

    # Save the plot, if requested
    if (save_plots) {
      filename <- file.path(output_dir, paste0(var, "_boxplot.png"))
      ggsave(filename = filename, plot = p)
    }
  }
}

generate_density_plots <- function(data, save_plots = FALSE, output_dir = "density_plots") {
  # Check if input is a valid data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Create directory for saving plots, if needed
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  # Loop through each numeric variable
  for (var in names(data)) {
    # Generate the density plot
    p <- ggplot(data, aes_string(x = var)) +
      geom_density(fill = "blue", alpha = 0.5) +
      labs(title = paste("Density Plot of", var), x = "Value", y = "Density") +
      theme_minimal()

    # Print the plot
    print(p)

    # Save the plot, if requested
    if (save_plots) {
      filename <- file.path(output_dir, paste0(var, "_density_plot.png"))
      ggsave(filename = filename, plot = p)
    }
  }
}
