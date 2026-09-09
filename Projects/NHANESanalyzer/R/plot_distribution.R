#-----------------------------------------------------------------------------------------------------
#' Plot the distribution of a single body measurement
#'
#' @param df from data frame
#' @param var name of a column in the df (string)
#' @param bins the number of bins for the histogram (default = 30)
#'
#' @return a histogram showing the distribution of a varaible across all rows
#' @export
#'
#' @examples
#' df <- get_data()
#' plot_distribution(df, "BMI")
plot_distribution <- function(df, var, bins = 30) {

  check_bmx_format(df)

  if (!(var %in% var_names())) {
    stop(paste("Variable", var, "not found. Use var_names() to see available variables."))
  }
  #-------------------------------------------list.files("R")------------------------------------------------------------

  #Set column, value, and other variable values.

  col <- variables[[var]][1]
  vals <- df[[col]]
  vals <- vals[!is.na(vals)]

  if (length(vals) == 0) {
    stop(paste("No non-missing values found for", var))
  }

  mean_val <- mean(vals)
  median_val <- median(vals)

  #---------------------------------------------------------------------------------------------------------
  # histogram of selected column of data
  # added descriptive statistics to help summarize data (mean median)
  # included labeling of plot and y axis to match x axis
  # added count of rows in column

  ggplot2::ggplot(data.frame(x = vals), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(bins = bins, fill = "blue", color = "white", alpha = 0.8) +
    ggplot2::geom_vline(xintercept = mean_val, color = "red", linetype = "dashed", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = median_val, color = "green", linetype = "dashed", linewidth = 0.8) +
    ggplot2::annotate("text", x = mean_val, y = Inf, label = paste("Mean:", round(mean_val, 1)),
                      color = "red", vjust = 2, hjust = -0.1, size = 3.5) +
    ggplot2::annotate("text", x = median_val, y = Inf, label = paste("Median:", round(median_val, 1)),
                      color = "green", vjust = 4, hjust = -0.1, size = 3.5) +
    ggplot2::labs(
      title = paste("distribution of", var),
      x = var,
      y = "count",
      caption = paste0("n = ", length(vals), " (", sum(is.na(df[[col]])), " missing)")
    ) +
    ggplot2::theme_minimal()
}
