#----------------------------------------------------------------------
# Create a scatter plot function using ggplot using df and columns as x and y variables
# to comepare correlation between two variables.
# added labels of x and y axis, and title.
# added in count of rows
# added regression line through the data to show trend between data.

#'Get scatterplot of two variables
#'
#' @param df for the data frame
#' @param x_var for one variable to use for x axis (string)
#' @param y_var for another variable to use for y axis (string)
#' @param smooth curve through the scatterplot showubg trend between data. Can be toggled on and off with TRUE/FALSE (String vector)
#'
#' @return a scatterplot showing trend between two variables
#' @export
#'
#' @examples
#' df <- get_data()
#' plot_scatter(df, "BMI", "arm circumference")

plot_scatter <- function(df, x_var, y_var, smooth = TRUE){

  check_bmx_format(df)

  for(name in c(x_var, y_var)){
    if(!(name %in% var_names())){
      stop(paste("Variable", name, "not found. Use var_names() to see available variables."))
    }
  }

  col1 <- variables[[x_var]][1]
  col2 <- variables[[y_var]][1]

  plot_data <- df[!is.na(df[[col1]]) & !is.na(df[[col2]]), ]

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[col1]], y = .data[[col2]])) +
    ggplot2::geom_point(alpha = 0.3, color = "blue", size = 0.8) +
    ggplot2::labs(
      title   = paste(x_var, "vs", y_var),
      x       = x_var,
      y       = y_var,
      caption = paste0("n = ", nrow(plot_data))
    ) +
    ggplot2::theme_minimal()

  if(smooth){
    plot <- plot + ggplot2::geom_smooth(method = "loess", color = "red", se = TRUE, linewidth = 0.8)
  }

  plot
}

