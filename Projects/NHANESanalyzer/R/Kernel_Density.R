#---------------------------------------------------------------------------------------------
# Create a Kernel Density Estimate curve along with shaded region underneath
# Added labels for the title, x and y axis
# added count of rows in column along with specified bandwidth used
# bandwidth can be choosen or left to default value by ignoring it
# compited KDE using Guassian Kernel

#' Get KDE Curve with shaded region
#'
#' @param df the data frame
#' @param var name for the vraible examined (string)
#' @param h is bandwidth to control how smooth curve is. Default is set but can be changed by choosing new h.
#'
#' @return a kde curve
#' @export
#'
#' @examples
#' df <- get_data()
#' plot_kde(df, "BMI")

plot_kde <- function(df, var, h = NULL){

  check_bmx_format(df)

  if(!(var %in% var_names())){
    stop(paste("Variable", var, "not found. Use var_names() to see available variables."))
  }

  col  <- variables[[var]][1]
  vals <- df[[col]]
  vals <- vals[!is.na(vals)]

  if(length(vals) == 0){
    stop(paste("No non-missing values found for", var))
  }
#default bandwidth
  if(is.null(h)){
    h <- bw.nrd0(vals)
  }

  kde <- density(vals, bw = h)
  kde_df <- data.frame(x = kde$x, y = kde$y)

  ggplot2::ggplot(kde_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(color = "blue", linewidth = 0.9) +
    ggplot2::geom_area(fill = "blue", alpha = 0.3) +
    ggplot2::labs(
      title   = paste("Kernel Density Estimate of", var),
      x       = var,
      y       = "density",
      caption = paste0("n = ", length(vals), "  |  bandwidth = ", round(h, 3))
    ) +
    ggplot2::theme_minimal()
}
