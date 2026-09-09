# ---------------------------------------------------------
# Import necessary libraries
#' @import dplyr
#' @import haven
#' @import ggplot2

# ---------------------------------------------------------
# Human-readable variable dictionary

variables <- list(
  "recumbent length" = c("BMXRECUM", "BMIRECUM"),
  "head circumference" = c("BMXHEAD","BMIHEAD"),
  "height" = c("BMXHT", "BMIHT"),
  "BMI" = c("BMXBMI"),
  "upper leg length" = c("BMXLEG", "BMILEG"),
  "upper arm length" = c("BMXARML", "BMIARML"),
  "arm circumference" = c("BMXARMC", "BMIARMC"),
  "waist circumference" = c("BMXWAIST", "BMIWAIST"),
  "hip circumference" = c("BMXHIP", "BMIHIP")
)


# ---------------------------------------------------------
# Helper Function: Validate BMX dataset structure
# Checks that dataset contains required NHANES ID column (SEQN). Stops execution if SEQN is missing

#' Validate NHANES BMX dataset format
#'
#' @param df data frame to validate
#'
#' @return TRUE if valid, otherwise stops with error
#' @export

check_bmx_format <- function(df) {

  if (!is.data.frame(df)) {
    stop("Input must be a data.frame.")
  }

  required_vars <- c("SEQN")

  missing <- required_vars[!required_vars %in% names(df)]

  if (length(missing) > 0) {
    stop(paste("Missing required column(s):", paste(missing, collapse = ", ")))
  }

  TRUE
}


# ---------------------------------------------------------
# Helper Function: Numeric Summary
# Calculates basic descriptive statistics for numeric vectors. Used internally by summarize_numeric()

#' Compute numeric summary statistics
#'
#' @param x numeric vector
#' @param na_rm logical, remove NA values
#'
#' @return tibble with summary statistics
#' @export

safe_numeric_summary <- function(x, na_rm = TRUE) {

  if (!is.numeric(x)) {
    stop("Input must be numeric.")
  }

  tibble::tibble(
    n = sum(!is.na(x)),
    missing = sum(is.na(x)),
    mean = mean(x, na.rm = na_rm),
    median = median(x, na.rm = na_rm),
    sd = sd(x, na.rm = na_rm),
    min = if (all(is.na(x))) NA else min(x, na.rm = na_rm),
    max = if (all(is.na(x))) NA else max(x, na.rm = na_rm)
  )
}


# ---------------------------------------------------------
# Summarize Numeric Variables
# Takes one or more numeric variables and returns summary stats for each variable in the BMX dataset

#' Summarize numeric NHANES variables
#'
#' @param df data frame
#' @param vars character vector of human-readable variable names
#' @param na_rm logical, remove NA values
#'
#' @return tibble of summary statistics
#' @export
#'
#' @examples
#' df <- get_data()
#' summarize_numeric(df, c("height", "BMI"))

summarize_numeric <- function(df, vars, na_rm = TRUE) {

  check_bmx_format(df)

  real_vars <- unlist(variables[vars], use.names = FALSE)

  missing_vars <- real_vars[!real_vars %in% names(df)]
  if (length(missing_vars) > 0) {
    stop(paste("Variables not found:", paste(missing_vars, collapse = ", ")))
  }

  results <- lapply(real_vars, function(v) {

    x <- df[[v]]

    if (!is.numeric(x)) {
      stop(paste(v, "is not numeric in BMX dataset"))
    }

    readable <- names(which(sapply(variables, function(col) v %in% col)))

    out <- safe_numeric_summary(x, na_rm)
    out$variable <- readable
    out$column <- v
    out
  })

  dplyr::bind_rows(results)
}


# ---------------------------------------------------------
# Summarize Categorical Variables
# Produces frequency counts and proportions for categorical variables. Includes NA values in the output

#' Summarize categorical NHANES variables
#'
#' @param df data frame
#' @param vars character vector of variable names
#'
#' @return tibble with counts and proportions
#' @export
#'
#' @examples
#' df <- get_data()
#' summarize_categorical(df, c("head circumference"))

summarize_categorical <- function(df, vars) {

  check_bmx_format(df)

  real_vars <- unlist(variables[vars], use.names = FALSE)

  missing_vars <- real_vars[!real_vars %in% names(df)]
  if (length(missing_vars) > 0) {
    stop(paste("Variables not found:", paste(missing_vars, collapse = ", ")))
  }

  results <- lapply(real_vars, function(v) {

    x <- df[[v]]
    tbl <- table(x, useNA = "ifany")

    readable <- names(which(sapply(variables, function(col) v %in% col)))

    tibble::tibble(
      variable = readable,
      column = v,
      category = names(tbl),
      count = as.numeric(tbl),
      proportion = round(as.numeric(tbl) / sum(tbl, na.rm = TRUE), 4)
    )
  })

  dplyr::bind_rows(results)
}


# ---------------------------------------------------------
# Missing Data Summary
# Shows number and percentage of missing values per variable

#' Summarize missing values in NHANES dataset
#'
#' @param df data frame
#'
#' @return tibble with missing counts and percentages
#' @export
#'
#' @examples
#' df <- get_data()
#' missing_summary(df)

missing_summary <- function(df) {

  check_bmx_format(df)

  readable_names <- sapply(names(df), function(col) {
    match <- names(variables)[sapply(variables, function(v) col %in% v)]
    if (length(match) == 1) return(match)
    return(col)
  })

  tibble::tibble(
    variable = readable_names,
    missing_count = sapply(df, function(x) sum(is.na(x))),
    missing_percent = round(sapply(df, function(x) mean(is.na(x)) * 100), 2)
  )
}


# ---------------------------------------------------------
# Grouped Summary Statistics
# Compares a numeric variable across different groups

#' Compare numeric variable across groups
#'
#' @param df data frame
#' @param group_var grouping variable (human-readable)
#' @param target_var numeric variable (human-readable)
#' @param na_rm logical, remove NA values
#'
#' @return tibble of group statistics
#' @export
#'
#' @examples
#' df <- get_data()
#' summarize_by_group(df, "waist circumference", "BMI")

summarize_by_group <- function(df, group_var, target_var, na_rm = TRUE) {

  check_bmx_format(df)

  group_real <- variables[[group_var]][1]
  target_real <- variables[[target_var]][1]

  if (!group_real %in% names(df) || !target_real %in% names(df)) {
    stop("One or more variables not found in dataset.")
  }

  if (!is.numeric(df[[target_real]])) {
    stop("Target variable must be numeric.")
  }

  groups <- unique(df[[group_real]])
  groups <- groups[!is.na(groups)]

  results <- lapply(groups, function(g) {

    subset_vals <- df[df[[group_real]] == g, target_real]

    tibble::tibble(
      group = g,
      mean = mean(subset_vals, na.rm = na_rm),
      sd = sd(subset_vals, na.rm = na_rm),
      count = sum(!is.na(subset_vals))
    )
  })

  dplyr::bind_rows(results)
}


# ---------------------------------------------------------
# Full Dataset Summary Report
# Provides overall dataset summary including: numeric summaries, categorical summaries, and missing data

#' Generate full NHANES summary report
#'
#' @param df data frame
#'
#' @return list with metadata, numeric summary, and missingness
#' @export
#'
#' @examples
#' df <- get_data()
#' health_summary(df)

health_summary <- function(df) {

  check_bmx_format(df)

  numeric_vars <- var_names()

  list(
    metadata = list(
      n_rows = nrow(df),
      n_columns = ncol(df)
    ),

    numeric_summary = summarize_numeric(df, numeric_vars),

    categorical_summary = NULL,

    missingness = missing_summary(df)
  )
}

# ---------------------------------------------------------
# Convenience Function
# Runs full dataset summary in one call

#' Run full dataset summary
#'
#' @param df data frame
#'
#' @return full summary list
#' @export
#'
#' @examples
#' df <- get_data()
#' summarize_all(df)

summarize_all <- function(df) {
  health_summary(df)
}
