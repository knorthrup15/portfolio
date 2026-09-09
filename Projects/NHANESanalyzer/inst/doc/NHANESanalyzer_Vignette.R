## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
suppressPackageStartupMessages(library(NHANESanalyzer))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(haven))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tibble))

## ----eval = FALSE-------------------------------------------------------------
# # common packages
# library(NHANESanalyzer)
# library(dplyr)
# library(haven)
# library(ggplot2)
# library(tibble)

## -----------------------------------------------------------------------------
df <- get_data()

## -----------------------------------------------------------------------------
var_names()

## -----------------------------------------------------------------------------
nhanes_results(130378, df)
nhanes_results(130378, df, c("recumbent length", "BMI"))

## -----------------------------------------------------------------------------
get_ratio(130378, df, "waist circumference", "hip circumference")

## -----------------------------------------------------------------------------
compare(130378, df, "BMI")

## -----------------------------------------------------------------------------
var_averages()

## -----------------------------------------------------------------------------
plot_distribution(df, "arm circumference")

## -----------------------------------------------------------------------------
plot_scatter(df, "height", "BMI")

## -----------------------------------------------------------------------------
plot_kde(df, "upper leg length", h = 5)

## -----------------------------------------------------------------------------
summarize_numeric(df, c("height", "BMI"))

## -----------------------------------------------------------------------------
summarize_categorical(df, c("head circumference"))

## -----------------------------------------------------------------------------
missing_summary(df)

## -----------------------------------------------------------------------------
summarize_by_group(df, "waist circumference", "BMI")

## -----------------------------------------------------------------------------
NHANESanalyzer::summarize_all(df)

