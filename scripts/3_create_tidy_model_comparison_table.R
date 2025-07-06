
rm(list = ls())
gc()

# Groundhog makes libraries consistent.
library("groundhog")
groundhog.day <- "2025-04-15"
libs <- c("metafor", "broom", "data.table",
          "ggplot2", "tidyr", "multcomp",
          "dplyr", "crayon",
          "glmmTMB",
          "cpp11", "withr", "colorspace", "mvtnorm",
          "foreach", "doSNOW")
groundhog.library(libs, groundhog.day)


guide <- readRDS("builds/batch_models_july_2025/model_guide.Rds")


# 1. Compare univariates to null models --------------------------------------

# >>> Cast wide -------------------



# >>> Compare -------------------------------------------------------------



# >>> Create tidy sidecar file --------------------------------------------



# 2. Compare location to location-scale -----------------------------------


# >>> Cast wide by location scale ID --------------------------------------


# >>> Compare -------------------------------------------------------------


# >>> Create sidecar ------------------------------------------------------




# 3. Merge products together ----------------------------------------------




