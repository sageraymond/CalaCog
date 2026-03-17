#
# 
# AIM: Filter to final models. Extract coefficients. And all that jazz
#
#

rm(list = ls())
gc()

# Groundhog makes libraries consistent.
library("groundhog")
groundhog.day <- "2025-04-15"
libs <- c("metafor", "broom", "data.table",
          "ggplot2", "tidyr", "multcomp",
          "dplyr", "crayon", "broom.mixed",
          "glmmTMB",
          "cpp11", "withr", "colorspace", "mvtnorm",
          "foreach", "doSNOW")
groundhog.library(libs, groundhog.day)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 0. Load data and guide --------------------------------------------------

master_guide <- readRDS("builds/batch_models_july_2025/model_guide_with_comparison_stats.Rds")

dat <- readRDS("builds/prepared_dataset.Rds")
dat

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
# 1. Let's look at our explicit hypotheses about urbanization -----------------------------------------------------------
master_guide

#sub_guide <- master_guide[var %in% c("urbanization",
 #                       "urbanization_score_scaled",
  #                      "pop_density_scaled",
   #                     "Road.density_scaled", "ANTH_scaled", "NAT_scaled",
    #                    "Nat50_scaled", "Nat100_scaled", "Nat250_scaled") &
     #          !is.na(null_uni_chisq), !c("formula_urbanization", "urbanization_var", 
      #                                    "uni_urban_chisq", "uni_urban_p")]

#Goal it to pull out models that (1) assess urbanization
# (2) animal was oriented
# (3) no subject
# (4) don't have anything else going on!!!
sub_guide <- master_guide[var %in% c("urbanization",
                       "urbanization_score_scaled") &
                        sensitivity_analysis == "orients" &
                         subject_id == "no" &
                         !is.na(null_uni_chisq), !c("formula_urbanization", "urbanization_var", 
                                    "uni_urban_chisq", "uni_urban_p")]


sub_guide

sub_guide[, sig := ifelse(null_uni_p < 0.05, "yes", "no")]

# >>> Oriented data & no subject ID -----------------------------------------------------------
setorder(sub_guide, extent, response, var)
sub_guide[sensitivity_analysis == "orients" &
            subject_id == "no", .(response, var, extent, sig)]

# >>> All data & subject ID -----------------------------------------------------------
setorder(sub_guide, extent, response, var)
sub_guide[sensitivity_analysis == "all_data" &
            subject_id == "yes", .(response, var, extent, sig)]


# >>> orients & no subject ID -----------------------------------------------------------

sub_guide[sensitivity_analysis == "orients" &
            subject_id == "no", .(response, var, extent, sig)]

# >>> orients & subject ID -----------------------------------------------------------

sub_guide[sensitivity_analysis == "orients" &
            subject_id == "yes", .(response, var, extent, sig)]

#' [Results seem pretty robust.]
#' *but the problem with this entire approach is interpreting so many models.*





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 2. Categorize extrinsic/intrinsic covariates + urbanization -------------
uni_urban_guide <- master_guide[!var %in% c("urbanization",
                                     "urbanization_score_scaled",
                                     "pop_density_scaled",
                                     "Road.density_scaled", "ANTH_scaled", "NAT_scaled",
                                     "Nat50_scaled", "Nat100_scaled", "Nat250_scaled") &
                            !is.na(null_uni_chisq), ]

uni_urban_guide[, uni_sig := ifelse(null_uni_p < 0.05, "yes", "no")]

uni_urban_guide[, urban_sig := ifelse(uni_urban_p < 0.05, "yes", "no")]

uni_urban_guide[, comp_conclusion := fcase(uni_sig == "yes" & urban_sig == "yes", "uni_sig_urban_sig",
                                           uni_sig == "no" , "uni_NOT_sig",
                                           uni_sig == "yes" & urban_sig == "no", "uni_sig_urban_NOT_sig")]
uni_urban_guide[is.na(comp_conclusion), ]


setorder(uni_urban_guide, extent, comp_conclusion, subject_id, sensitivity_analysis)

uni_urban_guide[comp_conclusion != "uni_NOT_sig", 
                .(n = .N,
                  null_uni_chisq = paste(round(range(null_uni_chisq), 2), collapse = ", "),
                  uni_urban_chisq = paste(round(range(uni_urban_chisq), 2), collapse = ", "),
                  
                  null_uni_p = paste(round(range(null_uni_p), 8), collapse = ", "),
                  uni_urban_p = paste(round(range(uni_urban_p), 8), collapse = ", ")),
                by = .(comp_conclusion,extent, sensitivity_analysis, subject_id)]

#' [OK, so urbanization  always improves model quality in city_and_park]



