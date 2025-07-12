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

guide <- readRDS("builds/batch_models_july_2025/model_guide_with_comparison_stats.Rds")

dat <- readRDS("builds/prepared_dataset.Rds")
#' [Urbanization score may need to be rethought.]
#' [Since it sucks so much I'm goign to exclude for now:]
guide <- guide[extent != "city", ]

#' [adding dispformula didn't improve model quality for urbanization hypotheses.]
#' [For simplicity, going to drop all locaiton_scale models. No citation for Lundy :( ]

guide <- guide[location_or_scale == "location", ]

unique(guide$location_or_scale)
unique(guide$dispformula)

# Check ratios...that's pretty good
dat[, .(n = .N), by = .(urbanization, Sex)]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 1. Report urbanization hypothesis testing -------------------------------
sub_guide <- guide[var %in% c("urbanization", "urbanization_score_scaled") &
                     model_type == "univariate"]
sub_guide

# >>> Compare to nulls (Orients = "Y")-----------------------------
sub_guide[sensitivity_analysis == "orients" &
            subject_id == "no"]
sub_guide[sensitivity_analysis == "orients" & 
            univariate_improved_quality_over_null == "yes" &
            subject_id == "no"]
#' *4 of 5 are improved by urbanization as a univariate factor*


sub_guide[sensitivity_analysis == "orients" &
            subject_id == "yes"]
sub_guide[sensitivity_analysis == "orients" & 
            univariate_improved_quality_over_null == "yes" &
            subject_id == "yes"]
#' *4 of 5 are improved by urbanization as a univariate factor*

# >>> See if results with all data --------------------------
sub_guide[sensitivity_analysis == "all_data" &
            subject_id == "yes" ]
unique(sub_guide$sensitivity_analysis)
sub_guide[sensitivity_analysis == "all_data" & 
            univariate_improved_quality_over_null == "yes"  &
            subject_id == "yes"]
#' *3 of 5 are improved by urbanization as a univariate factor*


sub_guide[sensitivity_analysis == "all_data" &
            subject_id == "no" ]
unique(sub_guide$sensitivity_analysis)
sub_guide[sensitivity_analysis == "all_data" & 
            univariate_improved_quality_over_null == "yes"  &
            subject_id == "no"]
#' *4 of 5 are improved by urbanization as a univariate factor*

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 2. Select univariates that improved model quality -----------------------
null_vs_uni <- guide[!var %in% c("urbanization", "urbanization_score_scaled") &
                     model_type == "univariate"]
null_vs_uni


# >>> Compare to nulls (Orients = "Y")-----------------------------
null_vs_uni[sensitivity_analysis == "orients"&
            subject_id == "no" ]
unique(null_vs_uni[sensitivity_analysis == "orients" & 
                   univariate_improved_quality_over_null == "yes" &
                   subject_id == "no",
                 .(var, response)])


null_vs_uni[sensitivity_analysis == "orients"&
            subject_id == "yes" ]
unique(null_vs_uni[sensitivity_analysis == "orients" & 
                   univariate_improved_quality_over_null == "yes" &
                   subject_id == "yes",
                 .(var, response)])


# >>> See if results with all data --------------------------
null_vs_uni[sensitivity_analysis == "all_data" ]
unique(null_vs_uni$sensitivity_analysis)
null_vs_uni[sensitivity_analysis == "all_data" & univariate_improved_quality_over_null == "yes"]

#' [I'll leave it to you when writing these up but can summarize as follows:]
#' 
null_vs_uni[ univariate_improved_quality_over_null == "yes", 
                      .(min_chisq = min(null_uni_chisq),
                        max_chisq = max(null_uni_chisq),
                        min_p = min(null_uni_p),
                        max_p = max(null_uni_p)),
                    by = .(univariate_improved_quality_over_null,
                           sensitivity_analysis, subject_id,
                           response)]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 3. Test if urbanization was still important with those univariat --------
uni_vs_urbanization <- sub_guide[univariate_improved_quality_over_null == "yes", ]
#' [Filter to just intrinsic/extrinsic factors that improved models]

uni_vs_urbanization[sensitivity_analysis == "all_data" & 
            subject_id == "no" &
            urbanization_improved_univariate == "yes", ] 

uni_vs_urbanization[sensitivity_analysis == "all_data" & 
            subject_id == "no", ]
#' *12 of 13 sig extrinsic/intrinsic variables are improved by adding urbanization*

uni_vs_urbanization[sensitivity_analysis == "all_data" & 
            subject_id == "yes" &
            urbanization_improved_univariate == "yes", ] 

uni_vs_urbanization[sensitivity_analysis == "all_data" & 
            subject_id == "yes", ]
#' *5 of 5 sig extrinsic/intrinsic variables are improved by adding urbanization*


# ORIENTS == "YES"
uni_vs_urbanization[sensitivity_analysis == "orients" & 
            subject_id == "no" &
            urbanization_improved_univariate == "yes", ] 

uni_vs_urbanization[sensitivity_analysis == "orients" & 
            subject_id == "no" , ]
#' *9 of 10 sig extrinsic/intrinsic variables are improved by adding urbanization*

uni_vs_urbanization[sensitivity_analysis == "orients" & 
            subject_id == "yes" &
            urbanization_improved_univariate == "yes", ] 

uni_vs_urbanization[sensitivity_analysis == "orients" & 
            subject_id == "yes" , ]
#' *4 of 4 sig extrinsic/intrinsic variables are improved by adding urbanization*


#' [I'll leave it to you when writing these up but can summarize as follows:]
#' 
uni_vs_urbanization[, .(min_chisq = min(urbanization_univariate_chisq),
                        max_chisq = max(urbanization_univariate_chisq),
                        min_p = min(urbanization_univariate_p),
                        max_p = max(urbanization_univariate_p)),
                    by = .(urbanization_improved_univariate,
                           sensitivity_analysis, subject_id,
                           response)]



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------


# DEPRECATED FOR REFERENCE ------------------------------------------------


# 3. Create tidy model summary table --------------------------------------

m <- readRDS(master_guide$model_path[1])
m
tidy(m)

confint(m) %>% tidy() %>% rename()

tidy_models <- function(m){
  
  m.tidy <- tidy(m)
  
  #' [Add R2 etc.]
  return(m.tidy)
  
}

ms.tidy <- lapply(master_guide$model_path,
                  readRDS)
ms.tidy <- lapply(ms.tidy,
                  tidy_models)

