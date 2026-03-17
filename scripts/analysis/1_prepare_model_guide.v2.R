# July 9th 2025
#
# 
#' *Prepare model guide*
#
#
#
# Prepare workspace ---------------------------------------
#
#
#

rm(list = ls())
gc()

# Groundhog makes libraries consistent.
library("groundhog")
groundhog.day <- "2025-04-15"
libs <- c("metafor", "broom", "data.table",
          "ggplot2", "tidyr", "multcomp",
          "dplyr","glmmTMB",
          "cpp11", "withr", "colorspace", "mvtnorm",
          "foreach", "doSNOW")
groundhog.library(libs, groundhog.day)

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
#  Create model guide for both extents ------------------------------------
dat <- fread("data/EventDataJul2025.csv")

guide <- rbind(
          CJ(response = c("Contact_duration", "Inv_duration", "Behav_Complexity", "Lope", "Solves"),
            var = c("urbanization", "PuzzleType", "Year", "Light", "GroupSize_scaled",
                    "Sex", "temp_scaled", "SiteSequence_scaled", "Disease"),
            urbanization_var = "urbanization",
            sensitivity_analysis = c("orients", "all_data"),
            subject_id = c("yes", "no"),
            location_or_scale = c("location", "location_scale"),
            extent = "city_and_park"),
          CJ(response = c("Contact_duration", "Inv_duration", "Behav_Complexity", "Lope", "Solves"),
             var = c("urbanization_score_scaled", "pop_density_scaled",
                     "Road.density_scaled", "ANTH_scaled", "NAT_scaled",
                     "Nat50_scaled", "Nat100_scaled", "Nat250_scaled",#' [This is a tad confusing...]
                     
                     "PuzzleType", "Year", "Light", "GroupSize_scaled",
                     "Sex", "temp_scaled", "SiteSequence_scaled", "Disease"),
             urbanization_var = c("urbanization_score_scaled", "pop_density_scaled",
                                  "Road.density_scaled", "ANTH_scaled", "NAT_scaled",
                                  "Nat50_scaled", "Nat100_scaled", "Nat250_scaled"),
             sensitivity_analysis = c("orients", "all_data"),
             subject_id = c("yes", "no"),
             location_or_scale = c("location", "location_scale"),
             extent = "city") )

#' [It's a bit confusing because we have urbanization vars in the 'var' column (for univariate comparisons)]
#' [But also in the new column 'urbanization_var' which is added to the univariates]
#' [Need to remove models with urbanization both in 'var' and in 'urbanization_var'. These models are the explicit hypotehsis testing models]
#
guide[var %in% c("urbanization_score_scaled", "pop_density_scaled", "Road.density_scaled", 
                 "ANTH_scaled", "NAT_scaled", "Nat50_scaled", "Nat100_scaled", "Nat250_scaled",
                 "urbanization"), 
      urbanization_var := NA]
# Now make unique:
guide <- unique(guide)
guide

#
setdiff(guide$var, names(dat))
#' [The scaled variables will be returned here. They need to be scaled on the fly]

# >>> Create formulas -----------------------------------------------------

#Create null model formulas
guide[, null_model_formula := paste(response, "~ 1")]

#' [How to write in dplyr:]
# guide <- guide %>%
#   mutate(null_model = paste(response, "~ 1 + (1|site_id"))

#Create univariate formulas
guide[, univariate_formula := paste(response, "~", var)]
guide

unique(guide[extent == "city_and_park", ]$univariate_formula)
unique(guide[extent == "city", ]$univariate_formula)

guide[!is.na(urbanization_var), 
      urbanization_formula := paste(response, "~", var, "+", urbanization_var)]
guide

unique(guide[extent == "city_and_park", ]$urbanization_formula)
unique(guide[extent == "city", ]$urbanization_formula)
#' *looks pretty pretty pretty prettty nice*
#' 
# >>> Specify model family -----------------------------------------
#' *From 0_data_exploration.R script:*
#' [Behav_Complexity == poisson]
#' [Contact_duration == lognormal + ziformula]
#' [Inv_dispersion == lognormal + ziformula + scale]
#' [Lope == binomial]
#' [Solves == binomial]

unique(guide$response)

guide[response == "Contact_duration", model_family := "lognormal()"]
guide[response == "Inv_duration", model_family := "lognormal()"]
guide[response == "Behav_Complexity", model_family := "poisson()"]
guide[response == "Lope", model_family := "binomial(link = 'logit')"]
guide[response == "Solves", model_family := "binomial(link = 'logit')"]

guide[is.na(model_family), ]

# >>> Specify zero-inflation models -----------------------------------------
#' * nice :) *
guide[, zero_inflation := ifelse(response %in% c("Contact_duration", "Inv_duration"),
                                 "yes", "no")]
guide

# >>> Add model comparison IDs --------------------------------------------

guide[, model_complexity_comparison_ID := paste0("model_complexity_id_",
                                                 seq(1:.N))]

guide[model_complexity_comparison_ID == "model_complexity_id_96"]

# >>> Add an exclusion formula to make sure models are comparable --------------------------------------------------
#Old code

guide[, exclusion := paste0("complete.cases(",
                            var, 
                            ", ", response,
                            ifelse(!is.na(urbanization_var), paste0(", ", urbanization_var), ""),
                            ifelse(subject_id == "yes", ", Subject", ""),
                            ")")]
# The ifelses evaluate whether to add the variable. I could have broken this into multiple lines...
# Oh well....
unique(guide$exclusion)
guide[, exclusion := gsub("_scaled", "", exclusion)]
guide

guide[sensitivity_analysis == "orients", exclusion := paste(exclusion, "& Orient == 'Y'")]
unique(guide$exclusion)
guide


# >>> Make the data long --------------------------------------------------

guide.long <- melt(guide,
                   measure.vars = c("null_model_formula",
                                    "univariate_formula",
                                    "urbanization_formula"),
                   variable.name = "model_type",
                   value.name = "formula")

guide.long

# Drop the NA models:
guide.long <- guide.long[!is.na(formula), ]
guide.long

guide.long[, model_type := gsub("_formula", "", model_type)]

# >>> Add location scale model comparison ID ------------------------------

#' [We need an ID to compare between location and scale models]
#' *The challenge is, these are on different rows. But no worries :)*
#' 

guide.long[, location_scale_model_comparison_ID := paste0("location_scale_ID_",
                                                          .GRP),
           by = .(response, var, extent, sensitivity_analysis, exclusion, model_type)]

guide.long

guide.long 

if(nrow(guide.long[, .(n = .N), by = .(location_scale_model_comparison_ID)][n != 2, ]) > 0){
  print("Oops, bummer")
}

guide.long[, .(n = .N), by = .(location_scale_model_comparison_ID)][n != 2, ]
#' [must be 0 rows]

guide.long

guide.long[location_scale_model_comparison_ID == "location_scale_ID_26", ]

# >>> Add an individual model ID ------------------------------------------

guide.long[, model_id := paste0("model_", seq(1:.N))]
guide.long

guide.long[, model_path := paste0("builds/batch_models_july_2025/models/", model_id, ".Rds")]
guide.long

guide.long[duplicated(model_path), ]

# >>> Drop unneeded location-scale models ---------------------------------

unique(guide.long[!(location_or_scale == "location_scale" & model_type == "null_model"), .(model_type, location_or_scale)])

guide.long <- guide.long[!(location_or_scale == "location_scale" & model_type == "null_model"), ]

# >>> Add dispformula -----------------------------------------------------

guide.long[location_or_scale == "location_scale",]

guide.long[location_or_scale == "location_scale", dispformula := gsub(response, "", formula),
           by = .(model_id)]

unique(guide.long[location_or_scale == "location_scale",]$dispformula)
# That should do.


# >>> Add random effects to formulas --------------------------------------
guide.long[subject_id == "yes", formula := paste0(formula, " + (1|SiteID/Subject)")]

guide.long[subject_id == "no", formula := paste0(formula, " + (1|SiteID)")]
unique(guide.long$formula)

# >>> Test that formulas are correctly formed -----------------------------

for(i in 1:nrow(guide.long)){
  as.formula(guide.long[i, ]$formula)
} # This will errror out if there's a syntactical mistake

x <- c()
for(i in 1:nrow(guide.long)){
  x <- dat[eval(parse(text = guide.long[i, ]$exclusion)), ]
  if(nrow(x) == 0){
    print("Watch it buddy")
  }
}

# >>> Create ELEGANT executable call in guide -----------------------------
#' [instead of multiple if statements for location/scale or ziformula and family, let's create a single call to execute]

guide.long[, model_call := paste0("glmmTMB(", 
                                  formula, ", ",
                                  ifelse(zero_inflation == "yes", "ziformula = ~ ., ", ""), 
                                  ifelse(location_or_scale == "location_scale", paste0("dispformula = ", dispformula, ", "), ""),
                                  "family=", model_family, ", ",
                                  "data = sub_dat)")]
guide.long

guide.long[1, ]$model_call
# eval(parse(text = guide.long[1, ]$model_call))

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
#  Save guide ------------------------------------
saveRDS(guide.long, "builds/batch_models_july_2025/model_guide.Rds")

