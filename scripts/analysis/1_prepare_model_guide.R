# July 5th 2025
#
# 
#' *Prepare model guide*
#
#
#
# Prepare workspace ---------------------------------------
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
#  Create full extent model guide ------------------------------------

guide <- CJ(response = c("persistence", "play", "stupidity", "happy", "hoho"),
            var = c("urbanization", "temperature", "darkness", 
                    "brightness", "loveliness"),
            location_or_scale = c("location", "location_scale"))

#' [How to write this in dplyr:]
# guide <- expand.grid(response = c("persistence", "play", "stupidity", "happy", "hoho"),
#                      var = c("urbanization", "temperature", "darkness", 
#                              "brightness", "loveliness",
#                              "cityfulness"),
#                      location_or_scale = c("location", "location_scale"))
# guide

# >>> Create formulas -----------------------------------------------------

guide[, null_model_formula := paste(response, "~ 1 + (1|site_id)")]

#' [How to write in dplyr:]
# guide <- guide %>%
#   mutate(null_model = paste(response, "~ 1 + (1|site_id"))

guide[, univariate_formula := paste(response, "~", var,  "+ (1|site_id)")]
guide

guide[, urbanization_formula := paste(response, "~", var, "+ urbanization + (1|site_id)")]
guide

guide[var == "urbanization", urbanization_formula := NA]

# guide %>%
#   mutate(urbanization_formula = ifelse(var == "urbanization",
#                                        NA, urbanization_formula))

guide[, extent := "city_and_park"]

# >>> Create within urban model guide -----------------------------------------

city_guide <- copy(guide)
city_guide[, urbanization_formula := gsub("urbanization", 
                                          "urbanization_score",
                                          urbanization_formula)]

city_guide[, univariate_formula := gsub("urbanization", 
                                          "urbanization_score",
                                        univariate_formula)]
city_guide

city_guide[, extent := "city"]

guide <- rbind(guide,
               city_guide)

guide

# >>> Specify model family -----------------------------------------
unique(guide$response)

guide[response == "happy", model_family := "gaussian()"]
guide[response == "hoho", model_family := "binomial(link = 'logit')"]
guide[response == "persistence", model_family := "poisson()"]
guide[response == "play", model_family := "nbinom1(link = 'log')"]
guide[response == "stupidity", model_family := "nbinom2(link = 'log')"]


# >>> Specify zero-inflation models -----------------------------------------
#' [This should be based on preliminary data exploration]
guide[, zero_inflation := ifelse(response %in% c("persistence", "play"),
                                 "yes", "no")]
guide


# >>> Add model comparison IDs --------------------------------------------

guide[, model_complexity_comparison_ID := paste0("model_complexity_id_",
                                                 seq(1:.N))]

guide[model_complexity_comparison_ID == "model_complexity_id_96"]

#' [We need an ID to compare between location and scale models]
#' *The challenge is, these are on different rows. But no worries :)*
#' 

guide[, location_scale_model_comparison_ID := paste0("location_scale_ID_",
                                                    .GRP),
      by = .(response, var, extent)]

guide

guide[location_scale_model_comparison_ID == "location_scale_ID_26", ]

if(nrow(guide[, .(n = .N), by = .(location_scale_model_comparison_ID)][n != 2, ]) > 0){
  print("You're a fucking piece of shit")
}
guide[, .(n = .N), by = .(location_scale_model_comparison_ID)][n != 2, ]
#' [must be 0 rows]

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

# >>> Test that formulas are correctly formed -----------------------------

for(i in 1:nrow(guide.long)){
  as.formula(guide.long[i, ]$formula)
} # This will errror out if there's a syntactical mistake


# >>> Add an individual model ID ------------------------------------------

guide.long[, model_id := paste0("model_", seq(1:.N))]
guide.long

guide.long[, model_path := paste0("outputs/models/", model_id, ".Rds")]
guide.long


# >>> Drop unneeded location-scale models ---------------------------------

unique(guide.long[!(location_or_scale == "location_scale" & model_type == "null_model"), .(model_type, location_or_scale)])

guide.long <- guide.long[!(location_or_scale == "location_scale" & model_type == "null_model"), ]

# >>> Add dispformula -----------------------------------------------------

guide.long[location_or_scale == "location_scale",]
guide.long[location_or_scale == "location_scale", 
           dispformula := paste("~", var)]
guide.long[location_or_scale == "location_scale" & model_type == "urbanization", 
           dispformula := paste(dispformula, "+", ifelse(extent == "city_and_park",
                                                         "urbanization", "urbanization_score"))]


guide.long[location_or_scale == "location_scale",]

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
#  Save guide ------------------------------------
saveRDS(guide.long, "builds/batch_models_july_2025/model_guide.Rds")



