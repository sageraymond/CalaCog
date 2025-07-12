#
#
#
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
          "dplyr", "crayon", "broom.mixed",
          "glmmTMB",
          "cpp11", "withr", "colorspace", "mvtnorm",
          "foreach", "doSNOW")
groundhog.library(libs, groundhog.day)

master_guide <- readRDS("builds/batch_models_july_2025/model_guide.Rds")
master_guide[!file.exists(model_path), ]

master_guide <- master_guide[file.exists(model_path), ]

# 1. Determine which univariate models improve quality --------------------------------------
#' *We don't need all of the variables. SO let's slim this bad boy down a little bit*
#' *And for this, we only want univariate vs null*
#' *And since we excluded location_scale from nulls, let's drop location_scale too*
unique(master_guide$model_type)
sub_guide <- master_guide[model_type %in% c("univariate", "null_model") &
                            location_or_scale == "location", 
                          .(response, var, extent,
                            sensitivity_analysis,
                            subject_id,
                              model_complexity_comparison_ID, 
                              model_type, model_id,
                              model_path, model_call)]
sub_guide

# >>> Cast wide -------------------
#' [dcast is equivalent to pivot_wider (i htink that's what the stupid thing is called?)]
#' [It makes data WIDE]

uni_null_comp <- dcast(sub_guide, ... ~ model_type,
                       value.var = c("model_id", "model_path",  "model_call"))
# The value vars become the values in the new columns, as defined by the right hand side of 
uni_null_comp

#' *The big spooker is if there are NAs. But I'm a lucky boy, after 5 tries, no NAs...*

#' [looks like we're lucky boys & this is just from the subject models not running. Drop those dirty dawgs]

uni_null_comp <- uni_null_comp[!is.na(model_id_univariate)]

uni_null_comp <- uni_null_comp[!is.na(model_id_null_model)]

# uni_null_comp <- uni_null_comp[!is.na(model_id_univariate)]

# >>> Compare -------------------------------------------------------------
comps <- list()
out <- c()
i <- 1

for(i in 1:nrow(uni_null_comp)){
  
  m1 <- readRDS(uni_null_comp[i, ]$model_path_null)
  m2 <- readRDS(uni_null_comp[i, ]$model_path_univariate)
  out <- anova(m1, m2)
  comps[[i]] <- data.table(uni_null_comp[i, .(response, var, extent, model_id_null_model,
                                              sensitivity_analysis,
                                             model_id_univariate, model_complexity_comparison_ID)],
                          chisq = out$Chisq[2], 
                           p = out$`Pr(>Chisq)`[2])
  cat(i, "/", nrow(uni_null_comp), "\r")
}
  
comps.dt <- rbindlist(comps)
nrow(comps.dt[p < 0.05, ]) / nrow(comps.dt)

comps.dt[p < 0.05 & extent == "city_and_park", ]
comps.dt[p < 0.05 & extent == "city", ]
comps.dt[extent == "city" & var == "urbanization_score_scaled", .(p)]

# >>> scratch -----------------------------------------------------------------
ms <- lapply(uni_null_comp[extent == "city" & var == "urbanization_score_scaled", ]$model_path_univariate,
             readRDS)
ms <- lapply(ms, summary)
ms

# >>> Create tidy sidecar file --------------------------------------------
comps.dt[, univariate_improved_quality_over_null := ifelse(p < 0.05,
                                                           "yes", "no")]

comps.dt
setnames(comps.dt, c("chisq", "p"), c("null_uni_chisq", "null_uni_p"))

master_guide.m1 <- merge(master_guide,
                         comps.dt[, .(model_complexity_comparison_ID, 
                                      univariate_improved_quality_over_null,
                                      null_uni_chisq, null_uni_p)],
                         all.x = T,
                         all.y = T,
                         by = "model_complexity_comparison_ID")

master_guide.m1

master_guide.m1[, .(n = .N), by = .(model_complexity_comparison_ID)]

# 2. Compare location to location-scale -----------------------------------
unique(master_guide.m1$model_type)

unique(master_guide.m1[var == "urbanization", ])
unique(master_guide.m1[var == "urbanization_score_scaled", ])

location_scale_comp <- master_guide.m1[var %in% c("urbanization", 
                                               "urbanization_score_scaled") &
                                      model_type == "univariate", 
                                    .(location_scale_model_comparison_ID,
                                        model_type, var, sensitivity_analysis,
                                        exclusion, subject_id,
                                        location_or_scale, model_id,
                                        model_path)]
location_scale_comp
# location_scale_comp[model_id %in% c("model_429", "model_430", "model_431", "model_432")]

# Check on: "model_719", "model_720"
# master_guide[model_id %in% c("model_429", "model_430", "model_431", "model_432")]

location_scale_comp.wide <- dcast(location_scale_comp,
                                  ... ~ location_or_scale,
                                  value.var = c("model_id", "model_path"))
location_scale_comp.wide

location_scale_comp.wide[model_id_location %in% c("model_429", "model_430", "model_431", "model_432")]

if(nrow(location_scale_comp.wide[is.na(model_path_location_scale), ]) > 0) print("Yellow bellied sapsucker you son of a bitch")
# location_scale_comp.wide <- location_scale_comp.wide[!is.na(model_path_location_scale), ]

location_scale_comp.wide

comps <- list()
out <- c()
i <- 1

for(i in 1:nrow(location_scale_comp.wide)){
  
  m1 <- readRDS(location_scale_comp.wide[i, ]$model_path_location)
  m2 <- readRDS(location_scale_comp.wide[i, ]$model_path_location_scale)
  out <- anova(m1, m2)
  comps[[i]] <- data.table(location_scale_comp.wide[i, ],
                           chisq = out$Chisq[2], 
                           p = out$`Pr(>Chisq)`[2])
  cat(i, "/", nrow(location_scale_comp.wide), "\r")
}

comps.dt <- rbindlist(comps)
nrow(comps.dt[p < 0.05, ]) / nrow(comps.dt)
comps.dt[ p < 0.05, ]

# >>> Create tidy sidecar file --------------------------------------------

comps.dt[, scale_improved_model_quality := ifelse(p < 0.05, "yes", "no")]
setnames(comps.dt, c("chisq", "p"), c("location_scale_chisq", "location_scale_p"))

master_guide.m2 <- merge(master_guide.m1,
                         comps.dt[, .(location_scale_model_comparison_ID,
                                      scale_improved_model_quality, location_scale_chisq,
                                      location_scale_p)],
                         all.x = T,
                         all.y = T,
                         by = "location_scale_model_comparison_ID")
master_guide.m2[!is.na(location_scale_chisq)]

# 3. Does adding urbanization to extrinsic/intrinsic factors improve model quality? ---------------------------------
#' [might want to ask either if adding univariate to urbanization matters or vice versa...]
unique(master_guide.m2$model_type)
sub_guide <- master_guide.m2[model_type %in% c("univariate", "urbanization") &
                               !var %in% c("urbanization", "urbanization_score_scaled"), 
                             .(response, var, extent,
                               sensitivity_analysis,
                               subject_id,
                               model_complexity_comparison_ID, 
                               model_type, model_id, model_path, model_call)]

# View(sub_guide)
uni_urban_comp <- dcast(sub_guide,
                        ... ~ model_type,
                        value.var = c("model_id", "model_path", "model_call")) 
View(uni_urban_comp)
uni_urban_comp[, .(model_call_univariate, model_call_urbanization)]

uni_urban_comp <- uni_urban_comp[!is.na(model_path_univariate)]
uni_urban_comp <- uni_urban_comp[!is.na(model_path_urbanization)]

# lookin pretty pretty pretty good.
# >>> Compare -------------------------------------------------------------
comps <- list()
out <- c()
i <- 1

for(i in 1:nrow(uni_urban_comp)){
  
  m1 <- readRDS(uni_urban_comp[i, ]$model_path_univariate)
  m2 <- readRDS(uni_urban_comp[i, ]$model_path_urbanization)
  out <- anova(m1, m2)
  comps[[i]] <- data.table(uni_urban_comp[i, ],
                           chisq = out$Chisq[2], 
                           p = out$`Pr(>Chisq)`[2])
  cat(i, "/", nrow(uni_urban_comp), "\r")
  
}

comps.dt <- rbindlist(comps)
nrow(comps.dt[p < 0.05, ]) / nrow(comps.dt)

comps.dt[p < 0.05 & extent == "city_and_park", ]
comps.dt[p < 0.05 & extent == "city", ]
comps.dt[extent == "city" & var == "urbanization_score_scaled", .(p)]

unique(comps.dt[extent == "city_and_park" &
                  subject_id == "yes" &
                  p < 0.05 &
                  sensitivity_analysis == "all_data", .(response, var)])

unique(comps.dt[extent == "city_and_park" &
                  subject_id == "no" &
                  sensitivity_analysis == "all_data", .(response, var)])

# >>> Merge into main dataset ---------------------------------------------

comps.dt[, urbanization_improved_univariate := ifelse(p < 0.05, "yes", "no")]
setnames(comps.dt, c("chisq", "p"), c("urbanization_univariate_chisq", "urbanization_univariate_p"))
comps.dt

master_guide.m3 <- merge(master_guide.m2,
                         comps.dt[, .(model_complexity_comparison_ID,
                                      urbanization_improved_univariate, urbanization_univariate_chisq,
                                      urbanization_univariate_p)],
                         all.x = T,
                         all.y = T,
                         by = "model_complexity_comparison_ID")

master_guide.m3[urbanization_improved_univariate == "yes" & 
                  univariate_improved_quality_over_null == "yes" &
                  sensitivity_analysis == "all_data",
                .(var, response)]

# Save final model comparison information ---------------------------------

saveRDS(master_guide.m3, "builds/batch_models_july_2025/model_guide_with_comparison_stats.Rds")




