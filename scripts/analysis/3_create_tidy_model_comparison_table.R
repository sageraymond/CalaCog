#
# AIM: compare models. and create tidy WIDE data frame of each comparison
#
#
#
#

rm(list = ls())
gc()

# Load data and prepare workspace -----------------------------------------

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

# master_guide <- master_guide[file.exists(model_path), ]

# 

# >>> Cast wide by model_type ---------------------------------------------
#' We'll only do location models here (since we didn't do a location_scale for null models)
master_guide_wide <- dcast(master_guide[location_or_scale == "location", !c("location_scale_model_comparison_ID",
                                             "dispformula")],
                           ... ~ model_type,
                           value.var = c("model_call", "model_path", "model_id",
                                         "formula"))

master_guide_wide[model_complexity_comparison_ID == "model_complexity_id_361"]
master_guide_wide

master_guide_wide[model_complexity_comparison_ID == "model_complexity_id_370"]
master_guide_wide[duplicated(model_complexity_comparison_ID)]
if(nrow(master_guide_wide[duplicated(model_complexity_comparison_ID)]) > 0){ 
  cat(yellow("Stupid Lundy!"))
}else{ 
  cat(blue("good Lundy!"))
}
master_guide_wide


# >>> Tests ---------------------------------------------------------------
#' [ALL of these should have NULL models and univariate]

master_guide_wide[is.na(model_id_null_model)]
#' *hopefully 0 rows*


master_guide_wide[is.na(model_id_univariate)]
#' *hopefully 0 rows*

#' [If var != an urban variable, there should be an urbanization model id)]

master_guide_wide[var %in% c("urbanization_score_scaled", "pop_density_scaled",
                             "Road.density_scaled", "ANTH_scaled", "NAT_scaled",
                             "Nat50_scaled", "Nat100_scaled", "Nat250_scaled",
                             "urbanization") & !is.na(model_id_urbanization), ]
#' *Hopefully 0 rows*


master_guide_wide[!var %in% c("urbanization_score_scaled", "pop_density_scaled",
                             "Road.density_scaled", "ANTH_scaled", "NAT_scaled",
                             "Nat50_scaled", "Nat100_scaled", "Nat250_scaled",
                             "urbanization") & is.na(model_id_urbanization), ]
#' *Hopefully 0 rows*

#' [Should only be 1 row per model_complexity_comparison_ID]
master_guide_wide[duplicated(model_complexity_comparison_ID), ]

#' [Now check for ones that don't exist and exclude]
master_guide_wide <- master_guide_wide[file.exists(model_path_null_model) &
                                         file.exists(model_path_univariate), ]

# These are all subject IDs:
master_guide_wide[(!is.na(model_path_urbanization) & !file.exists(model_path_urbanization)), ]
master_guide_wide <- master_guide_wide[!(!is.na(model_path_urbanization) & !file.exists(model_path_urbanization)), ]

master_guide_wide


# >>> Compare -------------------------------------------------------------
comps <- list()
out <- c()
i <- 1

for(i in 1:nrow(master_guide_wide)){
  
  m1 <- readRDS(master_guide_wide[i, ]$model_path_null)
  m2 <- readRDS(master_guide_wide[i, ]$model_path_univariate)
  out <- anova(m1, m2)
  comps[[i]] <- data.table(master_guide_wide[i, .(model_complexity_comparison_ID)],
                           null_uni_chisq = out$Chisq[2], 
                           null_uni_p = out$`Pr(>Chisq)`[2])
  
  out <- c()
  if(!is.na(master_guide_wide[i, ]$model_path_urbanization)){
    m1 <- readRDS(master_guide_wide[i, ]$model_path_univariate)
    m2 <- readRDS(master_guide_wide[i, ]$model_path_urbanization)
    out <- anova(m1, m2)
    comps[[i]] <- cbind(comps[[i]], 
                        data.table(uni_urban_chisq = out$Chisq[2], 
                             uni_urban_p = out$`Pr(>Chisq)`[2]))
  }
  cat(i, "/", nrow(master_guide_wide), "\r")
  out <- c()
}

comps.dt <- rbindlist(comps, fill = TRUE)
comps.dt[is.na(null_uni_chisq)]
# Good. Hopefully 0 rows


# >>> Merge into master guide wide ----------------------------------------

master_guide_wide.mrg <- merge(master_guide_wide,
                               comps.dt,
                               by = "model_complexity_comparison_ID",
                               all.x = T)
master_guide_wide.mrg

saveRDS(master_guide_wide.mrg, "builds/batch_models_july_2025/model_guide_with_comparison_stats.Rds")
master_guide_wide.mrg

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------
# Compare with location_scale models --------------------------------------
#' [for simplicity, let's only do this for univariate models]


#' location_comps <- dcast(master_guide[model_type %in% c("univariate", "urbanization"), 
#'                                      .(response, var, 
#'                                        model_type, model_complexity_comparison_ID,
#'                                        location_scale_model_comparison_ID, model_path,
#'                                        model_id, location_or_scale)],
#'                                 ... ~ location_or_scale,
#'                                 value.var = c("model_path", "model_id", "model_complexity_comparison_ID"))
#' location_comps
#' 
#' location_comps[is.na(model_path_location) | is.na(model_path_location_scale)]
#' #' *hopefully 0 rows*
#' 
#' location_comps <- location_comps[file.exists(model_path_location) & file.exists(model_path_location_scale)]
#' 
#' comps <- list()
#' i <- 1
#' 
#' # >>> Compare -------------------------------------------------------------
#' 
#' for(i in 1:nrow(location_comps)){
#'   m1 <- readRDS(location_comps[i, ]$model_path_location)
#'   m2 <- readRDS(location_comps[i, ]$model_path_location_scale)
#'   out <- anova(m1, m2)
#'   comps[[i]] <- data.table(location_comps[i, ],
#'                            location_scale_chisq = out$Chisq[2], 
#'                            location_scale_p = out$`Pr(>Chisq)`[2])
#'   cat(i, "/", nrow(location_comps), "\r")
#' }
#' location_scale_comps <- rbindlist(comps)
#' 
#' location_scale_comps[location_scale_p < 0.05, ]
#' # OK. We now have a few. Interesting. 
#' 
#' location_scale_comps
#' 
#' # >>> Tag which models are improved by location_scale... ------------------------------------------
#' # 
#' location_scale_comps[location_scale_p < 0.05, ]
#' 
#' master_guide_wide.mrg
#' 

