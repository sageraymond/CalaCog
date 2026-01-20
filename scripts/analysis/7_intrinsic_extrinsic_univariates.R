#Goal here is STILL to use bits of Erick code (without destroying them) to complete what
#I feel to be the logical progression of the analysis


rm(list = ls())
gc()

library(glmmTMB)
library(ggplot2)
library(data.table)
library(DHARMa)
library(dplyr)
library(broom.mixed)
library(sjPlot)
library(egg)
library(performance)

# 0. Load data and guide --------------------------------------------------

master_guide <- readRDS("builds/batch_models_july_2025/model_guide_with_comparison_stats.Rds")

dat <- readRDS("builds/prepared_dataset.Rds")
dat

#Make urbanization factor
dat$urbanization <- factor(dat$urbanization, levels = c("Wild", "City"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
# 1. The goal here is to explroe intrinsic and extrinsic factors... see what's important
#Ignore urbanization. This is exploring our second hypothesis
master_guide

# Pull out models that: 
# (1) animal was oriented
# (2) no subject
# (3) examine a single, non-urbanization predictor
prelim_sub_guide <- master_guide[var %in% c("Disease",
                                     "GroupSize_scaled",
                                     "PuzzleType",
                                     "Sex",
                                     "Light",
                                     "SiteSequence_scaled",
                                     "temp_scaled",
                                     "Year") &
                            sensitivity_analysis == "orients" &
                            subject_id == "no" &
                            !is.na(null_uni_chisq)]#, !c("formula_urbanization", "urbanization_var", 
                                                   #    "uni_urban_chisq", "uni_urban_p")]

#problem is, I was running around with so many metrics of urbanization that I ended up
#duplicating a bunch of univariates
#So filter to unique univariate formulas
sub_guide <- prelim_sub_guide[, .SD[1], by = model_call_univariate]
sub_guide  # 40 models is perf: 5 outcome variables x 8 int/ ext predictors

sub_guide[, sig := ifelse(null_uni_p < 0.05, "yes", "no")]


#Repeat for sensitivity analysis
# Pull out models that: 
# (1) animal was oriented
# (2) subject
# (3) examine a single, non-urbanization predictor
prelim_sub_guide_SA <- master_guide[var %in% c("Disease",
                                            "GroupSize_scaled",
                                            "PuzzleType",
                                            "Sex",
                                            "Light",
                                            "SiteSequence_scaled",
                                            "temp_scaled",
                                            "Year") &
                                   sensitivity_analysis == "orients" &
                                   subject_id == "yes" &
                                   !is.na(null_uni_chisq)]#, !c("formula_urbanization", "urbanization_var", 
#    "uni_urban_chisq", "uni_urban_p")]

#So filter to unique univariate formulas
sub_guide_SA <- prelim_sub_guide_SA[, .SD[1], by = model_call_univariate]
sub_guide_SA  # 40 models is perf: 5 outcome variables x 8 int/ ext predictors

sub_guide_SA[, sig := ifelse(null_uni_p < 0.05, "yes", "no")]




#Now I am going to extract info from all of these so I can put it in a big ugly table

#Build function to pull info out of glmmTMB models (thanks, Lundy!)-------------
tidy_glmmTMB <- function(m) {
  # Extract both conditional and zero-inflation components
  cond_dt <- tidy(m, effects = "fixed", component = "cond", conf.int = TRUE) |> as.data.table()
  zi_dt   <- tidy(m, effects = "fixed", component = "zi", conf.int = TRUE) |> as.data.table()
  
  # Add component labels
  cond_dt[, component := "conditional"]
  zi_dt[, component := "zero_inflation"]
  
  # Ensure both have same columns
  for (dt in list(cond_dt, zi_dt)) {
    if (nrow(dt) > 0) {
      dt[, `:=`(
        OR = exp(estimate), 
        OR_low = exp(conf.low), 
        OR_high = exp(conf.high)
      )]
    } else {
      # Add empty OR columns to keep structure consistent
      dt[, `:=`(
        estimate = numeric(),
        std.error = numeric(),
        conf.low = numeric(),
        conf.high = numeric(),
        p.value = numeric(),
        OR = numeric(),
        OR_low = numeric(),
        OR_high = numeric(),
        term = character()
      )]
    }
  }
  
  # Combine and return with consistent columns
  rbindlist(list(cond_dt, zi_dt), fill = TRUE)[, .(
    component, term, estimate, std.error, conf.low, conf.high, 
    p.value, OR, OR_low, OR_high
  )]
}

#Build function to extract model statistics
extract_model_stats <- function(m) {
  fd <- model.frame(m)
  offset_val <- if ("offset" %in% names(m$call)) {
    mean(eval(m$call$offset, envir = fd))
  } else NA_real_
  data.table(
    N = nrow(fd),
    AIC = AIC(m),
    offset = offset_val
  )
}

#And build function to extract LRT stuff-----------------------------------------
extract_lrt <- function(m, null_mod) {
  tryCatch({
    lr <- anova(null_mod, m)
    data.table(
      chi_sq = lr$Chisq[2],
      p_lrt = lr$`Pr(>Chisq)`[2]
    )
  }, error = function(e) {
    data.table(chi_sq = NA_real_, p_lrt = NA_real_)
  })
}


#Finally, build functino to extract r2 metrics---------------------------------
extract_r2 <- function(m) {
  tryCatch({
    r2 <- performance::r2_nakagawa(m)
    data.table(R2_marginal = r2$R2_m, R2_conditional = r2$R2_c)
  }, error = function(e) {
    data.table(R2_marginal = NA_real_, R2_conditional = NA_real_)
  })
}
#Now I need to make a model list-----------------------------------------------
#Load models
sub_guide[, model_uni := lapply(model_path_univariate, readRDS)]

#Load nulls
sub_guide[, model_null := lapply(model_path_null_model, readRDS)]

# This should apply functions to the dudes... 
sub_guide[, tidy_summary := lapply(model_uni, tidy_glmmTMB)]
sub_guide[, model_stats := lapply(model_uni, extract_model_stats)]
sub_guide[, r2_stats := lapply(model_uni, extract_r2)]
sub_guide[, lrt_stats := Map(extract_lrt, model_uni, model_null)]



#Repeat for SA
sub_guide_SA[, model_uni := lapply(model_path_univariate, readRDS)]

#Load nulls
sub_guide_SA[, model_null := lapply(model_path_null_model, readRDS)]

# This should apply functions to the dudes... 
sub_guide_SA[, tidy_summary := lapply(model_uni, tidy_glmmTMB)]
sub_guide_SA[, model_stats := lapply(model_uni, extract_model_stats)]
sub_guide_SA[, r2_stats := lapply(model_uni, extract_r2)]
sub_guide_SA[, lrt_stats := Map(extract_lrt, model_uni, model_null)]

#Now I have to pull all of this out...
summary_table <- rbindlist(lapply(1:nrow(sub_guide), function(i) {
  model_info <- sub_guide[i]
  tidy <- model_info$tidy_summary[[1]]
  stats <- model_info$model_stats[[1]]
  r2 <- model_info$r2_stats[[1]]
  lrt <- model_info$lrt_stats[[1]]
  
  tidy[, `:=`(
    response = model_info$response,
    predictor = model_info$var,
    model_id = model_info$model_id_univariate
  )]
  
  for (col in names(stats)) tidy[, (col) := stats[[col]]]
  for (col in names(r2)) tidy[, (col) := r2[[col]]]
  for (col in names(lrt)) tidy[, (col) := lrt[[col]]]
  
  tidy
}))

summary_table
summary_table <- summary_table[term != "(Intercept)"]

#My lrt thing isn't working. But I actually have that info in sub_guide
#Do a janky left join with dplyr

LRT <- sub_guide %>% dplyr::select(model_id_univariate, null_uni_p) %>%
  dplyr::rename("model_id" = model_id_univariate)

summary_table1 <- left_join(summary_table, LRT, by = "model_id")
#write.csv(summary_table1, "figures/EventModelInfo_IntExtUnivariates.csv")


#repeat for SA
summary_table_SA <- rbindlist(lapply(1:nrow(sub_guide_SA), function(i) {
  model_info <- sub_guide_SA[i]
  tidy <- model_info$tidy_summary[[1]]
  stats <- model_info$model_stats[[1]]
  r2 <- model_info$r2_stats[[1]]
  lrt <- model_info$lrt_stats[[1]]
  
  tidy[, `:=`(
    response = model_info$response,
    predictor = model_info$var,
    model_id = model_info$model_id_univariate
  )]
  
  for (col in names(stats)) tidy[, (col) := stats[[col]]]
  for (col in names(r2)) tidy[, (col) := r2[[col]]]
  for (col in names(lrt)) tidy[, (col) := lrt[[col]]]
  
  tidy
}))

summary_table_SA
summary_table_SA <- summary_table_SA[term != "(Intercept)"]

summary_table1_SA <- left_join(summary_table_SA, LRT, by = "model_id")

#write.csv(summary_table1_SA, "figures/EventModelInfo_IntExtUnivariates_SensitivityAnalysis.csv")

XX <- sub_guide %>% dplyr :: filter (response == "Contact_duration" | 
                                       response == "Inv_duration")

a <- readRDS("builds/batch_models_july_2025/models/model_4245.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_3341.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_4253.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_4317.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_3349.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_3357.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_3365.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_3373.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_3389.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_3397.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_3405.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_3413.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_3421.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_5125.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_5189.Rds")
summary(a)
confint(a)

a <- readRDS("builds/batch_models_july_2025/models/model_5261.Rds")
summary(a)
confint(a)



# Get unscaled stuff. So freakin annoying
dat2 <- dat[Orient == "Y"] #new df that has unscaled variables

#ID models you want to do this for
UnscaledGuide <- sub_guide[var == "GroupSize_scaled" |
                             var == "temp_scaled" |
                             var == "SiteSequence_scaled"]

#This function should do it

fit_and_extract_effects <- function(call_string, data_substitute = "dat2", model_id = NULL, response = NULL) {
  # Step 1: Replace the dataset name
  call_modified <- gsub("data\\s*=\\s*sub_dat", paste0("data = ", data_substitute), call_string)
  
  # Step 2: Replace all *_scaled with unscaled versions
  call_modified <- gsub("([a-zA-Z0-9_]+)_scaled", "\\1", call_modified)
  
  # Step 3: Evaluate the call
  model <- eval(parse(text = call_modified))
  
  # Step 4: Tidy model summary
  summ <- broom.mixed::tidy(model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)
  
  # Step 5: Optionally remove intercept
  summ <- summ[summ$term != "(Intercept)", ]
  
  # Step 6: Add model metadata
  if (!is.null(model_id)) summ$model_id <- model_id
  if (!is.null(response)) summ$response <- response
  
  # Step 7: Select columns
  summ <- summ[, c("component", "term", "estimate", "conf.low", "conf.high", "model_id", "response")]
  
  return(summ)
}

effect_results <- rbindlist(
  Map(fit_and_extract_effects,
      call_string = UnscaledGuide$model_call_univariate,
      model_id = UnscaledGuide$model_id_univariate,
      response = UnscaledGuide$response),
  fill = TRUE
)


effect_results
#write.csv(effect_results, "figures/XXXXX.csv")
