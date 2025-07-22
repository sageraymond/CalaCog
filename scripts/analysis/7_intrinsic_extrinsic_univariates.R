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


#Now I am going to extract info from all of these so I can put it in a big ugly table

#Build function to pull info out of glmmTMB models (thanks, Lundy!)-------------
tidy_glmmTMB <- function(m) {
  dt <- tidy(m, effects = "fixed", component = "cond", conf.int = TRUE) |> as.data.table()
  dt[, `:=`(
    OR = exp(estimate), 
    OR_low = exp(conf.low), 
    OR_high = exp(conf.high)
  )]
  dt[, .(term, estimate, std.error, conf.low, conf.high, p.value, OR, OR_low, OR_high)]
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

