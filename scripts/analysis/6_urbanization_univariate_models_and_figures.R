#Goal here is to use bits of Erick code (without destroying them) to complete what
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
# 1. Step 1 is to check out our 10 simplest, hypothesis-testing models
master_guide

# Pull out models that (1) assess urbanization
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


#My goal is to extract all the model summary info for these dudes and plonk in a table
#I will also plot these


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
extract_lrt <- function(m) {
  null_mod <- update(m, formula = reformulate("1", response = formula(m)[[2]]))
  lr <- anova(null_mod, m)
  data.table(
    chi_sq = lr$Chisq[2],
    p_lrt = lr$`Pr(>Chisq)`[2]
  )
}

#Finally, build functino to extract r2 metrics---------------------------------
extract_r2 <- function(m) {
  r2 <- performance::r2_nakagawa(m)
  data.table(R2_marginal = r2$R2_m, R2_conditional = r2$R2_c)
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
  
  # Append model-level metrics to every coefficient row
  for (col in names(stats)) tidy[, (col) := stats[[col]]]
  for (col in names(r2)) tidy[, (col) := r2[[col]]]
  for (col in names(lrt)) tidy[, (col) := lrt[[col]]]
  
  tidy
}))

summary_table

#Dude, did that fuckign work?! What a world we live in!!!
#write.csv(summary_table, "figures/EventModelInfo_UrbOnly.csv")
#Well, it didn't print LRT results. So I just raw-dogged it. Dawged it? Sorry Lundy...

sub_guide$null_uni_p

#Check some of this manually because it does feel a touch dubious
a <- readRDS("builds/batch_models_july_2025/models/model_3381.Rds")
b <- readRDS("builds/batch_models_july_2025/models/model_4749.Rds")
c <- readRDS("builds/batch_models_july_2025/models/model_5325.Rds")
d <- readRDS("builds/batch_models_july_2025/models/model_3453.Rds")

summary(a)
confint(a)
summary(b)
confint(b)
summary(c)
confint(c)
summary(d)
confint(d)


#Now I am going to plot these results, though they could be more riveting
#

#You'll hav eto plot categorical dudes and continuous dudes separately
sub_guide_cat <- sub_guide[var == "urbanization"]
model_paths_cat <- setNames(sub_guide_cat$model_path_univariate, sub_guide_cat$response)

#Now run your function--basically what we did with site scale
predict_urban_levels <- function(model_path, model_name) {
  model <- readRDS(model_path)
  
  newdat <- data.table(
    urbanization = factor(c("Wild", "City"), levels = c("Wild", "City")),
    SiteID = NA  # exclude Rand effects because otherwise drama
  )
  

  preds <- predict(model, newdata = newdat, type = "response", se.fit = TRUE, re.form = NA)
  
  
  newdat[, predicted := preds$fit]
  newdat[, lower := predicted - 1.96 * preds$se.fit]
  newdat[, upper := predicted + 1.96 * preds$se.fit]
  newdat[, response := model_name]
  
  return(newdat)
}

# Run the predictions for all models
predicted_all <- rbindlist(
  Map(predict_urban_levels, model_paths_cat, names(model_paths_cat))
)


#what are you going to call these dudes?
label_map <- c(
  "Inv_duration"    = "Investigate Duration (s)",
  "Contact_duration" = "Contact Duration (s)",
  "Behav_Complexity" = "No. Behaviours",
  "Lope"            = "Escape Gait",
  "Solves"          = "Solves"
)

# get everything in correct order and such
predicted_all[, response_label := label_map[response]]
predicted_all[, response_label := factor(response_label, levels = label_map)]
predicted_all[, urbanization := factor(urbanization, levels = c("Wild", "City"))]

# Plot
CvPPlots <- ggplot(predicted_all, aes(x = urbanization, y = predicted, fill = urbanization)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Wild" = "#56B4E9", "City" = "#0072B2")) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0, face = "plain", size = 12),
    strip.background = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    legend.position = "none"
  ) +
  labs(
    x = "Urbanization Category",
    y = "Predicted Value"
  )

CvPPlots

#City only plots---------------------------------------------------------------

#Make function to generate model predioctions
sub_guide_cont <- sub_guide[var == "urbanization_score_scaled"]
model_paths_cont <- setNames(sub_guide_cont$model_path_univariate, sub_guide_cont$response)

predict_continuous_model <- function(model_path, model_name) {
  model <- readRDS(model_path)
  
  score_seq <- seq(-3.2, 2.2, length.out = 100) 
  
  newdat <- data.table(
    urbanization_score_scaled = score_seq,
    SiteID = NA  # No RE!!!
  )
  
  preds <- predict(model, newdata = newdat, type = "response", se.fit = TRUE, re.form = NA)
  
  newdat[, predicted := preds$fit]
  newdat[, lower := predicted - 1.96 * preds$se.fit]
  newdat[, upper := predicted + 1.96 * preds$se.fit]
  newdat[, response := model_name]
  
  return(newdat)
}


#Apply to my mods
predicted_cont <- rbindlist(
  Map(predict_continuous_model, model_paths_cont, names(model_paths_cont))
)

# Same label map applies. 

predicted_cont[, response_label := label_map[response]]
predicted_cont[, response_label := factor(response_label, levels = label_map)]

#Plot!!!
CityPlots <- ggplot(predicted_cont, aes(x = urbanization_score_scaled, y = predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#56B4E9", alpha = 0.2) +
  geom_line(color = "#0072B2", size = 1) +
  #  geom_hline(yintercept = 0, color = "black") +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0, face = "plain", size = 12),
    strip.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    legend.position = "none"
  ) +
  labs(
    x = "Urbanization Score",
    y = "Predicted Count"
  ) +
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none")


#Combine plots
SitePlots <- ggarrange(CvPPlots, CityPlots, ncol = 2)
#ggsave("figures/EventPlotsUnivariate.png", SitePlots, width = 6, height = 9, dpi = 700,  bg = "white") 




