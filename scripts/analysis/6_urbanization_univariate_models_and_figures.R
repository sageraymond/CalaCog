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
                                     "Nat50_scaled") &
                            sensitivity_analysis == "orients" &
                            subject_id == "no" &
                            !is.na(null_uni_chisq), !c("formula_urbanization", "urbanization_var", 
                                                       "uni_urban_chisq", "uni_urban_p")]


sub_guide

sub_guide[, sig := ifelse(null_uni_p < 0.05, "yes", "no")]


#Repeat for sensiitvity analysis with subject ID
sub_guide_SA <- master_guide[var %in% c("urbanization",
                                     "Nat50_scaled") &
                            sensitivity_analysis == "orients" &
                            subject_id == "yes" &
                            !is.na(null_uni_chisq), !c("formula_urbanization", "urbanization_var", 
                                                       "uni_urban_chisq", "uni_urban_p")]


sub_guide_SA

sub_guide_SA[, sig := ifelse(null_uni_p < 0.05, "yes", "no")]


#My goal is to extract all the model summary info for these dudes and plonk in a table
#I will also plot these


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

#Sam for sensitivity analysis
sub_guide_SA[, model_uni := lapply(model_path_univariate, readRDS)]

#Nulls for sensitivity
sub_guide_SA[,model_null := lapply(model_path_null_model, readRDS)]


# This should apply functions to the dudes... 
sub_guide[, tidy_summary := lapply(model_uni, tidy_glmmTMB)]
sub_guide[, model_stats := lapply(model_uni, extract_model_stats)]
sub_guide[, r2_stats := lapply(model_uni, extract_r2)]
sub_guide[, lrt_stats := Map(extract_lrt, model_uni, model_null)]

#And for sensivity
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
  
  # Append model-level metrics to every coefficient row
  for (col in names(stats)) tidy[, (col) := stats[[col]]]
  for (col in names(r2)) tidy[, (col) := r2[[col]]]
  for (col in names(lrt)) tidy[, (col) := lrt[[col]]]
  
  tidy
}))

summary_table
summary_table <- summary_table[term != "(Intercept)"]

#Repeat for sensitivity analysis
#Now I have to pull all of this out...
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
  
  # Append model-level metrics to every coefficient row
  for (col in names(stats)) tidy[, (col) := stats[[col]]]
  for (col in names(r2)) tidy[, (col) := r2[[col]]]
  for (col in names(lrt)) tidy[, (col) := lrt[[col]]]
  
  tidy
}))

summary_table_SA
summary_table_SA <- summary_table_SA[term != "(Intercept)"]

#write.csv(summary_table_SA, "figures/EventModelInfo_UrbOnly_SensitivityAnalysis.csv")

#BUTTTTTT your effect sizes are gonna be wrong for nat50 because they're scaled... SO ANNOYING
#GROSSSSSSS


#Dude, did that fuckign work?! What a world we live in!!!
#write.csv(summary_table, "figures/EventModelInfo_UrbOnly.csv")
#Well, it didn't print LRT results. So I just raw-dogged it. Dawged it? Sorry Lundy...

sub_guide$null_uni_p
sub_guide[extent == "city"]

#Check some of this manually because it does feel a touch dubious
a <- readRDS("builds/batch_models_july_2025/models/model_3381.Rds")
b <- readRDS("builds/batch_models_july_2025/models/model_3453.Rds")


c <- readRDS("builds/batch_models_july_2025/models/model_4405.Rds")
d <- readRDS("builds/batch_models_july_2025/models/model_4981.Rds")
e <- readRDS("builds/batch_models_july_2025/models/model_5557.Rds")
f <- readRDS("builds/batch_models_july_2025/models/model_6133.Rds")
g <- readRDS("builds/batch_models_july_2025/models/model_3829.Rds")

c$call
d$call
e$call
f$call
g$call


dat2 <- dat[Orient == "Y"]

c1 <- glmmTMB(formula = Contact_duration ~ Nat50 + (1 | SiteID), 
              data = dat2, family = lognormal(), ziformula = ~., dispformula = ~1)
d1 <- glmmTMB(formula = Inv_duration ~ Nat50 + (1 | SiteID), 
              data = dat2, family = lognormal(), ziformula = ~., dispformula = ~1)
e1 <- glmmTMB(formula = Lope ~ Nat50 + (1 | SiteID), data = dat2, 
              family = binomial(link = "logit"), ziformula = ~0, dispformula = ~1)
f1 <- glmmTMB(formula = Solves ~ Nat50 + (1 | SiteID), data = dat2, 
              family = binomial(link = "logit"), ziformula = ~0, dispformula = ~1)
g1 <- glmmTMB(formula = Behav_Complexity ~ Nat50 + (1 | SiteID), 
              data = dat2, family = poisson(), ziformula = ~0, dispformula = ~1)


fixef(c1) #con
fixef(d1) #Inv
fixef(e1) #lope
fixef(f1) #solve
fixef(g1) #BC

exp(confint(c1))
exp(confint(d1))
exp(confint(e1))
exp(confint(f1))
exp(confint(g1))


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
  "Inv_duration"    = "Exploration Duration (s)",
  "Contact_duration" = "Persistence (s)",
  "Behav_Complexity" = "Behavioural Diversity",
  "Lope"            = "Fearfulness",
  "Solves"          = "Solutions"
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
#This will also have to get unscaled!!!!!!! GROSSSSSS

mean_nat50 <- mean(dat2$Nat50, na.rm = TRUE)
sd_nat50 <- sd(dat2$Nat50, na.rm = TRUE)

predict_continuous_model <- function(model, model_name) {
  nat50_raw <- seq(0, 100, length.out = 100)

    nat50_scaled <- (nat50_raw - mean_nat50) / sd_nat50
  
  newdat <- data.table(
    Nat50_scaled = nat50_scaled,
    SiteID = NA
  )
  
  preds <- predict(model, newdata = newdat, type = "response", se.fit = TRUE, re.form = NA)
  
  newdat[, Nat50 := nat50_raw]  # Keep raw values for plotting
  newdat[, predicted := preds$fit]
  newdat[, lower := predicted - 1.96 * preds$se.fit]
  newdat[, upper := predicted + 1.96 * preds$se.fit]
  newdat[, response := model_name]
  
  return(newdat)
}


model_list <- list(d = d,
                   c = c,
                   g = g,
                   e = e,
                   f = f)


label_map <- c(
  d = "Exploration Duration (s)",
  c = "Persistence (s)",
  g = "Behavioural Diversity",
  e = "Fearfulness",
  f = "Solutions"
)


predicted_cont <- rbindlist(
  Map(predict_continuous_model, model_list, names(model_list))
)
predicted_cont[, response_label := label_map[as.character(response)]]
predicted_cont[, response_label := factor(response_label, levels = label_map)]


# Plot
CityPlots <- ggplot(predicted_cont, aes(x = Nat50, y = predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#56B4E9", alpha = 0.2) +
  geom_line(color = "#0072B2", size = 1) +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0, face = "plain", size = 12),
    strip.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    axis.text.x = element_text(colour = "black", face = "plain", size = 12),
    axis.text.y = element_text(colour = "black", face = "plain", size = 12),
    axis.title.x = element_text(colour = "black", face = "plain", size = 12),
    axis.title.y = element_text(colour = "black", face = "plain", size = 12),
    legend.text = element_text(colour = "black", face = "plain", size = 12),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "Urbanization (%)",
    y = "Predicted Count"
  )




#Combine plots
SitePlots <- ggarrange(CvPPlots, CityPlots, ncol = 2)
#ggsave("figures/Fig2.pdf", SitePlots, width = 6, height = 9, dpi = 700,  bg = "white") 




