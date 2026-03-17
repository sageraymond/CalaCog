
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


#Build function to pull info out of glmmTMB models-------------
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

#write.csv(summary_table, "figures/EventModelInfo_UrbOnly.csv")

#Well, it didn't print LRT results. So I just did it manually

sub_guide$null_uni_p
sub_guide[extent == "city"]

#Check some of this manually 
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


#Now I am going to plot these results
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
  "Solves"          = "Solutions",
  "Behav_Complexity" = "Behavioural Diversity",
  "Inv_duration"    = "Exploration Duration (s)",
  "Contact_duration" = "Persistence (s)",
  "Lope"            = "Fearfulness"
  
)

# get everything in correct order and such
predicted_all[, response_label := label_map[response]]
predicted_all[, response_label := factor(response_label, levels = label_map)]
predicted_all[, urbanization := factor(urbanization, levels = c("Wild", "City"))]

# Plot
Contact1 <- ggplot(data = predicted_all[response_label == "Persistence (s)"], aes(x = urbanization, y = predicted, fill = urbanization)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
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

Contact1

# Plot
Inv1 <- ggplot(data = predicted_all[response_label == "Exploration Duration (s)"], aes(x = urbanization, y = predicted, fill = urbanization)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
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

Inv1

# Plot
BD1 <- ggplot(data = predicted_all[response_label == "Behavioural Diversity"], aes(x = urbanization, y = predicted, fill = urbanization)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
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

BD1

Lope1 <- ggplot(data = predicted_all[response_label == "Fearfulness"], aes(x = urbanization, y = predicted, fill = urbanization)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
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

Lope1


Solve1 <- ggplot(data = predicted_all[response_label == "Solutions"], aes(x = urbanization, y = predicted, fill = urbanization)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
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

Solve1




# >>> Alternative ---------------------------------------------------
# Plot actual data summaries to enable meta-analysis and jitter points to impress
dat2

dat2.mlt <- melt(dat2,
                 measure.vars = c("Contact_duration", "Inv_duration", "Behav_Complexity",
                                  "Solves", "Lope"))
dat2.mlt
unique(dat2.mlt$value)
#

summaries <- dat2.mlt[, .(mean_val = mean(value, na.rm = T),
                          sd_val = sd(value, na.rm = T)),
                      by = .(variable, urbanization)]
summaries[, ymax := mean_val + sd_val]
summaries[, ymin := mean_val - sd_val]
summaries[ymin < 0, ymin := 0]

# Plot
contactplot_full <- ggplot() +
  # geom_boxplot()+
  geom_jitter(data = dat2.mlt[variable == "Contact_duration"], 
              aes(x = urbanization, y = (value), fill = urbanization),
              position = position_jitter(width = .15, height = 0),
              # stroke = 5,
              shape = 21, alpha = .5, size = 3)+
  geom_pointrange(data = summaries[variable == "Contact_duration"], 
                  aes(x = urbanization, y = mean_val,
                                        ymin = ymin, ymax = ymax,
                                        fill = urbanization),
                                  shape = 21,
                                  lwd = 1,
                                  stroke = 1,
                                  size = 1.5)+
  # geom_pointrange(data = predicted_all[response == "Contact_duration"],
  #                 aes(x = urbanization, ymin = lower, ymax = upper,
  #                     fill = urbanization,
  #                     y = predicted),
  #                 shape = 21,
  #                 lwd = 1,
  #                 stroke = 1,
  #                 size = 1.5)+
 # coord_cartesian(ylim = c(0, 75))+
  # geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
  # facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
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
    y = "Mean Persistence (s)\n+/- SD"
  )

contactplot_full


# Plot
contactplot_reduced <- ggplot() +
  # geom_boxplot()+
  geom_jitter(data = dat2.mlt[variable == "Contact_duration"], 
              aes(x = urbanization, y = (value), fill = urbanization),
              position = position_jitter(width = .15, height = 0),
              # stroke = 5,
              shape = 21, alpha = .5, size = 3)+
  geom_pointrange(data = summaries[variable == "Contact_duration"], 
                  aes(x = urbanization, y = mean_val,
                      ymin = ymin, ymax = ymax,
                      fill = urbanization),
                  shape = 21,
                  lwd = 1,
                  stroke = 1,
                  size = 1.5)+
  # geom_pointrange(data = predicted_all[response == "Contact_duration"],
  #                 aes(x = urbanization, ymin = lower, ymax = upper,
  #                     fill = urbanization,
  #                     y = predicted),
  #                 shape = 21,
  #                 lwd = 1,
  #                 stroke = 1,
  #                 size = 1.5)+
   coord_cartesian(ylim = c(0, 70))+
  # geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
# facet_wrap(~ variable, scales = "free_y", ncol = 1) +
scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
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
    y = "Mean Persistence (s)\n+/- SD"
  )

contactplot_reduced

# Plot (investigation)
invplot_full <- ggplot() +
  # geom_boxplot()+
  geom_jitter(data = dat2.mlt[variable == "Inv_duration"], 
              aes(x = urbanization, y = (value), fill = urbanization),
              position = position_jitter(width = .15, height = 0),
              # stroke = 5,
              shape = 21, alpha = .5, size = 3)+
  geom_pointrange(data = summaries[variable == "Inv_duration"], 
                  aes(x = urbanization, y = mean_val,
                      ymin = ymin, ymax = ymax,
                      fill = urbanization),
                  shape = 21,
                  lwd = 1,
                  stroke = 1,
                  size = 1.5)+
  # geom_pointrange(data = predicted_all[response == "Contact_duration"],
  #                 aes(x = urbanization, ymin = lower, ymax = upper,
  #                     fill = urbanization,
  #                     y = predicted),
  #                 shape = 21,
  #                 lwd = 1,
  #                 stroke = 1,
  #                 size = 1.5)+
  # coord_cartesian(ylim = c(0, 75))+
  # geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
# facet_wrap(~ variable, scales = "free_y", ncol = 1) +
scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
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
    y = "Mean Exploration (s)\n+/- SD"
  )

invplot_full


# Plot
invplot_reduced <- ggplot() +
  # geom_boxplot()+
  geom_jitter(data = dat2.mlt[variable == "Inv_duration"], 
              aes(x = urbanization, y = (value), fill = urbanization),
              position = position_jitter(width = .15, height = 0),
              # stroke = 5,
              shape = 21, alpha = .5, size = 3)+
  geom_pointrange(data = summaries[variable == "Inv_duration"], 
                  aes(x = urbanization, y = mean_val,
                      ymin = ymin, ymax = ymax,
                      fill = urbanization),
                  shape = 21,
                  lwd = 1,
                  stroke = 1,
                  size = 1.5)+
  # geom_pointrange(data = predicted_all[response == "Contact_duration"],
  #                 aes(x = urbanization, ymin = lower, ymax = upper,
  #                     fill = urbanization,
  #                     y = predicted),
  #                 shape = 21,
  #                 lwd = 1,
  #                 stroke = 1,
  #                 size = 1.5)+
  coord_cartesian(ylim = c(0, 110))+
  # geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
  # facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
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
    y = "Mean Exploration (s)\n+/- SD"
  )

invplot_reduced



# Plot (investigation)
BDplot_full <- ggplot() +
  # geom_boxplot()+
  geom_jitter(data = dat2.mlt[variable == "Behav_Complexity"], 
              aes(x = urbanization, y = (value), fill = urbanization),
              position = position_jitter(width = .15, height = .3),
              # stroke = 5,
              shape = 21, alpha = .5, size = 3)+
  geom_pointrange(data = summaries[variable == "Behav_Complexity"], 
                  aes(x = urbanization, y = mean_val,
                      ymin = ymin, ymax = ymax,
                      fill = urbanization),
                  shape = 21,
                  lwd = 1,
                  stroke = 1,
                  size = 1.5)+
  # geom_pointrange(data = predicted_all[response == "Contact_duration"],
  #                 aes(x = urbanization, ymin = lower, ymax = upper,
  #                     fill = urbanization,
  #                     y = predicted),
  #                 shape = 21,
  #                 lwd = 1,
  #                 stroke = 1,
  #                 size = 1.5)+
  # coord_cartesian(ylim = c(0, 75))+
  # geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
# facet_wrap(~ variable, scales = "free_y", ncol = 1) +
scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
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
    y = "Mean Behavioural Diversity\n+/- SD"
  )

BDplot_full


# Plot
BDplot_reduced <- ggplot() +
  # geom_boxplot()+
  geom_jitter(data = dat2.mlt[variable == "Behav_Complexity"], 
              aes(x = urbanization, y = (value), fill = urbanization),
              position = position_jitter(width = .15, height = .15),
              # stroke = 5,
              shape = 21, alpha = .5, size = 3)+
  geom_pointrange(data = summaries[variable == "Behav_Complexity"], 
                  aes(x = urbanization, y = mean_val,
                      ymin = ymin, ymax = ymax,
                      fill = urbanization),
                  shape = 21,
                  lwd = 1,
                  stroke = 1,
                  size = 1.5)+
  # geom_pointrange(data = predicted_all[response == "Contact_duration"],
  #                 aes(x = urbanization, ymin = lower, ymax = upper,
  #                     fill = urbanization,
  #                     y = predicted),
  #                 shape = 21,
  #                 lwd = 1,
  #                 stroke = 1,
  #                 size = 1.5)+
  coord_cartesian(ylim = c(1, 5))+
  # geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
  # facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
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
    y = "Mean Behavioural Diversity\n+/- SD"
  )

BDplot_reduced

#Happy with these reduced plots for the main text; put full in SI


#Move on to binary chaps
binary_variables <- dat2.mlt[variable%in% c("Solves", "Lope")]
binary_variables[, total_tries := .N, by = .(variable, urbanization)]
binary_variable_summary <- binary_variables[, .(total_events = sum(value)),
                                            by = .(total_tries, variable, urbanization)]

binary_variable_summary[, proportion := total_events / total_tries]
binary_variable_summary

SolvesPlot <- ggplot() +
  # geom_boxplot()+
  # geom_jitter(data = xxxxxx, 
  #             aes(x = urbanization, y = (value), fill = urbanization),
  #             position = position_jitter(width = .15, height = 0),
  #             # stroke = 5,
  #             shape = 21, alpha = .5, size = 3)+
  geom_col(data = binary_variable_summary[variable == "Solves"], 
                  aes(x = urbanization, y = proportion,
                      fill = urbanization),
                  shape = 21,
                  lwd = 1,
                  stroke = 1,
                  size = 1.5)+
  geom_text(data = binary_variable_summary[variable == "Solves"],
            aes(x = urbanization, y = proportion + 0.005,
                label = paste(total_events, "/", total_tries)))+
  coord_cartesian(ylim = c(0, .06))+
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
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
    y = "Solve success rate"
  )

SolvesPlot

LopePlot <- ggplot() +
  # geom_boxplot()+
  # geom_jitter(data = xxxxxx, 
  #             aes(x = urbanization, y = (value), fill = urbanization),
  #             position = position_jitter(width = .15, height = 0),
  #             # stroke = 5,
  #             shape = 21, alpha = .5, size = 3)+
  geom_col(data = binary_variable_summary[variable == "Lope"], 
           aes(x = urbanization, y = proportion,
               fill = urbanization),
           shape = 21,
           lwd = 1,
           stroke = 1,
           size = 1.5)+
  geom_text(data = binary_variable_summary[variable == "Lope"],
            aes(x = urbanization, y = proportion + 0.005,
                label = paste(total_events, "/", total_tries)))+
#  coord_cartesian(ylim = c(0, .06))+
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
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
    y = "Fearfulness Rate"
  )

LopePlot


#For SI, plot raw data
Lopeplot_full <- ggplot() +
  # geom_boxplot()+
  geom_jitter(data = dat2.mlt[variable == "Lope"], 
              aes(x = urbanization, y = (value), fill = urbanization),
              position = position_jitter(width = .15, height = .2),
              # stroke = 5,
              shape = 21, alpha = .5, size = 3)+
  geom_pointrange(data = summaries[variable == "Lope"], 
                  aes(x = urbanization, y = mean_val,
                      ymin = ymin, ymax = ymax,
                      fill = urbanization),
                  shape = 21,
                  lwd = 1,
                  stroke = 1,
                  size = 1.5)+
  # geom_pointrange(data = predicted_all[response == "Contact_duration"],
  #                 aes(x = urbanization, ymin = lower, ymax = upper,
  #                     fill = urbanization,
  #                     y = predicted),
  #                 shape = 21,
  #                 lwd = 1,
  #                 stroke = 1,
  #                 size = 1.5)+
  # coord_cartesian(ylim = c(0, 75))+
  # geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
# facet_wrap(~ variable, scales = "free_y", ncol = 1) +
scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
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
    y = "Mean Fearfulness\n+/- SD"
  )

Lopeplot_full


#For SI, plot raw data
Solveplot_full <- ggplot() +
  # geom_boxplot()+
  geom_jitter(data = dat2.mlt[variable == "Solves"], 
              aes(x = urbanization, y = (value), fill = urbanization),
              position = position_jitter(width = .15, height = .2),
              # stroke = 5,
              shape = 21, alpha = .5, size = 3)+
  geom_pointrange(data = summaries[variable == "Solves"], 
                  aes(x = urbanization, y = mean_val,
                      ymin = ymin, ymax = ymax,
                      fill = urbanization),
                  shape = 21,
                  lwd = 1,
                  stroke = 1,
                  size = 1.5)+
  # geom_pointrange(data = predicted_all[response == "Contact_duration"],
  #                 aes(x = urbanization, ymin = lower, ymax = upper,
  #                     fill = urbanization,
  #                     y = predicted),
  #                 shape = 21,
  #                 lwd = 1,
  #                 stroke = 1,
  #                 size = 1.5)+
  # coord_cartesian(ylim = c(0, 75))+
  # geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
# facet_wrap(~ variable, scales = "free_y", ncol = 1) +
scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
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
    y = "Mean Solutions\n+/- SD"
  )

Solveplot_full



#Conbine the 5 SI plots
A <- arrangeGrob(Solve1, Solveplot_full, nrow = 1, widths = c(2,3))
grid::grid.draw(A)

B <- arrangeGrob(Inv1, invplot_full, nrow = 1, widths = c(2,3))
grid::grid.draw(B)

C <- arrangeGrob(Contact1, contactplot_full, nrow = 1, widths = c(2,3))
grid::grid.draw(C)

D <- arrangeGrob(BD1, BDplot_reduced, nrow = 1, widths = c(2,3))
grid::grid.draw(D)

E <- arrangeGrob(Lope1, Lopeplot_full, nrow = 1, widths = c(2,3))
grid::grid.draw(E)


FinalSIPlot <- arrangeGrob(A, D, B, C, E, ncol = 1)
grid::grid.draw(FinalSIPlot)

ggsave("C:/Users/sager/OneDrive/Desktop/school/MSc/manuscripts/Science Cognition/PNAS_SUbmission/FigS1_Jan20.pdf", FinalSIPlot, width = 7, height = 10, dpi = 700,  bg = "white") 




#Combine with cvplots and then save
FinalSIPlots <- ggarrange(CvPPlots, SI_plots, ncol = 2)


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
  f = "Solutions",
  g = "Behavioural Diversity",
  d = "Exploration Duration (s)",
  c = "Persistence (s)",
  e = "Fearfulness"
)


predicted_cont <- rbindlist(
  Map(predict_continuous_model, model_list, names(model_list))
)
predicted_cont[, response_label := label_map[as.character(response)]]
predicted_cont[, response_label := factor(response_label, levels = label_map)]


# Plot
SolveCity <- ggplot(predicted_cont[response_label == "Solutions"], aes(x = Nat50, y = predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#9F4E4A", alpha = 0.2) +
  geom_line(color = "#78206E", size = 1) +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
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
    x = "Urbanization (City sites only; %)",
    y = "Predicted Value"
  )



LopeCity <- ggplot(predicted_cont[response_label == "Fearfulness"], aes(x = Nat50, y = predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#9F4E4A", alpha = 0.2) +
  geom_line(color = "#78206E", size = 1) +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
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
    x = "Urbanization (City sites only; %)",
    y = "Predicted Value"
  )

InvCity <- ggplot(predicted_cont[response_label == "Exploration Duration (s)"], aes(x = Nat50, y = predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#9F4E4A", alpha = 0.2) +
  geom_line(color = "#78206E", size = 1) +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
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
    x = "Urbanization (City sites only; %)",
    y = "Predicted Duration"
  )

ConCity <- ggplot(predicted_cont[response_label == "Persistence (s)"], aes(x = Nat50, y = predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#9F4E4A", alpha = 0.2) +
  geom_line(color = "#78206E", size = 1) +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
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
    x = "Urbanization (City sites only; %)",
    y = "Predicted Duration"
  )


BDCity <- ggplot(predicted_cont[response_label == "Behavioural Diversity"], aes(x = Nat50, y = predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#9F4E4A", alpha = 0.2) +
  geom_line(color = "#78206E", size = 1) +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
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
    x = "Urbanization (City sites only; %)",
    y = "Predicted Value"
  )



#Conbine the 5 SI plots
A <- arrangeGrob(SolvesPlot, SolveCity, nrow = 1, widths = c(2,2))
grid::grid.draw(A)

B <- arrangeGrob(invplot_reduced, InvCity, nrow = 1, widths = c(2,2))
grid::grid.draw(B)

C <- arrangeGrob(contactplot_reduced, ConCity, nrow = 1, widths = c(2,2))
grid::grid.draw(C)

D <- arrangeGrob(BDplot_reduced, BDCity, nrow = 1, widths = c(2,2))
grid::grid.draw(D)

E <- arrangeGrob(LopePlot, LopeCity, nrow = 1, widths = c(2,2))
grid::grid.draw(E)


FinalMTPlot <- arrangeGrob(A, D, B, C, E, ncol = 1)
grid::grid.draw(FinalMTPlot)

#ggsave("C:/Users/sager/OneDrive/Desktop/school/MSc/manuscripts/Science Cognition/PNAS_SUbmission/Fig2_Jan20.pdf", FinalMTPlot, width = 7, height = 10, dpi = 700,  bg = "white") 



#Combine plots
SitePlots <- ggarrange(CvPPlots, CityPlots, ncol = 2)
#ggsave("C:/Users/sager/OneDrive/Desktop/school/MSc/manuscripts/Science Cognition/PNAS_SUbmission/FigS1_Jan20.pdf", SitePlots, width = 6, height = 7.5, dpi = 700,  bg = "white") 


# >>> Lundy alternative ---------------------------------------------------

dat2

#data = dat2, aes(x = Nat50, y = Contact_duration)
unique(predicted_cont$response)
ggplot()+
  # geom_smooth(data = dat2, aes(x = Nat50, y = Contact_duration),
  #             color = "black",
  #             method = "glm")+
  geom_ribbon(data = predicted_cont[response_label == "Persistence (s)"],
              aes(ymin = lower, ymax = upper, x = Nat50),
              fill = "grey60", alpha = 0.2) +
  geom_line(data = predicted_cont[response_label == "Persistence (s)"],
            aes(y = predicted, x = Nat50),
  color = "black", size = 1) +
  geom_jitter(data = dat2,
              aes(x = Nat50, y = Contact_duration, 
                  fill = Nat50),
              size = 3,
              alpha = .5, shape = 21)+
  scale_fill_gradient(low = "goldenrod", high = "orchid")+
  coord_cartesian(ylim = c(0, 10))+
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




