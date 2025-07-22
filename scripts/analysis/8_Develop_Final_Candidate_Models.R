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
library(jtools)

# 0. Load data and guide --------------------------------------------------

master_guide <- readRDS("builds/batch_models_july_2025/model_guide_with_comparison_stats.Rds")

dat <- readRDS("builds/prepared_dataset.Rds")
dat

#Make urbanization factor
dat$urbanization <- factor(dat$urbanization, levels = c("Wild", "City"))

#FIlter to events in which animal was oriented
dat <- dat[Orient == "Y"]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
# 1. List of models I am trying to build:
#Inv Duration + Sex + Puzzle Type
#Con Duration + disease, light, sex, year, puzzle
# escape gait + group size
# solves + sex

#I think the best way is to just bust these out manually...
Inv <- glmmTMB(Inv_duration ~   Sex + PuzzleType + 
                 (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)
Con <- glmmTMB(Contact_duration ~ Disease + Light + Sex + Year + PuzzleType + 
                 (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)

Lope <- glmmTMB(Lope ~ GroupSize + 
                  (1|SiteID), family=binomial(link = "logit"), data = dat)

Solve <- glmmTMB(Solves ~ Sex + 
                   (1|SiteID), family=binomial(link = "logit"), data = dat)

#Let's pull out all of their info, plus make some coefficient plots for them

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
model_list <- list(Inv = Inv,
                   Con = Con,
                   Lope = Lope,
                   Solve = Solve)

#I guess I have to write nulls. I'm sure there is a better way. Oh well.
Inv_null <- glmmTMB(Inv_duration ~  1 + 
                 (1|SiteID), ziformula = ~ ., family=lognormal(), data = model.frame(Inv))
Con_null <- glmmTMB(Contact_duration ~ 1 + 
                 (1|SiteID), ziformula = ~ ., family=lognormal(), data = model.frame(Con))

Lope_null <- glmmTMB(Lope ~ 1 + 
                  (1|SiteID), family=binomial(link = "logit"), data = model.frame(Lope))

Solve_null <- glmmTMB(Solves ~ 1 + 
                   (1|SiteID), family=binomial(link = "logit"), data = model.frame(Solve))

null_model_list <- list(Inv_null, Con_null, Lope_null, Solve_null)
names(null_model_list) <- names(model_list)

# NOW do the thing
summary_table <- rbindlist(lapply(names(model_list), function(name) {
  mod <- model_list[[name]]
  null_mod <- null_model_list[[name]]
  
  tidy_dt <- tidy_glmmTMB(mod)
  
  
  stats_dt <- extract_model_stats(mod)
  
  lrt_dt <- extract_lrt(mod, null_mod)
  r2_dt <- extract_r2(mod)
  
  tidy_dt[, response := name]
  for (col in names(stats_dt)) tidy_dt[, (col) := stats_dt[[col]]]
  for (col in names(lrt_dt)) tidy_dt[, (col) := lrt_dt[[col]]]
  for (col in names(r2_dt)) tidy_dt[, (col) := r2_dt[[col]]]
  
  tidy_dt
}))


summary_table <- summary_table[term != "(Intercept)"]
#write.csv(summary_table, "figures/EventModelInfo_IntExtCandidates.csv")


#And plot them
#Not snazzy or sexy but just do it manually for everyone
coef_Inv <- tidy(Inv, effects = "fixed", conf.int = TRUE) |> as.data.table()

cond_coefs_Inv <- coef_Inv[component == "cond"]
zi_coefs_Inv   <- coef_Inv[component == "zi"]

cond_coefs_Inv[, part := "Conditional"]
zi_coefs_Inv[, part := "Zero-inflation"]

all_coefs_Inv <- rbind(cond_coefs_Inv, zi_coefs_Inv)

coef_Con <- tidy(Con, effects = "fixed", conf.int = TRUE) |> as.data.table()

cond_coefs_Con <- coef_Con[component == "cond"]
zi_coefs_Con   <- coef_Con[component == "zi"]

cond_coefs_Con[, part := "Conditional"]
zi_coefs_Con[, part := "Zero-inflation"]

all_coefs_Con <- rbind(cond_coefs_Con, zi_coefs_Con)

coef_Lope <- tidy(Lope, effects = "fixed", conf.int = TRUE) |> as.data.table()
coef_Solve <- tidy(Solve, effects = "fixed", conf.int = TRUE) |> as.data.table()


# Plot Inv-------------------------------------------------------------
all_coefs_Inv <- all_coefs_Inv[term != "(Intercept)"]
all_coefs_Inv$term <- ifelse(all_coefs_Inv$term == "SexM", "Male", "Wood Puzzle")

InvPlot <- ggplot(all_coefs_Inv, aes(x = term, y = estimate, color = "tomato")) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "A. Investigate Duration",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))


#Plot Con-------------------------------------------------------------------
all_coefs_Con <- all_coefs_Con[term != "(Intercept)"]
all_coefs_Con$term <- ifelse(all_coefs_Con$term == "SexM", "Male", all_coefs_Con$term)
all_coefs_Con$term <- ifelse(all_coefs_Con$term == "DiseaseY", "Diseased", all_coefs_Con$term)
all_coefs_Con$term <- ifelse(all_coefs_Con$term == "LightL", "Daylight", all_coefs_Con$term)
all_coefs_Con$term <- ifelse(all_coefs_Con$term == "Year2025", "Second Year", all_coefs_Con$term)
all_coefs_Con$term <- ifelse(all_coefs_Con$term == "PuzzleTypeW", "Wood Puzzle", all_coefs_Con$term)

ConPlot <- ggplot(all_coefs_Con, aes(x = term, y = estimate, color = "tomato")) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "B. Contact Duration",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))

#Plot Lope-------------------------------------------------------------------
coef_Lope <- coef_Lope[term != "(Intercept)"]
coef_Lope$term <- ifelse(coef_Lope$term == "GroupSize", "Group Size", coef_Lope$term)
LopePlot <- ggplot(coef_Lope, aes(x = term, y = estimate, color = "tomato")) +
  geom_point(position = position_dodge(width = 0.5), size =5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "C. Escape Gait",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))

#Solve plot-----------------------------------------------------------------
coef_Solve <- coef_Solve[term != "(Intercept)"]
coef_Solve$term <- ifelse(coef_Solve$term == "SexM", "Male", coef_Solve$term)

SolvePlot <- ggplot(coef_Solve, aes(x = term, y = estimate, color = "tomato")) +
  geom_point(position = position_dodge(width = 0.5), size =5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "D. Solves",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))


#Put all plots together
FinalPlot <- ggarrange(InvPlot, LopePlot, ConPlot, SolvePlot, nrow = 2, widths = c(5,3))
#ggsave("figures/IntExtCandidateModelPlot.png", FinalPlot, width = 8, height = 6, dpi = 700,  bg = "white") 

