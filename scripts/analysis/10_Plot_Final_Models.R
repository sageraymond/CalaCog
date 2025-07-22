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

#Create df with city only
datcity <- dat[urbanization == "City"]

#Step1. Create. There should be 8 of them (what to do about behavioural complexity...)-----------------------
#Step 1: Build FULL models------------------------------------------
InvFinal <- glmmTMB(Inv_duration ~   Sex + PuzzleType + urbanization +
                     (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)

ConFinal <- glmmTMB(Contact_duration ~ urbanization + Disease + Light + Sex + Year + PuzzleType + 
                     (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)

LopeFinal <- glmmTMB(Lope ~ GroupSize + urbanization +
                      (1|SiteID), family=binomial(link = "logit"), data = dat)

SolveFinal <- glmmTMB(Solves ~ urbanization +
                      (1|SiteID), family=binomial(link = "logit"), data = dat)


InvCityFinal <- glmmTMB(Inv_duration ~   Sex + PuzzleType + urbanization_score +
                          (1|SiteID), ziformula = ~ ., family=lognormal(), data = datcity)
  
ConCityFinal <- glmmTMB(Contact_duration ~ Disease + Light + Sex + Year + PuzzleType + 
                          (1|SiteID), ziformula = ~ ., family=lognormal(), data = datcity)

LopeCityFinal <- glmmTMB(Lope ~ GroupSize + urbanization_score +
                           (1|SiteID), family=binomial(link = "logit"), data = datcity)

SolveCityFinal <- glmmTMB(Solves ~ Sex + 
                            (1|SiteID), family=binomial(link = "logit"), data = datcity)



#Now plot these dudes--------------------------------------------------------------
#Use loop to pull out the info I want to plot (pay attention to ZI!!!)

models <- list(
  InvFinal = InvFinal,
  ConFinal = ConFinal,
  LopeFinal = LopeFinal,
  SolveFinal = SolveFinal,
  InvCityFinal = InvCityFinal,
  ConCityFinal = ConCityFinal,
  LopeCityFinal = LopeCityFinal,
  SolveCityFinal = SolveCityFinal
)

#Stash em here:
all_coefs_list <- list()

for (name in names(models)) {
  model <- models[[name]]
  
  coef_dt <- tidy(model, effects = "fixed", conf.int = TRUE) |> as.data.table()
  
  if ("component" %in% colnames(coef_dt)) {
    cond_coefs <- coef_dt[component == "cond"] #this should address ZIs
    zi_coefs   <- coef_dt[component == "zi"]
    
    cond_coefs[, part := "Conditional"]
    zi_coefs[, part := "Zero-inflation"]
    
    combined <- rbind(cond_coefs, zi_coefs)
  } else {
    
    
    combined <- coef_dt
    combined[, part := "Conditional"]
  }
  
  combined[, model_name := name]
  all_coefs_list[[name]] <- combined
}

all_model_coefs <- rbindlist(all_coefs_list)
 
all_model_coefs
all_model_coefs[, term := fcase(
  term == "SexM", "Male",
  term == "PuzzleTypeW", "Wood Puzzle",
  term == "urbanizationCity", "City",
  term == "DiseaseY", "Diseased",
  term == "LightL", "Daylight",
  term == "Year2025", "Second Year",
  term == "GroupSize", "Group Size",
  term == "urbanization_score", "Urb. Score",
  default = term
)]

all_model_coefs <- all_model_coefs[term != "(Intercept)"]

unique(all_model_coefs$term)
all_model_coefs$term <- factor(all_model_coefs$term, levels = c("City",
                                                                "Urb. Score",
                                                                "Male",
                                                                "Wood Puzzle",
                                                                "Diseased",
                                                                "Daylight",
                                                                "Second Year",
                                                               "Group Size"))
# Plot Inv (all sites)
InvPlot <- ggplot(data = all_model_coefs[model_name == "InvFinal"],
                  aes(x = term, y = estimate, color = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "A. Investigate Duration (All Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "City" = "#0072B2",
    "Male" = "tomato",
    "Wood Puzzle" = "tomato")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))


#Plot Con (All Sites)-------------------------------------------------------------------
ConPlot <- ggplot(data = all_model_coefs[model_name == "ConFinal"],
                  aes(x = term, y = estimate, color = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "C. Contact Duration (All Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "City" = "#0072B2",
    "Male" = "tomato",
    "Wood Puzzle" = "tomato",
    "Diseased" = "tomato",
    "Daylight" = "tomato",
    "Second Year" = "tomato")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))

#Plot Lope-------------------------------------------------------------------
LopePlot <- ggplot(data = all_model_coefs[model_name == "LopeFinal"],
                  aes(x = term, y = estimate, color = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "E. Escape Gait\n(All Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "City" = "#0072B2",
    "Group Size" = "tomato")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_blank())

#Solve plot (All Sites)-----------------------------------------------------------------
SolvePlot <- ggplot(data = all_model_coefs[model_name == "SolveFinal"],
                   aes(x = term, y = estimate, color = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "G. Solves\n(All Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "City" = "#0072B2")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_blank())

#Inv-city sites-----------------------------------------------------------------
InvPlotCity <- ggplot(data = all_model_coefs[model_name == "InvCityFinal"],
                  aes(x = term, y = estimate, color = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "C. Investigate Duration (City Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "Urb. Score" = "#0072B2",
    "Male" = "tomato",
    "Wood Puzzle" = "tomato")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))


#Plot Con (All Sites)-------------------------------------------------------------------
ConPlotCity <- ggplot(data = all_model_coefs[model_name == "ConCityFinal"],
                      aes(x = term, y = estimate, color = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "D. Contact Duration (City Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "Diseased" = "tomato",
    "Daylight" = "tomato",
    "Second Year" = "tomato",
    "Male" = "tomato",
    "Wood Puzzle" = "tomato")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))

#Plot Lope-------------------------------------------------------------------
LopePlotCity <- ggplot(data = all_model_coefs[model_name == "LopeCityFinal"],
                      aes(x = term, y = estimate, color = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "F. Escape Gait\n(City Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "Urb. Score" = "#0072B2",
    "Group Size" = "tomato")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_blank())

#Solve plot (All Sites)-----------------------------------------------------------------
SolvePlotCity <- ggplot(data = all_model_coefs[model_name == "SolveCityFinal"],
                    aes(x = term, y = estimate, color = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "H. Solves\n(City Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "Male" = "tomato")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_blank())


#Put all plots together
AB <- ggpubr::ggarrange(InvPlot, ConPlot, nrow = 1, widths = c(4,4.5))
CD <- ggpubr::ggarrange(InvPlotCity, ConPlotCity, nrow = 1, widths = c(4,4.5))
EFGH <- ggpubr::ggarrange(LopePlot, LopePlotCity, SolvePlot, SolvePlotCity, nrow = 1,
                          widths = c(2.5,2.5,2,2))

FinalPlot <- ggpubr::ggarrange(AB, CD, EFGH, nrow = 3)
#ggsave("figures/FinalModelPlot.png", FinalPlot, width = 8, height = 9, dpi = 700,  bg = "white") 


