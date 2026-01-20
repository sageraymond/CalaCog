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

#Scale Nat50
dat$Nat50_scaled <- scale(dat$Nat50)
dat$GroupSize_scaled <- scale(dat$GroupSize)

#Create df with city only
datcity <- dat[urbanization == "City"]

#Step1. Create. There should be 8 of them (what to do about behavioural complexity...)-----------------------
#Step 1: Build FULL models------------------------------------------
InvFinal <- glmmTMB(Inv_duration ~   Sex + PuzzleType + urbanization +
                     (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)

ConFinal <- glmmTMB(Contact_duration ~ urbanization + Disease + Light + Sex + Year + PuzzleType + 
                     (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)

LopeFinal <- glmmTMB(Lope ~ GroupSize_scaled + urbanization +
                      (1|SiteID), family=binomial(link = "logit"), data = dat)

SolveFinal <- glmmTMB(Solves ~ urbanization +
                      (1|SiteID), family=binomial(link = "logit"), data = dat)

BDFinal <- glmmTMB(Behav_Complexity ~ urbanization +
                        (1|SiteID), family="poisson", data = dat)


InvCityFinal <- glmmTMB(Inv_duration ~   Sex + PuzzleType + Nat50_scaled +
                          (1|SiteID), ziformula = ~ ., family=lognormal(), data = datcity)
  
ConCityFinal <- glmmTMB(Contact_duration ~ Nat50_scaled + Disease + Light + Sex + Year + PuzzleType + 
                          (1|SiteID), ziformula = ~ ., family=lognormal(), data = datcity)

LopeCityFinal <- glmmTMB(Lope ~ GroupSize_scaled + Nat50_scaled +
                           (1|SiteID), family=binomial(link = "logit"), data = datcity)

SolveCityFinal <- glmmTMB(Solves ~ Sex + Nat50_scaled +
                            (1|SiteID), family=binomial(link = "logit"), data = datcity)

BDCityFinal <- glmmTMB(Behav_Complexity ~ Nat50_scaled +
                            (1|SiteID), family= "poisson", data = datcity)


#Now plot these dudes--------------------------------------------------------------
#Use loop to pull out the info I want to plot (pay attention to ZI!!!)

models <- list(
  InvFinal = InvFinal,
  ConFinal = ConFinal,
  LopeFinal = LopeFinal,
  SolveFinal = SolveFinal,
  BDFinal = BDFinal,
  InvCityFinal = InvCityFinal,
  ConCityFinal = ConCityFinal,
  LopeCityFinal = LopeCityFinal,
  SolveCityFinal = SolveCityFinal,
  BDCityFinal = BDCityFinal
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
  term == "GroupSize_scaled", "Group Size",
  term == "Nat50_scaled", "Urbanization (%)",
  default = term
)]

all_model_coefs <- all_model_coefs[term != "(Intercept)"]

unique(all_model_coefs$term)
all_model_coefs$term <- factor(all_model_coefs$term, levels = c("City",
                                                                "Urbanization (%)",
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
  labs(title = "A. Exploration Duration (All Sites)",
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
  labs(title = "B. Persistence (All Sites)",
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
  labs(title = "G. Fearfulness\n(All Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "City" = "#0072B2",
    "Group Size" = "tomato")) +
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 11),
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
 # geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
  #              width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "I. Solutions\n(All Sites)*",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "City" = "#0072B2")) +
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_blank())

#BD plot (All Sites)-----------------------------------------------------------------
BDPlot <- ggplot(data = all_model_coefs[model_name == "BDFinal"],
                    aes(x = term, y = estimate, color = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "E. Behavioural\nDiversity\n(All Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "City" = "#0072B2")) +
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
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
  labs(title = "C. Exploration Duration (City Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "Urbanization (%)" = "#0072B2",
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
  labs(title = "D. Persistence (City Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "Urbanization (%)" = "#0072B2",
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
  labs(title = "H. Fearfulness\n(City Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "Urbanization (%)" = "#0072B2",
    "Group Size" = "tomato")) +
  theme(axis.text.x = element_text( colour = "black", face = "plain", size = 11),
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
  labs(title = "J. Solutions\n(City Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "Urbanization (%)" = "#0072B2",
    "Male" = "tomato")) +
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_blank())

#BD plot (City Sites)-----------------------------------------------------------------
BDPlotCity <- ggplot(data = all_model_coefs[model_name == "BDCityFinal"],
                    aes(x = term, y = estimate, color = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~part, scales = "free_x") +
  theme_classic() +
  labs(title = "F. Behavioural\nDiversity\n(City Sites)",
       y = "Coefficient Estimate", x = "Predictor", color = "Model Part") +
  scale_color_manual(values = c(
    "Urbanization (%)" = "#0072B2")) +
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_blank())


blank_plot <- ggplot() + theme_void()

#Put all plots together
AB <- ggpubr::ggarrange(InvPlot, ConPlot, nrow = 1, widths = c(4,4.5))
CD <- ggpubr::ggarrange(InvPlotCity, ConPlotCity, nrow = 1, widths = c(4,4.5))
EFGH <- ggpubr::ggarrange(BDPlot, BDPlotCity, LopePlot, LopePlotCity, nrow = 1,
                          widths = c(2,2,2,3))
IJ <- ggpubr::ggarrange(SolvePlot, SolvePlotCity, blank_plot, nrow = 1,
                          widths = c(2.5,2.5, 3))

FinalPlot <- ggpubr::ggarrange(AB, CD, EFGH, IJ, nrow = 4)
#ggsave("figures/Fig3.pdf", FinalPlot, width = 9, height = 11, dpi = 700,  bg = "white") 



#You also need to save model information (summary and all that)
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
model_list <- list(InvFinal = InvFinal,
                   ConFinal = ConFinal,
                   LopeFinal = LopeFinal,
                   SolveFinal = SolveFinal,
                   InvCityFinal = InvCityFinal,
                   ConCityFinal = ConCityFinal,
                   LopeCityFinal = LopeCityFinal,
                   SolveCityFinal = SolveCityFinal)



#NOW do the thing
summary_table <- rbindlist(lapply(names(model_list), function(name) {
  mod <- model_list[[name]]
#  null_mod <- null_model_list[[name]]
  
  tidy_dt <- tidy_glmmTMB(mod)
  
  
  stats_dt <- extract_model_stats(mod)
  
#  lrt_dt <- extract_lrt(mod, null_mod)
  r2_dt <- extract_r2(mod)
  
  tidy_dt[, response := name]
  for (col in names(stats_dt)) tidy_dt[, (col) := stats_dt[[col]]]
 # for (col in names(lrt_dt)) tidy_dt[, (col) := lrt_dt[[col]]]
  for (col in names(r2_dt)) tidy_dt[, (col) := r2_dt[[col]]]
  
  tidy_dt
}))


summary_table <- summary_table[term != "(Intercept)"]
#write.csv(summary_table, "figures/EventModelInfo_FinalModels.csv")

LopeFinal$call
LOpeFinal_unscaled <- glmmTMB(formula = Lope ~ GroupSize + urbanization + (1 | 
                                                                                    SiteID), data = dat, family = binomial(link = "logit"), ziformula = ~0, 
                              dispformula = ~1)
summary(LOpeFinal_unscaled)
exp(confint(LOpeFinal_unscaled))

InvCityFinal$call
ConCityFinal$call
LopeCityFinal$call
SolveCityFinal$call


InvCityFinal_unscaled <- glmmTMB(formula = Inv_duration ~ Sex + PuzzleType + Nat50 + 
          (1 | SiteID), data = datcity, family = lognormal(), ziformula = ~., 
        dispformula = ~1)

ConCityFinal_unscaled <- glmmTMB(formula = Contact_duration ~ Nat50 + Disease + 
          Light + Sex + Year + PuzzleType + (1 | SiteID), data = datcity, 
        family = lognormal(), ziformula = ~., dispformula = ~1)

LopeCityFinal_unscaled <- glmmTMB(formula = Lope ~ GroupSize + Nat50 + (1 | 
                                                              SiteID), data = datcity, family = binomial(link = "logit"), 
        ziformula = ~0, dispformula = ~1)

SolveCityFinal_unscaled <- glmmTMB(formula = Solves ~ Sex + Nat50 + (1 | SiteID), 
        data = datcity, family = binomial(link = "logit"), ziformula = ~0, 
        dispformula = ~1)

summary(InvCityFinal_unscaled)
exp(confint(InvCityFinal_unscaled))

summary(ConCityFinal_unscaled)
exp(confint(ConCityFinal_unscaled))

summary(LopeCityFinal_unscaled)
exp(confint(LopeCityFinal_unscaled))

summary(SolveCityFinal_unscaled)
exp(confint(SolveCityFinal_unscaled))





#Do interaction or disp formula terms improve final models (I hope not...)
InvFinal1 <- glmmTMB(Inv_duration ~   Sex + PuzzleType + urbanization + Sex:urbanization +
                      (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)

InvFinal2 <- glmmTMB(Inv_duration ~   Sex + PuzzleType + urbanization + PuzzleType:urbanization +
                       (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)

ConFinal1 <- glmmTMB(Contact_duration ~ urbanization + Disease + Light + Sex + Year + PuzzleType + urbanization:Disease +
                      (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)

ConFinal2 <- glmmTMB(Contact_duration ~ urbanization + Disease + Light + Sex + Year + PuzzleType + urbanization:Light +
                       (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)

ConFinal3 <- glmmTMB(Contact_duration ~ urbanization + Disease + Light + Sex + Year + PuzzleType + urbanization:Sex +
                       (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)

ConFinal4 <- glmmTMB(Contact_duration ~ urbanization + Disease + Light + Sex + Year + PuzzleType + urbanization:Year + 
                       (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)

ConFinal5 <- glmmTMB(Contact_duration ~ urbanization + Disease + Light + Sex + Year + PuzzleType + urbanization:PuzzleType +
                       (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)


LopeFinal1 <- glmmTMB(Lope ~ GroupSize_scaled + urbanization + urbanization:GroupSize_scaled +
                       (1|SiteID), family=binomial(link = "logit"), data = dat)

summary(InvFinal1)
anova(InvFinal, InvFinal1) #NOOOOOOOOOOOOOOO Sex:Ubranization improved things. Fuck

summary(InvFinal2)
anova(InvFinal, InvFinal2)

summary(ConFinal1)
summary(ConFinal2)
summary(ConFinal3)
anova(ConFinal, ConFinal3)
summary(ConFinal4)
anova(ConFinal, ConFinal4)
summary(LopeFinal1)
anova(LopeFinal1, LopeFinal)


InvCityFinal1 <- glmmTMB(Inv_duration ~   Sex + PuzzleType + Nat50_scaled + Nat50_scaled:Sex +
                          (1|SiteID), ziformula = ~ ., family=lognormal(), data = datcity)

InvCityFinal2 <- glmmTMB(Inv_duration ~   Sex + PuzzleType + Nat50_scaled + Nat50_scaled:PuzzleType +
                           (1|SiteID), ziformula = ~ ., family=lognormal(), data = datcity)


ConCityFinal1 <- glmmTMB(Contact_duration ~ Nat50_scaled + Disease + Light + Sex + Year + PuzzleType + Nat50_scaled:Disease +
                          (1|SiteID), ziformula = ~ ., family=lognormal(), data = datcity)

ConCityFinal2 <- glmmTMB(Contact_duration ~ Nat50_scaled + Disease + Light + Sex + Year + PuzzleType + Nat50_scaled:Light +
                           (1|SiteID), ziformula = ~ ., family=lognormal(), data = datcity)

ConCityFinal3 <- glmmTMB(Contact_duration ~ Nat50_scaled + Disease + Light + Sex + Year + PuzzleType + Nat50_scaled:Sex +
                           (1|SiteID), ziformula = ~ ., family=lognormal(), data = datcity)

ConCityFinal4 <- glmmTMB(Contact_duration ~ Nat50_scaled + Disease + Light + Sex + Year + PuzzleType + Nat50_scaled:Year +
                           (1|SiteID), ziformula = ~ ., family=lognormal(), data = datcity)

ConCityFinal5 <- glmmTMB(Contact_duration ~ Nat50_scaled + Disease + Light + Sex + Year + PuzzleType + Nat50_scaled:PuzzleType +
                           (1|SiteID), ziformula = ~ ., family=lognormal(), data = datcity)

LopeCityFinal1 <- glmmTMB(Lope ~ GroupSize + Nat50_scaled + Nat50_scaled:GroupSize +
                           (1|SiteID), family=binomial(link = "logit"), data = datcity)

SolveCityFinal1 <- glmmTMB(Solves ~ Sex + Nat50_scaled + Nat50_scaled:Sex +
                            (1|SiteID), family=binomial(link = "logit"), data = datcity)

summary(InvCityFinal1)
summary(InvCityFinal2)
summary(ConCityFinal1)
summary(ConCityFinal2)
summary(ConCityFinal3)
summary(ConCityFinal4)
summary(ConCityFinal5)
summary(LopeCityFinal1)
summary(SolveCityFinal1)


#OK. what about dispformula?
InvFinal_disp <- glmmTMB(Inv_duration ~   Sex + PuzzleType + urbanization +
                      (1|SiteID), ziformula = ~ Sex + PuzzleType + urbanization, 
                      dispformula = ~ Sex + PuzzleType + urbanization,  
                      family=lognormal(), data = dat)

ConFinal_disp <- glmmTMB(Contact_duration ~ urbanization + Disease + Light + Sex + Year + PuzzleType + 
                      (1|SiteID), ziformula = ~ .,
                      dispformula = ~ urbanization + Disease + Light + Sex + Year + PuzzleType,  
                      family=lognormal(), data = dat)

LopeFinal_disp <- glmmTMB(Lope ~ GroupSize_scaled + urbanization +
                       (1|SiteID), 
                       dispformula = ~ GroupSize_scaled + urbanization,  
                       family=binomial(link = "logit"), data = dat)

SolveFinal_disp <- glmmTMB(Solves ~ urbanization +
                        (1|SiteID), 
                        dispformula = ~ urbanization,  
                        family=binomial(link = "logit"), data = dat)


InvCityFinal_disp <- glmmTMB(Inv_duration ~   Sex + PuzzleType + Nat50_scaled +
                          (1|SiteID), ziformula = ~ ., 
                          dispformula = ~ Sex + PuzzleType + Nat50_scaled,  
                          family=lognormal(), data = datcity)

ConCityFinal_disp <- glmmTMB(Contact_duration ~ Nat50_scaled + Disease + Light + Sex + Year + PuzzleType + 
                          (1|SiteID), ziformula = ~ ., 
                          dispformula = ~ Nat50_scaled + Disease + Light + Sex + Year + PuzzleType,  
                          family=lognormal(), data = datcity)

LopeCityFinal_disp <- glmmTMB(Lope ~ GroupSize_scaled + Nat50_scaled +
                           (1|SiteID), 
                           dispformula = ~ GroupSize_scaled + Nat50_scaled,  
                           family=binomial(link = "logit"), data = datcity)

SolveCityFinal_disp <- glmmTMB(Solves ~ Sex + Nat50_scaled +
                            (1|SiteID), 
                            dispformula = ~ Sex + Nat50_scaled,
                            family=binomial(link = "logit"), data = datcity)

anova(InvFinal, InvFinal_disp)
anova(ConFinal, ConFinal_disp)
anova(LopeFinal, LopeFinal_disp)
anova(SolveFinal, SolveFinal_disp)

anova(InvCityFinal, InvCityFinal_disp)
anova(ConCityFinal, ConCityFinal_disp)
anova(LopeCityFinal, LopeCityFinal_disp)
anova(SolveCityFinal, SolveCityFinal_disp)

