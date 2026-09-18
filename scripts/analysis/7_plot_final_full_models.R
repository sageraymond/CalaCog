
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

#Start by loading up your csv that has the full model results
full_mods <- fread("figures/TableS5_hyp3_model_coefficients.csv")

unique(full_mods$Term)
full_mods$Term <- ifelse(full_mods$Term == "temp", "Temperature", full_mods$Term)
full_mods$Term <- ifelse(full_mods$Term == "GroupSize", "Group size", full_mods$Term)

View(full_mods)

#get rid of the ones that aren't top models
#this is, for all sites, BD, ID, CD
#and for city sites, ID, CD

full_mods$response_extent <- paste0(full_mods$Response, full_mods$Extent)

unique(full_mods$response_extent)
full_mods <- full_mods[response_extent != "Behavioural diversityAll"]
full_mods <- full_mods[response_extent != "Investigate durationAll"]
full_mods <- full_mods[response_extent != "Contact durationAll"]
full_mods <- full_mods[response_extent != "Investigate durationCity"]
full_mods <- full_mods[response_extent != "Contact durationCity"]


#OK. now I need to get the right models for those other things

# "Behavioural diversityAll"] -> intrinsic 
# "Investigate durationAll"]  ->  null
# "Contact durationAll"] ->  null
# "Investigate durationCity"] ->  null
# "Contact durationCity"] -> urb only

#Well, I can't reallt bring in nulls. 

#Urb only:
urb <- fread("figures/TableS2_urb_only_model_results.csv")
urb <- urb %>% dplyr::select(Response, Extent, B, `Low CI`, `Upp. CI`) %>%
  dplyr::filter(Response == "Contact duration") %>%
  dplyr::filter(Extent == "City")
urb$Term <- "City"


#Int only:
int <- fread("figures/TableS4_hyp2_model_coefficients.csv")
int <- int %>% dplyr::select(Response, Extent, B, `Low CI`, `Upp. CI`, Term) %>%
  dplyr::filter(Response == "Behavioural diversity") %>%
  dplyr::filter(Extent == "All")


add <- rbind(urb, int)
remove(int)
remove(urb)


str(full_mods)
full_mods <- full_mods %>%
  dplyr::select(Response, Extent, Term, B, `Low CI`, `Upp. CI`)


all_coef <- rbind(full_mods, add)

remove(full_mods)
remove(add)

#check/ fix some things
unique(all_coef$Term)

all_coef$Term <- ifelse(all_coef$Term == "City", "Urbanization (%)", all_coef$Term)

View(all_coef)

#PLotting time----------------------------------------------------------------


#Inv plot
InvPlot <- ggplot(data = all_coef[Response == "Investigate" & Extent == "All"],
                  aes(x = reorder(Term, abs(B)), y = B, color = Term))+
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = `Low CI`, ymax = `Upp. CI`),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "C. Investigation (All Sites, n = 1,156)",
       y = "Estimate", x = "Predictor") +
  scale_color_manual(values = c(
    "Diseased  " = "#84828f",
    "Plastic Puzzle  " = "#84828f",
    "Second study year  " = "#84828f",
    "Sequence  " = "#84828f",
    "Sex (M)  " = "#84828f",
    "Sex (F)  " = "#84828f",
    "City (vs. Wild)" = "#78206E"
  )) +
  coord_flip()+
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))
InvPlot

#Plot Con (All Sites)-------------------------------------------------------------------
ConPlot <- ggplot(data = all_coef[Response == "Contact" & Extent == "All"],
                  aes(x = reorder(Term, abs(B)), y = B, color = Term))+
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = `Low CI`, ymax = `Upp. CI`),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "D. Contact (All Sites, n = 1,156)",
       y = "Estimate", x = "Predictor") +
  scale_color_manual(values = c(
    "Diseased  " = "#84828f",
    "Darkness  " = "#84828f",
        "Plastic Puzzle  " = "#84828f",
    "Second study year  " = "#84828f",
    "Sequence  " = "#84828f",
    "Sex (M)  " = "#84828f",
    "Sex (F)  " = "#84828f",
    "City (vs. Wild)" = "#78206E"
  )) +
  coord_flip()+
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))
ConPlot


#Plot solves (All Sites)-------------------------------------------------------------------
SolvePlot <- ggplot(data = all_coef[Response == "Solution" & Extent == "All"],
                    aes(x = reorder(Term, abs(B)), y = B, color = Term))+
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = `Low CI`, ymax = `Upp. CI`),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "A. Solution (All Sites, n = 1,156)",
       y = "Estimate", x = "Predictor") +
  scale_color_manual(values = c(
    "Diseased  " = "#84828f",
    "Darkness  " = "#84828f",
    "Plastic Puzzle  " = "#84828f",
    "Second study year  " = "#84828f",
    "Sequence  " = "#84828f",
    "Sex (M)  " = "#84828f",
    "Sex (F)  " = "#84828f",
    "City (vs. Wild)" = "#78206E"
  )) +
  coord_flip()+
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))
SolvePlot


#Plot solves (All Sites)-------------------------------------------------------------------
LopePlot <- ggplot(data = all_coef[Response == "Escape gait" & Extent == "All"],
                   aes(x = reorder(Term, abs(B)), y = B, color = Term))+
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = `Low CI`, ymax = `Upp. CI`),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "E. Escape gait (All Sites, n = 1,156)",
       y = "Estimate", x = "Predictor") +
  scale_color_manual(values = c(
    "Diseased  " = "#84828f",
    "Darkness  " = "#84828f",
    "Plastic Puzzle  " = "#84828f",
    "Second study year  " = "#84828f",
    "Sequence  " = "#84828f",
    "Sex (M)  " = "#84828f",
    "Sex (F)  " = "#84828f",
    "City (vs. Wild)" = "#78206E"
  )) +
  coord_flip()+
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))
LopePlot


#Plot BD (All Sites)-------------------------------------------------------------------
BehavDivPlot <- ggplot(data = all_coef[Response == "Behavioural diversity" & Extent == "All"],
                       aes(x = reorder(Term, abs(B)), y = B, color = Term))+
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = `Low CI`, ymax = `Upp. CI`),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "B. Behavioural diversity (All Sites, n = 231)",
       y = "Estimate", x = "Predictor") +
  scale_color_manual(values = c(
    "Diseased  " = "#84828f",
    "Darkness  " = "#84828f",
    "Plastic Puzzle  " = "#84828f",
    "Second study year  " = "#84828f",
    "Sequence  " = "#84828f",
    "Sex (M)  " = "#84828f",
    "Sex (F)  " = "#84828f",
    "City (vs. Wild)" = "#78206E"
  )) +
  coord_flip()+
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))
BehavDivPlot


#Inv plot
InvPlot_city <- ggplot(data = all_coef[Response == "Investigate" & Extent == "City"],
                       aes(x = reorder(Term, abs(B)), y = B, color = Term))+
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = `Low CI`, ymax = `Upp. CI`),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "H. Investigation (City Sites, n = 996)",
       y = "Estimate", x = "Predictor") +
  scale_color_manual(values = c(
    "Diseased  " = "#84828f",
    "Plastic Puzzle  " = "#84828f",
    "Second study year  " = "#84828f",
    "Sequence  " = "#84828f",
    "Sex (M)  " = "#84828f",
    "Sex (F)  " = "#84828f",
    "Urbanization (%)" = "#78206E"
  )) +
  coord_flip()+
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))
InvPlot_city

#Plot Con (All Sites)-------------------------------------------------------------------
ConPlot_city <- ggplot(data = all_coef[Response == "Contact" & Extent == "City"],
                       aes(x = reorder(Term, abs(B)), y = B, color = Term))+
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = `Low CI`, ymax = `Upp. CI`),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "I. Contact (City Sites, n = 996)",
       y = "Estimate", x = "Predictor") +
  scale_color_manual(values = c(
    "Diseased  " = "#84828f",
    "Darkness  " = "#84828f",
    "Plastic Puzzle  " = "#84828f",
    "Second study year  " = "#84828f",
    "Sequence  " = "#84828f",
    "Sex (M)  " = "#84828f",
    "Sex (F)  " = "#84828f",
    "Urbanization (%)" = "#78206E"
  )) +
  coord_flip()+
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))
ConPlot_city


#Plot solves (All Sites)-------------------------------------------------------------------
SolvePlot_city <- ggplot(data = all_coef[Response == "Solution" & Extent == "City"],
                         aes(x = reorder(Term, abs(B)), y = B, color = Term))+
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = `Low CI`, ymax = `Upp. CI`),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "F. Solution (City Sites, n = 996)",
       y = "Estimate", x = "Predictor") +
  scale_color_manual(values = c(
    "Diseased  " = "#84828f",
    "Darkness  " = "#84828f",
    "Plastic Puzzle  " = "#84828f",
    "Second study year  " = "#84828f",
    "Sequence  " = "#84828f",
    "Sex (M)  " = "#84828f",
    "Sex (F)  " = "#84828f",
    "Urbanization (%)" = "#78206E"
  )) +
  coord_flip()+
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))
SolvePlot_city


#Plot solves (All Sites)-------------------------------------------------------------------
LopePlot_city <- ggplot(data = all_coef[Response == "Escape gait" & Extent == "City"],
                        aes(x = reorder(Term, abs(B)), y = B, color = Term))+
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = `Low CI`, ymax = `Upp. CI`),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "K. Escape gait (All Sites, n = 996)",
       y = "Estimate", x = "Predictor") +
  scale_color_manual(values = c(
    "Diseased  " = "#84828f",
    "Darkness  " = "#84828f",
    "Plastic Puzzle  " = "#84828f",
    "Second study year  " = "#84828f",
    "Sequence  " = "#84828f",
    "Sex (M)  " = "#84828f",
    "Sex (F)  " = "#84828f",
    "Urbanization (%)" = "#78206E"
  )) +
  coord_flip()+
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))
LopePlot


#Plot BD (All Sites)-------------------------------------------------------------------
BehavDivPlot_city <- ggplot(data = all_coef[Response == "Behavioural diversity" & Extent == "City"],
                            aes(x = reorder(Term, abs(B)), y = B, color = Term))+
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = `Low CI`, ymax = `Upp. CI`),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "G. Behavioural diversity (All Sites, n = 220)",
       y = "Estimate", x = "Predictor") +
  scale_color_manual(values = c(
    "Diseased  " = "#84828f",
    "Darkness  " = "#84828f",
    "Plastic Puzzle  " = "#84828f",
    "Second study year  " = "#84828f",
    "Sequence  " = "#84828f",
    "Sex (M)  " = "#84828f",
    "Sex (F)  " = "#84828f",
    "Urbanization (%)" = "#78206E"
  )) +
  coord_flip()+
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))
BehavDivPlot_city


#Plot contact duration (All Sites)-------------------------------------------------------------------
Contact_duration_plot_city <- ggplot(data = all_coef[Response == "Contact duration" & Extent == "City"],
                                     aes(x = reorder(Term, abs(B)), y = B, color = Term))+
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = `Low CI`, ymax = `Upp. CI`),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "J. Contact duration (City Sites, n = 220)",
       y = "Estimate", x = "Predictor") +
  scale_color_manual(values = c(
    "Diseased  " = "#84828f",
    "Darkness  " = "#84828f",
    "Plastic Puzzle  " = "#84828f",
    "Second study year  " = "#84828f",
    "Sequence  " = "#84828f",
    "Sex (M)  " = "#84828f",
    "Sex (F)  " = "#84828f",
    "Urbanization (%)" = "#78206E"
  )) +
  coord_flip()+
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))
Contact_duration_plot_city



#Time to put the plots together--------------------------------------------------

blank_plot <- ggplot() + theme_void()

#Put all plots together

ABCDE <- ggpubr::ggarrange(SolvePlot, BehavDivPlot, InvPlot, ConPlot, LopePlot, nrow = 5, heights = c(5,3,8,8,6))
FGHIJK <- ggpubr::ggarrange(SolvePlot_city, BehavDivPlot_city, InvPlot_city, ConPlot_city, Contact_duration_plot_city, LopePlot_city, nrow = 6, heights = c(3,3,7,8,3,6))

FinalPlot <- ggpubr::ggarrange(ABCDE, FGHIJK, ncol = 2)
#ggsave("figures/Fig3.pdf", FinalPlot, width = 10, height = 14, dpi = 700,  bg = "white") 





#Check interaction formulas for final models
guide <- readRDS("outputs/final_guide_with_metrics.Rds")
guide1 <- guide[rand_ef == "(1|SiteID)"]
guide2 <- guide1[scaled == "yes"]

guide2[Response == "Inv" & dat == "dat_all"]
m1 <- glmmTMB(Inv~urbanization + Disease + PuzzleType + Year + SiteSequence + Sex + (1|SiteID), family = binomial(link = 'logit'), data = dat_all)
m2 <- glmmTMB(Inv~urbanization*PuzzleType + Disease + Year + SiteSequence + Sex + (1|SiteID), family = binomial(link = 'logit'), data = dat_all)
AIC(m1)
AIC(m2)


guide2[Response == "Contact" & dat == "dat_all"]
m1 <- glmmTMB(Contact~urbanization + Light + Disease + PuzzleType + Year + Sex + temp + (1|SiteID), family = binomial(link = 'logit'), data = dat_all)
m2 <- glmmTMB(Contact~urbanization*PuzzleType + Light + Disease + Year + Sex + temp + (1|SiteID), family = binomial(link = 'logit'), data = dat_all)
AIC(m1)
AIC(m2)


guide2[Response == "Inv" & dat == "dat_city"]
m1 <- glmmTMB(Inv~Nat50_scaled + Disease + PuzzleType + Year + SiteSequence_scaled + Sex + (1|SiteID), family = binomial(link = 'logit'), data = dat_city)
m2 <- glmmTMB(Inv~Nat50_scaled*PuzzleType + Disease + Year + SiteSequence_scaled + Sex + (1|SiteID), family = binomial(link = 'logit'), data = dat_city)
AIC(m1)
AIC(m2)


guide2[Response == "Contact" & dat == "dat_city"]
m1 <- glmmTMB(Contact~Nat50_scaled + Light + Disease + PuzzleType + Year + Sex + temp_scaled + (1|SiteID), family = binomial(link = 'logit'), data = dat_city)
m2 <- glmmTMB(Contact~Nat50_scaled*PuzzleType + Light + Disease + Year + Sex + temp_scaled + (1|SiteID), family = binomial(link = 'logit'), data = dat_city)
AIC(m1)
AIC(m2)


guide2[Response == "Lope" & dat == "dat_city"]
m1 <- glmmTMB(Lope~Nat50_scaled + GroupSize_scaled + PuzzleType + SiteSequence_scaled + Sex + temp_scaled + (1|SiteID), family = binomial(link = 'logit'), data = dat_city)
m2 <- glmmTMB(Lope~Nat50_scaled*PuzzleType + GroupSize_scaled + SiteSequence_scaled + Sex + temp_scaled + (1|SiteID), family = binomial(link = 'logit'), data = dat_city)
AIC(m1)
AIC(m2)



#Do interaction or disp formula terms improve final models 
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
anova(InvFinal, InvFinal1)

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

