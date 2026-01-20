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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
#Key here is that if you want to compare using LR tests, and I do, everything eneds to be fit
#to the same data
#the limiting thing here is gonna be int/ ext factors because dataset is smaller for things like sex

#Step 1: Build FULL models------------------------------------------
InvFull <- glmmTMB(Inv_duration ~   Sex + PuzzleType + urbanization +
                     (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)

ConFull <- glmmTMB(Contact_duration ~ urbanization + Disease + Light + Sex + Year + PuzzleType + 
                     (1|SiteID), ziformula = ~ ., family=lognormal(), data = dat)

LopeFull <- glmmTMB(Lope ~ GroupSize_scaled + urbanization +
                      (1|SiteID), family=binomial(link = "logit"), data = dat)

SolveFull <- glmmTMB(Solves ~ Sex + urbanization +
                       (1|SiteID), family=binomial(link = "logit"), data = dat)

BDFull <- glmmTMB(Behav_Complexity ~urbanization +
                       (1|SiteID), family="poisson", data = dat)


InvCityFull <- glmmTMB(Inv_duration ~   Sex + PuzzleType + Nat50_scaled +
                         (1|SiteID), ziformula = ~ ., family=lognormal(), data = datcity)

ConCityFull <- glmmTMB(Contact_duration ~ Disease + Light + Sex + Year + PuzzleType + Nat50_scaled +
                         (1|SiteID), ziformula = ~ ., family=lognormal(), data = datcity)

LopeCityFull <- glmmTMB(Lope ~ GroupSize_scaled + Nat50_scaled +
                          (1|SiteID), family=binomial(link = "logit"), data = datcity)

SolveCityFull <- glmmTMB(Solves ~ Sex + Nat50_scaled +
                           (1|SiteID), family=binomial(link = "logit"), data = datcity)

BDCityFull <- glmmTMB(Behav_Complexity ~ Nat50_scaled +
                           (1|SiteID), family="poisson", data = datcity)

#Extract these data as independent model frames or else LRT will fail later LAME
mf_Inv     <- model.frame(InvFull)
mf_Con     <- model.frame(ConFull)
mf_Lope    <- model.frame(LopeFull)
mf_Solve   <- model.frame(SolveFull)
mf_BD   <- model.frame(BDFull)

mf_InvCity  <- model.frame(InvCityFull)
mf_ConCity  <- model.frame(ConCityFull)
mf_LopeCity <- model.frame(LopeCityFull)
mf_SolveCity<- model.frame(SolveCityFull)
mf_BDCity<- model.frame(BDCityFull)

InvFull <- glmmTMB(Inv_duration ~   Sex + PuzzleType + urbanization +
                     (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_Inv)

ConFull <- glmmTMB(Contact_duration ~ urbanization + Disease + Light + Sex + Year + PuzzleType + 
                     (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_Con)

LopeFull <- glmmTMB(Lope ~ GroupSize_scaled + urbanization +
                      (1|SiteID), family=binomial(link = "logit"), data = mf_Lope)

SolveFull <- glmmTMB(Solves ~ Sex + urbanization +
                       (1|SiteID), family=binomial(link = "logit"), data = mf_Solve)

BDFull <- glmmTMB(Behav_Complexity ~ urbanization +
                       (1|SiteID), family="poisson", data = mf_BD)


InvCityFull <- glmmTMB(Inv_duration ~   Sex + PuzzleType + Nat50_scaled +
                         (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_InvCity)

ConCityFull <- glmmTMB(Contact_duration ~ Disease + Light + Sex + Year + PuzzleType + Nat50_scaled +
                         (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_ConCity)

LopeCityFull <- glmmTMB(Lope ~ GroupSize_scaled + Nat50_scaled +
                          (1|SiteID), family=binomial(link = "logit"), data = mf_LopeCity)

SolveCityFull <- glmmTMB(Solves ~ Sex + Nat50_scaled +
                           (1|SiteID), family=binomial(link = "logit"), data = mf_SolveCity)

BDCityFull <- glmmTMB(Behav_Complexity ~ Nat50_scaled +
                           (1|SiteID), family="poisson", data = mf_BDCity)



#Step2: Build Intrinsic models-----------------------------------------------------
Inv <- glmmTMB(Inv_duration ~   Sex + PuzzleType + 
                 (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_Inv)

Con <- glmmTMB(Contact_duration ~ Disease + Light + Sex + Year + PuzzleType + 
                 (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_Con)

Lope <- glmmTMB(Lope ~ GroupSize_scaled + 
                  (1|SiteID), family=binomial(link = "logit"), data = mf_Lope)

Solve <- glmmTMB(Solves ~ Sex + 
                   (1|SiteID), family=binomial(link = "logit"), data = mf_Solve)

InvCity <- glmmTMB(Inv_duration ~   Sex + PuzzleType + 
                     (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_InvCity)

ConCity <- glmmTMB(Contact_duration ~ Disease + Light + Sex + Year + PuzzleType + 
                     (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_ConCity)

LopeCity <- glmmTMB(Lope ~ GroupSize_scaled + 
                      (1|SiteID), family=binomial(link = "logit"), data = mf_LopeCity)

SolveCity <- glmmTMB(Solves ~ Sex + 
                       (1|SiteID), family=binomial(link = "logit"), data = mf_SolveCity)


#Step 3. Build urbanization models using model.frame -----------------------------------------------
InvUrb <- glmmTMB(Inv_duration ~   urbanization +
                    (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_Inv)

ConUrb <- glmmTMB(Contact_duration ~ urbanization +
                    (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_Con)

LopeUrb <- glmmTMB(Lope ~ urbanization +
                     (1|SiteID), family=binomial(link = "logit"), data = mf_Lope)

SolveUrb <- glmmTMB(Solves ~ urbanization +
                      (1|SiteID), family=binomial(link = "logit"), data = mf_Solve)

InvCityUrb <- glmmTMB(Inv_duration ~   Nat50_scaled +
                        (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_InvCity)

ConCityUrb <- glmmTMB(Contact_duration ~ Nat50_scaled +
                        (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_ConCity)

LopeCityUrb <- glmmTMB(Lope ~ Nat50_scaled +
                         (1|SiteID), family=binomial(link = "logit"), data = mf_LopeCity)

SolveCityUrb <- glmmTMB(Solves ~ Nat50_scaled +
                          (1|SiteID), family=binomial(link = "logit"), data = mf_SolveCity)


#4 Build nulls?---------------------------------------------------
InvNull <- glmmTMB(Inv_duration ~   1  +
                     (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_Inv)

ConNull <- glmmTMB(Contact_duration ~ 1 +
                     (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_Con)

LopeNull <- glmmTMB(Lope ~ 1 +
                      (1|SiteID), family=binomial(link = "logit"), data = mf_Lope)

SolveNull <- glmmTMB(Solves ~ 1 +
                       (1|SiteID), family=binomial(link = "logit"), data = mf_Solve)

InvCityNull <- glmmTMB(Inv_duration ~   1 +
                         (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_InvCity)

ConCityNull <- glmmTMB(Contact_duration ~ 1 +
                         (1|SiteID), ziformula = ~ ., family=lognormal(), data = mf_ConCity)

LopeCityNull <- glmmTMB(Lope ~ 1 +
                          (1|SiteID), family=binomial(link = "logit"), data = mf_LopeCity)

SolveCityNull <- glmmTMB(Solves ~ 1 +
                           (1|SiteID), family=binomial(link = "logit"), data = mf_SolveCity)

#Check nobs for a few of these dudes
nobs(InvNull)
nobs(Inv)
nobs(InvUrb)
nobs(InvFull)

nobs(InvCityNull)
nobs(InvCity)
nobs(InvCityUrb)
nobs(InvCityFull)

nobs(ConNull)
nobs(Con)
nobs(ConUrb)
nobs(ConFull)

nobs(ConCityNull)
nobs(ConCity)
nobs(ConCityUrb)
nobs(ConCityFull)

nobs(LopeNull)
nobs(Lope)
nobs(LopeUrb)
nobs(LopeFull)

nobs(LopeCityNull)
nobs(LopeCity)
nobs(LopeCityUrb)
nobs(LopeCityFull)

nobs(SolveNull)
nobs(Solve)
nobs(SolveUrb)
nobs(SolveFull)

nobs(SolveCityNull)
nobs(SolveCity)
nobs(SolveCityUrb)
nobs(SolveCityFull)

#Looks good

#5 Compare these dudes---------------------------------------------------------
sub_mod_guide <- data.table(
  response = c("InvUrb", "ConUrb", "LopeUrb", "SolveUrb", "InvCity", "ConCity", "LopeCity", "SolveCity"),
  model_null = list(InvNull, ConNull, LopeNull, SolveNull, InvCityNull, ConCityNull, LopeCityNull, SolveCityNull),
  model_urban = list(InvUrb, ConUrb, LopeUrb, SolveUrb, InvCityUrb, ConCityUrb, LopeCityUrb, SolveCityUrb),
  model_intrinsic = list(Inv, Con, Lope, Solve, InvCity, ConCity, LopeCity, SolveCity),
  model_full = list(InvFull, ConFull, LopeFull, SolveFull, InvCityFull, ConCityFull, LopeCityFull, SolveCityFull)
)

#Make function, basically just what I've done before
extract_metrics <- function(model, model_name, response_name) {
  r2 <- tryCatch(performance::r2_nakagawa(model), error = function(e) list(R2_m = NA, R2_c = NA))
  
  data.table(
    response = response_name,
    model_type = model_name,
    AIC = AIC(model),
    logLik = logLik(model),
    R2_marginal = r2$R2_m,
    R2_conditional = r2$R2_c
  )
}

# Loop over each row, not column
comparison_table <- rbindlist(
  lapply(1:nrow(sub_mod_guide), function(i) {
    row <- sub_mod_guide[i]
    response_name <- row$response
    mods <- row[, .(model_null, model_urban, model_intrinsic, model_full)]
    model_names <- names(mods)
    
    rbindlist(lapply(model_names, function(modname) {
      extract_metrics(mods[[modname]][[1]], modname, response_name)
    }))
  })
)

comparison_table
#write.csv(comparison_table, "figures/AllModelComparisonTable.csv")

#I think it would be really good to compare FULL and URBAN using LRT
#Also FULL vs. Intrinsic using LRT
extract_lrt <- function(model1, model2) {
  tryCatch({
    lr <- anova(model1, model2)
    data.table(
      chi_sq = lr$Chisq[2],
      df = lr$Df[2],
      p_lrt = lr$`Pr(>Chisq)`[2]
    )
  }, error = function(e) {
    message("LRT error: ", e$message)
    data.table(chi_sq = NA_real_, df = NA_integer_, p_lrt = NA_real_)
  })
}

sub_mod_guide[, lrt_urb_vs_full := Map(extract_lrt, model_urban, model_full)]
sub_mod_guide[, lr1t_int_vs_full := Map(extract_lrt, model_intrinsic, model_full)]
sub_mod_guide[, lr1t_int_vs_urb := Map(extract_lrt, model_intrinsic, model_urban)]

lrt_results <- rbindlist(list(
  data.table(response = sub_mod_guide$response, comparison = "urban_vs_full", rbindlist(sub_mod_guide$lrt_urb_vs_full)),
  data.table(response = sub_mod_guide$response, comparison = "intrinsic_vs_full", rbindlist(sub_mod_guide$lr1t_int_vs_full)),
  data.table(response = sub_mod_guide$response, comparison = "intrinsic_vs_urban", rbindlist(sub_mod_guide$lr1t_int_vs_urb))
))

lrt_results
#write.csv(lrt_results, "figures/AllModelLRTResults.csv")
