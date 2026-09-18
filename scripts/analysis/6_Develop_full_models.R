
rm(list = ls())
gc()


#Load libraries

#load lib
library(metafor)
library(broom)
library(data.table)
library(ggplot2)
library(tidyr)
library(multcomp)
library(dplyr)
library(glmmTMB)
library(cpp11)
library(withr)
library(colorspace)
library(mvtnorm)
library(foreach)
library(doSNOW)
library(performance)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 0. Load data --------------------------------------------------
dat <- readRDS("data/EventDataJul2025.Rds")
dat

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
# 1. The goal here is to explroe intrinsic and extrinsic factors... see what's important
#Start by adding scaled variable for eveyrhting as necessary
str(dat)
dat$Year <- factor(dat$Year, levels = c(2024, 2025))
dat$GroupSize_scaled <- scale(dat$GroupSize, scale = TRUE, center = TRUE)
dat$temp_scaled <- scale(dat$temp, scale = TRUE, center = TRUE)
dat$Nat50_scaled <- scale(dat$Nat50, scale = TRUE, center = TRUE)
dat$SiteSequence_scaled <- scale(dat$SiteSequence, scale = TRUE, center = TRUE)



#read in info for null models, urb only models, and intr/ contextual models
int <- fread("figures/TableS3_hyp2_model_performance.csv")
int$bind <- paste0(int$Response, int$Dataset)


int <- int %>% 
  dplyr::select(Response, Dataset, N, `Null AIC`, `Top. AIC`, `Marg. R2`, `Cond. R2`, bind) %>%
  dplyr::rename("Int_cont_AIC" = `Top. AIC`,
                "Int_cont_Marg" = `Marg. R2`,
                "Int_cont_cond" = `Cond. R2`)


urb <- fread("figures/TableS2_urb_only_model_results.csv")
urb$bind <- paste0(urb$Response, urb$Extent)

urb <- urb %>% 
  dplyr::select(bind, `Urb. AIC`, `Marg. R2`, `Cond. R2`) %>%
  dplyr::rename("Urb_AIC" = `Urb. AIC`,
                "Urb_Marg" = `Marg. R2`,
                "Urb_cond" = `Cond. R2`)


#Bind these guys
comparison <- full_join(int, urb, by = "bind")


#Great. I guess I need R2 values for null models, which feels a bit silly, but
#so be it
master_guide <- readRDS("builds/model_guide_main.Rds")

str(master_guide)


#limit to null models and scaled (doesn't matter either way)
master_guide <- master_guide[subject_id == "no"]
master_guide <- master_guide[var != "Nat50"]
master_guide <- master_guide[model_type == "null_model"]

#Good--this is 14 things

#Now make a function to read in the models and extract the R2 metrics
#Make a function for model performance only
perform_function <- function(m) {
  
  # Extract model-level stats
  n_obs <- nobs(m)
  aic_val <- AIC(m)
  
  # Try to get R² values
  r2_vals <- tryCatch({
    r2(m)
  }, error = function(e) NULL)
  
  # Handle successful and failed R2 calculations
  if (is.list(r2_vals) && 
      "R2_marginal" %in% names(r2_vals) &&
      "R2_conditional" %in% names(r2_vals)) {
    
    R2_marginal <- as.numeric(r2_vals$R2_marginal[1])
    R2_conditional <- as.numeric(r2_vals$R2_conditional[1])
    
  } else {
    
    R2_marginal <- NA_real_
    R2_conditional <- NA_real_
    
  }
  
  # Output
  out <- data.table(
    n_obs = n_obs,
    AIC = aic_val,
    R2_marginal = R2_marginal,
    R2_conditional = R2_conditional
  )
  
  return(out)
}

#Apply to urb models
ms <- lapply(master_guide$model_path,
             FUN=readRDS)
ms.tidy <- lapply(ms,
                  FUN=perform_function)
ms.tidy
names(ms.tidy) <- master_guide$model_id

ms.tidy <- rbindlist(ms.tidy, idcol = "model_id", fill = TRUE)
ms.tidy


#Inrtegrate into model guide
null_performance <- merge(master_guide,
                                 ms.tidy,
                                 by = "model_id",
                                 all.x = T)


#Looks good. save this and bind to other guy
null_performance <- as.data.frame(null_performance)
null_performance <- null_performance %>%
  dplyr::select(R2_marginal, R2_conditional, response, dat) %>%
  dplyr::rename("Null_Marg_R2" = R2_marginal,
                "Null_Cond_R2" = R2_conditional,
                "Response" = response,
                "Datset" = dat)

setDT(null_performance)

null_performance[, Response := fcase(Response == "Behav_Complexity", "Behavioural diversity",
                               Response == "Contact_duration", "Contact duration",
                               Response == "Inv", "Investigate",
                               Response == "Inv_duration", "Investigate duration",
                               Response == "Lope", "Escape gait",
                               Response == "Solves", "Solution",
                               default = Response)]

null_performance[, Datset := fcase(Datset == "dat_city", "City",
                             Datset == "dat_all", "All",
                             default = Datset)]


null_performance$bind <- paste0(null_performance$Response, null_performance$Datset)
null_performance$Response <- NULL
null_performance$Datset <- NULL

null_performance <- as.data.frame(null_performance)

comparison.1 <- left_join(comparison, null_performance, by = "bind")



#xlean things up a bit before making your full models
remove(comparison)
remove(int)
remove(urb)
remove(null_performance)
remove(ms)
remove(ms.tidy)
remove(master_guide)



#read in predictor list
preds <- fread("data/predictors_for_full_models.csv")
preds$bind <- paste0(preds$Response, preds$Extent)
preds <- preds %>% dplyr::rename("int_con_preds" = terms)


guide <- CJ(response = c("Contact_duration", "Inv_duration", "Behav_Complexity", "Lope", "Solves",
                            "Contact", "Inv"),
               dat = c("dat_all", "dat_city"),
               subject_id = c("yes", "no"),
               scaled = c("yes", "no"))

#Amake bind columns
guide$bind <- paste0(guide$response, guide$dat)

guide.1 <- left_join(guide, preds, by = "bind")
guide.1$Extent <- NULL
guide.1$bind <- NULL

guide.1[scaled == "yes" & dat == "dat_city", urb_term := "Nat50_scaled"]
guide.1[scaled == "no" & dat == "dat_city", urb_term := "Nat50"]
guide.1[dat == "dat_all", urb_term := "urbanization"]

#this will be hella clunky, but i'm doing it
guide.1$int_con_terms <- "XXX"

guide.1[scaled == "yes" & dat == "dat_city" & int_con_preds == "Light + Disease + PuzzleType + Year + Sex + temp", int_con_terms := "Light + Disease + PuzzleType + Year + Sex + temp_scaled"]
guide.1[scaled == "yes" & dat == "dat_city" & int_con_preds == "Disease + PuzzleType + Year + SiteSequence + Sex", int_con_terms := "Disease + PuzzleType + Year + SiteSequence_scaled + Sex"]
guide.1[scaled == "yes" & dat == "dat_city" & int_con_preds == "GroupSize + temp", int_con_terms := "GroupSize + temp_scaled"]
guide.1[scaled == "yes" & dat == "dat_city" & int_con_preds == "Light + SiteSequence + Sex + temp", int_con_terms := "Light + SiteSequence + Sex + temp_scaled"]
guide.1[scaled == "yes" & dat == "dat_city" & int_con_preds == "GroupSize + PuzzleType + SiteSequence + Sex + temp", int_con_terms := "GroupSize_scaled + PuzzleType + SiteSequence_scaled + Sex + temp_scaled"]
guide.1[scaled == "yes" & dat == "dat_city" & int_con_preds == "Light + Sex + temp", int_con_terms := "Light + Sex + temp_scaled"]
guide.1[scaled == "yes" & dat == "dat_city" & int_con_preds == "Light + temp", int_con_terms := "Light + temp_scaled"]


guide.1[int_con_terms == "XXX", int_con_terms := int_con_preds]

guide.1$int_con_preds <- NULL


#Add family
guide.1[response == "Contact_duration", model_family := "lognormal()"]
guide.1[response == "Inv_duration", model_family := "lognormal()"]
guide.1[response == "Behav_Complexity", model_family := "poisson()"]
guide.1[response == "Lope", model_family := "binomial(link = 'logit')"]
guide.1[response == "Solves", model_family := "binomial(link = 'logit')"]
guide.1[response == "Contact", model_family := "binomial(link = 'logit')"]
guide.1[response == "Inv", model_family := "binomial(link = 'logit')"]


#add random effect
guide.1[subject_id == "yes", rand_ef := "(1|SiteID/Subject)"]
guide.1[subject_id == "no", rand_ef := "(1|SiteID)"]



# make model call
guide.1[, model_call := paste0("glmmTMB(", response, 
                               "~",
                                           urb_term,
                                           " + ",
                                           int_con_terms, 
                                           " + ",
                               rand_ef,
                               ", family = ",
                                           model_family,
                                           ", data = ",
                                           dat,
                                           ")")]


#Add an ID
guide.1[, model_id := paste0("model_", seq(1:.N))]

#Add a model path
guide.1[, model_path := paste0("outputs/full_models/", model_id, ".Rds")]



#run these dudes
dat_all <- dat
dat_city <- dat %>% dplyr::filter(urbanization == "City")
remove(dat)


#FIRST SET NA OPTION
options(na.action = "na.omit")   # or "na.exclude"

fit_and_save <- function(model_call, model_path) {
  
  message("Running: ", model_path)
  
  # Convert string to expression and run model
  model <- eval(parse(text = model_call))
  
  # Save fitted model
  saveRDS(model, file = model_path)
  
  return(TRUE)
}

# Run for all the models  models
guide.1[, success := mapply(
  fit_and_save,
  model_call,
  model_path
)]



#OK great. they all ran

#Now my goals are going to be (1) extract AIC and performance metrics-----------
#Make a function for model performance only
perform_function <- function(m) {
  
  # Extract model-level stats
  n_obs <- nobs(m)
  aic_val <- AIC(m)
  
  # Try to get R² values
  r2_vals <- tryCatch({
    r2(m)
  }, error = function(e) NULL)
  
  # Handle successful and failed R2 calculations
  if (is.list(r2_vals) && 
      "R2_marginal" %in% names(r2_vals) &&
      "R2_conditional" %in% names(r2_vals)) {
    
    R2_marginal <- as.numeric(r2_vals$R2_marginal[1])
    R2_conditional <- as.numeric(r2_vals$R2_conditional[1])
    
  } else {
    
    R2_marginal <- NA_real_
    R2_conditional <- NA_real_
    
  }
  
  # Output
  out <- data.table(
    n_obs = n_obs,
    AIC = aic_val,
    R2_marginal = R2_marginal,
    R2_conditional = R2_conditional
  )
  
  return(out)
}

#Apply to urb models
ms <- lapply(guide.1$model_path,
             FUN=readRDS)
ms.tidy <- lapply(ms,
                  FUN=perform_function)
ms.tidy
names(ms.tidy) <- guide.1$model_id

ms.tidy <- rbindlist(ms.tidy, idcol = "model_id", fill = TRUE)
ms.tidy


#Inrtegrate into model guide
guide.1.mrg <- merge(guide.1,
                          ms.tidy,
                          by = "model_id",
                          all.x = T)


#Now bind to the comparison 1 guy
guide.1.mrg.1 <- guide.1.mrg[subject_id == "no"]
guide.1.mrg.1 <- guide.1.mrg.1[scaled == "yes"]

guide.1.mrg.1[, dat := fcase(dat == "dat_city", "City",
                                   dat == "dat_all", "All",
                                   default = dat)]

guide.1.mrg.1[, response := fcase(response == "Behav_Complexity", "Behavioural diversity",
                                     response == "Contact_duration", "Contact duration",
                                     response == "Inv", "Investigate",
                                     response == "Inv_duration", "Investigate duration",
                                     response == "Lope", "Escape gait",
                                     response == "Solves", "Solution",
                                     default = response)]

guide.1.mrg.1$bind <- paste0(guide.1.mrg.1$response, guide.1.mrg.1$dat)

guide.1.mrg.1 <- as.data.frame(guide.1.mrg.1)
guide.1.mrg.1 <- guide.1.mrg.1 %>%
  dplyr::select(bind, AIC, R2_marginal, R2_conditional) %>%
  dplyr::rename("Full_AIC" = AIC,
                "Full_R2_marg" = R2_marginal,
                "Full_R2_cond" = R2_conditional)

guide.1.mrg.1
comparison.1

#save guide for later use
saveRDS(guide.1.mrg, file = "outputs/final_guide_with_metrics.Rds")

comparison_final <- full_join(comparison.1, guide.1.mrg.1, by = "bind")

#OK, now clean this up
comparison_final <- comparison_final %>%
  dplyr::select(-(bind))

comparison_final <- comparison_final %>%
  dplyr::rename("AIC_null" = `Null AIC`,
                "AIC_cont" = Int_cont_AIC,
                "Marg_cont" = Int_cont_Marg,
                "Cond_cont" = Int_cont_cond,
                "AIC_urb" = Urb_AIC,
                "Marg_urb" = Urb_Marg,
                "Cond_urb" = Urb_cond,
                "Marg_null" = Null_Marg_R2,
                "Cond_null" = Null_Cond_R2,
                "AIC_full" = Full_AIC,
                "Marg_full" = Full_R2_marg,
                "Cond_full" = Full_R2_cond)

#reformat by making long
comparison_long <- comparison_final %>%
  pivot_longer(
    cols = -c(Response, Dataset, N),
    names_to = c(".value", "Model"),
    names_sep = "_"
  ) %>%
  rename(
    AIC = AIC,
    R2_marginal = Marg,
    R2_conditional = Cond
  ) %>%
  relocate(Response, Dataset, Model, N, AIC, R2_marginal, R2_conditional)

comparison_long


setDT(comparison_long)

comparison_long$order <- "XXX"
comparison_long[, order := fcase(Response == "Behavioural diversity", "2",
                            Response == "Contact duration", "6",
                            Response == "Investigate", "3",
                            Response == "Contact", "5",
                            Response == "Investigate duration", "4",
                            Response == "Escape gait", "7",
                            Response == "Solution", "1",
                            default = order)]

#now save this guy
write.csv(comparison_long, file = "figures/Table2_final_model_comparison.csv")



#Now get out coefficients, like usual

#Now get coefficients
#make a function
get_coefficients <- function(m) {
  
  tidy_df <- broom::tidy(
    m, effects = "fixed", conf.int = TRUE)
  setDT(tidy_df)
  
  
  tidy_df |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::select(
      term,
      beta = estimate,
      p = p.value,
      lwr = conf.low,
      upr = conf.high
    )
}

#Apply to focal models
remove(ms)
remove(ms.tidy)

ms <- lapply(guide.1.mrg$model_path,
             FUN=readRDS)
ms.tidy <- lapply(ms,
                  FUN=get_coefficients)
ms.tidy
names(ms.tidy) <- guide.1.mrg$model_id

ms.tidy <- rbindlist(ms.tidy, idcol = "model_id", fill = TRUE)
ms.tidy


#Inrtegrate into model guide
coefficients <-  merge(ms.tidy,
                       guide.1.mrg,
                       by = "model_id",
                       all.x = T)


#Clean this up


#make coefficient table
str(coefficients)

coefficients <- coefficients %>%
  dplyr::select(term,
                beta,
                p,
                lwr,
                upr,
                response, 
                dat,
                scaled,
                rand_ef,
                n_obs) %>%
  dplyr::rename("Response" = response,
                "Extent" = dat,
                "Term" = term,
                "B" = beta,
                "P" = p,
                "Low CI" = lwr,
                "Upp. CI" = upr)

unique(coefficients$rand_ef)

coefficients <- coefficients %>%
  dplyr::filter(rand_ef != "(1|SiteID/Subject)") %>%
  dplyr::select(-(rand_ef))


coefficients$bind <- paste0(coefficients$Response, coefficients$Extent, coefficients$Term)


#Split into scaled and unscaled
unscaled <- coefficients %>% dplyr::filter(scaled == "no")
scaled <- coefficients %>% dplyr::filter(scaled == "yes")


#Calvculate effect size using unscaled
unscaled <- unscaled %>%
  dplyr::mutate(ES = exp(B),
                ES_low_CI = exp(`Low CI`),
                ES_upp_CI = exp(`Upp. CI`))
unscaled <- unscaled %>%
  dplyr::select(bind, ES, ES_low_CI, ES_upp_CI)

#unscaled has effect sized for everyone
#scaled has everything else

#We can now remove unscaled from  scaled
scaled <- scaled %>%
  dplyr::select(-(scaled))

#now we should be able to bind
#BUT THERE IS ONE PROBLEM
#right now, the bind column contained "_scaled" for scaled variables
#and this is not the truth for the usncaled
#so the bind wont work unless i first take out the term scaled throughout
scaled.1 <- scaled
scaled.1$bind <- gsub("_scaled", "", scaled.1$bind, fixed = TRUE)


final_coef <- left_join(scaled.1, unscaled, by = "bind")
final_coef <- final_coef %>% dplyr::select(-(bind))

final_coef <- final_coef %>%
  dplyr::rename("ES Low CI" = ES_low_CI,
                "ES Upp. CI" = ES_upp_CI,
                "N" = n_obs)


setDT(final_coef)

final_coef$order <- "XXX"
final_coef[, order := fcase(Response == "Behav_Complexity", "2",
                            Response == "Contact_duration", "6",
                            Response == "Inv", "3",
                            Response == "Contact", "5",
                            Response == "Inv_duration", "4",
                            Response == "Lope", "7",
                            Response == "Solves", "1",
                            default = order)]

final_coef[, Response := fcase(Response == "Behav_Complexity", "Behavioural diversity",
                               Response == "Contact_duration", "Contact duration",
                               Response == "Inv", "Investigate",
                               Response == "Inv_duration", "Investigate duration",
                               Response == "Lope", "Escape gait",
                               Response == "Solves", "Solution",
                               default = Response)]

final_coef[, Extent := fcase(Extent == "dat_city", "City",
                             Extent == "dat_all", "All",
                             default = Extent)]

unique(final_coef$Term)
final_coef[, Term := fcase(Term == "GroupSize_scaled", "Group size",
                           Term == "temp_scaled", "Temperature",
                           Term == "LightD", "Darkness",
                           Term == "SexF", "Sex (F)",
                           Term == "SexM", "Sex (M)",
                           Term == "SiteSequence", "Sequence",
                           Term == "PuzzleTypeA", "Plastic Puzzle",
                           Term == "Disease1", "Diseased",
                           Term == "Year2025", "Second study year",
                           Term == "urbanizationCity", "City (vs. Wild)",
                           Term == "Nat50", "Urbanization (%)",
                           Term == "Nat50_scaled", "Urbanization (%)",
                           Term == "SiteSequence_scaled", "Sequence",
                           default = Term)]
#save this
#write.csv(final_coef, file = "figures/TableS5_hyp3_model_coefficients.csv")


