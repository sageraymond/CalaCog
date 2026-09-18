
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

# 0. Load data
dat <- readRDS("data/EventDataJul2025.Rds")
dat

#Make it nice
str(dat)
dat$Year <- factor(dat$Year, levels = c(2024, 2025))
dat$GroupSize_scaled <- scale(dat$GroupSize, scale = TRUE, center = TRUE)
dat$temp_scaled <- scale(dat$temp, scale = TRUE, center = TRUE)
dat$Nat50_scaled <- scale(dat$Nat50, scale = TRUE, center = TRUE)
dat$SiteSequence_scaled <- scale(dat$SiteSequence, scale = TRUE, center = TRUE)

#I will make a model guide
#In the end I will print AIC and such
#The idea is to keep this as simple as possible, while corroboratins ressults
guide <- CJ(response = c("Contact_duration", "Inv_duration", "Behav_Complexity", "Lope", "Solves",
                            "Contact", "Inv"),
               dat = c("dat_all", "dat_city"))



#Create NULL formulas
guide[, null_model_formula := paste(response, "~ 1 + (1|SiteID/Subject)")]

#Create urb formulas
guide[dat == "dat_all", urb_formula := paste(response, "~ urbanization + (1|SiteID/Subject)")]
guide[dat == "dat_city", urb_formula := paste(response, "~ Nat50_scaled + (1|SiteID/Subject)")]

guide[dat == "dat_all" & response == "Solves", int_formula := paste(response, "~ Sex + Light + temp_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_all" & response == "Behav_Complexity", int_formula := paste(response, "~ Light + (1|SiteID/Subject)")]
guide[dat == "dat_all" & response == "Contact", int_formula := paste(response, "~ Disease + PuzzleType + Disease + Sex + Year + Light + temp_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_all" & response == "Contact_duration", int_formula := paste(response, "~ Light + Disease + (1|SiteID/Subject)")]
guide[dat == "dat_all" & response == "Inv", int_formula := paste(response, "~ Sex + PuzzleType + Light + Disease + Year + SiteSequence_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_all" & response == "Inv_duration", int_formula := paste(response, "~ temp_scaled + GroupSize_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_all" & response == "Lope", int_formula := paste(response, "~ Sex + Light + temp_scaled + SiteSequence_scaled + (1|SiteID/Subject)")]

guide[dat == "dat_city" & response == "Solves", int_formula := paste(response, "~ Light + temp_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_city" & response == "Behav_Complexity", int_formula := paste(response, "~ Light + (1|SiteID/Subject)")]
guide[dat == "dat_city" & response == "Contact", int_formula := paste(response, "~ Sex + Disease + PuzzleType + Year + Light + temp_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_city" & response == "Contact_duration", int_formula := paste(response, "~ 1 + (1|SiteID/Subject)")]
guide[dat == "dat_city" & response == "Inv", int_formula := paste(response, "~ Sex + PuzzleType + Year + Disease + SiteSequence_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_city" & response == "Inv_duration", int_formula := paste(response, "~ GroupSize_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_city" & response == "Lope", int_formula := paste(response, "~ Sex + PuzzleType + GroupSize_scaled + SiteSequence_scaled + temp_scaled + (1|SiteID/Subject)")]


#Now make the full models
guide[dat == "dat_all" & response == "Solves", full_formula := paste(response, "~ urbanization + Sex + Light + temp_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_all" & response == "Behav_Complexity", full_formula := paste(response, "~ urbanization +  Light + (1|SiteID/Subject)")]
guide[dat == "dat_all" & response == "Contact", full_formula := paste(response, "~ urbanization +  Disease + PuzzleType + Disease + Sex + Year + Light + temp_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_all" & response == "Contact_duration", full_formula := paste(response, "~ urbanization +  Light + Disease + (1|SiteID/Subject)")]
guide[dat == "dat_all" & response == "Inv", full_formula := paste(response, "~ urbanization +  Sex + PuzzleType + Light + Disease + Year + SiteSequence_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_all" & response == "Inv_duration", full_formula := paste(response, "~ urbanization +  temp_scaled + GroupSize_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_all" & response == "Lope", full_formula := paste(response, "~ urbanization +  Sex + Light + temp_scaled + SiteSequence_scaled + (1|SiteID/Subject)")]

guide[dat == "dat_city" & response == "Solves", full_formula := paste(response, "~ Nat50_scaled +  Light + temp_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_city" & response == "Behav_Complexity", full_formula := paste(response, "~ Nat50_scaled +  Light + (1|SiteID/Subject)")]
guide[dat == "dat_city" & response == "Contact", full_formula := paste(response, "~ Nat50_scaled +  Sex + Disease + PuzzleType + Year + Light + temp_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_city" & response == "Contact_duration", full_formula := paste(response, "~ Nat50_scaled +  1 + (1|SiteID/Subject)")]
guide[dat == "dat_city" & response == "Inv", full_formula := paste(response, "~ Nat50_scaled +  Sex + PuzzleType + Year + Disease + SiteSequence_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_city" & response == "Inv_duration", full_formula := paste(response, "~ Nat50_scaled +  GroupSize_scaled + (1|SiteID/Subject)")]
guide[dat == "dat_city" & response == "Lope", full_formula := paste(response, "~ Nat50_scaled +  Sex + PuzzleType + GroupSize_scaled + SiteSequence_scaled + temp_scaled + (1|SiteID/Subject)")]



guide[response == "Contact_duration", model_family := "lognormal()"]
guide[response == "Inv_duration", model_family := "lognormal()"]
guide[response == "Behav_Complexity", model_family := "poisson()"]
guide[response == "Lope", model_family := "binomial(link = 'logit')"]
guide[response == "Solves", model_family := "binomial(link = 'logit')"]
guide[response == "Contact", model_family := "binomial(link = 'logit')"]
guide[response == "Inv", model_family := "binomial(link = 'logit')"]


#nmake comparison ID
guide[, model_complexity_comparison_ID := paste0("model_complexity_id_",
                                                 seq(1:.N))]




# >>> Make the data long --------------------------------------------------
guide.long <- melt(guide,
                   measure.vars = c("null_model_formula",
                                    "urb_formula",
                                    "int_formula",
                                    "full_formula"),
                   variable.name = "model_type",
                   value.name = "formula")

guide.long



# >>> Add an individual model ID ------------------------------------------

guide.long[, model_id := paste0("model_", seq(1:.N))]
guide.long

guide.long[, model_path := paste0("outputs/sensitivity_analysis_models/", model_id, ".Rds")]
guide.long


# >>> Create  executable call in guide -----------------------------
guide.long[, model_call := paste0("glmmTMB(", 
                                  formula, ", ",
                                  "family=", model_family, ", ",
                                  "data = ",
                                  dat,
                                  ")")]
guide.long

guide.long[1, ]$model_call
# eval(parse(text = guide.long[1, ]$model_call))

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
#  Save guide ------------------------------------
saveRDS(guide.long, "outputs/model_guide_sensitvity_analysis.Rds")





#Now i want to run all the models
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
guide.long[, success := mapply(
  fit_and_save,
  model_call,
  model_path
)]

#OK. everybody went


#Now use your long guide to pull out the AIC and marg and cond random effects for all these dudes

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

#Apply to all models
ms <- lapply(guide.long$model_path,
             FUN=readRDS)
ms.tidy <- lapply(ms,
                  FUN=perform_function)
ms.tidy
names(ms.tidy) <- guide.long$model_id

ms.tidy <- rbindlist(ms.tidy, idcol = "model_id", fill = TRUE)
ms.tidy



#OK now integrate into guide
guide.long.2 <- merge(guide.long,
                                 ms.tidy,
                                 by = "model_id",
                                 all.x = T)

guide.long.2
guide.long.2 <- as.data.frame(guide.long.2)
guide.long.2 <- guide.long.2 %>%
  dplyr::select(n_obs, AIC, R2_marginal, R2_conditional, model_type, response, dat) %>%
  dplyr::rename("N" = n_obs,
                "Model" = model_type,
                "Response" = response,
                "Extent" = dat)

setDT(guide.long.2)
guide.long.2$order <- "XXX"
guide.long.2[, order := fcase(Response == "Behav_Complexity", "2",
                            Response == "Contact_duration", "6",
                            Response == "Inv", "3",
                            Response == "Contact", "5",
                            Response == "Inv_duration", "4",
                            Response == "Lope", "7",
                            Response == "Solves", "1",
                            default = order)]

guide.long.2[, Response := fcase(Response == "Behav_Complexity", "Behavioural diversity",
                               Response == "Contact_duration", "Contact duration",
                               Response == "Inv", "Investigate",
                               Response == "Inv_duration", "Investigate duration",
                               Response == "Lope", "Escape gait",
                               Response == "Solves", "Solution",
                               default = Response)]

guide.long.2[, Extent := fcase(Extent == "dat_city", "City",
                             Extent == "dat_all", "All",
                             default = Extent)]

guide.long.2[, Model := fcase(Model == "null_model_formula", "Null",
                               Model == "urb_formula", "Urb. only",
                              Model == "int_formula", "Int. + Context",
                                Model == "full_formula", "Full",
                              
                               default = Model)]

#save this
write.csv(guide.long.2, file = "figures/TableS6_sensitvity_analysis_results.csv")




#OK. I also need to do a sensitvity analysis for hypothesis 4 stuff. 

#I already built these models. Load the model guide

guide <- readRDS("builds/model_guide_behav_effects.Rds")

#back up guide
master_guide <- guide


#cast wide by model type
master_guide_wide <- dcast(master_guide,
                           ... ~ model_type,
                           value.var = c("model_call", "model_path", "model_id",
                                         "formula"))


# >>> Compare null and urb------------------------------------------------------
comps <- list()  # make list

for (i in 1:nrow(master_guide_wide)) {
  
  model_path_behav_model  <- master_guide_wide[i, model_path_behav_formula]
  model_path_urb <- master_guide_wide[i, model_path_urb_formula]
  model_id <- master_guide_wide[i, model_complexity_comparison_ID]
  
  # Skip if model paths are missing or file(s) don't exist
  # if (is.na(model_path_urb_time_formula) || is.na(model_path_urb_cross_time_formula)) next
  # if (!file.exists(model_path_urb_time_formula) || !file.exists(model_path_urb_cross_time_formula)) next
  
  # Read models
  m1 <- readRDS(model_path_behav_model)
  m2 <- readRDS(model_path_urb)
  
  # Compare  models
  out <- anova(m1, m2)
  out2 <- AIC(m1)
  out3 <- AIC(m2)
  
  # Store comparison
  comps[[i]] <- data.table(
    model_complexity_comparison_ID = model_id,
    behav_urb_chisq = out$Chisq[2],
    behav_urb_p = out$`Pr(>Chisq)`[2],
    AIC_behav = out2,
    AIC_urb = out3
  )
  
  cat(i, "/", nrow(master_guide_wide), "\r")
}

# Combine all results into one data.table
comps.dt <- rbindlist(comps, fill = TRUE)

# where did comparison fail?
comps.dt[is.na(null_urb_chisq)] # everybody went!!
# Ideally 0 rows


# >>> Merge into master guide wide ----------------------------------------

master_guide_wide.mrg <- merge(master_guide_wide,
                               comps.dt,
                               by = "model_complexity_comparison_ID",
                               all.x = T)
master_guide_wide.mrg
master_guide_wide.mrg$urb_behav_improves <- ifelse(master_guide_wide.mrg$behav_urb_p < 0.05, "YES", "NO")



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
guide.1.mrg <- merge(master_guide,
                     ms.tidy,
                     by = "model_id",
                     all.x = T)



#OK now get out your coefficients
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

#OK. now all your performance metrics and coefficients are in coefficients
#Your performance metrics are in guide.1.mrg
#And your comparison table is in master_guide_wide.mrg

#Clean each of these up in turn
master_guide_wide.mrg

#select sensitvity analysis dudes
table <- master_guide_wide.mrg[rand_ef == "(1|SiteID/Subject)"]

#only need one of scaled or unscaled
table <- table[scaled == "yes"]

#I am also going to limit to whole extent
table <- table[dat == "dat_all"]


table <- as.data.frame(table)
table <- table %>%
  dplyr::select(vars, behav_urb_chisq, behav_urb_p, AIC_behav, AIC_urb)



#I would like to add perfromance metrics to this table
perf <- guide.1.mrg
#remove sensitvity analysis dudes
perf <- perf[rand_ef == "(1|SiteID/Subject)"]

#only need one of scaled or unscaled
perf <- perf[scaled == "yes"]

#I am also going to limit to whole extent
perf <- perf[dat == "dat_all"]

perf <- as.data.frame(perf)
perf <- perf %>%
  dplyr::select(vars, n_obs, R2_marginal, R2_conditional, model_type)

setDT(perf)

perf_wide <- dcast(vars ~ model_type,
                   value.var = c("n_obs", "R2_marginal", "R2_conditional"),
                   data = perf)
perf_wide

perf_wide <- as.data.frame(perf_wide)

perf_all <- full_join(perf_wide, table, by = "vars")

#OK, now clean this thing up
perf_all <- perf_all %>%
  dplyr::select(-(n_obs_behav_formula)) %>%
  dplyr::rename("Response" = vars,
                "N" = n_obs_urb_formula,
                "Marg. R2 (B)" = R2_marginal_behav_formula,
                "Marg. R2 (U)" = R2_marginal_urb_formula,
                "Cond. R2 (B)" = R2_conditional_behav_formula,
                "Cond. R2 (U)" = R2_conditional_urb_formula,
                "X" = behav_urb_chisq,
                "P" = behav_urb_p,
                "AIC (B)" = AIC_behav,
                "AIC (U)" = AIC_urb )


setDT(perf_all)
perf_all[, Response := fcase(Response == "Behav_Complexity", "Behavioural diversity",
                             Response == "Contact_duration", "Contact duration",
                             Response == "Inv_duration", "Investigate duration",
                             Response == "Lope", "Escape gait",
                             default = Response)]

#Save this puppy

#write.csv(perf_all, file = "figures/Tables9_sens_analysis_2_performance.csv")



#curious to see the coeffcieitns too

coefficients
coefficients <- as.data.frame(coefficients)
coefficients <- coefficients %>% dplyr::filter(rand_ef != "(1|SiteID)") %>%
  dplyr::filter(dat == "dat_all") %>%
  dplyr::filter(model_type == "urb_formula") %>%
  dplyr::select(vars, vars_final, term, beta, p, lwr, upr, scaled, n_obs)

coefficients <- coefficients %>% dplyr::select(-(vars_final))

#we need term to match, regardless of whether it's scaled
coefficients$term <- ifelse(coefficients$term == "Nat50_scaled", "Nat50", coefficients$term)
coefficients$term <- ifelse(coefficients$term == "Behav_Complexity_scaled", "Behav_Complexity", coefficients$term)
coefficients$term <- ifelse(coefficients$term == "Contact_duration_scaled", "Contact_duration", coefficients$term)
coefficients$term <- ifelse(coefficients$term == "Inv_duration_scaled", "Inv_duration", coefficients$term)

unique(coefficients$term)

#Great. now caulcuate effect sizes
coefficients$link <- paste0(coefficients$vars, coefficients$term)

scaled <- coefficients %>% dplyr::filter(scaled == "yes")
unscaled <- coefficients %>% dplyr::filter(scaled == "no")

#calc effect sizes
unscaled <- unscaled %>%
  dplyr::mutate(es = exp(beta),
                es_low_ci = exp(lwr),
                es_upp_ci = exp(upr))

#looks good
#remove things
unscaled <- unscaled %>% dplyr::select(link, es, es_low_ci, es_upp_ci)

scaled <- scaled %>% dplyr::select(-(scaled))

coef_all <- full_join(scaled, unscaled, by = "link")


#clean up
coef_all <- coef_all %>%
  dplyr::rename("Response" = vars,
                "Term" = term,
                "B" = beta,
                "P" = p,
                "Low CI" = lwr,
                "Upp. CI" = upr,
                "N" = n_obs,
                "ES" = es,
                "ES Low CI" = es_low_ci,
                "ES Upp. CI" = es_upp_ci)



setDT(coef_all)

coef_all$link <- NULL

coef_all[, Response := fcase(Response == "Behav_Complexity", "Behavioural diversity",
                             Response == "Contact_duration", "Contact duration",
                             Response == "Inv_duration", "Investigate duration",
                             Response == "Lope", "Escape gait",
                             default = Response)]


coef_all[, Term := fcase(Term == "Behav_Complexity", "Behavioural diversity",
                         Term == "Contact_duration", "Contact duration",
                         Term == "Inv_duration", "Investigate duration",
                         Term == "Lope1", "Escape gait",
                         Term == "Nat50", "Urbanization (%)",
                         default = Term)]

write.csv(coef_all, file = "figures/TableS9_sensitvity_2_coefs.csv")

