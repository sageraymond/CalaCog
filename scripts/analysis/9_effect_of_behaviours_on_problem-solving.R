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

# 0. Load data --------------------------------------------------
dat <- readRDS("data/EventDataJul2025.Rds")
dat

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
# 1. T
str(dat)
dat$Nat50_scaled <- scale(dat$Nat50, scale = TRUE, center = TRUE)
dat$Inv_duration_scaled <- scale(dat$Inv_duration, scale = TRUE, center = TRUE)
dat$Contact_duration_scaled <- scale(dat$Contact_duration, scale = TRUE, center = TRUE)
dat$Behav_Complexity_scaled <- scale(dat$Behav_Complexity, scale = TRUE, center = TRUE)


#Reformat data so that there is one row per event, summrising all the info
dat[, event_id := seq(1:.N)]
dat$V1 <- NULL
dat$EventID_Year_Subject <- NULL
dat$EventID_Year <- NULL
dat$Year <- NULL
dat$Light <- NULL
dat$GroupSize <- NULL
dat$urbanization_score <- NULL
dat$Sex <- NULL
dat$DateTime <- NULL
dat$temp <- NULL
dat$SiteSequence <- NULL
dat$Disease <- NULL
dat$Lat <- NULL
dat$Long <- NULL

#reformat so there is one row per event

dat_all <- dat
dat_city <- dat %>% dplyr::filter(urbanization == "City")
remove(dat)

#For dat_all only, change up the urb metric
str(dat_all)
dat_all$Nat50[is.na(dat_all$Nat50)] <- 0
dat_all$Nat50_scaled[is.na(dat_all$Nat50_scaled)] <- 0



#Make model guide--I'm afraid I have to
guide <- CJ(rand_ef = c("(1|SiteID)", "(1|SiteID/Subject)"),
            vars = c("Inv_duration", "Contact_duration", "Lope", "Behav_Complexity"),
            scaled = c("yes", "no"),
            extent = c("all", "city"))
#Note, i've left contact and inv out because you can't get a solve with those being 0, so that would mess models up

guide$response <- "Solves"

#add dat
guide[extent == "all", dat := "dat_all"]
guide[extent == "city", dat := "dat_city"]


#Modify the vars column based on scaling
guide$vars_final <- guide$vars

guide[scaled == "yes" & vars == "Behav_Complexity", vars_final := "Behav_Complexity_scaled"]
guide[scaled == "yes" & vars == "Inv_duration", vars_final := "Inv_duration_scaled"]
guide[scaled == "yes" & vars == "Contact_duration", vars_final := "Contact_duration_scaled"]


guide[scaled == "yes", urb := "Nat50_scaled"]
guide[scaled == "no", urb := "Nat50"]


#Create null model formulas
guide[, behav_formula := paste(response, "~", vars_final)]

#Create urb formulas
guide[, urb_formula := paste(response, "~", vars_final, "+", urb)]
guide


guide[, model_complexity_comparison_ID := paste0("model_complexity_id_",
                                                 seq(1:.N))]



# >>> Make the data long --------------------------------------------------
guide.long <- melt(guide,
                   measure.vars = c("behav_formula",
                                    "urb_formula"),
                   variable.name = "model_type",
                   value.name = "formula")

guide.long

# >>> Add an individual model ID ------------------------------------------

guide.long[, model_id := paste0("model_", seq(1:.N))]
guide.long

#add model path
guide.long[, model_path := paste0("outputs/behav_models/", model_id, ".Rds")]
guide.long

#specify dataset
guide.long[, comparison_data := NA_character_]

guide.long[model_type == "behav_formula",
           comparison_data := paste0(model_complexity_comparison_ID)]

guide.long[model_type == "urb_formula",
           comparison_data := paste0(model_complexity_comparison_ID)]


# >>> Create  executable call in guide -----------------------------
guide.long[, model_call := paste0("glmmTMB(", 
                                  formula, " + ",
                                  rand_ef, 
                                  ", family= binomial(link = 'logit'), ", 
                                  "data =", dat,
                                  ")")]
guide.long

#make the datasets
# for(i in unique(guide.long$model_complexity_comparison_ID)){
#   
#   this <- guide.long[model_complexity_comparison_ID == i]
#   
#   predictor <- this[model_type=="behav_formula", vars]
#   dataset <- this$dat[1]
#   
#   analysis_dat <- get(dataset)
#   analysis_dat <- analysis_dat[!is.na(analysis_dat[[predictor]]),]
#   
#   assign(
#     paste0("analysis_", i),
#     analysis_dat
#   )
# }
# 

#saveRDS(guide.long, "builds/model_guide_behav_effects.Rds")



#now run these models. 
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


#everybody ran!! Yay

#Now extract some info--start with AIC compairosn and LRT
#back up guide
master_guide <- guide.long


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

#remove sensitvity analysis dudes
table <- master_guide_wide.mrg[rand_ef != "(1|SiteID/Subject)"]

#only need one of scaled or unscaled
table <- table[scaled == "yes"]

#I am also going to limit to whole extent
table <- table[extent == "all"]


table <- as.data.frame(table)
table <- table %>%
  dplyr::select(vars, behav_urb_chisq, behav_urb_p, AIC_behav, AIC_urb)



#I would like to add perfromance metrics to this table
perf <- guide.1.mrg
#remove sensitvity analysis dudes
perf <- perf[rand_ef != "(1|SiteID/Subject)"]

#only need one of scaled or unscaled
perf <- perf[scaled == "yes"]

#I am also going to limit to whole extent
perf <- perf[extent == "all"]

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

#write.csv(perf_all, file = "figures/Table3_trait_urb_model_comp.csv")



#Now we want to get the coefficients. this will be for the SI. 

coefficients
coefficients <- as.data.frame(coefficients)
coefficients <- coefficients %>% dplyr::filter(rand_ef == "(1|SiteID)") %>%
  dplyr::filter(extent == "all") %>%
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

write.csv(coef_all, file = "figures/TableS7_behav_urb_coefficients.csv")
