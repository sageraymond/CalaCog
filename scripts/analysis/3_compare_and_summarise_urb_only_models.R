#
# AIM: compare models. and create tidy WIDE data frame of each comparison
#
#

rm(list = ls())
gc()

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
library(broom.mixed)
library(performance)

# Load data and prepare workspace -----------------------------------------
master_guide <- readRDS("builds/model_guide_main.Rds")

#Step 1 is to make a table that contains all the model info for each model
#This should include both beta coefficients and such and model performance stuff
#start by doing for non sensitivity analysis only
master_guide <- master_guide[subject_id == "no"]

#cast wide by model type
master_guide_wide <- dcast(master_guide,
                           ... ~ model_type,
                           value.var = c("model_call", "model_path", "model_id",
                                         "formula"))

master_guide_wide[model_complexity_comparison_ID == "model_complexity_id_3"] #this looks good I think

# >>> Compare null and urb------------------------------------------------------
comps <- list()  # make list

for (i in 1:nrow(master_guide_wide)) {
  
  model_path_null_model  <- master_guide_wide[i, model_path_null_model]
  model_path_urb <- master_guide_wide[i, model_path_urb]
  model_id <- master_guide_wide[i, model_complexity_comparison_ID]
  
  # Skip if model paths are missing or file(s) don't exist
  # if (is.na(model_path_urb_time_formula) || is.na(model_path_urb_cross_time_formula)) next
  # if (!file.exists(model_path_urb_time_formula) || !file.exists(model_path_urb_cross_time_formula)) next
  
  # Read models
  m1 <- readRDS(model_path_null_model)
  m2 <- readRDS(model_path_urb)
  
  # Compare  models
  out <- anova(m1, m2)
  out2 <- AIC(m1)
  out3 <- AIC(m2)
  
  # Store comparison
  comps[[i]] <- data.table(
    model_complexity_comparison_ID = model_id,
    null_urb_chisq = out$Chisq[2],
    null_urb_p = out$`Pr(>Chisq)`[2],
    AIC_null = out2,
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
master_guide_wide.mrg$urb_null_improves <- ifelse(master_guide_wide.mrg$null_urb_p < 0.05, "YES", "NO")


#OK. now I want to extract model performance info

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
ms <- lapply(master_guide_wide.mrg$model_path_urb,
             FUN=readRDS)
ms.tidy <- lapply(ms,
                  FUN=perform_function)
ms.tidy
names(ms.tidy) <- master_guide_wide.mrg$model_id_urb

ms.tidy <- rbindlist(ms.tidy, idcol = "model_id_urb", fill = TRUE)
ms.tidy


#Inrtegrate into model guide
master_guide_wide.mrg.2 <- merge(master_guide_wide.mrg,
                              ms.tidy,
                              by = "model_id_urb",
                              all.x = T)

#OK, now because these are univariate, we can additionally incorporate beta coefs and such
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

#Apply to time + urbanization models SCALED ONLY
remove(ms)
remove(ms.tidy)

ms <- lapply(master_guide_wide.mrg$model_path_urb,
             FUN=readRDS)
ms.tidy <- lapply(ms,
                  FUN=get_coefficients)
ms.tidy
names(ms.tidy) <- master_guide_wide.mrg$model_path_urb

ms.tidy <- rbindlist(ms.tidy, idcol = "model_path_urb", fill = TRUE)
ms.tidy


#Inrtegrate into model guide
master_guide_wide.mrg.3 <- merge(master_guide_wide.mrg.2,
                                 ms.tidy,
                                 by = "model_path_urb",
                                 all.x = T)

#OK master_guide_wide.mrg.3 contains everything we need but we need to turbo clean it up
master_guide_wide.mrg.3 <- as.data.frame(master_guide_wide.mrg.3)
str(master_guide_wide.mrg.3)

master_guide_wide.mrg.3 <- master_guide_wide.mrg.3 %>%
  dplyr::select(response, var, dat, null_urb_chisq, null_urb_p, urb_null_improves,
                n_obs, AIC_null, AIC_urb, R2_marginal, R2_conditional, term, beta, p,
                lwr, upr) %>%
  dplyr::rename("Response" = response,
                "Dataset" = dat,
                "X" = null_urb_chisq,
                "P" = null_urb_p,
                "N" = n_obs,
                "Marg. R2" = R2_marginal,
                "Cond. R2" = R2_conditional,
                "B" = beta,
                "Low CI" = lwr,
                "Upp. CI" = upr,
                "Null AIC" = AIC_null,
                "Urb. AIC" = AIC_urb)

master_guide_wide.mrg.3$bind <- paste0(master_guide_wide.mrg.3$Response, master_guide_wide.mrg.3$Dataset)


#Split into scaled and unscaled
model_info_unscaled <- master_guide_wide.mrg.3 %>% dplyr::filter(var == "Nat50" | var == "urbanization")
model_info_unscaled <- model_info_unscaled %>%
  dplyr::mutate(ES = exp(B),
                ES_low_CI = exp(`Low CI`),
                ES_upp_CI = exp(`Upp. CI`))
model_info_unscaled <- model_info_unscaled %>%
  dplyr::select(bind, ES, ES_low_CI, ES_upp_CI)

#model_info_unscaled has effect sized for everyone

#We can now remove unscaled from  master_guide_wide.mrg.3
master_guide_wide.mrg.3 <- master_guide_wide.mrg.3 %>%
  dplyr::filter(var != "Nat50")

#now we should be able to bind
model_info <- left_join(master_guide_wide.mrg.3, model_info_unscaled, by = "bind")
model_info <- model_info %>% dplyr::select(-c(var, term, bind, urb_null_improves))

model_info <- model_info %>%
  dplyr::rename("Extent" = Dataset,
                "ES Low CI" = ES_low_CI,
                "ES Upp. CI" = ES_upp_CI)


setDT(model_info)

model_info$order <- "XXX"
model_info[, order := fcase(Response == "Behav_Complexity", "2",
                            Response == "Contact_duration", "6",
                            Response == "Inv", "3",
                            Response == "Contact", "5",
                            Response == "Inv_duration", "4",
                            Response == "Lope", "7",
                            Response == "Solves", "1",
                            default = order)]

model_info[, Response := fcase(Response == "Behav_Complexity ", "Behavioural diversity",
                               Response == "Contact_duration ", "Contact duration",
                               Response == "Inv", "Investigate",
                               Response == "Inv_duration", "Investigate duration",
                               Response == "Lope", "Escape gait",
                               Response == "Solves", "Solution",
                                            default = Response)]

model_info[, Extent := fcase(Extent == "dat_city", "City",
                             Extent == "dat_all", "All",
                                default = Extent)]

#save this
#write.csv(model_info, file = "figures/TableX_urb_only_model_results.csv")
