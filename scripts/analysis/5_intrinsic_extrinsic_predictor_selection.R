#Goal here is to run all subsets model selection to ID the top group of predictors
#For each outcome variable + dataset combo


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
library(MuMIn)

# 0. Load data and guide --------------------------------------------------

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

#I think I will make a model guide?? Just to develop my formulas...
formulas <- CJ(response = c("Contact_duration", "Inv_duration", "Behav_Complexity", "Lope", "Solves",
                         "Contact", "Inv"),
            dat = c("dat_all", "dat_city"),
            subject_id = c("yes", "no"),
            scaled = c("yes", "no"))



#Create null model formulas
formulas[, null_model_formula := paste(response, "~ 1")]

#Create focal formulas
formulas[scaled == "yes", focal_model_formula := paste(response, "~ PuzzleType + Year + Light + GroupSize_scaled + Sex + temp_scaled + SiteSequence + Disease")]
formulas[scaled == "no", focal_model_formula := paste(response, "~ PuzzleType + Year + Light + GroupSize + Sex + temp + SiteSequence + Disease")]
formulas


#Add response
formulas[response == "Contact_duration", model_family := "lognormal()"]
formulas[response == "Inv_duration", model_family := "lognormal()"]
formulas[response == "Behav_Complexity", model_family := "poisson()"]
formulas[response == "Lope", model_family := "binomial(link = 'logit')"]
formulas[response == "Solves", model_family := "binomial(link = 'logit')"]
formulas[response == "Contact", model_family := "binomial(link = 'logit')"]
formulas[response == "Inv", model_family := "binomial(link = 'logit')"]


#add random effect
formulas[subject_id == "yes", rand_ef := "(1|SiteID/Subject)"]
formulas[subject_id == "no", rand_ef := "(1|SiteID)"]


# >>> Add model comparison IDs --------------------------------------------

formulas[, model_complexity_comparison_ID := paste0("model_complexity_id_",
                                                 seq(1:.N))]

formulas[model_complexity_comparison_ID == "model_complexity_id_22"]



# >>> Make the data long --------------------------------------------------
formula.long <- melt(formulas,
                   measure.vars = c("null_model_formula",
                                    "focal_model_formula"),
                   variable.name = "model_type",
                   value.name = "formula")

formula.long




#now make global model call
formula.long[, global_model_call := paste0("glmmTMB(", 
                                       formula,
                                       " + ",
                                       rand_ef, 
                                       ", family = ",
                                       model_family,
                                       ", data = ",
                                  dat,
                                  ")")]

#Add an ID
formula.long[, model_id := paste0("model_", seq(1:.N))]

#Add a model path
formula.long[, model_path := paste0("outputs/global_models/", model_id, ".Rds")]



#run these dudes
dat_all <- dat
dat_city <- dat %>% dplyr::filter(urbanization == "City")
remove(dat)


#this function should make rthe model and save it

#FIRST SET NA OPTION
options(na.action = "na.omit")   # or "na.exclude"

fit_and_save <- function(global_model_call, model_path) {
  
  message("Running: ", model_path)
  
  # Convert string to expression and run model
  model <- eval(parse(text = global_model_call))
  
  # Save fitted model
  saveRDS(model, file = model_path)
  
  return(TRUE)
}

# Run for all the models  models
formula.long[, success := mapply(
  fit_and_save,
  global_model_call,
  model_path
)]

#they all went 

#Now run dredge
#THIS WILL TYAKE A WHILE


#start by checking one model
m <- readRDS("outputs/global_models/model_110.Rds")

dat_city_dredge <- model.frame(m)


m2 <- update(
  m,
  data = dat_city_dredge
)

options(na.action = "na.fail")

dd <- dredge(
  m2,
  fixed = "cond((Int))",
  trace = TRUE
)

dd



# run for everyone

options(na.action = "na.fail")

run_dredge <- function(model_path, top_model_path) {
  
  message("Processing: ", model_path)
  
  tryCatch({
    
    m <- readRDS(model_path)
    
    ## Save the original dataset name before updating
    dataset_name <- tryCatch(
      paste(deparse(m$call$data), collapse = ""),
      error = function(e) NA_character_
    )
    
    dat <- model.frame(m)
    sample_size <- nrow(dat)
    
    m <- update(m, data = dat)
    
    dd <- dredge(
      m,
      fixed = "cond((Int))",
      trace = FALSE
    )
    
    top <- get.models(dd, subset = 1)[[1]]
    
    saveRDS(top, top_model_path)
    
    data.table(
      success = TRUE,
      dataset = dataset_name,
      sample_size = sample_size,
      AICc = dd$AICc[1],
      formula = paste(deparse(formula(top)), collapse = " "),
      error = NA_character_
    )
    
  }, error = function(e) {
    
    data.table(
      success = FALSE,
      dataset = NA_character_,
      sample_size = NA_integer_,
      AICc = NA_real_,
      formula = NA_character_,
      error = conditionMessage(e)
    )
    
  })
}

results <- formula.long[
  ,
  run_dredge(
    model_path,
    file.path(
      "outputs/top_models",
      paste0(model_id, "_top.Rds")
    )
  ),
  by = model_id
]


results

#saveRDS(results, file = "data/results_from_dredge.rds")

results <- readRDS("data/results_from_dredge.rds")

#OK. now can I link results to formilas.long? I should be able to based on the model_id column
#Which in the formulas.long is known as this too
#let's try to merge them, knowing there will be some blanks
formula.long.mrg <- merge(formula.long,
                                                     results,
                                                     by = "model_id",
                                                     all.x = T)

#OK good. now we have formila.long.mrg which includes a formula for the top model
#And it also includes AIC and n for that model
#Although I actually did make and store those top models, I dont think I left any way to
#actually link them
#So I am going to add this again...

#First clean things up a little. things are out of hand
setnames(formula.long.mrg, old = "model_id", new = "global_model_id")
setnames(formula.long.mrg, old = "model_complexity_comparison_ID", new = "null_vs_focal_comparison_id")
setnames(formula.long.mrg, old = "formula.x", new = "global_formula")
setnames(formula.long.mrg, old = "model_path", new = "global_model_path")
setnames(formula.long.mrg, old = "formula.y", new = "top_model_formula")

str(formula.long.mrg)

formula.long.mrg$success <- NULL
formula.long.mrg$error <- NULL

str(formula.long.mrg)


#My next goal is to run the top models and save them
formula.long.mrg[, top_model_call := paste0("glmmTMB(",
                                            top_model_formula,
                                            ", family = ",
                                            model_family,
                                            ", dat = ",
                                            dat, ")")]

  
#Add an ID
formula.long.mrg[, top_model_id := paste0("model_", seq(1:.N))]

#Add a model path
formula.long.mrg[, top_model_path := paste0("outputs/top_models_final/", top_model_id, ".Rds")]



#OK now run those puppies. 
#FIRST SET NA OPTION
options(na.action = "na.omit")   # or "na.exclude"

fit_top_model <- function(top_model_call, top_model_path) {
  
  message("Running: ", top_model_path)
  
  # Convert string to expression and run model
  model <- eval(parse(text = top_model_call))
  
  # Save fitted model
  saveRDS(model, file = top_model_path)
  
  return(TRUE)
}

# Run for all the models  models
formula.long.mrg[, success := mapply(
  fit_top_model,
  top_model_call,
  top_model_path
)]





#OK. now I want to extract model info

#start by doing for non sensitivity analysis only
formula.long.mrg.2 <- formula.long.mrg[subject_id == "no"]

str(formula.long.mrg.2)

#cast wide by model type
formula.long.mrg.2_wide <- dcast(formula.long.mrg.2,
                            null_vs_focal_comparison_id  ~ model_type,
                           value.var = c("top_model_call", "top_model_path", "top_model_id",
                                         "top_model_formula", "response", "dataset",
                                         "scaled"))

#eveyrthing that is unique for each row needs to be excluded or in the value.var



# >>> Compare null and urb------------------------------------------------------
comps <- list()  # make list

for (i in 1:nrow(formula.long.mrg.2_wide)) {
  
  top_model_path_null_model_formula  <- formula.long.mrg.2_wide[i, top_model_path_null_model_formula]
  top_model_path_focal_model_formula <- formula.long.mrg.2_wide[i, top_model_path_focal_model_formula]
  model_id <- formula.long.mrg.2_wide[i, null_vs_focal_comparison_id]
  
  # Skip if model paths are missing or file(s) don't exist
  # if (is.na(model_path_urb_time_formula) || is.na(model_path_urb_cross_time_formula)) next
  # if (!file.exists(model_path_urb_time_formula) || !file.exists(model_path_urb_cross_time_formula)) next
  
  # Read models
  m1 <- readRDS(top_model_path_null_model_formula)
  m2 <- readRDS(top_model_path_focal_model_formula)
  
  # Compare  models
  out <- anova(m1, m2)
  out2 <- AIC(m1)
  out3 <- AIC(m2)
  
  # Store comparison
  comps[[i]] <- data.table(
    null_vs_focal_comparison_id = model_id,
    null_focal_chisq = out$Chisq[2],
    null_focal_p = out$`Pr(>Chisq)`[2],
    AIC_null = out2,
    AIC_focal = out3
  )
  
  cat(i, "/", nrow(formula.long.mrg.2_wide), "\r")
}

# Combine all results into one data.table
comps.dt <- rbindlist(comps, fill = TRUE)

# where did comparison fail?
comps.dt[is.na(null_focal_chisq)] # everybody went!!
# Ideally 0 rows


# >>> Merge into master guide wide ----------------------------------------

formula.long.mrg.3_wide <- merge(formula.long.mrg.2_wide,
                               comps.dt,
                               by = "null_vs_focal_comparison_id",
                               all.x = T)
formula.long.mrg.3_wide
formula.long.mrg.3_wide$focal_null_improves <- ifelse(formula.long.mrg.3_wide$null_focal_p < 0.05, "YES", "NO")


#OK. now try to get out performance metrocs
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
ms <- lapply(formula.long.mrg.2_wide$top_model_path_focal_model_formula,
             FUN=readRDS)
ms.tidy <- lapply(ms,
                  FUN=perform_function)
ms.tidy
names(ms.tidy) <- formula.long.mrg.2_wide$top_model_id_focal_model_formula

ms.tidy <- rbindlist(ms.tidy, idcol = "top_model_id_focal_model_formula", fill = TRUE)
ms.tidy


#Inrtegrate into model guide
formula.long.mrg.4_wide <- merge(formula.long.mrg.3_wide,
                                 ms.tidy,
                                 by = "top_model_id_focal_model_formula",
                                 all.x = T)




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

ms <- lapply(formula.long.mrg.2_wide$top_model_path_focal_model_formula,
             FUN=readRDS)
ms.tidy <- lapply(ms,
                  FUN=get_coefficients)
ms.tidy
names(ms.tidy) <- formula.long.mrg.2_wide$top_model_path_focal_model_formula

ms.tidy <- rbindlist(ms.tidy, idcol = "top_model_path_focal_model_formula", fill = TRUE)
ms.tidy


#Inrtegrate into model guide
coefficients <-  merge(ms.tidy,
                       formula.long.mrg.3_wide,
                       by = "top_model_path_focal_model_formula",
                       all.x = T)



#make nice, cleaned up tables---------------------------------------------------

#make performance metric table
#Start with this dude
formula.long.mrg.4_wide

str(formula.long.mrg.4_wide)

formula.long.mrg.4_wide <- formula.long.mrg.4_wide %>%
  dplyr::select(response_focal_model_formula, dataset_focal_model_formula, null_focal_chisq, null_focal_p, 
                n_obs, AIC_null, AIC_focal, focal_null_improves, R2_marginal, R2_conditional, scaled_focal_model_formula) %>%
  dplyr::rename("Response" = response_focal_model_formula,
                "Dataset" = dataset_focal_model_formula,
                "X" = null_focal_chisq,
                "P" = null_focal_p,
                "N" = n_obs,
                "Marg. R2" = R2_marginal,
                "Cond. R2" = R2_conditional,
                "scaled" = scaled_focal_model_formula,
                "Null AIC" = AIC_null,
                "Urb. AIC" = AIC_focal)


model_info <- formula.long.mrg.4_wide

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

model_info[, Response := fcase(Response == "Behav_Complexity", "Behavioural diversity",
                               Response == "Contact_duration", "Contact duration",
                               Response == "Inv", "Investigate",
                               Response == "Inv_duration", "Investigate duration",
                               Response == "Lope", "Escape gait",
                               Response == "Solves", "Solution",
                               default = Response)]

model_info[, Dataset := fcase(Dataset == "dat_city", "City",
                             Dataset == "dat_all", "All",
                             default = Dataset)]

model_info <- model_info[scaled == "no"]
model_info$scaled <- NULL

#save this
#write.csv(model_info, file = "figures/TableS3_hyp2_model_performance.csv")



#make coefficient table
#start with this dude
coefficients

str(coefficients)

coefficients <- coefficients %>%
  dplyr::select(response_focal_model_formula, dataset_focal_model_formula, 
                scaled_focal_model_formula,
                term,
                beta,
                p,
                lwr,
                upr) %>%
  dplyr::rename("Response" = response_focal_model_formula,
                "Dataset" = dataset_focal_model_formula,
                "scaled" = scaled_focal_model_formula,
                "Term" = term,
                "B" = beta,
                "P" = p,
                "Low CI" = lwr,
                "Upp. CI" = upr)


coefficients$bind <- paste0(coefficients$Response, coefficients$Dataset, coefficients$Term)


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
final_coef <- final_coef %>% dplyr::select(-c(scaled, bind))

final_coef <- final_coef %>%
  dplyr::rename("Extent" = Dataset,
                "ES Low CI" = ES_low_CI,
                "ES Upp. CI" = ES_upp_CI)


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

final_coef[, Response := fcase(Response == "Behav_Complexity ", "Behavioural diversity",
                               Response == "Contact_duration ", "Contact duration",
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
                               default = Term)]
#save this
#write.csv(final_coef, file = "figures/TableS4_hyp2_model_coefficients.csv")
