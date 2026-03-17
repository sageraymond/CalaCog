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

# Load data --------------------------------------------------------------

sitedata <- read.csv("data/SiteSummaryDataJul2025.csv")
sitedata <- as.data.frame(sitedata)

#Code urbanization as factor
sitedata$urbanization <- factor(sitedata$urbanization, levels = c("Wild", "City"))


# Remove SE3 where there were no events
sitedata <- sitedata %>% dplyr::filter(SiteID != "SE3")

#Create offset column
sitedata$offset <- log(sitedata$TotalEvents)

#Step 1:Write a function to create models (null + univariate + dispformula)
build_models <- function(data, outcomes, predictor, offset_var, dispformula, family = nbinom2()) {
  models <- list()
  
  for (outcome in outcomes) {
    # Null model
    null_formula <- as.formula(
      paste0(outcome, " ~ 1 + offset(", offset_var, ")")
    )
    null_model <- glmmTMB(null_formula, data = data, family = family)
    
    # Univariate model
    uni_formula <- as.formula(
      paste0(outcome, " ~ ", predictor, " + offset(", offset_var, ")")
    )
    uni_model <- glmmTMB(uni_formula, data = data, family = family)
    
    # Univariate model with dispersion formula
    disp_model <- glmmTMB(uni_formula, data = data, family = family, dispformula = dispformula)
    
    # Store all three models
    models[[outcome]] <- list(
      null = null_model,
      univariate = uni_model,
      disp = disp_model
    )
  }
  
  return(models)
}

#Apply to my data
model_list <- build_models(
  data = sitedata,
  outcomes = c("Solves", "No.Lope", "No.Contact", "No.Inv", "No.Behav"),
  predictor = "urbanization",
  offset_var = "offset",
  dispformula = ~urbanization,
  family = nbinom2()
)


#Step 2. Determine whether models perform better than null
#Build function to compare
Null.v.Uni.lrt <- function(model_list) {
  lrt_results <- list()
  
  for (outcome in names(model_list)) {
    null_mod <- model_list[[outcome]]$null
    uni_mod <- model_list[[outcome]]$univariate
    
        lrt <- anova(null_mod, uni_mod, test = "Chisq")
    
    lrt_results[[outcome]] <- lrt
  }
  
  return(lrt_results)
}

#Now apply function to the model list i already made:
lrt_results <- Null.v.Uni.lrt(model_list)
lrt_results
#Look like the univariate is better than the null in all cases. This is good


#Step 3. Determine whether models perform better with disp formula
Uni.v.Disp.lrt <- function(model_list) {
  lrt_results <- list()
  
  for (outcome in names(model_list)) {
    uni_mod <- model_list[[outcome]]$univariate
    disp_mod <- model_list[[outcome]]$disp
    
    
    lrt <- anova(uni_mod, disp_mod, test = "Chisq")
    
    lrt_results[[outcome]] <- lrt
  }
  
  return(lrt_results)
}

#Apply to my data
lrt_results <- Uni.v.Disp.lrt(model_list)
lrt_results
#This improved model results for Investigate only


#Step 4. Store your best models
best_models <- list(
  Solves = model_list$Solves$univariate,
  No.Lope = model_list$No.Lope$univariate,
  No.Contact = model_list$No.Contact$univariate,
  No.Inv = model_list$No.Inv$univariate,
  No.Behav = model_list$No.Behav$univariate
)

#Store nulls too
null_models <- lapply(names(best_models), function(name) model_list[[name]]$null)
names(null_models) <- names(best_models)


#Step 5. Put their information in a table
summarize_models <- function(model_list, null_models, predictor_base = "urbanization") {

  summary_table <- lapply(names(model_list), function(outcome) {
    model <- model_list[[outcome]]
    null_model <- null_models[[outcome]]
    
    fixed_coefs <- tidy(model, effects = "fixed", conf.int = TRUE) #extract fixef
    
    term_row <- fixed_coefs %>% filter(grepl(predictor_base, term)) #id predictor
    
    if (nrow(term_row) == 0) {
      warning(paste("Predictor term not found in mean model for", outcome))
      beta <- ci_low <- ci_high <- rr <- rr_low <- rr_high <- beta_p <- NA
      beta_term <- NA
    } else {
      beta_term <- term_row$term[1]
      beta <- term_row$estimate[1]
      ci_low <- term_row$conf.low[1]
      ci_high <- term_row$conf.high[1]
      beta_p <- term_row$p.value[1]
      rr <- exp(beta)
      rr_low <- exp(ci_low)
      rr_high <- exp(ci_high)
    }
    
    lrt <- tryCatch({ #LR test. I dont know what the trycatch is doing here...
      anova(null_model, model, test = "Chisq")
    }, error = function(e) {
      warning(paste("LRT failed for", outcome))
      return(NULL)
    })
    
    lrt_p <- if (!is.null(lrt)) lrt$`Pr(>Chisq)`[2] else NA
    
    disp_coefs <- tryCatch({ #extract disp formula... seems broken
      tidy(model, effects = "disp", conf.int = TRUE)
    }, error = function(e) NULL)
    
    disp_row <- if (!is.null(disp_coefs)) disp_coefs %>% filter(grepl(predictor_base, term)) else NULL
    
    if (!is.null(disp_row) && nrow(disp_row) > 0) {
      disp_term <- disp_row$term[1]
      disp_beta <- disp_row$estimate[1]
      disp_ci_low <- disp_row$conf.low[1]
      disp_ci_high <- disp_row$conf.high[1]
    } else {
      disp_term <- disp_beta <- disp_ci_low <- disp_ci_high <- NA
    }
    
    data.frame(
      outcome = outcome,
      beta_term = beta_term,
      beta = beta,
      beta_p = beta_p,
      ci_lower = ci_low,
      ci_upper = ci_high,
      rate_ratio = rr,
      rr_ci_lower = rr_low,
      rr_ci_upper = rr_high,
      AIC = AIC(model),
      LRT_p = lrt_p,
      disp_term = disp_term,
      disp_beta = disp_beta,
      disp_ci_lower = disp_ci_low,
      disp_ci_upper = disp_ci_high
    )
  }) %>% bind_rows()
  
  return(summary_table)
}

#Apply to my data (best models)
summary <- summarize_models(best_models, null_models)

summary

#It's only 5 models, so just save them manually
SolveCP <- model_list$Solves$univariate
LopeCP <- model_list$No.Lope$univariate
ContactCP <- model_list$No.Contact$univariate
InvCP <- model_list$No.Inv$univariate
BehavCP <- model_list$No.Behav$univariate

#Repeat for urbanization score
#Filter to city sites only
citysites <- sitedata %>%
  dplyr::filter(urbanization == "City")

#Step 1. I need to built a new function that can deal with > 1 predictor
build_models2 <- function(data, outcomes, predictors, offset_var, family = nbinom2()) {
  models <- list()
  
  for (outcome in outcomes) {
    models[[outcome]] <- list()
    
    for (predictor in predictors) {
      # Null model
      null_formula <- as.formula(
        paste0(outcome, " ~ 1 + offset(", offset_var, ")")
      )
      null_model <- glmmTMB(null_formula, data = data, family = family)
      
      # Univariate model
      uni_formula <- as.formula(
        paste0(outcome, " ~ ", predictor, " + offset(", offset_var, ")")
      )
      uni_model <- glmmTMB(uni_formula, data = data, family = family)
      
      # Dispersion formula: ~ predictor (built dynamically)
      disp_formula <- as.formula(paste0("~", predictor))
      
      # Univariate model with dispersion
      disp_model <- glmmTMB(uni_formula, data = data, family = family, dispformula = disp_formula)
      
      # Store models
      models[[outcome]][[predictor]] <- list(
        null = null_model,
        univariate = uni_model,
        disp = disp_model
      )
    }
  }
  
  return(models)
}


#Apply to my data
model_list <- build_models2(
  data = citysites,
  outcomes = c("Solves", "No.Lope", "No.Contact", "No.Inv", "No.Behav"),
  predictors = c("Road.density", "pop_density", "ANTH", "NAT", "Nat50", "Nat100", "Nat250", "urbanization_score"),
  offset_var = "offset",
  family = nbinom2()
) 

#Check a few of these dudes
fixef(model_list$Solves$pop_density$null)
fixef(model_list$Solves$pop_density$univariate)
fixef(model_list$Solves$pop_density$disp)

#Step 2. Determine whether models perform better than null
#Build function to compare
Null.v.Uni.lrt <- function(model_list) {
  lrt_results <- list()
  
  for (outcome in names(model_list)) {
    for (predictor in names(model_list[[outcome]])) {
      mods <- model_list[[outcome]][[predictor]]
      
      if (is.null(mods$null) || is.null(mods$univariate)) {
        message(paste("Skipping", outcome, predictor, "- null or univariate model missing"))
        next
      }
      
      lrt <- tryCatch({
        anova(mods$null, mods$univariate, test = "Chisq")
      }, error = function(e) {
        message(paste("LRT failed for", outcome, predictor, ":", e$message))
        return(NULL)
      })
      
      if (is.null(lrt)) {
        message(paste("LRT returned NULL for", outcome, predictor))
        next
      }
      
      lrt_results[[paste(outcome, predictor, sep = "_")]] <- lrt
    }
  }
  
  return(lrt_results)
}


#Now apply function to the model list i already made:
lrt_results <- Null.v.Uni.lrt(model_list)

lrt_results


#Step 3. Determine whether models perform better with disp formula
Uni.v.Disp.lrt <- function(model_list) {
  lrt_results <- list()
  
  for (outcome in names(model_list)) {
    for (predictor in names(model_list[[outcome]])) {
      models <- model_list[[outcome]][[predictor]]
      
      # Proceed only if both models exist
      if (!is.null(models$univariate) && !is.null(models$disp)) {
        lrt <- tryCatch({
          anova(models$univariate, models$disp, test = "Chisq")
        }, error = function(e) {
          warning(paste("LRT failed for", outcome, "-", predictor, ":", e$message))
          return(NULL)
        })
        
        lrt_results[[paste(outcome, predictor, sep = "_")]] <- lrt
      }
    }
  }
  
  return(lrt_results)
}

#Apply to my data
lrt_results <- Uni.v.Disp.lrt(model_list)
lrt_results #that did not help matters

#This didn't improve any of the models that I want to hang on to

#Step 4. Store your best models
best_models2 <- list(
  Solves = model_list$Solves$Nat50$univariate,
  No.Lope = model_list$No.Lope$Nat50$univariate,
  No.Contact = model_list$No.Contact$Nat50$univariate,
  No.Inv = model_list$No.Inv$Nat50$univariate,
  No.Behav = model_list$No.Behav$Nat50$univariate
)

#Store nulls too
null_models2 <- list(
  Solves = model_list$Solves$Nat50$null,
  No.Lope = model_list$No.Lope$Nat50$null,
  No.Contact = model_list$No.Contact$Nat50$null,
  No.Inv = model_list$No.Inv$Nat50$null,
  No.Behav = model_list$No.Behav$Nat50$null
)



#Step 5. Put their information in a table
summarize_models <- function(model_list, null_models2, predictor_base = "Nat50") {
  
  summary_table <- lapply(names(model_list), function(outcome) {
    model <- model_list[[outcome]]
    null_model <- null_models2[[outcome]]
    
    fixed_coefs <- tidy(model, effects = "fixed", conf.int = TRUE) #extract fixef
    
    term_row <- fixed_coefs %>% filter(grepl(predictor_base, term)) #id predictor
    
    if (nrow(term_row) == 0) {
      warning(paste("Predictor term not found in mean model for", outcome))
      beta <- ci_low <- ci_high <- rr <- rr_low <- rr_high <- beta_p <- NA
      beta_term <- NA
    } else {
      beta_term <- term_row$term[1]
      beta <- term_row$estimate[1]
      ci_low <- term_row$conf.low[1]
      ci_high <- term_row$conf.high[1]
      beta_p <- term_row$p.value[1]
      rr <- exp(beta)
      rr_low <- exp(ci_low)
      rr_high <- exp(ci_high)
    }
    
    lrt <- tryCatch({ #LR test. I dont know what the trycatch is doing here...
      anova(null_model, model, test = "Chisq")
    }, error = function(e) {
      warning(paste("LRT failed for", outcome))
      return(NULL)
    })
    
    lrt_p <- if (!is.null(lrt)) lrt$`Pr(>Chisq)`[2] else NA
    
    disp_coefs <- tryCatch({ #extract disp formula... seems broken
      tidy(model, effects = "disp", conf.int = TRUE)
    }, error = function(e) NULL)
    
    disp_row <- if (!is.null(disp_coefs)) disp_coefs %>% filter(grepl(predictor_base, term)) else NULL
    
    if (!is.null(disp_row) && nrow(disp_row) > 0) {
      disp_term <- disp_row$term[1]
      disp_beta <- disp_row$estimate[1]
      disp_ci_low <- disp_row$conf.low[1]
      disp_ci_high <- disp_row$conf.high[1]
    } else {
      disp_term <- disp_beta <- disp_ci_low <- disp_ci_high <- NA
    }
    
    data.frame(
      outcome = outcome,
      beta_term = beta_term,
      beta = beta,
      beta_p = beta_p,
      ci_lower = ci_low,
      ci_upper = ci_high,
      rate_ratio = rr,
      rr_ci_lower = rr_low,
      rr_ci_upper = rr_high,
      AIC = AIC(model),
      LRT_p = lrt_p,
      disp_term = disp_term,
      disp_beta = disp_beta,
      disp_ci_lower = disp_ci_low,
      disp_ci_upper = disp_ci_high
    )
  }) %>% bind_rows()
  
  return(summary_table)
}

#Apply to my data (best models)
summary2 <- summarize_models(best_models2, null_models2)

summary2

FullSiteSumamry <-rbind(summary, summary2)

#write.csv(FullSiteSumamry, "figures/SiteModelInfo.csv")

#Again, save the 5 relevant models for plotting
SolveCity <- model_list$Solves$Nat50$univariate
LopeCity <- model_list$No.Lope$Nat50$univariate
ContactCity <- model_list$No.Contact$Nat50$univariate
InvCity <- model_list$No.Inv$Nat50$univariate
BehavCity <- model_list$No.Behav$Nat50$univariate

#Create Plots for City vs. Park
#City vs Park Plots------------------------------------------------------------------

#Make model prediction plots for city vs. park-----------------------------
#
models_categorical <- list(
  InvCP = InvCP,
  LopeCP = LopeCP,
  ContactCP = ContactCP,
  SolveCP = SolveCP,
  BehavCP = BehavCP
)

# Function to extract predictions
predict_urban_levels <- function(model, model_name) {
  newdat <- data.table(urbanization = c("Wild", "City"))
  
  # Calculate log-mean of offset variable
  newdat[, offset := log(mean(sitedata$offset))]
  
  preds <- predict(model, newdata = newdat, type = "response", se.fit = TRUE)
  
  newdat[, predicted := preds$fit]
  newdat[, lower := predicted - 1.96 * preds$se.fit]
  newdat[, upper := predicted + 1.96 * preds$se.fit]
  newdat[, response := model_name]
  
  return(newdat)
}

# Apply to all models
predicted_all <- rbindlist(
  Map(predict_urban_levels, models_categorical, names(models_categorical))
)

head(predicted_all)


#These plots will show a model's predocted count for a typical observation with specified urb level :
#Also average offset and any other predictors held constant, which doesn't apply here
label_map <- c(
  "SolveCP"   = "Number of Solutions",
  "BehavCP" = "Total Behavioural Diversity",
  "InvCP"     = "Number of Events with\nExplorations",
  "ContactCP" = "Number of Events with\nPersistence > 0",
  "LopeCP"    = "Number of Events with\nFearfulness"
)

# Apply new labels
predicted_all[, response_label := label_map[response]]
predicted_all[, response_label := factor(response_label, levels = label_map)]
predicted_all[, urbanization := factor(urbanization, levels = c("Wild", "City"))]


# Plot
CvPPlots <- ggplot(predicted_all, aes(x = urbanization, y = predicted, fill = urbanization)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
#  geom_hline(yintercept = 0, color = "black") +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0, face = "plain", size = 12),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "Urbanization Category",
    y = "Predicted Count"
  ) +
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none")

CvPPlots

#City only plots---------------------------------------------------------------
#Store models
models_continuous <- list(
  SolveCity = SolveCity,
  BehavCity = BehavCity,
  InvCity = InvCity,
  ContactCity = ContactCity,
  LopeCity = LopeCity
)

#Make function to generate model predioctions
predict_continuous_model <- function(model, model_name) {
  score_seq <- seq(0, 100, length.out = 100) 
  
  newdat <- data.table(Nat50 = score_seq)
  
  newdat[, offset := log(mean(citysites$offset))]
  
  preds <- predict(model, newdata = newdat, type = "response", se.fit = TRUE)
  
  newdat[, predicted := preds$fit]
  newdat[, lower := predicted - 1.96 * preds$se.fit]
  newdat[, upper := predicted + 1.96 * preds$se.fit]
  newdat[, response := model_name]
  
  return(newdat)
}


#Apply to my mods
predicted_cont <- rbindlist(
  Map(predict_continuous_model, models_continuous, names(models_continuous))
)

#Specify desired order and names
label_map_cont <- c(
  "SolveCity"   = "Number of Solutions",
  "BehavCity" = "Total Behavioural Diversity",
  "InvCity"     = "Number of Events with\nExplorations",
  "ContactCity" = "Number of Events with\nPersistence > 0",
  "LopeCity"    = "Number of Events with\nFearfulness"
)

# Add new labels
predicted_cont[, response_label := label_map_cont[response]]
predicted_cont[, response_label := factor(response_label, levels = label_map_cont)]

#Plot!!!
CityPlots <- ggplot(predicted_cont, aes(x = Nat50, y = predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#9F4E4A", alpha = 0.2) +
  geom_line(color = "#78206E", size = 1) +
#  geom_hline(yintercept = 0, color = "black") +
  facet_wrap(~ response_label, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0, face = "plain", size = 12),
    strip.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    legend.position = "none"
  ) +
  labs(
    x = "Urbanization (City Only; %)",
    y = "Predicted Count"
  ) +
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none")


#Combine plots
SitePlots <- ggarrange(CvPPlots, CityPlots, ncol = 2)
#ggsave("C:/Users/sager/OneDrive/Desktop/school/MSc/manuscripts/Science Cognition/PNAS_SUbmission/FigS3_Jan20.pdf", SitePlots, width = 6, height = 7.5, dpi = 700,  bg = "white") 
