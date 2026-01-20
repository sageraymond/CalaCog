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

#Repeat for urbanization score, which is now actually something else
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

#The univariate does not always improve model perfomance...
lrt_results
#It does under following circs:


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
  "InvCP"     = "Number of Events with\nExplorations",
  "ContactCP" = "Number of Events with\nPersistence > 0",
  "BehavCP" = "Total Behavioural Diversity",
  "LopeCP"    = "Number of Events with\nFearfulness",
  "SolveCP"   = "Number of Solutions"
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
  scale_fill_manual(values = c("Wild" = "#56B4E9", "City" = "#0072B2")) +
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
  InvCity = InvCity,
  ContactCity = ContactCity,
  BehavCity = BehavCity,
  LopeCity = LopeCity,
  SolveCity = SolveCity
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
  "InvCity"     = "Number of Events with\nExplorations",
  "ContactCity" = "Number of Events with\nPersistence > 0",
  "BehavCity" = "Total Behavioural Diversity",
  "LopeCity"    = "Number of Events with\nFearfulness",
  "SolveCity"   = "Number of Solutions"
)

# Add new labels
predicted_cont[, response_label := label_map_cont[response]]
predicted_cont[, response_label := factor(response_label, levels = label_map_cont)]

#Plot!!!
CityPlots <- ggplot(predicted_cont, aes(x = Nat50, y = predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#56B4E9", alpha = 0.2) +
  geom_line(color = "#0072B2", size = 1) +
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
#ggsave("figures/FigS1.pdf", SitePlots, width = 6, height = 7.5, dpi = 700,  bg = "white") 


























M_Contact <- glmmTMB(No.Contact ~ urbanization + offset(log(TotalEvents)), 
                     family = "nbinom2", 
                     #dispformula = ~urbanization,
                     data = sitedata,
                     na.action = na.omit)
summary(M_Contact)

resids <- simulateResiduals(M_Contact)
plot(resids)


M_Inv <- glmmTMB(No.Inv ~ urbanization + offset(log(TotalEvents)), 
                               family = "nbinom2", 
                               dispformula = ~urbanization,
                               data = sitedata)
summary(M_Inv)
resids <- simulateResiduals(M_Inv)
plot(resids)


M_Lope <- glmmTMB(No.Lope ~ urbanization + offset(log(TotalEvents)), 
                 family = "nbinom2", 
                 #dispformula = ~urbanization,
                 data = sitedata)
summary(M_Lope)
resids <- simulateResiduals(M_Lope)
plot(resids)


M_Solve <- glmmTMB(Solves ~ urbanization + offset(TotalEvents), 
                  family = "poisson", 
                  #dispformula = ~urbanization,
                  data = sitedata)






#No model possible




#Repeat with urbanization score
M_ContactS <- glmmTMB(No.Contact ~ urbanization_score + offset(log(TotalEvents)), 
                               family = "nbinom2", 
                               #dispformula = ~urbanization_score,
                               data = sitedata)
summary(M_ContactS)
resids <- simulateResiduals(M_ContactS)
plot(resids)


M_InvS <- glmmTMB(No.Inv ~ urbanization_score + offset(log(TotalEvents)), 
                 family = "nbinom2", 
                 dispformula = ~urbanization_score,
                 data = sitedata)
summary(M_InvS)
resids <- simulateResiduals(M_InvS)
plot(resids)


M_LopeS <- glmmTMB(No.Lope ~ urbanization_score + offset(log(TotalEvents)), 
                  family = "nbinom2", 
                  #dispformula = ~urbanization_score,
                  data = sitedata)
summary(M_LopeS)
resids <- simulateResiduals(M_Lope)
plot(resids)


M_SolveS <- glmmTMB(Solves ~ urbanization_score + offset(log(TotalEvents)), 
                   family = "nbinom2", 
                   #dispformula = ~urbanization_score,
                   data = sitedata)
summary(M_SolveS)
resids <- simulateResiduals(M_SolveS)
plot(resids)













#############BELOW THIS LINE IS ALL CRAP THAT LIKELY WON'T EVER BE USEFUL

#Add SiteID
Complex1$SiteID <- gsub("\\_.*","",Complex1$EventID)

#Add urbanization, remove sites with no contact events
Complex2 <- full_join(Complex1, Urb_Levels, by = "SiteID") %>% na.omit(Complex2)


#Get mean contact behaviours by site ID
Complex3 <- Complex2 %>%
  group_by(SiteID) %>%
  summarise(Mean_Complexity = mean(Count_Contact_Behaviours))

Complex3 <- as.data.frame(Complex3)

Complex4 <- left_join(Complex3, Urb_Levels, by = "SiteID")






#





Events_Summed <- left_join(Events_Summed, Urb_Levels, by = "site_i")
Events_Summed$site_id <- gsub("\\_.*","",Events_Summed$Identifier2)
Events_Summed$Subject <- sub(".*_", "", Events_Summed$Identifier2)
Events_Summed$Identifier3 <- sub("_[^_]+$", "", Events_Summed$Identifier2)


#What I really want is a dataframe with each of my outcome variables and predictors

















#Create SiteID column
Contact_by_Event$Site_ID <- gsub("\\_.*","",Contact_by_Event$EventID)

#To get contact by urbanization level, attach this to Urbanization levels
Contact_by_Urb <- left_join(Contact_by_Event, Urb_Levels, by = "Site_ID")

#Make a plot showing mean persistence time at park vs. city
PersistenceChartData <- Contact_by_Urb %>%
  group_by(UrbLevel) %>%
  summarise(mean_contact = mean(Contact_duration),
            SD = sd(Contact_duration))
PersistenceChartData <- as.data.frame(PersistenceChartData)
PersistenceChartData$UrbLevel <- ifelse(PersistenceChartData$UrbLevel!= "City", "Wild", PersistenceChartData$UrbLevel)
PerisenceChartMeans <- ggplot(data = PersistenceChartData,
       mapping = aes(y=mean_contact, x=UrbLevel)) +
  geom_bar(fill="#33CCFF", stat = "identity", colour = "black") +
  theme_classic() +
  geom_errorbar(aes(ymin = mean_contact - SD, ymax = mean_contact + SD), width = 0.2) + 
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none") +
  labs(x="Urb. Level", y="Mean contact time (s)") + 
  ggtitle("A. Persistence (City vs. Wild)")

#The analagous chart for the continuous metric would be a scatter plot... I think
PersistenceChartData2 <- Contact_by_Urb %>%
  dplyr::filter(UrbLevel != "Park") %>%
  group_by(Site_ID) %>%
  summarise(mean_contact = mean(Contact_duration))
PersistenceChartData2 <- as.data.frame(PersistenceChartData2)
PersistenceChartData2 <- left_join(PersistenceChartData2, YEGUrbScores, by = "Site_ID")

PerisenceChartLine <- ggplot(PersistenceChartData2, aes(x = urbanization_score, y = mean_contact)) +
  geom_point(color = "#33CCFF", size = 2) +  
  theme_classic() +
  geom_smooth(method = "lm", color = "black", fill = "#33CCFF") + 
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none") +
  labs(x="Urb. Score", y="Mean contact time (s)") + 
  ggtitle("B. Persistence (City Only)")

#Modelling will happen later
#Remove some stuff:
remove(Contact_by_Event)
remove(PersistenceChartData)
remove(PersistenceChartData2)

#Model it now
#Step 1. Should I include the 0s? Or perhaps do a hurdle model?
#Let's start by removeing events in which the animal was never oriented towards the puzzle
#Those are events where it's safe to say they didn't notice the puzzle
#Make a list of events that DO include orient_puzzle OR orient_camera
Orient <- Events_Summed %>%
  dplyr::filter(Behave == "orient_puzzle" |
                  Behave == "orient_camera") %>%
  dplyr::select(EventIDSubject)
Orient <- unique(Orient) #1146 events in which coyote was oriented at some point

Contact_by_Urb1 <- left_join(Orient, Contact_by_Urb, by = "EventIDSubject")
Contact_by_Urb1 <- Contact_by_Urb1 %>% dplyr::select(EventIDSubject, Contact_duration)
Contact_by_Urb1$Contact_duration[is.na(Contact_by_Urb1$Contact_duration)] <- 0
remove(Contact_by_Urb)








#Let's move on to behavioural complexity
#First metric of behavioural complexity is the number of behaviours exhibited while contacting
Complex1 <- Events_Summed %>% #start with events_summed
  dplyr::filter(Contact == "Y") %>% #extract events caharcterised by some form of contact
  dplyr::select(EventID, Behave) %>% #remove unecessary columns
  group_by(EventID) %>% summarise(Count_Contact_Behaviours = n())

Complex1 <- as.data.frame(Complex1) #only 58 events included some form of contact

#Add SiteID
Complex1$SiteID <- gsub("\\_.*","",Complex1$EventID)

#Add urbanization, remove sites with no contact events
Complex2 <- full_join(Complex1, Urb_Levels, by = "SiteID") %>% na.omit(Complex2)


#Get mean contact behaviours by site ID
Complex3 <- Complex2 %>%
  group_by(SiteID) %>%
  summarise(Mean_Complexity = mean(Count_Contact_Behaviours))

Complex3 <- as.data.frame(Complex3)

Complex4 <- left_join(Complex3, Urb_Levels, by = "SiteID")

#Summarise this in a model
#First park vs city
Complex_mod_1 <- glm(Mean_Complexity ~ UrbLevel, data = Complex4)
summ(Complex_mod_1, digits = 3)

Complex5 <- Complex4 %>% dplyr::filter(UrbLevel == "City")
Complex_mod_2 <- glm(Mean_Complexity ~ urbanization_score, data = Complex5)
summ(Complex_mod_2, digits = 3)

#Make relevant plots
ComplexChartData1 <- Complex4 %>%
  group_by(UrbLevel) %>%
  summarise(mean_complex = mean(Mean_Complexity),
            SD = sd(Mean_Complexity))
ComplexChartData1 <- as.data.frame(ComplexChartData1)
ggplot(data = ComplexChartData1,
       mapping = aes(y=mean_complex, x=UrbLevel)) +
  geom_bar(fill="#33CCFF", stat = "identity", colour = "black") +
  theme_classic() +
  geom_errorbar(aes(ymin = mean_complex - SD, ymax = mean_complex + SD), width = 0.2) + 
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none") +
  labs(x="Urb. Level", y="Mean behavioural complexity") + 
  ggtitle("Behavioural Complexity (Park vs. City)")


A <- plot_model(Complex_mod_2, type = "pred", terms = "urbanization_score[all]", 
                colors = "#33CCFF")
#Make prettier
B <- A + theme_classic() + ggtitle("Behavioural Complexity (within City)") + 
  theme(axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        plot.title = element_text(size = 12, face = "bold")) +
  labs(x = "Urbanization level", y = "Mean Behavioural Complexity")








Complex2$Urbanization <- factor(Complex2$Urbanization, levels = c("Moderate", "Park", "Very"))
Complex_mod <- glmer(Count_Contact_Behaviours ~ (1|SiteID) + Urbanization, data = Complex2, family = "poisson")
summ(Complex_mod, digits = 3)


#My second complexity indicator is the number of gait changes. 
#Begin with the Events dataframe and go from there

Gaits <- Events %>%
  dplyr::select(EventID, Behave, SiteID) %>%
  dplyr::filter(Behave == "walk" | Behave == "trot" | Behave == "still" | Behave == "lopegallop") %>%
  #up to now, we have a datafram that has only locomotion in it, with selected columns
  #to get the number of different gaits, we can just group by EventID and then count the number of rows
  group_by(EventID) %>% summarise(Gait_Count = n())
Gaits <- as.data.frame(Gaits) #459 events can be considered here

#Add SiteID
Gaits$SiteID <- gsub("\\_.*","",Gaits$EventID)

#Add urbanization
Gaits2 <- full_join(Gaits, Urb_Levels, by = "SiteID")

Gaits3 <- na.omit(Gaits2) #remove any NA values (i.e., sites with no events)

#Summarise as a model (poisson because count data)
#Start by setting moderate as reference category
Gaits3$Urbanization <- factor(Gaits3$Urbanization, levels = c("Moderate", "Park", "Very"))
Complex_mod1 <- glmer(Gait_Count ~ (1|SiteID) + Urbanization, data = Gaits3, family = "poisson")
summ(Complex_mod1, digits = 3)



#Moving on to exploration. First metric of exploration is investigate time
#Calculate investigate time for each event
Events_Summed$Inv <- "N"
Events_Summed$Inv <- ifelse(Events_Summed$Behave == "inv_cam", "Y", Events_Summed$Inv)
Events_Summed$Inv <- ifelse(Events_Summed$Behave == "inv_puzzle", "Y", Events_Summed$Inv)
Events_Summed$Inv <- ifelse(Events_Summed$Behave == "inv_ground", "Y", Events_Summed$Inv)
Events_Summed$Inv <- ifelse(Events_Summed$Behave == "inv_unk", "Y", Events_Summed$Inv)

#Should I retain events in which invetsigation occurred, or look at all? Let's do both
#Events where investigation occurred
Inv_by_Event <- Events_Summed %>% 
  dplyr::filter(Inv == "Y") %>% #consider only events where investigation occurred
  group_by(EventID) %>% #group by event ID 
  summarise(Inv_duration = sum(Total_behave_duration)) #calculate length of investigation
Inv_by_Event <- as.data.frame(Inv_by_Event) #140 events can be considered

#Create SiteID column
Inv_by_Event$SiteID <- gsub("\\_.*","",Inv_by_Event$EventID)

#To get contact by urbanization level, attach this to UrbData
Inv_by_Urb <- full_join(Urb_Levels, Inv_by_Event, by = "SiteID")
Inv_by_Urb <- na.omit(Inv_by_Urb)

#Model
Inv_by_Urb$UrbLevel <- factor(Inv_by_Urb$UrbLevel, levels = c("Park", "City"))
Explore1 <- glm(Inv_duration ~ (1|SiteID) + Urbanization, data = Inv_by_Urb)
summ(Explore1, digits = 3)



#Should I retain events in which invetsigation occurred, or look at all? Let's do both
#Events where investigation occurred + events when it didn't
EventList <- Events_Summed %>% dplyr::select(EventID) 
EventList <- unique(EventList) #458 unique events

#the dataframe Inv_by_Event includes inevstigation length for all events in which
#invetsigation occurred. Now merge it with the event list, and give 0 to other events
Inv_by_Event2 <- full_join(EventList, Inv_by_Event, by = "EventID")

#replace Nas with 0 (i.e., 0 iinvetsigate time)
Inv_by_Event2$Inv_duration[is.na(Inv_by_Event2$Inv_duration)] <- 0

#Create SiteID column
Inv_by_Event2$SiteID <- gsub("\\_.*","",Inv_by_Event2$EventID)

#To get contact by urbanization level, attach this to UrbData
Inv_by_Urb2 <- full_join(Urb_Levels, Inv_by_Event2, by = "SiteID")
Inv_by_Urb2 <- na.omit(Inv_by_Urb2)

#Now get mean investigation time
InvData <- Inv_by_Urb2 %>%
  group_by(SiteID) %>%
  summarise(MeanInvTime = mean(Inv_duration))

InvData <- as.data.frame(InvData)

InvData1 <- left_join(InvData, Urb_Levels, by = "SiteID")

#First model is for city vs park
InvMod1 <- glm(MeanInvTime ~ UrbLevel, data = InvData1)
summ(InvMod1, digits = 3)

InvData2 <- InvData1 %>%
  dplyr::filter(UrbLevel == "City")
InvMod2 <- glm(MeanInvTime ~ urbanization_score, data = InvData2)
summ(InvMod2, digits = 3)

#Make relevant plots
InvChartData1 <- InvData1 %>%
  group_by(UrbLevel) %>%
  summarise(mean_inv = mean(MeanInvTime),
            SD = sd(MeanInvTime))
InvChartData1 <- as.data.frame(InvChartData1)
ggplot(data = InvChartData1,
       mapping = aes(y=mean_inv, x=UrbLevel)) +
  geom_bar(fill="#33CCFF", stat = "identity", colour = "black") +
  theme_classic() +
  geom_errorbar(aes(ymin = mean_inv - SD, ymax = mean_inv + SD), width = 0.2) + 
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none") +
  labs(x="Urb. Level", y="Mean investigate time (s)") + 
  ggtitle("Exploration (Park vs. City)")


A <- plot_model(InvMod2, type = "pred", terms = "urbanization_score[all]", 
                colors = "#33CCFF")
#Make prettier
B <- A + theme_classic() + ggtitle("Exploration (within City)") + 
  theme(axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        plot.title = element_text(size = 12, face = "bold")) +
  labs(x = "Urbanization level", y = "Mean Investigate Time (s)")





#Repeat exlcudiung 0s
InvData3 <- Inv_by_Urb2 %>%
  dplyr::filter(Inv_duration != 0) %>%
  group_by(SiteID) %>%
  summarise(MeanInvTime = mean(Inv_duration))

InvData3 <- as.data.frame(InvData3)

InvData3 <- left_join(InvData3, Urb_Levels, by = "SiteID")

#First model is for city vs park
InvMod3 <- glm(MeanInvTime ~ UrbLevel, data = InvData3)
summ(InvMod3, digits = 3)

InvData4 <- InvData3 %>%
  dplyr::filter(UrbLevel == "City")
InvMod4 <- glm(MeanInvTime ~ urbanization_score, data = InvData4)
summ(InvMod4, digits = 3)



#Model
Inv_by_Urb2$Urbanization <- factor(Inv_by_Urb2$Urbanization, levels = c("Moderate", "Park", "Very"))
Explore2 <- lmer(Inv_duration ~ (1|SiteID) + Urbanization, data = Inv_by_Urb2)
summ(Explore2, digits = 3) #note that this is probably zero inflated


#Another way to get at this is the number of events charaterised by investigate!
#Simply summarise the number of events characterised by invetsigate at each site
Events_Summed$SiteID <- gsub("\\_.*","",Events_Summed$EventID)

Inv_by_Urb3 <- Events_Summed %>%
  dplyr::filter(Inv == "Y") %>%
  group_by(SiteID) %>%
  summarise(no.Inv = n())

Inv_by_Urb4 <- full_join(Inv_by_Urb3, Urb_Levels, by = "SiteID")
Inv_by_Urb4 <- as.data.frame(Inv_by_Urb4)
Inv_by_Urb4$no.Inv[is.na(Inv_by_Urb4$no.Inv)] <- 0
Inv_by_Urb4 <- na.omit(Inv_by_Urb4)
Inv_by_Urb4 <- left_join(Inv_by_Urb4, sitedata, by = "SiteID")


#First model is for city vs park (which needs to be corrected for no. events)
InvMod3 <- glm(no.Inv ~ UrbLevel.y, data = Inv_by_Urb4, offset = Total_Events)
summ(InvMod3, digits = 3)

Inv_by_Urb5 <- Inv_by_Urb4 %>%
  dplyr::filter(UrbLevel.y == "City")
InvMod4 <- glm(no.Inv ~ urbanization_score.y, data = Inv_by_Urb5, offset = Total_Events)
summ(InvMod4, digits = 3)




#MODEL
Inv_by_Urb4$Urbanization <- factor(Inv_by_Urb4$Urbanization, levels = c("Moderate", "Park", "Very"))
Explore3 <- glm(no.Inv ~ Urbanization, data = Inv_by_Urb4, 
                offset = log(Total_Events), family = "poisson")
summ(Explore3, digits = 3)


#Next exploration metric is the number of plastic puzzles (offset by total events)
UrbData$Urbanization <- factor(UrbData$Urbanization, levels = c("Moderate", "Park", "Very"))
UrbData1 <- UrbData %>% dplyr::filter(SiteID != "E4") 

Explore4 <- glm(Plastic ~ Urbanization, data = UrbData1, 
                offset = log(Total_Events), family = "poisson")
summ(Explore4, digits = 3)


####SHYNESS##
#First shyness variable is latency to first approach
sitedata$UrbLevel <- factor(sitedata$UrbLevel, levels = c("Park", "City"))
Shy1 <- glm(Days_To_First_Event ~ UrbLevel, data = sitedata, family = "poisson")
summ(Shy1, digits = 3)

sitedata1 <- sitedata %>% dplyr::filter(UrbLevel == "City")
Shy2 <- glm(Days_To_First_Event ~ urbanization_score, data = sitedata1, family = "poisson")
summ(Shy2, digits = 3)

#######
#######
#######

#Second shyness variables is frequency of galloping (i.e., events with gallop, offset by events)
Gallop <- Events_Summed %>%
  dplyr::filter(Behave == "lopegallop")

#Create SiteID column
Gallop$SiteID <- gsub("\\_.*","",Gallop$EventID)

Gallop2 <- Gallop %>% group_by(SiteID) %>%
  summarise(No.gallop = n()) 

Gallop2 <- as.data.frame(Gallop2) #now we have no gallop events at each site

#Join with UrbData
Gallop3 <- full_join(Gallop2, Urb_Levels, by = "SiteID") %>%
  dplyr::filter(SiteID != "E4")
Gallop3$No.gallop[is.na(Gallop3$No.gallop)] <- 0
Gallop4 <- left_join(Gallop3, sitedata, by = "SiteID")


#Model
Shy3 <- glm(No.gallop ~ UrbLevel.y, data = Gallop4, family = "poisson", 
            offset = log(Total_Events))
summ(Shy3, digits = 3)


Gallop5 <- Gallop4 %>%
  dplyr::filter(UrbLevel.y == "City")
Shy4 <- glm(No.gallop ~ urbanization_score.y + offset(log(Total_Events)), data = Gallop5, family = "poisson")
summ(Shy4, digits = 3)

#Make relevant charts
ShyChartData1 <- Gallop4 %>%
  dplyr::mutate(percgallop = No.gallop/ Total_Events*100) %>%
  group_by(UrbLevel.y) %>%
  summarise(mean_perc_gallop = mean(percgallop),
            SD = sd(percgallop))
ShyChartData1 <- as.data.frame(ShyChartData1)
ggplot(data = ShyChartData1,
       mapping = aes(y=mean_perc_gallop, x=UrbLevel.y)) +
  geom_bar(fill="#33CCFF", stat = "identity", colour = "black") +
  theme_classic() +
  geom_errorbar(aes(ymin = mean_perc_gallop - SD, ymax = mean_perc_gallop + SD), width = 0.2) + 
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none") +
  labs(x="Urb. Level", y="Mean Events with Run (%)") + 
  ggtitle("Fearfulness (Park vs. City)")


A <- plot_model(Shy4, type = "pred", terms = "urbanization_score.y[all]", 
                colors = "#33CCFF")
#Make prettier
B <- A + theme_classic() + ggtitle("Fearfulness (within City)") + 
  theme(axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        plot.title = element_text(size = 12, face = "bold")) +
  labs(x = "Urbanization level", y = "Mean contact time (s)")


#Third shyness variable is no. nocturnal events
Shy2 <- glm(Dark ~ Urbanization, data = UrbData1, family = "poisson", 
            offset = log(Total_Events))
summ(Shy2, digits = 3) #





#Next Up is problem solving
#Model number of solves offset by number of events
Solves <- Events_Summed %>%
  dplyr::filter(Behave == "solve") %>%
  group_by(EventID) %>%
  summarise(No.Solves = n())

#Create SiteID
Solves$SiteID <- gsub("\\_.*","",Solves$EventID)

Solves <- Solves %>%
  group_by(SiteID) %>%
  summarise(Total_Solves = n())

Solves1 <- full_join(Solves, sitedata, by = "SiteID")
Solves1$Total_Solves[is.na(Solves1$Total_Solves)] <- 0
Solves1 <- as.data.frame(Solves1)

Solves2 <- Solves1 %>%
  dplyr::filter(UrbLevel == "City")

SolveMod <- glm(Total_Solves ~ urbanization_score + offset(log(Total_Events)), 
                data = Solves2, family = "poisson")
summ(SolveMod, digits = 3) #



#Make relevant charts
SolveData <- Solves1 %>%
  group_by(UrbLevel) %>%
  summarise(NoSolves = sum(Total_Solves))
SolveData <- as.data.frame(SolveData)
ggplot(data = SolveData,
       mapping = aes(y=NoSolves, x=UrbLevel)) +
  geom_bar(fill="#33CCFF", stat = "identity", colour = "black") +
  theme_classic() +
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none") +
  labs(x="Urb. Level", y="No. Solves") + 
  ggtitle("Problem Solving (Park vs. City)")


A <- plot_model(SolveMod, type = "pred", terms = "urbanization_score[all]", 
                colors = "#33CCFF")
#Make prettier
B <- A + theme_classic() + ggtitle("Problem Solving (within City)") + 
  theme(axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        plot.title = element_text(size = 12, face = "bold")) +
  labs(x = "Urbanization level", y = "No. Solves (offset by total events)")




#OK, that's the end of the landscape piece. Now remove irrelevant objects
remove(Shy)
remove(Shy1)
remove(Shy2)
remove(SolveMod)
remove(Solves)
remove(Solves1)
remove(Complex_mod)
remove(Complex_mod1)
remove(Complex1)
remove(Complex2)
remove(Explore1)
remove(Explore2)
remove(Explore3)
remove(Explore4)
remove(Contact_by_Event)
remove(Contact_by_Urb)
remove(Gaits)
remove(Gaits2)
remove(Gaits3)
remove(Gallop)
remove(Gallop2)
remove(Gallop3)
remove(Inv_by_Event)
remove(Inv_by_Event2)
remove(Inv_by_Urb)
remove(Inv_by_Urb2)
remove(Inv_by_Urb3)
remove(Inv_by_Urb4)
remove(Orient)
remove(Orient2)
remove(Persistence_mod)
remove(Persistence2_mod)

#Get some stuff just for ACTWS abstract : P
#Total events
sum(LocData$Total_Events)

#try to get number of investigate, contact, solve
Events_Summed_Inv <- Events_Summed %>% dplyr::filter(Inv == "Y")
Events_Summed_Inv <- distinct(Events_Summed_Inv, EventID, .keep_all = TRUE)

Events_Summed_Contact <- Events_Summed %>% dplyr::filter(Contact == "Y")
Events_Summed_Contact <- distinct(Events_Summed_Contact, EventID, .keep_all = TRUE)

Events_Summed_Solve <- Events %>% dplyr::filter(Behave == "solve")
Events_Summed_Solve <- distinct(Events_Summed_Solve, SiteEventID, .keep_all = TRUE)

#Let's just compare the five things between urban and non urban for now
#Behavioural complexity, measured as number of different behaviours used when contacting
Contact <- Events %>% dplyr::filter(Behave == "contact_top" |
                                      Behave == "contact_bottom" |
                                      Behave == "contact_mid"|
                                      Behave == "contact_bungee"|
                                      Behave == "contact_spin")
Contact1 <- Contact %>%
  group_by(SiteEventID, Behave) %>%
  summarise(n = n())
Contact1 <- as.data.frame(Contact1)

Contact2 <- Contact1 %>%
  group_by(SiteEventID) %>%
  summarise(no.contact.behave = n())

Contact2$SiteID <- gsub("\\_.*","",Contact2$SiteEventID)
Contact2 <- as.data.frame(Contact2)

Contact3 <- left_join(Contact2, Urb_Levels, by = "SiteID")
Contact3$Urbanization <- ifelse(Contact3$Urbanization == "Park", "Wild", "City")
t.test(no.contact.behave~Urbanization, data = Contact3)
#t = 0.47188, df = 8.6224, p-value = 0.6487

#Exploration = investigate time (omit instances with no investigation)
Investigate <- Events %>% dplyr::filter(Behave == "inv_puzzle" |
                                      Behave == "inv_ground" |
                                      Behave == "inv_cam"|
                                      Behave == "inv_unk")
Investigate1 <- Investigate %>%
  group_by(SiteEventID) %>%
  summarise(Total.Inv.Time = sum(Behave_Duration))
Investigate1 <- as.data.frame(Investigate1)

Investigate1$SiteID <- gsub("\\_.*","",Investigate1$SiteEventID)
Investigate1 <- as.data.frame(Investigate1)

Investigate2 <- left_join(Investigate1, Urb_Levels, by = "SiteID")
Investigate2$Urbanization <- ifelse(Investigate2$Urbanization == "Park", "Wild", "City")
t.test(Total.Inv.Time~Urbanization, data = Investigate2)
#t = 0.22061, df = 30.062, p-value = 0.8269


# Persistence = duration of contact time
Contact4 <- Contact %>%
  group_by(SiteEventID) %>%
  summarise(Total.Cont.Time = sum(Behave_Duration))
Contact4 <- as.data.frame(Contact4)

Contact4$SiteID <- gsub("\\_.*","",Contact4$SiteEventID)
Contact4 <- as.data.frame(Contact4)

Contact5 <- left_join(Contact4, Urb_Levels, by = "SiteID")
Contact5$Urbanization <- ifelse(Contact5$Urbanization == "Park", "Wild", "City")
t.test(Total.Cont.Time~Urbanization, data = Contact5)
# t = 1.5072, df = 23.94, p-value = 0.1448


#Fear/ shyness
LocData$Urbanization <- ifelse(LocData$Letter == "E", "Wild", "City")
t.test(Days_To_First_Event~Urbanization, data = LocData)
#t = -2.7015, df = 22.058, p-value = 0.01302


#problem solving
Solve <- Events_Summed_Solve %>%
  group_by(SiteID) %>%
  summarise(no.solves = n())

##########I'll haver to do that but with urbanization as a continuous
PC <- YEGData2 %>%
  dplyr::select(SiteID, development)
#No contact behaviuors (behavioural complexity)
Contact6 <- inner_join(PC, Contact3, by = "SiteID")
ComplexityMod <- glm(no.contact.behave ~ development, data = Contact6, family = "poisson")
summary(ComplexityMod) #b = Events_Summed_Solve, P = 0.794

#Investigate (exploratopnm)
Investigate3 <- inner_join(PC, Investigate2, by = "SiteID")
InvestigateMod <- glm(Total.Inv.Time ~ development, data = Investigate3)
summary(InvestigateMod) #b = 5.835      , P = 0.0642 

#persistence
Persistence3 <- inner_join(PC, Contact5, by = "SiteID")
PersistenceMod <- glm(Total.Cont.Time ~ development, data = Persistence3)
summary(PersistenceMod) #b = 3.534      , P = 0.254686 

#Shyness
LocData2 <- inner_join(PC, LocData, by = "SiteID")
ShynessMod <- glm(Days_To_First_Event ~ development, data = LocData2, family = "poisson")
summary(ShynessMod) #b =  -3.531      , P = 0.000414  

#problem solving
Solve2 <- left_join(PC, Solve, by = "SiteID")
Solve2$no.solves[is.na(Solve2$no.solves)] <- 0

SolveMod <- glm(no.solves ~ development, data = Solve2, family = "poisson")
summary(SolveMod) #b =  0.3925      , P = 0.04423  



#Now, let's work on the event level analysis

#Begin with the effect of EXTRINSIC variables on each of your five things
#Extrinsic variables are as follows: urbanization level, darkness, puzzle type, group size, whether it is the first puzzle

#Start by ensuring you have all this info for every event
#Make a column that contains both event AND individual
Events$EventIDSubject <- paste(Events$EventID, Events$Subject, sep = "_")

#Reads in data file containing independent variables for each event
Ind <- read.csv("All_Independent_Vars.csv")
Ind <- Ind %>%dplyr::rename("EventID" = Identifier2)

#Join event info and Independent variables
Event_Data <- full_join(Events, Ind, by = "EventID")

#Remove one null event
Event_Data <- Event_Data %>%
  dplyr::filter(EventID != "NW6_EVENT37_W")

#Clean this up
Event_Data <- Event_Data %>%
  dplyr::select(EventID, Date, SiteEventID, Behave, SiteID.x, Behave_Duration, 
                Subject, EventIDSubject, PuzzleType.y, LorD, GroupSize, agesex,
                Disease, PuzzleSequence) %>%
  dplyr::rename("SiteID" = SiteID.x,
                "PuzzleType" = PuzzleType.y)

#Add urbanization level (first extrinsic variable)
Event_Data <- full_join(Event_Data, Urb_Levels, by = "SiteID")

#Create a dataframe that includes ONLY EventIDSubject and related info
Event_Data_Red <- Event_Data %>%
  dplyr::select(EventIDSubject, PuzzleType, LorD, GroupSize, agesex, Disease, 
                PuzzleSequence, Urbanization, SiteID, Subject)

Event_Data_Red <- unique(Event_Data_Red)

#Darkness, puzzle type, and group size, and sequence are already included in the df

#Create list of events (by subject)
EventSubjectList <- Event_Data %>% dplyr::select(EventIDSubject)
EventSubjectList <- unique(EventSubjectList) #539 events


#Metric 1 = PERSISTENCE
#The first outcome variable here is the length of time the animal contacts the puzzle
Event_Data_Summed <- Event_Data %>% group_by(EventIDSubject, Behave) %>% 
  summarise(Total_behave_duration = sum(Behave_Duration))

Event_Data_Summed <- as.data.frame(Event_Data_Summed)
Event_Data_Summed$Contact <- "N"
Event_Data_Summed$Contact <- ifelse(Event_Data_Summed$Behave == "contact_top", "Y", Event_Data_Summed$Contact)
Event_Data_Summed$Contact <- ifelse(Event_Data_Summed$Behave == "contact_bottom", "Y", Event_Data_Summed$Contact)
Event_Data_Summed$Contact <- ifelse(Event_Data_Summed$Behave == "contact_mid", "Y", Event_Data_Summed$Contact)
Event_Data_Summed$Contact <- ifelse(Event_Data_Summed$Behave == "contact_bungee", "Y", Event_Data_Summed$Contact)
Event_Data_Summed$Contact <- ifelse(Event_Data_Summed$Behave == "contact_spin", "Y", Event_Data_Summed$Contact)

E_Contact <- Event_Data_Summed %>%
  dplyr::filter(Contact == "Y") %>%
  dplyr::select(EventIDSubject, Total_behave_duration) %>%
  group_by(EventIDSubject) %>%
  summarise(Contact_Duration = sum(Total_behave_duration))

#Note: Here, we are ONLY interested in events where some contact occurred! SO this df is complete
#Join this df with the DF with all the other variables

E_Contact2 <- full_join(E_Contact, Event_Data_Red, by = "EventIDSubject") %>%
  na.omit(E_Contact2)
E_Contact2 <- as.data.frame(E_Contact2)

#Create model explaining persistence as a function of each extrinsic variable
E_Contact2$Urbanization <- factor(E_Contact2$Urbanization, levels = c("Moderate", "Park", "Very"))
E_Contact2$PuzzleSequence <- ifelse(E_Contact2$PuzzleSequence == 0, 1, E_Contact2$PuzzleSequence)

PersMod1 <- lmer(Contact_Duration ~ (1|SiteID/Subject) + Urbanization, data = E_Contact2)
summ(PersMod1, digits = 3)

PersMod2 <- lmer(Contact_Duration ~ (1|SiteID/Subject) + PuzzleType, data = E_Contact2)
summ(PersMod2, digits = 3)

PersMod3 <- lmer(Contact_Duration ~ (1|SiteID/Subject) + LorD, data = E_Contact2)
summ(PersMod3, digits = 3)

PersMod4 <- lmer(Contact_Duration ~ (1|SiteID/Subject) + GroupSize, data = E_Contact2)
summ(PersMod4, digits = 3)

PersMod5 <- lmer(Contact_Duration ~ (1|SiteID/Subject) + PuzzleSequence, data = E_Contact2)
summ(PersMod5, digits = 3)

#All of these models generate a notice about singular fit, which suggests that the models
#are overfitted. These warnings dissapear if I remove the complicated randome effect
#Leave for now, but keep in mind for future

#Remove unecessry dataframes
remove(E_Contact)
remove(E_Contact2)
remove(PersMod1)
remove(PersMod2)
remove(PersMod3)
remove(PersMod4)
remove(PersMod5)



#Second outcome variable to test is Behavioural complexity.
#First metric is number of behaviours exhibited while contacting the puzzle 
#Restrict to events with Contact

#Start with event data summed
BCData <- Event_Data_Summed %>%
  dplyr::filter(Contact == "Y") %>%
  group_by(EventIDSubject) %>%
  summarise(No.Contact.Behave = n())

BCData <- as.data.frame(BCData)

#Join to Event Data Red
BCData1 <- left_join(BCData, Event_Data_Red, by = "EventIDSubject")

#Create model explaining behavioural complexity as a function of each extrinsic variable
BCData1$Urbanization <- factor(BCData1$Urbanization, levels = c("Moderate", "Park", "Very"))
BCData1$PuzzleSequence <- ifelse(BCData1$PuzzleSequence == 0, 1, BCData1$PuzzleSequence)

BCMod1 <- glmer(No.Contact.Behave ~ (1|SiteID/Subject) + Urbanization, family = "poisson", data = BCData1)
summ(BCMod1, digits = 3)

BCMod2 <- glmer(No.Contact.Behave ~ (1|SiteID/Subject) + PuzzleType, family = "poisson", data = BCData1)
summ(BCMod2, digits = 3)

BCMod3 <- glmer(No.Contact.Behave ~ (1|SiteID/Subject) + LorD, family = "poisson", data = BCData1)
summ(BCMod3, digits = 3)

BCMod4 <- glmer(No.Contact.Behave ~ (1|SiteID/Subject) + GroupSize, family = "poisson", data = BCData1)
summ(BCMod4, digits = 3)

BCMod5 <- glmer(No.Contact.Behave ~ (1|SiteID/Subject) + PuzzleSequence, family = "poisson", data = BCData1)
summ(BCMod5, digits = 3)

#Same thing as before with singular fits!


#Try a second metric of behavioural complexity = number of gait changes
BCData3 <- Event_Data %>%
  dplyr::filter(Behave == "walk" | Behave == "trot" | Behave == "still" | Behave == "lopegallop") %>%
  group_by(EventIDSubject) %>%
  summarise(No.Gaits = n())
  

#Join to Event Data Red
BCData4 <- left_join(BCData3, Event_Data_Red, by = "EventIDSubject")
BCData4 <- as.data.frame(BCData4)

#Create model explaining behavioural complexity as a function of each extrinsic variable
BCData4$Urbanization <- factor(BCData4$Urbanization, levels = c("Moderate", "Park", "Very"))
BCData4$PuzzleSequence <- ifelse(BCData4$PuzzleSequence == 0, 1, BCData4$PuzzleSequence)
BCData4$LorD <- ifelse(BCData4$LorD == "N", "D", BCData4$LorD)

BC1Mod1 <- glmer(No.Gaits ~ (1|SiteID/Subject) + Urbanization, family = "poisson", data = BCData4)
summ(BC1Mod1, digits = 3)

BC1Mod2 <- glmer(No.Gaits ~ (1|SiteID/Subject) + PuzzleType, family = "poisson", data = BCData4)
summ(BC1Mod2, digits = 3)

BC1Mod3 <- glmer(No.Gaits ~ (1|SiteID/Subject) + LorD, family = "poisson", data = BCData4)
summ(BC1Mod3, digits = 3)

BC1Mod4 <- glmer(No.Gaits ~ (1|SiteID/Subject) + GroupSize, family = "poisson", data = BCData4)
summ(BC1Mod4, digits = 3)

BC1Mod5 <- glmer(No.Gaits ~ (1|SiteID/Subject) + PuzzleSequence, family = "poisson", data = BCData4)
summ(BC1Mod5, digits = 3)


#Remove unecessary dataframes
remove(BCData)
remove(BCData1)
remove(BCMod1)
remove(BCMod2)
remove(BCMod3)
remove(BCMod4)
remove(BCMod5)

remove(BCData3)
remove(BCData4)
remove(BC1Mod1)
remove(BC1Mod2)
remove(BC1Mod3)
remove(BC1Mod4)
remove(BC1Mod5)

#Next metric is Exploration. The first way we'll measure that is investigate time (omit 0s)
Event_Data_Summed$Inv <- "N"
Event_Data_Summed$Inv <- ifelse(Event_Data_Summed$Behave == "inv_cam", "Y", Event_Data_Summed$Inv)
Event_Data_Summed$Inv <- ifelse(Event_Data_Summed$Behave == "inv_puzzle", "Y", Event_Data_Summed$Inv)
Event_Data_Summed$Inv <- ifelse(Event_Data_Summed$Behave == "inv_ground", "Y", Event_Data_Summed$Inv)
Event_Data_Summed$Inv <- ifelse(Event_Data_Summed$Behave == "inv_unk", "Y", Event_Data_Summed$Inv)

IData <- Event_Data_Summed %>%
  dplyr::filter(Inv == "Y") %>%
  group_by(EventIDSubject) %>%
  summarise(Inv_Length = sum(Total_behave_duration))

IData1 <- left_join(IData, Event_Data_Red, by = "EventIDSubject")

#Create model explaining investigate time as a function of each extrinsic variable
IData1$Urbanization <- factor(IData1$Urbanization, levels = c("Moderate", "Park", "Very"))
IData1$PuzzleSequence <- ifelse(IData1$PuzzleSequence == 0, 1, IData1$PuzzleSequence)

IMod1 <- lmer(Inv_Length ~ (1|SiteID/Subject) + Urbanization, data = IData1)
summ(IMod1, digits = 3)

IMod2 <- lmer(Inv_Length ~ (1|SiteID/Subject) + PuzzleType, data = IData1)
summ(IMod2, digits = 3)

IMod3 <- lmer(Inv_Length ~ (1|SiteID/Subject) + LorD, data = IData1)
summ(IMod3, digits = 3)

IMod4 <- lmer(Inv_Length ~ (1|SiteID/Subject) + GroupSize, data = IData1)
summ(IMod4, digits = 3)

IMod5 <- lmer(Inv_Length ~ (1|SiteID/Subject) + PuzzleSequence, data = IData1)
summ(IMod5, digits = 3)


#The second way we'll measure that is investigate time (include 0s)
IData2 <- full_join(IData, Event_Data_Red, by = "EventIDSubject")
IData2$Inv_Length[is.na(IData2$Inv_Length)] <- 0


#Create model explaining investigate time as a function of each extrinsic variable
IData2$Urbanization <- factor(IData2$Urbanization, levels = c("Moderate", "Park", "Very"))
IData2$PuzzleSequence <- ifelse(IData2$PuzzleSequence == 0, 1, IData2$PuzzleSequence)

IData2$Urbanization <- factor(IData2$Urbanization, levels = c("Moderate", "Park", "Very"))
IData2$PuzzleSequence <- ifelse(IData2$PuzzleSequence == 0, 1, IData2$PuzzleSequence)
IData2$LorD <- ifelse(IData2$LorD == "N", "D", IData2$LorD)

I1Mod1 <- lmer(Inv_Length ~ (1|SiteID/Subject) + Urbanization, data = IData2)
summ(I1Mod1, digits = 3)

I1Mod2 <- lmer(Inv_Length ~ (1|SiteID/Subject) + PuzzleType, data = IData2)
summ(I1Mod2, digits = 3)

I1Mod3 <- lmer(Inv_Length ~ (1|SiteID/Subject) + LorD, data = IData2)
summ(I1Mod3, digits = 3)

I1Mod4 <- lmer(Inv_Length ~ (1|SiteID/Subject) + GroupSize, data = IData2)
summ(I1Mod4, digits = 3)

I1Mod5 <- lmer(Inv_Length ~ (1|SiteID/Subject) + PuzzleSequence, data = IData2)
summ(I1Mod5, digits = 3)



#The third way we'll look at this is whether or not investigation occurred (binary)
IData3 <- IData2
IData3$InvYV <- IData3$Inv_Length
IData3$InvYV <- ifelse(IData3$Inv_Length == 0, 0, 1)

#Model
I2Mod1 <- glmer(InvYV ~ (1|SiteID/Subject) + Urbanization, data = IData3, family = "binomial")
summ(I2Mod1, digits = 3)

I2Mod2 <- glmer(InvYV ~ (1|SiteID/Subject) + PuzzleType, data = IData3, family = "binomial")
summ(I2Mod2, digits = 3)

I2Mod3 <- glmer(InvYV ~ (1|SiteID/Subject) + LorD, data = IData3, family = "binomial")
summ(I2Mod3, digits = 3)

I2Mod4 <- glmer(InvYV ~ (1|SiteID/Subject) + GroupSize, data = IData3, family = "binomial")
summ(I2Mod4, digits = 3)

I2Mod5 <- glmer(InvYV ~ (1|SiteID/Subject) + PuzzleSequence, data = IData3, family = "binomial")
summ(I2Mod5, digits = 3)

#remove unecessary dudes
remove(I1Mod1)
remove(I1Mod2)
remove(I1Mod3)
remove(I1Mod4)
remove(I1Mod5)

remove(I2Mod1)
remove(I2Mod2)
remove(I2Mod3)
remove(I2Mod4)
remove(I2Mod5)

remove(IMod1)
remove(IMod2)
remove(IMod3)
remove(IMod4)
remove(IMod5)

remove(IData)
remove(IData1)
remove(IData2)
remove(IData3)


#Next up, we wanto to look at effects of extrinsic variables on our fear metrics
#These are (1) Time spent galloping (indication of stress), offset by time visible
# (2) binary: whether or not galloping is observed

#1 time spent galloping
#Start with Events_Summed
EventTime <- Events %>%
  dplyr::filter(Behave == "close" | Behave == "far") %>%
  dplyr::mutate(inview = Behave_Duration) %>%
  group_by(EventIDSubject) %>%
  summarise(EventTime = sum(inview))

EventTime <- as.data.frame(EventTime)

GallopTime <- Events %>%
  dplyr::filter(Behave == "lopegallop") %>%
  group_by(EventIDSubject) %>%
  summarise(GallopTime = sum(Behave_Duration))
GallopTime <- as.data.frame(GallopTime)

Gallop <- full_join(EventTime, GallopTime, by = "EventIDSubject")

Gallop1 <- full_join(Gallop, EventSubjectList, by = "EventIDSubject")
Gallop1$GallopTime[is.na(Gallop1$GallopTime)] <- 0

Gallop2 <- full_join(Gallop1, Event_Data_Red, by = "EventIDSubject")

#Model
Gallop2$Urbanization <- factor(Gallop2$Urbanization, levels = c("Moderate", "Park", "Very"))
Gallop2$PuzzleSequence <- ifelse(Gallop2$PuzzleSequence == 0, 1, Gallop2$PuzzleSequence)

GMod1 <- lmer(GallopTime ~ (1|SiteID/Subject) + Urbanization, data = Gallop2, offset = EventTime)
summ(GMod1)

GMod2 <- lmer(GallopTime ~ (1|SiteID/Subject) + PuzzleType, data = Gallop2, offset = EventTime)
summ(GMod2)

GMod3 <- lmer(GallopTime ~ (1|SiteID/Subject) + LorD, data = Gallop2, offset = EventTime)
summ(GMod3)

GMod4 <- lmer(GallopTime ~ (1|SiteID/Subject) + GroupSize, data = Gallop2, offset = EventTime)
summ(GMod4)

GMod5 <- lmer(GallopTime ~ (1|SiteID/Subject) + PuzzleSequence, data = Gallop2, offset = EventTime)
summ(GMod5)


#Above is probably very Zi. Now try as a binomial
#Model
Gallop2$GallopYN <- ifelse(Gallop2$GallopTime == 0, 0, 1)

G1Mod1 <- glmer(GallopYN ~ (1|SiteID/Subject) + Urbanization, data = Gallop2, family = "binomial")
summ(G1Mod1)

G1Mod2 <- glmer(GallopYN ~ (1|SiteID/Subject) + PuzzleType, data = Gallop2, family = "binomial")
summ(G1Mod2)

G1Mod3 <- glmer(GallopYN ~ (1|SiteID/Subject) + LorD, data = Gallop2, family = "binomial")
summ(G1Mod3)

G1Mod4 <- glmer(GallopYN ~ (1|SiteID/Subject) + GroupSize, data = Gallop2, family = "binomial")
summ(G1Mod4)

G1Mod5 <- glmer(GallopYN ~ (1|SiteID/Subject) + PuzzleSequence, data = Gallop2, family = "binomial")
summ(G1Mod5)

#cleanup
remove(G1Mod1)
remove(G1Mod2)
remove(G1Mod3)
remove(G1Mod4)
remove(G1Mod5)

remove(GMod1)
remove(GMod2)
remove(GMod3)
remove(GMod4)
remove(GMod5)

#Last one is problem solving
Solves <- Events %>%
  dplyr::filter(Behave == "solve") %>%
  dplyr::select(EventIDSubject, Behave) %>%
  rename("Solves" = Behave)

Solves <- unique(Solves)

Solves1 <- full_join(Solves, Event_Data_Red, by = "EventIDSubject")
Solves1$Solves <- ifelse(Solves1$Solves == "solve", 1, 0)
Solves1$Solves[is.na(Solves1$Solves)] <- 0


#MODEL
Solves1$Urbanization <- factor(Solves1$Urbanization, levels = c("Moderate", "Park", "Very"))
Solves1$PuzzleSequence <- ifelse(Solves1$PuzzleSequence == 0, 1, Solves1$PuzzleSequence)
Solves1$LorD <- ifelse(Solves1$LorD == "N", "D", Solves1$LorD)

SolveMod1 <- glmer(Solves ~ (1|SiteID/Subject) + Urbanization, data = Solves1, family = "binomial")
summ(SolveMod1, digits = 3)

SolveMod2 <- glmer(Solves ~ (1|SiteID/Subject) + PuzzleType, data = Solves1, family = "binomial")
summ(SolveMod2, digits = 3)

SolveMod3 <- glmer(Solves ~ (1|SiteID/Subject) + LorD, data = Solves1, family = "binomial")
summ(SolveMod3, digits = 3)

SolveMod4 <- glmer(Solves ~ (1|SiteID/Subject) + GroupSize, data = Solves1, family = "binomial")
summ(SolveMod4, digits = 3)

SolveMod5 <- glmer(Solves ~ (1|SiteID/Subject) + PuzzleSequence, data = Solves1, family = "binomial")
summ(SolveMod5, digits = 3)

#cleanup
remove(SolveMod1)
remove(SolveMod2)
remove(SolveMod3)
remove(SolveMod4)
remove(SolveMod5)

#OK. Now do the whole thing over but with INTRINSIC variables
#These are sex, disease status, and no. previous encounters

#First, create a df that indicates the no. times an individual has appeared
EvSeq <- Event_Data_Red %>%
  dplyr::select(EventIDSubject, Subject)

EvSeq$Seq <- 1




