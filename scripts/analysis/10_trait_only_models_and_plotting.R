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


#Make model guide-
guide <- CJ(rand_ef = c("(1|SiteID)", "(1|SiteID/Subject)"),
            vars = c("Inv_duration", "Contact_duration", "Lope", "Behav_Complexity"),
            scaled = c("yes", "no"),
            extend = c("city", "all"))
#Note, i've left contact and inv out because you can't get a solve with those being 0, so that would mess models up

guide$response <- "Solves"

#add dat
guide[extend == "all", dat := "dat_all"]
guide[extend == "city", dat := "dat_city"]


#Modify the vars column based on scaling
guide$vars_final <- guide$vars

guide[scaled == "yes" & vars == "Behav_Complexity", vars_final := "Behav_Complexity_scaled"]
guide[scaled == "yes" & vars == "Inv_duration", vars_final := "Inv_duration_scaled"]
guide[scaled == "yes" & vars == "Contact_duration", vars_final := "Contact_duration_scaled"]


#Create null model formulas
guide[, behav_formula := paste(response, "~", vars_final)]

guide


guide[, model_complexity_comparison_ID := paste0("model_complexity_id_",
                                                 seq(1:.N))]

guide.long <- guide

# >>> Add an individual model ID ------------------------------------------

guide.long[, model_id := paste0("model_", seq(1:.N))]
guide.long

#add model path
guide.long[, model_path := paste0("outputs/behav_only_models/", model_id, ".Rds")]
guide.long

#specify dataset
guide.long[, comparison_data := NA_character_]

# >>> Create  executable call in guide -----------------------------
guide.long[, model_call := paste0("glmmTMB(", 
                                  behav_formula, " + ",
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
ms <- lapply(guide.long$model_path,
             FUN=readRDS)
ms.tidy <- lapply(ms,
                  FUN=perform_function)
ms.tidy
names(ms.tidy) <- guide.long$model_id

ms.tidy <- rbindlist(ms.tidy, idcol = "model_id", fill = TRUE)
ms.tidy


#Inrtegrate into model guide
performance <- merge(guide.long,
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

ms <- lapply(guide.long$model_path,
             FUN=readRDS)
ms.tidy <- lapply(ms,
                  FUN=get_coefficients)
ms.tidy
names(ms.tidy) <- guide.long$model_id

ms.tidy <- rbindlist(ms.tidy, idcol = "model_id", fill = TRUE)
ms.tidy


#Inrtegrate into model guide
coefficients <-  merge(ms.tidy,
                       performance,
                       by = "model_id",
                       all.x = T)

#Eveyrhting i need, coefs and perd metrics are in one table, called coeffcieitns

#remove sensitvity analysis dudes
table <- coefficients[rand_ef != "(1|SiteID/Subject)"]

table <- as.data.frame(table)
table <- table %>%
  dplyr::select(beta, p, lwr, upr, vars, scaled, extend, n_obs, AIC, R2_marginal, R2_conditional )
table <- table %>% dplyr::rename("Response" = vars,
                                 "Extent" = extend)


setDT(table)

table$order <- "XXX"
table[, order := fcase(Response == "Behav_Complexity", "1",
                            Response == "Contact_duration", "3",
                            Response == "Inv_duration", "2",
                            Response == "Lope", "4",
                            default = order)]

table[, Response := fcase(Response == "Behav_Complexity", "Behavioural diversity",
                               Response == "Contact_duration", "Contact duration",
                               Response == "Inv_duration", "Investigate duration",
                               Response == "Lope", "Escape gait",
                               default = Response)]

table[, Extent := fcase(Extent == "city", "City",
                             Extent == "all", "All",
                             default = Extent)]


#OK. I need to get effect sizes now
table$link <- paste0(table$Response, table$Extent)

scaled <- table[scaled =="yes"]
unscaled <- table[scaled =="no"]

unscaled <- as.data.frame(unscaled)

#limit to relevant columns
unscaled <- unscaled %>% dplyr::select(link, beta, lwr, upr)

#calculate effect sizes
unscaled <- unscaled %>%
  dplyr::mutate(es = exp(beta),
                es_low_ci = exp(lwr),
                es_upp_ci = exp(upr))

#Looks good. now remove dudes you don't need
setDT(unscaled)
unscaled$beta <- NULL
unscaled$lwr <- NULL
unscaled$upr <- NULL


#scaled
scaled
scaled$scaled <- NULL

#Now merge these buddies back together 
model_info <- merge(scaled,
                                   unscaled,
                                   by = "link",
                                   all.x = T)
model_info$link <- NULL

write.csv(model_info, file = "figures/TableS6_behaviour_solving.csv")




#Plot this. 
rm(list = ls())
gc()

dat <- read.csv("figures/TableS6_behaviour_solving.csv")


#I think a coefficient plot makes most sense here
dat
dat$Extent <- factor(dat$Extent, levels = c("City", "All"))
dat$Response <- factor(dat$Response, levels = c("Escape gait",
                                                "Contact duration",
                                                "Investigate duration",
                                                "Behavioural diversity"))



#Inv plot
plot <- ggplot(data = dat,
                  aes(x = Response, y = B, color = Extent, shape = Extent))+
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = Low.CI, ymax = Upp..CI),
                width = 0.2, position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "A. Trait-Only Models",
       y = "Estimate", x = "Trait") +
  scale_color_manual(values = c(
    "All" = "#D00000",
    "City" = "#D00000"
  )) +
  scale_shape_manual(values = c(
    "All" = 19,
    "City" = 17
  )) +
  coord_flip()+
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
       # legend.position = "none",
        strip.text = element_text(size = 12, face = "plain"))
plot




#OK, now bring in final model stuff
dat2 <- read.csv("figures/TableS7_behav_urb_coefficients.csv")

dat2$plot_order <- with(dat2, paste(Model, Term))

dat2$plot_order <- factor(
  dat2$plot_order,
  levels = c(
    "Escape gait Escape gait",
    "Escape gait Urbanization (%)",
    "Contact duration Contact duration",
    "Contact duration Urbanization (%)",
    "Investigate duration Investigate duration",
    "Investigate duration Urbanization (%)",
    "Behavioural diversity Behavioural diversity",
    "Behavioural diversity Urbanization (%)"))

dat2$plot_label <- ifelse(
  dat2$Term == "Urbanization (%)",
  "    Urbanization (%)",
  as.character(dat2$Model)
)


plot2 <- ggplot(dat2,
       aes(x = plot_order, y = B, color = Term, shape = Term)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = Low.CI, ymax = Upp..CI),
                width = 0.2,
                size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "B. Trait + Urbanization Models",
       y = "Estimate",
       x = "Trait") +
  scale_color_manual(values = c(
    "Urbanization (%)" = "#78206E",
    "Behavioural diversity" = "#D00000",
    "Investigate duration" = "#D00000",
    "Contact duration" = "#D00000",
    "Escape gait" = "#D00000"
  )) +
  scale_shape_manual(values = c(
    "Urbanization (%)" = 19,
    "Behavioural diversity" = 17,
    "Investigate duration" = 17,
    "Contact duration" = 17,
    "Escape gait" = 17
  )) +
  scale_x_discrete(
    limits = rev(levels(dat2$plot_order)),
    labels = rev(dat2$plot_label)
  ) +
  coord_flip() +
  theme(
    axis.text.x = element_text(colour = "black", face = "plain", size = 12),
    axis.text.y = element_text(colour = "black", face = "plain", size = 12),
    axis.title.y = element_text(colour = "black", face = "plain", size = 12),
    legend.text = element_text(colour = "black", face = "plain", size = 12),
    legend.title = element_blank(),
    strip.text = element_text(size = 12, face = "plain")
  )


plot2


#Combine these plots
combined_plot <- ggarrange(plot, plot2, nrow = 1)

ggsave(combined_plot, file = "figures/Fig4.pdf", width = 12, height = 5)
