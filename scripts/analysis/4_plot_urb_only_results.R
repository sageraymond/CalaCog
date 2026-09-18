#
# 
# AIM: plot the urbanization only models, including their raw data
#

rm(list = ls())
gc()

#libraries
library(glmmTMB)
library(ggplot2)
library(data.table)
library(DHARMa)
library(dplyr)
library(broom.mixed)
library(sjPlot)
library(egg)
library(performance)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 0. Load data and guide --------------------------------------------------
master_guide <- readRDS("builds/model_guide_main.Rds")

#get rid of sensitivity analysis
master_guide <- master_guide[subject_id == "no"]

#also get rid of stuff that is scaled--we want real and raw values
master_guide <- master_guide[var != "Nat50_scaled"]


#read in dat
dat <- readRDS("data/EventDataJul2025.Rds")

dat$Nat50_scaled <-scale(dat$Nat50, scale = TRUE, center = TRUE)

dat_all <- dat
dat_city <- dat %>% dplyr::filter(urbanization == "City")
remove(dat)



# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------


# >>> Alternative ---------------------------------------------------
# Plot actual data summaries to enable meta-analysis and jitter points to impress
dat_all2 <- dat_all

dat2_all.mlt <- melt(dat_all2,
                 measure.vars = c("Contact_duration", "Inv_duration", "Behav_Complexity",
                                  "Solves", "Lope", "Contact", "Inv"))
dat2_all.mlt
unique(dat2_all.mlt$value)
dat2_all.mlt$value <- as.numeric(dat2_all.mlt$value)

#

summaries <- dat2_all.mlt[, .(mean_val = mean(value, na.rm = T),
                          sd_val = sd(value, na.rm = T)),
                      by = .(variable, urbanization)]
summaries[, ymax := mean_val + sd_val]
summaries[, ymin := mean_val - sd_val]
summaries[ymin < 0, ymin := 0]


#fact check this
dat_all %>% group_by(urbanization, Solves) %>% summarise(n = n())
dat_all %>% group_by(urbanization, Inv) %>% summarise(n = n()) #looks good


# Plot
f_all_contact_duration_plot <- ggplot() +
  # geom_boxplot()+
  geom_jitter(data = dat2_all.mlt[variable == "Contact_duration"], 
              aes(x = urbanization, y = (value), fill = urbanization),
              position = position_jitter(width = .15, height = 0),
              # stroke = 5,
              shape = 21, alpha = .5, size = 3)+
  geom_pointrange(data = summaries[variable == "Contact_duration"], 
                  aes(x = urbanization, y = mean_val,
                      ymin = ymin, ymax = ymax,
                      fill = urbanization),
                  shape = 21,
                  lwd = 1,
                  stroke = 1,
                  size = 1.5)+
  #coord_cartesian(ylim = c(0, 100))+
  
  # geom_pointrange(data = predicted_all[response == "Contact_duration"],
  #                 aes(x = urbanization, ymin = lower, ymax = upper,
  #                     fill = urbanization,
  #                     y = predicted),
  #                 shape = 21,
  #                 lwd = 1,
  #                 stroke = 1,
  #                 size = 1.5)+
  # coord_cartesian(ylim = c(0, 75))+
  # geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
# facet_wrap(~ variable, scales = "free_y", ncol = 1) +
scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0, face = "plain", size = 12),
    strip.background = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    legend.position = "none"
  ) +
  labs(
    x = "Urbanization category",
    y = "Contact duration (s)"
  ) +ggtitle("F. Contact duration (n = 231)")

f_all_contact_duration_plot


# Plot
d_all_inv_duration_plot <- ggplot() +
  # geom_boxplot()+
  geom_jitter(data = dat2_all.mlt[variable == "Inv_duration"], 
              aes(x = urbanization, y = (value), fill = urbanization),
              position = position_jitter(width = .15, height = 0),
              # stroke = 5,
              shape = 21, alpha = .5, size = 3)+
  geom_pointrange(data = summaries[variable == "Inv_duration"], 
                  aes(x = urbanization, y = mean_val,
                      ymin = ymin, ymax = ymax,
                      fill = urbanization),
                  shape = 21,
                  lwd = 1,
                  stroke = 1,
                  size = 1.5)+
  #coord_cartesian(ylim = c(0, 100))+
  
  # geom_pointrange(data = predicted_all[response == "Contact_duration"],
  #                 aes(x = urbanization, ymin = lower, ymax = upper,
  #                     fill = urbanization,
  #                     y = predicted),
  #                 shape = 21,
  #                 lwd = 1,
  #                 stroke = 1,
  #                 size = 1.5)+
  # coord_cartesian(ylim = c(0, 75))+
# geom_col(position = position_dodge(width = 0.8), width = 0.6) +
# geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
# facet_wrap(~ variable, scales = "free_y", ncol = 1) +
scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0, face = "plain", size = 12),
    strip.background = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    legend.position = "none"
  ) +
  labs(
    x = "Urbanization category",
    y = "Investigate duration (s)"
  ) +ggtitle("D. Investigate duration (n = 486)")

d_all_inv_duration_plot




# Plot
b_all_behav_div_plot <- ggplot() +
  # geom_boxplot()+
  geom_jitter(data = dat2_all.mlt[variable == "Behav_Complexity"], 
              aes(x = urbanization, y = (value), fill = urbanization),
              position = position_jitter(width = .15, height = .1),
              # stroke = 5,
              shape = 21, alpha = .5, size = 3)+
  geom_pointrange(data = summaries[variable == "Behav_Complexity"], 
                  aes(x = urbanization, y = mean_val,
                      ymin = ymin, ymax = ymax,
                      fill = urbanization),
                  shape = 21,
                  lwd = 1,
                  stroke = 1,
                  size = 1.5)+
  coord_cartesian(ylim = c(1, 5))+
  
  # geom_pointrange(data = predicted_all[response == "Contact_duration"],
  #                 aes(x = urbanization, ymin = lower, ymax = upper,
  #                     fill = urbanization,
  #                     y = predicted),
  #                 shape = 21,
  #                 lwd = 1,
  #                 stroke = 1,
  #                 size = 1.5)+
  # coord_cartesian(ylim = c(0, 75))+
# geom_col(position = position_dodge(width = 0.8), width = 0.6) +
# geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.8)) +
# facet_wrap(~ variable, scales = "free_y", ncol = 1) +
scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0, face = "plain", size = 12),
    strip.background = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    legend.position = "none"
  ) +
  labs(
    x = "Urbanization category",
    y = "Behavioural diversity"
  ) +ggtitle("B. Behavioural diversity (n = 231)")

b_all_behav_div_plot



#Move on to binary chaps-----------------------------------------------------
binary_variables <- dat2_all.mlt[variable%in% c("Solves", "Lope", "Inv", "Contact")]
binary_variables[, total_tries := .N, by = .(variable, urbanization)]
binary_variable_summary <- binary_variables[, .(total_events = sum(value)),
                                            by = .(total_tries, variable, urbanization)]

binary_variable_summary[, proportion := total_events / total_tries]
binary_variable_summary

a_all_SolvesPlot <- ggplot() +
  # geom_boxplot()+
  # geom_jitter(data = xxxxxx, 
  #             aes(x = urbanization, y = (value), fill = urbanization),
  #             position = position_jitter(width = .15, height = 0),
  #             # stroke = 5,
  #             shape = 21, alpha = .5, size = 3)+
  geom_col(data = binary_variable_summary[variable == "Solves"], 
           aes(x = urbanization, y = proportion,
               fill = urbanization),
           shape = 21,
           lwd = 1,
           stroke = 1)+
  geom_text(data = binary_variable_summary[variable == "Solves"],
            aes(x = urbanization, y = proportion + 0.005,
                label = paste(total_events, "/", total_tries)))+
  coord_cartesian(ylim = c(0, .06))+
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0, face = "plain", size = 12),
    strip.background = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    legend.position = "none"
  ) +
  labs(
    x = "Urbanization Category",
    y = "Solution rate"
  ) +ggtitle("A. Solution (n = 1,156)")

a_all_SolvesPlot

g_all_LopePlot <- ggplot() +
  # geom_boxplot()+
  # geom_jitter(data = xxxxxx, 
  #             aes(x = urbanization, y = (value), fill = urbanization),
  #             position = position_jitter(width = .15, height = 0),
  #             # stroke = 5,
  #             shape = 21, alpha = .5, size = 3)+
  geom_col(data = binary_variable_summary[variable == "Lope"], 
           aes(x = urbanization, y = proportion,
               fill = urbanization),
           shape = 21,
           lwd = 1)+
  geom_text(data = binary_variable_summary[variable == "Lope"],
            aes(x = urbanization, y = proportion + 0.005,
                label = paste(total_events, "/", total_tries)))+
  #  coord_cartesian(ylim = c(0, .06))+
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0, face = "plain", size = 12),
    strip.background = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    legend.position = "none"
  ) +
  labs(
    x = "Urbanization Category",
    y = "Escape gait rate"
  ) +ggtitle("G. Escape Gait (n = 1,156)")

g_all_LopePlot


#ContaCT plot
e_all_ContactPlot <- ggplot() +
  # geom_boxplot()+
  # geom_jitter(data = xxxxxx, 
  #             aes(x = urbanization, y = (value), fill = urbanization),
  #             position = position_jitter(width = .15, height = 0),
  #             # stroke = 5,
  #             shape = 21, alpha = .5, size = 3)+
  geom_col(data = binary_variable_summary[variable == "Contact"], 
           aes(x = urbanization, y = proportion,
               fill = urbanization),
           shape = 21,
           lwd = 1)+
  geom_text(data = binary_variable_summary[variable == "Contact"],
            aes(x = urbanization, y = proportion + 0.005,
                label = paste(total_events, "/", total_tries)))+
  #  coord_cartesian(ylim = c(0, .06))+
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0, face = "plain", size = 12),
    strip.background = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    legend.position = "none"
  ) +
  labs(
    x = "Urbanization Category",
    y = "Contact rate"
  ) +ggtitle("E. Contact (n = 1,156)")

e_all_ContactPlot



#ContaCT plot
c_all_inv_plot <- ggplot() +
  # geom_boxplot()+
  # geom_jitter(data = xxxxxx, 
  #             aes(x = urbanization, y = (value), fill = urbanization),
  #             position = position_jitter(width = .15, height = 0),
  #             # stroke = 5,
  #             shape = 21, alpha = .5, size = 3)+
  geom_col(data = binary_variable_summary[variable == "Inv"], 
           aes(x = urbanization, y = proportion,
               fill = urbanization),
           shape = 21,
           lwd = 1)+
  geom_text(data = binary_variable_summary[variable == "Inv"],
            aes(x = urbanization, y = proportion + 0.005,
                label = paste(total_events, "/", total_tries)))+
  #  coord_cartesian(ylim = c(0, .06))+
  scale_fill_manual(values = c("Wild" = "#FDC213", "City" = "#78206E")) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0, face = "plain", size = 12),
    strip.background = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    legend.position = "none"
  ) +
  labs(
    x = "Urbanization Category",
    y = "Investigation rate"
  ) +ggtitle("C. Investigation (n = 1,156)")

c_all_inv_plot



#City only plots---------------------------------------------------------------

#Statrt by selecting the relevant models
city_guide <- master_guide[var == "Nat50"]
city_guide <- city_guide[model_type == "urb"]

dat_city




#Make function to generate model predioctions
make_prediction_table <- function(model_guide,
                                  n = 100,
                                  re.form = NA,
                                  se.fit = TRUE) {
  
  pred_list <- vector("list", nrow(model_guide))
  
  for(i in seq_len(nrow(model_guide))) {
    
    guide <- model_guide[i]
    
    #Read model
    mod <- readRDS(guide$model_path)
    
    # get data used to fir the model
    dat <- mod$frame
    
    #this is our predictor of interest (there\'s only one)
    focal <- guide$var
    
    ## Sequence across observed range
    newdat <- data.frame(
      x = seq(
        min(dat[[focal]], na.rm = TRUE),
        max(dat[[focal]], na.rm = TRUE),
        length.out = n
      )
    )
    
    names(newdat) <- focal
    
    ## Predictions
    pred <- predict(
      mod,
      newdata = newdat,
      type = "response",
      se.fit = se.fit,
      re.form = re.form
    )
    
    if(se.fit){
      
      newdat$fit <- pred$fit
      newdat$se <- pred$se.fit
      newdat$lwr <- pred$fit - 1.96 * pred$se.fit
      newdat$upr <- pred$fit + 1.96 * pred$se.fit
      
    } else {
      
      newdat$fit <- pred
      
    }
    
    ## Add metadata
    newdat$response <- guide$response
    newdat$model_id <- guide$model_id
    newdat$model_type <- guide$model_type
    newdat$model_complexity_comparison_ID <-
      guide$model_complexity_comparison_ID
    
    pred_list[[i]] <- as.data.table(newdat)
  }
  
  rbindlist(pred_list, fill = TRUE)
}


predictions <- make_prediction_table(city_guide)
predictions #lloks good


dat_city$Solves <- as.numeric(dat_city$Solves)
dat_city <- dat_city %>% dplyr::rename("Solves_old" = Solves)
dat_city <- dat_city %>% dplyr::mutate(Solves = Solves_old -1)

dat_city$Inv <- as.numeric(dat_city$Inv)
dat_city <- dat_city %>% dplyr::rename("Inv_old" = Inv)
dat_city <- dat_city %>% dplyr::mutate(Inv = Inv_old -1)

dat_city$Contact <- as.numeric(dat_city$Contact)
dat_city <- dat_city %>% dplyr::rename("Contact_old" = Contact)
dat_city <- dat_city %>% dplyr::mutate(Contact = Contact_old -1)

dat_city$Lope <- as.numeric(dat_city$Lope)
dat_city <- dat_city %>% dplyr::rename("Lope_old" = Lope)
dat_city <- dat_city %>% dplyr::mutate(Lope = Lope_old -1)


# Plot
h_city_solve_plot <- ggplot(predictions[response == "Solves"], aes(x = Nat50, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#9F4E4A", alpha = 0.2) +
  geom_jitter(data= dat_city, aes(x = Nat50, y = Solves),
              height = 0, width = .1) +
  geom_line(color = "#78206E", size = 1) +
  facet_wrap(~ response, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    axis.text.x = element_text(colour = "black", face = "plain", size = 12),
    axis.text.y = element_text(colour = "black", face = "plain", size = 12),
    axis.title.x = element_text(colour = "black", face = "plain", size = 12),
    axis.title.y = element_text(colour = "black", face = "plain", size = 12),
    legend.text = element_text(colour = "black", face = "plain", size = 12),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "Urbanization (City sites only; %)",
    y = "Predicted Value"
  ) + ggtitle("A. Solutions (n = 996)")
h_city_solve_plot


# Plot
i_city_BehavDiv_plot <- ggplot(predictions[response == "Behav_Complexity"], aes(x = Nat50, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#9F4E4A", alpha = 0.2) +
  geom_jitter(data= dat_city, aes(x = Nat50, y = Behav_Complexity),
              height = 0, width = .1) +
  geom_line(color = "#78206E", size = 1) +
 facet_wrap(~ response, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    axis.text.x = element_text(colour = "black", face = "plain", size = 12),
    axis.text.y = element_text(colour = "black", face = "plain", size = 12),
    axis.title.x = element_text(colour = "black", face = "plain", size = 12),
    axis.title.y = element_text(colour = "black", face = "plain", size = 12),
    legend.text = element_text(colour = "black", face = "plain", size = 12),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  ylim(0,5) +
  labs(
    x = "Urbanization (City sites only; %)",
    y = "Predicted Value"
  ) + ggtitle("B. Behavioural diversity (n = 220)")
i_city_BehavDiv_plot


# Plot
j_city_inv_plot <- ggplot(predictions[response == "Inv"], aes(x = Nat50, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#9F4E4A", alpha = 0.2) +
  geom_jitter(data= dat_city, aes(x = Nat50, y = Inv),
              height = 0, width = .1) +
  geom_line(color = "#78206E", size = 1) +
  facet_wrap(~ response, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    axis.text.x = element_text(colour = "black", face = "plain", size = 12),
    axis.text.y = element_text(colour = "black", face = "plain", size = 12),
    axis.title.x = element_text(colour = "black", face = "plain", size = 12),
    axis.title.y = element_text(colour = "black", face = "plain", size = 12),
    legend.text = element_text(colour = "black", face = "plain", size = 12),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  ylim(0, 1) +
  labs(
    x = "Urbanization (City sites only; %)",
    y = "Predicted Value"
  ) + ggtitle("C. Investigation (n = 996)")
j_city_inv_plot



# Plot
k_city_invdur_plot <- ggplot(predictions[response == "Inv_duration"], aes(x = Nat50, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#9F4E4A", alpha = 0.2) +
  geom_jitter(data= dat_city, aes(x = Nat50, y = Inv_duration),
              height = 0, width = .1) +
  geom_line(color = "#78206E", size = 1) +
  facet_wrap(~ response, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    axis.text.x = element_text(colour = "black", face = "plain", size = 12),
    axis.text.y = element_text(colour = "black", face = "plain", size = 12),
    axis.title.x = element_text(colour = "black", face = "plain", size = 12),
    axis.title.y = element_text(colour = "black", face = "plain", size = 12),
    legend.text = element_text(colour = "black", face = "plain", size = 12),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  ylim(0, 250) +
  labs(
    x = "Urbanization (City sites only; %)",
    y = "Predicted Value"
  ) + ggtitle("D. Investigate duration (n = 458)")
k_city_invdur_plot


# Plot
l_city_contact_plot <- ggplot(predictions[response == "Contact"], aes(x = Nat50, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#9F4E4A", alpha = 0.2) +
  geom_jitter(data= dat_city, aes(x = Nat50, y = Contact),
              height = 0, width = .1) +
  geom_line(color = "#78206E", size = 1) +
  facet_wrap(~ response, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    axis.text.x = element_text(colour = "black", face = "plain", size = 12),
    axis.text.y = element_text(colour = "black", face = "plain", size = 12),
    axis.title.x = element_text(colour = "black", face = "plain", size = 12),
    axis.title.y = element_text(colour = "black", face = "plain", size = 12),
    legend.text = element_text(colour = "black", face = "plain", size = 12),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  ylim(0,1) +
  labs(
    x = "Urbanization (City sites only; %)",
    y = "Predicted Value"
  ) + ggtitle("E. Contact (n = 996)")
l_city_contact_plot


# Plot
m_city_contactdur_plot <- ggplot(predictions[response == "Contact_duration"], aes(x = Nat50, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#9F4E4A", alpha = 0.2) +
  geom_jitter(data= dat_city, aes(x = Nat50, y = Contact_duration),
              height = 0, width = .1) +
  geom_line(color = "#78206E", size = 1) +
  facet_wrap(~ response, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    axis.text.x = element_text(colour = "black", face = "plain", size = 12),
    axis.text.y = element_text(colour = "black", face = "plain", size = 12),
    axis.title.x = element_text(colour = "black", face = "plain", size = 12),
    axis.title.y = element_text(colour = "black", face = "plain", size = 12),
    legend.text = element_text(colour = "black", face = "plain", size = 12),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  ylim(0,150) +
  labs(
    x = "Urbanization (City sites only; %)",
    y = "Predicted Value"
  ) + ggtitle("F. Contact duration (n = 220)")
m_city_contactdur_plot
max(dat_city$Contact_duration, na.rm = TRUE)

# Plot
n_city_escapegait_plot <- ggplot(predictions[response == "Lope"], aes(x = Nat50, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#9F4E4A", alpha = 0.2) +
  geom_jitter(data= dat_city, aes(x = Nat50, y = Lope),
              height = 0, width = .1) +
  geom_line(color = "#78206E", size = 1) +
  facet_wrap(~ response, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    axis.text.x = element_text(colour = "black", face = "plain", size = 12),
    axis.text.y = element_text(colour = "black", face = "plain", size = 12),
    axis.title.x = element_text(colour = "black", face = "plain", size = 12),
    axis.title.y = element_text(colour = "black", face = "plain", size = 12),
    legend.text = element_text(colour = "black", face = "plain", size = 12),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  ylim(0, 1) +
  labs(
    x = "Urbanization (City sites only; %)",
    y = "Predicted Value"
  ) + ggtitle("G. Escape gait (n = 996)")
n_city_escapegait_plot



#Conbine the 5 combined all sites plots
A <- arrangeGrob(a_all_SolvesPlot, h_city_solve_plot, nrow = 1, widths = c(2,2))
grid::grid.draw(A)

B <- arrangeGrob(b_all_behav_div_plot, i_city_BehavDiv_plot, nrow = 1, widths = c(2,2))
grid::grid.draw(B)

C <- arrangeGrob(c_all_inv_plot, j_city_inv_plot, nrow = 1, widths = c(2,2))
grid::grid.draw(C)

D <- arrangeGrob(d_all_inv_duration_plot, k_city_invdur_plot, nrow = 1, widths = c(2,2))
grid::grid.draw(D)

E <- arrangeGrob(e_all_ContactPlot, l_city_contact_plot, nrow = 1, widths = c(2,2))
grid::grid.draw(E)

F <- arrangeGrob(f_all_contact_duration_plot, m_city_contactdur_plot, nrow = 1, widths = c(2,2))
grid::grid.draw(F)

G <- arrangeGrob(g_all_LopePlot, n_city_escapegait_plot, nrow = 1, widths = c(2,2))
grid::grid.draw(G)



Finallot <- arrangeGrob(A, B, C, D, E, F, G, ncol = 1)
grid::grid.draw(Finallot)

#ggsave("figures/Fig2_urb_only_effects.pdf", Finallot, width = 6.5, height = 13.5, dpi = 700,  bg = "white") 


#OK, and for SI:

plots <- arrangeGrob(h_city_solve_plot, i_city_BehavDiv_plot, j_city_inv_plot,
                     k_city_invdur_plot, l_city_contact_plot, m_city_contactdur_plot,
                    n_city_escapegait_plot, nrow = 4)



grid::grid.draw(plots)
Finallot <- arrangeGrob(plots, ncol = 1)


ggsave("figures/FigS1_urb_city_only_raw_data.pdf", Finallot, width = 8, height = 13.5, dpi = 700,  bg = "white") 


