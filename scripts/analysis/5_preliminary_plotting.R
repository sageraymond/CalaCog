
rm(list = ls())
gc()

# Groundhog makes libraries consistent.
library("groundhog")
groundhog.day <- "2025-04-15"
libs <- c("metafor", "broom", "broom.mixed",
          "data.table",
          "ggplot2", "tidyr", "multcomp",
          "dplyr", "crayon", 
          "glmmTMB",
          "cpp11", "withr", "colorspace", "mvtnorm",
          "foreach", "doSNOW")
groundhog.library(libs, groundhog.day)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 0. Load data and guide --------------------------------------------------

master_guide <- readRDS("builds/batch_models_july_2025/model_guide_with_comparison_stats.Rds")

dat <- readRDS("builds/prepared_dataset.Rds")
dat

# Just a test model:
m <- readRDS("builds/batch_models_july_2025/models/model_6481.Rds")
summary(m)

# Encapsulate some functions --------------------------------------

unscale <- function(vec1, vec2){
  # vec1 is the vector to unscale (usually part of the prediction output)
  # vec2 is the scaled vector from the dataset
  scalar <- attr(vec2, "scaled:scale")
  center <- attr(vec2, "scaled:center")
  
  unscaled <- vec1 * scalar + center
  
  return(unscaled)
}

#' *This will be for model coefficients*
tidy_glmmTMB <- function(m){
  m.tidy <- tidy(m)
  setDT(m.tidy)
  
  if(length(unique(m.tidy$component)) > 1){
    m.tidy[, key := paste(component, term, sep = ".")]
  }else{
    m.tidy[, key := term]
  }
  m.tidy
  
  cis <- confint(m) |> as.data.frame()
  cis$key <- row.names(cis)
  setDT(cis)
  setnames(cis, c("2.5 %", "97.5 %"), c("lwr_CI", "upper_CI"))
  cis
  
  cis[key %in% m.tidy$key, ]
  m.tidy.mrg <- merge(m.tidy,
                      cis,
                      by = "key",
                      all.x = T,
                      all.y = F)
  m.tidy.mrg
  return(m.tidy.mrg)
  
}


#' *This will be for model predictions, both for categorical and for continuous*
#' You can input a grid of new data for all the variables involved...
pred_glmmTMB <- function(m, newgrid){
  
  newgrid <- cbind(newgrid,
                   predict(m, newgrid, re.form = NA, allow.new.levels = TRUE,
                          se.fit = TRUE) |> as.data.frame())
  
  #' [ I think this is how this is done? ]
  newgrid[, lwr_ci := fit - (se.fit * qnorm(0.975))]
  newgrid[, upper_ci := fit + (se.fit * qnorm(0.975))]
  
  return(newgrid)
}


# Extract coefficients with CIs --------------------------------------------------------
#' [Let's plot the coefficients + CIs for cases when the extrinsic/intrinsic factor was significant.]
sub_guide <- master_guide[var %in% c("urbanization")]
sub_guide

#' [You understand lapply right? It took me forever to understand it.]
#' *it's basically a compact for loop. It applies the 'FUN' for each element in the first argument 
ms <- lapply(sub_guide$model_path_univariate,
                      FUN=readRDS)
ms.tidy <- lapply(ms,
                  FUN=tidy_glmmTMB)
ms.tidy
names(ms.tidy) <- sub_guide$model_id_univariate

ms.tidy <- rbindlist(ms.tidy, idcol = "model_id_univariate")
ms.tidy
# The random effect parameters are NA for CIs because I didn't feel like munging the text...

#' [You could do this for ALL models and put them all in a giant supplementary table. If you felt like that.]

#
ms.tidy <- ms.tidy[effect != "ran_pars"]
ms.tidy[term == "cond.urbanizationWild", ]
# Why are these NA...

ms.tidy.mrg <- merge(sub_guide[, .(model_path_univariate, var, response, 
                                   sensitivity_analysis, subject_id,
                                   model_id_univariate,
                                   exclusion)], 
                    ms.tidy, 
                     by = "model_id_univariate",
                     all.x = T,
                     all.y = F)
ms.tidy.mrg

#' [The problem with plotting this though is that it's difficult to convert intercepts]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------------
# Coefficient plot of primary hypotheses ---------------------------------------------

sub.coeffs <- ms.tidy.mrg[sensitivity_analysis == "all_data" &
                            subject_id == "no", ]

sub.coeffs[, Habitat := ifelse(term == "(Intercept)", "City", "Wild")]

sub.coeffs[, sig := ifelse(p.value < 0.05, "sig", "not sig")]

ggplot()+
  geom_hline(yintercept = 0)+
  geom_pointrange(data = sub.coeffs[component == "cond"], aes(x = Habitat, ymin = lwr_CI, ymax = upper_CI,
                                         y = Estimate, color = Habitat, group = Habitat))+
  facet_wrap(~response, scales = "free", ncol = 2)+
  theme_bw()+
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())
  
#' [There's an issue with GIANT CIs for Solves and Wild...Hmmmm. Might need to do some investigation]

# ziformulas:
ggplot()+
  geom_hline(yintercept = 0)+
  geom_pointrange(data = sub.coeffs[component == "zi"], aes(x = Habitat, ymin = lwr_CI, ymax = upper_CI,
                                                              y = Estimate, color = Habitat, group = Habitat))+
  facet_wrap(~response, scales = "free", ncol = 2)+
  theme_bw()+
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())


# Alternative approach: plot predictions ----------------------------------
#' [this approach will allow us to plot the univariates...But it's going to be harder to do this for say ALL models.]
#' *cause you have to specify the new data for each model*

master_guide

sub_guide
sub_guide <- master_guide[model_id_urbanization == "model_7363"]
m <- readRDS(sub_guide$model_path_urbanization)
summary(m)

# Having scaled variables is so fucking annoying. That's one thing here...Goddamnit
sub_dat <- dat[eval(parse(text = sub_guide$exclusion)), ]
sub_dat[, pop_density_scaled := scale(pop_density)]
sub_dat

# Create grid of new data:
newgrid <- CJ(Disease = unique(dat$Disease),
              pop_density_scaled = seq(min(sub_dat$pop_density_scaled),
                                       max(sub_dat$pop_density_scaled), by = .1))
newgrid

# Now use prediction function:
pred <- pred_glmmTMB(m = m,
             newgrid = newgrid)
pred

# Now, unscale the pop_density
pred[, pop_density := unscale(vec1 = pop_density_scaled, vec2 = sub_dat$pop_density_scaled)]

p <- ggplot()+
  geom_ribbon(data = pred, aes(x = pop_density, 
                               ymin = lwr_ci,
                               ymax = upper_ci,
                               fill = Disease),
              alpha = .5)+
  geom_line(data = pred, aes(x = pop_density, 
                             y = fit,
                             color = Disease))+
  geom_jitter(data = sub_dat, aes(x = pop_density,
                                  color = Disease,
                                  y = Contact_duration))+
  ylab("Contact duration")+
  theme_bw()
p

p + coord_cartesian(ylim = c(0, 5))
# Hmmm. Hahah. Not a very pretty one lol. But hopefully you can use this code?