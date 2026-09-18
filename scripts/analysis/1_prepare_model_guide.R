# July 15th 2026
#
# 
#' *Prepare model guide*
#
#
#
# Prepare workspace ---------------------------------------
#
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



# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
#  Create full extent model guide ------------------------------------
#read in dat for reference
dat <- readRDS("data/EventDataJul2025.Rds")

guide <- CJ(response = c("Contact_duration", "Inv_duration", "Behav_Complexity", "Lope", "Solves",
                         "Contact", "Inv"),
            var = c("urbanization", "Nat50", "Nat50_scaled"),
            subject_id = c("yes", "no"))

# >>> Create formulas -----------------------------------------------------

#Create null model formulas
guide[, null_model_formula := paste(response, "~ 1")]

#Create urb formulas
guide[, urb_formula := paste(response, "~", var)]
guide



# >>> Specify model family -----------------------------------------
#' *From 0_data_exploration.R script:*

#' [Behav_Complexity == Poisson]
#' [Lope == binomial]
#' [Solves == binomial]
#' [Contact == binomial]
#' [Inv == binomial]
#' [Contact_duration == lognormal]
#' [Inv_duration == lognormal]
#' 

unique(guide$response)

guide[response == "Contact_duration", model_family := "lognormal()"]
guide[response == "Inv_duration", model_family := "lognormal()"]
guide[response == "Behav_Complexity", model_family := "poisson()"]
guide[response == "Lope", model_family := "binomial(link = 'logit')"]
guide[response == "Solves", model_family := "binomial(link = 'logit')"]
guide[response == "Contact", model_family := "binomial(link = 'logit')"]
guide[response == "Inv", model_family := "binomial(link = 'logit')"]

unique(guide$model_family)


#Add dat
guide[var == "urbanization", dat := "dat_all"]
guide[var == "Nat50", dat := "dat_city"]
guide[var == "Nat50_scaled", dat := "dat_city"]


# >>> Add model comparison IDs --------------------------------------------

guide[, model_complexity_comparison_ID := paste0("model_complexity_id_",
                                                 seq(1:.N))]

guide[model_complexity_comparison_ID == "model_complexity_id_40"]


# >>> Make the data long --------------------------------------------------
guide.long <- melt(guide,
                   measure.vars = c("null_model_formula",
                                    "urb_formula"),
                   variable.name = "model_type",
                   value.name = "formula")

guide.long

# Drop the NA models:
guide.long <- guide.long[!is.na(formula), ]
guide.long

guide.long[, model_type := gsub("_formula", "", model_type)]

# >>> Add an individual model ID ------------------------------------------

guide.long[, model_id := paste0("model_", seq(1:.N))]
guide.long

guide.long[, model_path := paste0("outputs/models/", model_id, ".Rds")]
guide.long



# >>> Add random effects to formulas --------------------------------------
guide.long[subject_id == "yes", formula := paste0(formula, " + (1|SiteID/Subject)")]

guide.long[subject_id == "no", formula := paste0(formula, " + (1|SiteID)")]
unique(guide.long$formula)


# >>> Test that formulas are correctly formed -----------------------------

for(i in 1:nrow(guide.long)){
  as.formula(guide.long[i, ]$formula)
} # This will errror out if there's a syntactical mistake

x <- c()
for(i in 1:nrow(guide.long)){
  x <- dat[eval(parse(text = guide.long[i, ]$exclusion)), ]
  if(nrow(x) == 0){
    print("Watch it buddy")
  }
}

# >>> Create ELEGANT executable call in guide -----------------------------
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
saveRDS(guide.long, "builds/model_guide_main.Rds")

