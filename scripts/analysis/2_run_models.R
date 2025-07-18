
rm(list = ls())
gc()

library("data.table")
library("glmmTMB")
library("doParallel")
library("foreach")
library("doSNOW")
library("crayon")
library("ggplot2")

# groundhog.library(libs, groundhog.day)

# >>> Helper functions ----------------------------------------------------

prepare_cluster <- function(n){
  require("parallel")
  require("foreach")
  require("doSNOW")
  
  nCores <- parallel::detectCores() -1 
  cl <- makeCluster(nCores)
  registerDoSNOW(cl)
  
  # Progress bar
  pb <- txtProgressBar(max = n, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  ret <- list(opts, pb, cl)
  names(ret) <- c("options", "progress", "cluster")
  return(ret)
  
  cat("Pass 'x$options' to .opts in foreach;
      'x$progress' to setTxtProgressBar(x$progress, i);
      'x$cluster' to stopCluster(x$cluster) after foreach")
}

# Load guide --------------------------------------------------------------

guide <- readRDS("builds/batch_models_july_2025/model_guide.Rds")

# Prepare data ------------------------------------------------------------
dat <- fread("data/EventDataJul2025.csv")

unique(guide$model_call)
guide[location_or_scale == "location_scale"]$model_call
guide[location_or_scale == "location_scale" & zero_inflation == "yes"]$model_call

#' [Year wants to be treated categorically, not continuously]
#' *We wouldn't hypothesize a linear relationship to year would we?*
dat[, Year := as.factor(Year)]
unique(dat$Sex)
dat[, Sex := fcase(Sex == "SM", "M",
                   Sex == "U", NA,
                   Sex == "SF", "F")]

dat[, n_events_per_individual := .N, by = Subject]
dat[n_events_per_individual == 1, Subject := NA] # This will omit unknown subjects
dat

dat$n_events_per_individual <- NULL
saveRDS(dat, "builds/prepared_dataset.Rds")

# Run models -------------------------------------------------------
rerun_all <- TRUE #' [But only if you really hate them all]
if(rerun_all){
  file.remove(list.files("builds/batch_models_july_2025/models/", full.names = T))
}

# >>> Sequential version --------------------------------------------------
guide

m <- c()
# sub.dat <- c() #' [This might not be necessary]
i <- 1
#' *Note that some models won't run because there is insufficient N for their random effects*
working_guide <- guide[!file.exists(model_path), ]

# >>> Sequential version ---------------------------------------------------
warnings <- list()
sub_dat <- c()

for(i in 1:nrow(working_guide)){
  
  tryCatch(
    expr={
      
      sub_dat <- dat[eval(parse(text = working_guide[i, ]$exclusion))]
      sub_dat[, `:=` (urbanization_score_scaled = scale(urbanization_score),
                      GroupSize_scaled = scale(GroupSize),
                      temp_scaled = scale(temp),
                      SiteSequence_scaled = scale(SiteSequence))]
      # setdiff(working_guide$var, names(sub_dat))
      m <- eval(parse(text = working_guide[i, ]$model_call))
      saveRDS(m, working_guide[i, ]$model_path)
      
    },
    error=function(e){
      warnings[[i]] <- data.table(model_id = working_guide[i, ]$model_id,
                                  error = e)
      cat(red("error at"), i, "\r")
    },
    warning=function(w){ #' *there were some convergence warnings...spooky*
      # Let's store them...And well I guess drop those models? 
      warnings[[i]] <- data.table(model_id = working_guide[i, ]$model_id,
                                  warning = w)
    }
  )
  
  cat(blue(i), magenta("/"), red(nrow(working_guide)), "\r")
}

warnings

working_guide <- guide[!file.exists(model_path), ]
working_guide #about 700 didjn't run...
#' [Somehow this increased from 2 to 19 when I fixed a couple errors in the dispformula...But then I made Year a factor and it went down to 2]

# >>> Test the models that didn't run -------------------------------------
working_guide[1, ]$model_call

eval(parse(text = working_guide[1, ]$model_call))
eval(parse(text = working_guide[2, ]$model_call))

unique(working_guide$var)
unique(working_guide$response)

dat[complete.cases(Contact_duration, urbanization_score, SiteID)]
unique(dat[complete.cases(Contact_duration, urbanization_score, SiteID)]$SiteID)

hist(dat[complete.cases(Contact_duration, urbanization_score, SiteID)]$Contact_duration)

plot(dat[complete.cases(Contact_duration, urbanization_score, SiteID)]$Contact_duration ~ dat[complete.cases(Contact_duration, urbanization_score, SiteID)]$urbanization_score)

# Well small loss I reckon.
# :)


# >>> Parallel version ---------------------------------------------------
# Having a weird problem I've never had before: not finding objects in global environment...
# Strange...
#' clust_out <- prepare_cluster(n = nrow(guide))
#' #'
#' success <- foreach(i = 1:nrow(guide),
#'                    .options.snow = clust_out$options,
#'                    .errorhandling = "pass",
#'                    .packages = c("data.table", "glmmTMB")) %dopar% {
#'                     # for some reason foreach is not inheriting dat from env...
#' 
#'                      # return(guide[i, ]$model_call )
#'                      m <- eval(parse(text = guide[i, ]$model_call))
#'                      saveRDS(m, guide[i, ]$model_path)
#'                      #
#'                      setTxtProgressBar(clust_out$progress, i)
#'                    }
#' stopCluster(clust_out$cluster)
#' success
#' 
#' guide[!file.exists(model_path), ]
#' 
#' guide[file.exists(model_path), ]


