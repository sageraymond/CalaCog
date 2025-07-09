
rm(list = ls())
gc()

# Groundhog makes libraries consistent.
# groundhog.day <- "2023-04-24"
# libs <- c("broom", "data.table",
#           "ggplot2", "tidyr", "multcomp",
#           "dplyr", "crayon",
#           "glmmTMB",
#           "cpp11", "withr", "colorspace", "mvtnorm",
#           "foreach", "doSNOW")
library("data.table")
library("glmmTMB")
library("doParallel")
library("foreach")
library("doSNOW")
library("crayon")

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

dat <- fread("data/EventDataJul2025.csv")

guide[, model_path := paste0("builds/batch_models_july_2025/models/", model_id, ".Rds")]
guide$model_path[1:10]
# >>> Create ELEGANT executable call in guide -----------------------------
#' [instead of multiple if statements for location/scale or ziformula and family, let's create a single call to execute]

guide[, model_call := paste0("glmmTMB(", 
                             formula, ", ",
                             ifelse(zero_inflation == "yes", "ziformula = ~ ., ", ""), 
                             ifelse(location_or_scale == "location_scale", paste0("dispformula = ", dispformula, ", "), ""),
                             "family=", model_family, ", ",
                             "data = dat)")]
unique(guide$model_call)
guide[location_or_scale == "location_scale"]$model_call
guide[location_or_scale == "location_scale" & zero_inflation == "yes"]$model_call

# Run models -------------------------------------------------------
file.remove(list.files("builds/batch_models_july_2025/models/", full.names = T))

#' [This prepares the cluster]

guide
sub.guide <- c()

m <- c()
# sub.dat <- c() #' [This might not be necessary]
i <- 1
#' *Note that some models won't run because there is insufficient N for their random effects*
working_guide <- guide[!file.exists(model_path), ]

#' [SEQUENTIAL VERSION:]
for(i in 1:nrow(working_guide)){

  tryCatch(
    expr={
      m <- eval(parse(text = working_guide[i, ]$model_call))
      saveRDS(m, working_guide[i, ]$model_path)
    },
    error=function(e){
      cat(red("error at"), i, "\r")
    }
  )

  cat(blue(i), "/", red(nrow(working_guide)), "\r")
}

working_guide <- guide[!file.exists(model_path), ]
working_guide
# x <- "5 + 10"
# eval(parse(text = x))
#' glmmTMB(Behav_Complexity ~ 1 + (1|SiteID), family=poisson(), data = dat)
#' 
#' eval(parse(text="glmmTMB(Behav_Complexity ~ 1 + (1|SiteID), family=poisson(), data = dat)" ))
#' 
#' 
#' clust_out <- prepare_cluster(n = nrow(guide))
#' #' [PARALLEL VERSION:]
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

