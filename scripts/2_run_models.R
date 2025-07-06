
rm(list = ls())
gc()

# Groundhog makes libraries consistent.
library("groundhog")
groundhog.day <- "2025-04-15"
libs <- c("metafor", "broom", "data.table",
          "ggplot2", "tidyr", "multcomp",
          "dplyr", "crayon",
          "glmmTMB",
          "cpp11", "withr", "colorspace", "mvtnorm",
          "foreach", "doSNOW")
groundhog.library(libs, groundhog.day)

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

dat <- fread("data/blahblahblah.csv")

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
clust_out <- prepare_cluster(n = nrow(guide))

guide
sub.guide <- c()

m <- c()
# sub.dat <- c() #' [This might not be necessary]
i <- 1
#' *Note that some models won't run because there is insufficient N for their random effects*

#' [SEQUENTIAL VERSION:]
# for(i in 1:nrow(guide)){
#   
#   tryCatch(
#     expr={
#       m <- eval(parse(text = guide[i, ]$model_call))
#       saveRDS(m, guide[i, ]$model_path)
#     }
#   )
#   
#   cat(blue(i), "/", red(nrow(guide)), "\r")
# }


# x <- "5 + 10"
# eval(parse(text = x))


#' [PARALLEL VERSION:]
success <- foreach(i = 1:nrow(guide), 
                   .options.snow = clust_out$options,
                   .errorhandling = "pass",
                   .packages = c("data.table", "glmmTMB")) %dopar% {
                     
                     m <- eval(parse(text = guide[i, ]$model_call))
                     saveRDS(m, guide[i, ]$model_path)
                     
                     setTxtProgressBar(clust_out$progress, i)
                   }
stopCluster(clust_out$cluster)

guide[!file.exists(model_path), ]

guide[file.exists(model_path), ]

