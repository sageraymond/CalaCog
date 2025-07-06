
rm(list = ls())
gc()

# Groundhog makes libraries consistent.
library("groundhog")
groundhog.day <- "2025-04-15"
libs <- c("metafor", "broom", "data.table",
          "ggplot2", "tidyr", "multcomp",
          "dplyr", 
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

guide <- readRDS("outputs/guide.Rds")
dat <- fread("data/blahblahblah.csv")


# Run models -------------------------------------------------------
file.remove(list.files("outputs/models/", full.names = T))

#' [This prepares the cluster]
clust_out <- prepare_cluster(n = nrow(guide))

guide
m <- c()
sub.dat <- c() #' [This might not be necessary]
i <- 1
#' *Note that some models won't run because there is insufficient N for their random effects*

success <- foreach(i = 1:nrow(guide), 
                   .options.snow = clust_out$options,
                   .errorhandling = "pass",
                   .packages = c("data.table", "glmmTMB")) %dopar% {
                     
                     # sub.dat <- dat[eval(parse(text = guide[i, ]$exclusion)), ]
                     
                     if(guide[i, ]$location_or_scale == "location"){
                       
                       m <- glmmTMB(as.formula(guide[i, ]$formula),
                                    family = eval(parse(text = guide[i, ]$model_family)),
                                    data = dat)
                       
                       
                       
                     }else{
                       
                       m <- glmmTMB(as.formula(guide[i, ]$formula),
                                    dispformula = xxx,
                                    family = eval(parse(text = guide[i, ]$model_family)),
                                    data = dat)
                       
                     }
                     
                     saveRDS(m, guide[i, ]$model_path)
                     
                     setTxtProgressBar(clust_out$progress, i)
                   }
stopCluster(clust_out$cluster)

guide[!file.exists(model_path), ]

guide[file.exists(model_path), ]

