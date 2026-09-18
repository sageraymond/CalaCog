
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

guide <- readRDS("builds/model_guide_Main.Rds")

# Prepare data ------------------------------------------------------------
dat <- readRDS("data/EventDataJul2025.Rds")

dat$Nat50_scaled <-scale(dat$Nat50, scale = TRUE, center = TRUE)

dat_all <- dat
dat_city <- dat %>% dplyr::filter(urbanization == "City")
remove(dat)


# Run models -------------------------------------------------------
rerun_all <- TRUE #' [But only if you really hate them all]
if(rerun_all){
  file.remove(list.files("outputs/models/", full.names = T))
}

# >>> Sequential version --------------------------------------------------

guide

m <- c()
# sub.dat <- c() #' [This might not be necessary]
i <- 1
#' *Note that some models won't run because there is insufficient N for their random effects*
working_guide <- guide[!file.exists(model_path), ] #this is if you want to run everybpdy

warnings <- list()
#sub_dat <- c()

#' [Make sure to have all variables scaled inside loop:]
#' [We could be more clever and only scale the variables that are in the formula...But it's a pain in the butt]
#unique(guide[grepl("scaled", var)]$var)
#unique(guide[grepl("scaled", var)]$urbanization_var)

#' [the first time I run this I commented out the tryCatch business so we can see what errors are being thrown]

for(i in 1:nrow(working_guide)){
  tryCatch(
    expr={
      # setdiff(working_guide$var, names(sub_dat))
      m <- eval(parse(text = working_guide[i, ]$model_call))
      
      if(!is.null(m)) saveRDS(m, working_guide[i, ]$model_path)
      
      m <- NULL # just in case...
      
    },
    error=function(e){
      warnings[[i]] <<- data.table(model_id = working_guide[i, ]$model_id,
                                   error = e)
      cat(red("error at"), i, "\r")
    },
    warning=function(w){ #' *there were some convergence warnings...spooky*
      # Let's store them...And well I guess drop those models?
      warnings[[i]] <<- data.table(model_id = working_guide[i, ]$model_id,
                                   warning = w)
      cat(blue(i))
    }
    
  )
  
  cat(blue(i), magenta("/"), red(nrow(working_guide)), "\r")
}

names(warnings) <- guide$model_id
# Hmmm.


working_guide <- guide[!file.exists(model_path), ]
nrow(working_guide) # these are the models that didn't run; it's 2


# >>> Test the models that didn't run -------------------------------------
working_guide$model_call
working_guide$model_path

# I will run these puppies manually; in both cases i understand these errors, and they're unpronlematic
m1 <- glmmTMB(Inv_duration ~ Nat50 + (1|SiteID/Subject), family=lognormal(), data = dat_city)
m2 <- glmmTMB(Solves ~ urbanization + (1|SiteID/Subject), family=binomial(link = 'logit'), data = dat_all)

# saveRDS(m1, file = "outputs/models/model_68.Rds")
# saveRDS(m2, file = "outputs/models/model_84.Rds")
