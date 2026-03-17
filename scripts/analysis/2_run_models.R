
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
dat[, Year := as.factor(Year)]
unique(dat$Sex)
dat$Sex <- ifelse(dat$Sex == "SF", "F", dat$Sex)
dat$Sex <- ifelse(dat$Sex == "SM", "M", dat$Sex)
dat$Sex <- ifelse(dat$Sex == "U", NA, dat$Sex)

dat[, n_events_per_individual := .N, by = Subject]
dat[n_events_per_individual == 1, Subject := NA] # This will omit unknown subjects
dat

dat$n_events_per_individual <- NULL

#Set wild as reference category
dat$urbanization <- factor(dat$urbanization, levels = c("Wild", "City"))
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
working_guide <- guide[!file.exists(model_path), ] #this is if you want to run everybpdy

warnings <- list()
sub_dat <- c()

#' [Make sure to have all variables scaled inside loop:]
#' [We could be more clever and only scale the variables that are in the formula...But it's a pain in the butt]
unique(guide[grepl("scaled", var)]$var)
unique(guide[grepl("scaled", var)]$urbanization_var)

#' [the first time I run this I commented out the tryCatch business so we can see what errors are being thrown]

for(i in 1:nrow(working_guide)){
  tryCatch(
    expr={
      
      sub_dat <- dat[eval(parse(text = working_guide[i, ]$exclusion))]
      #' [Need to score the new variables too...]
      sub_dat[, `:=` (urbanization_score_scaled = scale(urbanization_score),
                      GroupSize_scaled = scale(GroupSize),
                      temp_scaled = scale(temp),
                      SiteSequence_scaled = scale(SiteSequence),
                      ANTH_scaled = scale(ANTH),
                      NAT_scaled = scale(NAT),
                      Nat100_scaled = scale(Nat100),
                      Nat250_scaled = scale(Nat250),
                      Nat50_scaled = scale(Nat50),
                      Road.density_scaled = scale(Road.density),
                      pop_density_scaled = scale(pop_density))]
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
nrow(working_guide) # these are the models that didn't run

# >>> Test the models that didn't run -------------------------------------
working_guide[1, ]$model_call

sub_dat <- dat[eval(parse(text = working_guide[1, ]$exclusion))]
eval(parse(text = working_guide[1, ]$model_call))

sub_dat <- dat[eval(parse(text = working_guide[2, ]$exclusion))]
eval(parse(text = working_guide[2, ]$model_call))


# >>> Screen model objects that didn't converge ---------------------------

m$pdHess

m <- readRDS(guide[1, ]$model_path)
m$sdr$pdHess

guide <- guide[file.exists(model_path), ]

for(i in 1:nrow(guide)){
  m <- readRDS(guide[i, ]$model_path)
  
  guide[i, model_converged := m$sdr$pdHess]
  cat(i, "/", nrow(guide), "\r")
}

#' *should have done this in the loop above...Next time...*
guide[model_converged == FALSE, ]

# Well that's good news. Everybody ran!!

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


