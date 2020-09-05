# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comparison of MICE & AmeliaII for imputation of longitudinal data ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# To ADD #######################################################################

# interrupted time series (treatment can change slope/SD at specified t)
# curvilinear relationships
# pick top 3 most frequent missing data patterns from SIHS data

# HOUSEKEEPING #################################################################

library(data.table)
library(mice)
library(randomForest)
library(Amelia)
library(colortools)
library(here)       # make relative filepaths on both Mac & PC; avoids setwd()

# ::: source data merging functions ####
source(here('src', 'merging_functions.r'))
source(here('src', 'multiple_imputation_functions.r'))

# FUNCTIONS ####################################################################

# ::: fake data generator function ####
fake.data <- function(beta = 12, icept = 201 + 23, e.mean = 0, e.sd = 20, 
                      miss.ind, n.pts = 10)
  {
  # generates fake longitudinal data linearly, with noise
  # produces one vector of complete data & one vector with missing data (per miss.ind)
  
  # beta.........slope (change )
  # icept........intercept (value at t=0)
  # e.sd.........standard deviation of error
  # e.mean.......mean of error
  # miss.ind.....indicator (vector) for whether the observation is missing
  # n.pts........number of time points
  
  vec <- c(icept + beta*0:9 + rnorm(n= n.pts, mean=e.mean, sd = e.sd))
  
  colnames.vec <- paste0('t', 1:n.pts)
  names(vec) <- colnames.vec
  vec.na <- ifelse(miss.ind == 1, NA, vec)
  rbind(vec = vec, vec.na = vec.na)
}

undebug(fake.data)
fake.data(miss.ind = rep(c(0, 1), each = 5))
fake.data(miss.ind = rep(c(0, 1), times = 5))

# ::: create fake data vectors ####
fd.out <- mapply(fake.data, 
                 beta = c(12, 12, 36, 36), 
                 icept = c(201, 201, 224, 224), 
                 e.sd = rep(10, 4),
                 miss.ind = rep(list(c(1, rep(0, 9)), c(rep(1, 6), rep(0, 4))), 2), SIMPLIFY = F)


# ::: add fake data vectors to jfk.W ####
# ::: : extract vec.na's from fd.out ####
fd.list <- lapply(1:length(fd.out), function(x)fd.out[[x]]['vec.na',])


# ::: function to add fake data to data.table ####
fd.add <- function(fd.list, dt, cohort=201314){
  # browser()
  n <- length(fd.list)
  dt2 <- merge(dt, data.table(stu_id = paste0('s', 1:n), 
                              cohort9 = rep(cohort, n)), 
               by = c('stu_id', 'cohort9'), all = T)
  colnames.vec <- names(dt2)[sapply(names(fd.list[[1]]), grep, x = names(dt2))]
  for(i in 1:n){
    dt2[stu_id == paste0('s', i), (colnames.vec) := as.list(fd.list[[i]])]
  }
  print(dt2)
  
  
}

# ::: data plotting function ####
my.plot <- function(dt, ids, ...){
  
  # browser()
  
  col.vec <- unlist(sapply(c('ante', 'post'), function(x)grep(x, names(dt))))
  matplot(t(dt[stu_id %in% ids, col.vec, with = F]), 
          type = 'b', xaxt = 'n', ylab = 'nss_spr_M', ...)
  axis(1, at = 1:(ncol(dt[, col.vec, with = F])), labels = sapply(colnames(jfk.W[, col.vec, with = F]), function(x)substr(x, 5, nchar(x))))
  title(xlab = 'Years from first 9th grade value')
}