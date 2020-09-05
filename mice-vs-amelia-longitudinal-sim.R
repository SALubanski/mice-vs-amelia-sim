# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comparison of MICE & AmeliaII for imputation of longitudinal data ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# TO ADD #######################################################################

# ::: p1
# streamline data creation process for more units (miss_mat & unit_mat)

# ::: p2
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

# HARD-CODED VARIABLES #########################################################
n_pts <- 10    # number of time points
n_units <- 100 # number of units

# FUNCTIONS ####################################################################

# ::: fake data generator function ####
fake.data <- function(beta = 12, icept = 201 + 23, e.mean = 0, e.sd = 20, n.pts = n_pts)
  {
  # generates fake longitudinal data linearly, with normally-distributed noise
  # produces one vector of complete data
  
  # beta.........slope (change )
  # icept........intercept (value at t=0)
  # e.sd.........standard deviation of error
  # e.mean.......mean of error
  # n.pts........number of time points
  
  vec <- c(icept + beta*0:(n.pts-1) + rnorm(n= n.pts, mean=e.mean, sd = e.sd))
  
  colnames.vec <- paste0('t', 1:n.pts)
  names(vec) <- colnames.vec
  vec
  }

undebug(fake.data)
fake.data()

# GENERATE DATA ################################################################

# ::: data.frame of unit betas, icepts, & e.sd's ####
unit_mat <- data.frame(beta = c(12, 13, 36, 37),
                       icept = c(201, 200, 224, 223),
                       e.sd = rep(10, 4))

# ::: generate missing data indicators ####
miss_mat <- rbind(c(1, rep(0, n_pts-1)),
                  c(1, rep(0, n_pts-1)),
                  c(rep(0, n_pts-1), 1),
                  c(rep(0, n_pts-1), 1))


# ::: create fake data vectors ####
fake_complete <- t(mapply(fake.data, 
                   beta = unit_mat$beta, 
                   icept = unit_mat$icept, 
                   e.sd = unit_mat$e.sd, SIMPLIFY = T))

# ::: create fake data vectors with missingness ####
fake_missing <- t(sapply(1:nrow(miss_mat), function(x)
  {
  ifelse(miss_mat[x, ] == 1, NA, fake_complete[x, ])
  })
)
colnames(fake_missing) <- colnames(fake_complete)

# PLOT DATA ####################################################################
matplot(t(fake_complete), type = 'b')
matplot(t(fake_missing), type = 'b')

# IMPUTE DATA ##################################################################

# ::: MICE package ####

# ::: missing data patterns ####
md.pattern(fake_missing, rotate.names = F)

# ::: Impute missing data ####

imp_out <- mice(data = fake_missing, method = 'norm', m = 5, maxit = 5)


# ::: plot imputed data ####
imp.plot(mids = imp_out, ids = unique(jfk.W$stu_id)[31:34],
         ylims = c(100, 600))

