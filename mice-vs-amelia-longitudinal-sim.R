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

# ::: vectors of unit parameters ####
betas <- rnorm(n_units, 15, 2)
icepts <- rnorm(n_units, 200, 2)
e_sds <- rep(10, n_units)

# ::: data.frame of unit betas, icepts, & e.sd's ####
unit_mat <- data.frame(beta = betas,
                       icept = icepts,
                       e.sd = e_sds)

# ::: types of missingness ####

# contiguous missing at start
miss.start <- function(n = n_pts, miss_end = NULL)
  {
  # generates binary vector: 1=missing, 0=observed
  # missingness is at the start of the vector & contiguous (no gaps)
  
  # n............number of time points
  # miss_end.....place where the missingness ends
  
  # by default, endpoint of missingness is determined by binomial w/p=0.25
  
  stopifnot(miss_end < n, miss_end > 0)
  if(is.null(miss_end))
  {
    # miss_end <- sample(1:(n-1), 1)
    miss_end <- rbinom(1, n_pts, 0.25)
  }
  miss_vec <- rep(0, n)
  miss_vec[1:miss_end] <- 1
  miss_vec
}

undebug(miss.start)
hist(replicate(1000, sum(miss.start())))

# contiguous missing at end
# contiguous missing in middle
# intermittent missing 

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

