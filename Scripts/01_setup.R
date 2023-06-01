#+ echo = FALSE
# Setup -------------------------------------------------------------------

# Create folders for output

dir.create("Data/Intermediate")
dir.create("Data/Datasets")
dir.create("Results")
dir.create("Results/Figures")

# If possible, use renv to restore the packages.
renv::restore()

# Otherwise install by hand e.g.,
# install.packages(c("tidyverse", "sf")


# Install specific package versions:

# dggridR was archived on CRAN, can either use specific version, or GitHub version
#remotes::install_version("dggridR", version = "2.0.4", repos = "http://cran.us.r-project.org")

# We require a specific version of segmented to reproduce results exactly
#remotes::install_version("segmented", version = "1.2-0", repos = "http://cran.us.r-project.org")

# SDMTools was archived on CRAN
#remotes::install_version("SDMTools", version = "1.1-221", repos = "http://cran.us.r-project.org")

# Although we used SDMTools, it has since been archived on CRAN and doesn't always install
# properly. If that is the case, consider using the functions directly by uncommenting
# the functions below and running them prior to running Scripts/05_gam.R
#
# Code for the following functions are from https://www.rforge.net/SDMTools/files/
# License GPL (>= 3), Jeremy VanDerWal

wt.mean <- function(x,wt) {
  s = which(is.finite(x*wt)); wt = wt[s]; x = x[s] #remove NA info
  return( sum(wt * x)/sum(wt) ) #return the mean
}


wt.var <- function(x,wt) {
  s = which(is.finite(x + wt)); wt = wt[s]; x = x[s] #remove NA info
  xbar = wt.mean(x,wt) #get the weighted mean
  return( sum(wt *(x-xbar)^2)*(sum(wt)/(sum(wt)^2-sum(wt^2))) ) #return the variance
}

wt.sd <- function(x,wt) {
  return( sqrt(wt.var(x,wt)) ) #return the standard deviation
}
