################################################################################
# Master Thesis
# Chair of Development Economics
# Tracking the progress of the COVID-19 pandemic
# 11905481
################################################################################

# Configure data directories  #
# Source base functions #
# Load libraries  #

################################################################################

library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(parallel)
library(gridExtra)
library(reshape2)
library(plotly)
library(htmlwidgets)
library(assertthat)
library(gsheet)
library(RColorBrewer)
library(stringr)
library(tidyverse)
library(viridis)
library(tictoc)

#--------------------------------------------
# load base functions
#--------------------------------------------
source(paste0(here::here(), "/0-base-functions/0-all-functions.R"))

#--------------------------------------------
# define raw data paths
#--------------------------------------------
data_path = paste0(here::here(),"/1-data")
country_abbrev_path = paste0(data_path,"/country-abbrev.csv")

#--------------------------------------------
# define output paths
#--------------------------------------------
plot_path = paste0(here::here(), "/4-figures/")
results_path = paste0(here::here(), "/5-results/")
estimates_path = paste0(here::here(), "/5-results/5-1-estimated-infections/")
samples_path = paste0(here::here(), "/5-results/5-2-corrected-samples/")

#--------------------------------------------
# set number of samples to be sampled from each prior
#--------------------------------------------
nsamp <- 1e5

#