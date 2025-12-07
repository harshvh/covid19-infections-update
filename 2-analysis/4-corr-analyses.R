################################################################################
# Master Thesis
# Chair of Development Economics
# Tracking the progress of the COVID-19 pandemic
# 11905481
################################################################################

# Correlation analyses #

################################################################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

#---------------------------------------
# Read in all estimates, derive country-specific estimates
#---------------------------------------
all_estimates <- readRDS((paste0(results_path, "all_estimates.RDS")))

estimate_IN <- all_estimates %>% filter(country == "India")
estimate_MX <- all_estimates %>% filter(country == "Mexico")
estimate_UK <- all_estimates %>% filter(country == "United Kingdom")
estimate_US <- all_estimates %>% filter(country == "United States")

#---------------------------------------
# Analyses
#---------------------------------------
# Correlation between testing and detection rates
res <- cor.test(estimate_IN$detection_rate_month, estimate_IN$avg_daily_tests)
res

res2 <- cor.test(estimate_MX$detection_rate_month, estimate_MX$avg_daily_tests)
res2

res3 <- cor.test(estimate_UK$detection_rate_month, estimate_UK$avg_daily_tests)
res3

res4 <- cor.test(estimate_US$detection_rate_month, estimate_US$avg_daily_tests)
res4

# Correlation between test positivity and detection rates
res5 <- cor.test(estimate_IN$detection_rate_month, estimate_IN$test_positivty)
res5

res6 <- cor.test(estimate_MX$detection_rate_month, estimate_MX$test_positivty)
res6

res7 <- cor.test(estimate_UK$detection_rate_month, estimate_UK$test_positivty)
res7

res8 <- cor.test(estimate_US$detection_rate_month, estimate_US$test_positivty)
res8



#