################################################################################
# Tracking the progress of the COVID-19 pandemic
################################################################################

# Obtaining priors for countries #

################################################################################
rm(list=ls())
setwd("")
source("./0-config.R")

#---------------------------------------
# Select month to obtain priors
# My estimation period is defined from 01.03.2020-28.02.2021
# 1st of each month is fixed as the start_date
#---------------------------------------
dates <- c("2020-03-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01", "2021-02-01")
for (date in dates) {
start_date <- date

#---------------------------------------
# Fix end_date, countries, and load data
#---------------------------------------
if(start_date == "2020-03-01") {
  end_date <- toString(ymd(as.Date(start_date)) %m+% months(2) - days(1))
}     else if(start_date == "2020-04-01")   {
  stop("Date not allowed, March and April infections estimated together")
}     else    {
  end_date <- toString(ymd(as.Date(start_date)) %m+% months(1) - days(1))
}

countries <- c("United States","India", "United Kingdom", "Mexico")
for (country in countries)  {

# Load individual country data
if(end_date <= "2020-06-30")   {
  source(paste0(here::here(), "/0-base-functions/0-1-prior-functions/0-1-", country, "-wave1.R"))
}   else  if(end_date <= "2020-10-31")    {
  source(paste0(here::here(), "/0-base-functions/0-1-prior-functions/0-1-", country, "-wave2.R"))
}   else    {
  source(paste0(here::here(), "/0-base-functions/0-1-prior-functions/0-1-", country, "-wave3.R"))
}

#---------------------------------------
# Process data
#---------------------------------------
covid_country <- load_world_data(country = country, min_date = start_date, max_date = end_date)
maxdate = max(covid_country$date)

covid_country <- covid_country %>% select(date, location, total_cases, new_cases, total_deaths, new_deaths, total_tests, new_tests, positive_rate, population)
covid_country <- covid_country %>% arrange(date)  %>%
  rename(state=location, total=total_tests, totalTestResultsIncrease=new_tests, positive=total_cases, positiveIncrease=new_cases, death=total_deaths, deathIncrease=new_deaths)

#----Averaging test positivity over a month-----
covid_country = covid_country %>%
  group_by(state) %>%
  mutate(posrate=mean(positive_rate, na.rm=T))
#----------------------------------------------

covid_country = covid_country %>%
  filter(date==as.Date(maxdate)) %>%
  dplyr::select(date, state, positive, total, population, posrate) %>%
  arrange(state)

##the next 3 are not really necessary --
total_tests_world <-sum(covid_country$total)
total_cases_world <- sum(covid_country$positive)
avg_testpos <- mean(covid_country$posrate)

#---------------------------------------
# Constrain priors - does not vary by date
#---------------------------------------
# run time < 1 min
Sys.time()
tic()
theta_samp_constrained = constrain_priors(priors = theta_samp)
toc()

#---------------------------------------
# calculate P_testpos_AS, check distributions
# list of list of priors for each date in each country
# P_testpos_AS is used later to constrain priors
#---------------------------------------
theta_samp_countries = lapply(as.list(covid_country$posrate),
   function(x) est_P_testpos_AS(
    priors = theta_samp_constrained,
    est_testpos = x))

names(theta_samp_countries) = unique(covid_country$state)

#---------------------------------------
# World - process priors
#---------------------------------------
#run time =
Sys.time()
tic()
world_proc_priors = lapply(theta_samp_countries,
                               function(x)
                                 process_priors(priors = x,
                                                Se = dist_Se,
                                                Sp = dist_Sp))

toc()


names(world_proc_priors) = unique(covid_country$state)

saveRDS(world_proc_priors, paste0(results_path, country, "_", end_date, "_priors_out.RDS"))

}
}

#