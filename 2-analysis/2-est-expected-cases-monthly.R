################################################################################
# Tracking the progress of the COVID-19 pandemic
################################################################################

# Obtaining estimated number of infections #

################################################################################
source(paste0(here::here(), "/0-config.R"))

#---------------------------------------
# Select month to obtain priors
# My estimation period is defined from 01.03.2020-28.02.2021
# 1st of each month is fixed as the start_date
#---------------------------------------
reps <- 100
dates <- c("2020-03-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01", "2021-02-01")

for (date in dates) {
  start_date <- date

#---------------------------------------
# Fix end_date, countries, and load data
#---------------------------------------
if(start_date == "2020-03-01") {
  end_date <- toString(ymd(as.Date(start_date)) %m+% months(2) - days(1))
}     else if(start_date == "2020-03-28")   {
  stop("Date not allowed, March and April infections estimated together")
}     else    {
  end_date <- toString(ymd(as.Date(start_date)) %m+% months(1) - days(1))
}
load_date <- toString(as.Date(start_date) - 1)

countries <- c("United States", "India", "United Kingdom", "Mexico")
for (country in countries)  {

# Load individual country data
set.seed(123)
simdata = readRDS(paste0(results_path, country, "_", end_date, "_priors_out.RDS"))

#---------------------------------------
# Process data
#---------------------------------------
covid_world <- load_world_data(country = country, min_date = start_date, max_date = end_date)
maxdate = max(covid_world$date)

covid_world <- covid_world %>% select(date, location, total_cases, new_cases, total_deaths, new_deaths, total_tests, new_tests, positive_rate, population)
covid_world <- covid_world %>% arrange(date)  %>%
  rename(state=location, death=total_deaths, deathIncrease=new_deaths)
#total=total_tests,
#---------------------------------------
# Monthly estimates - reading in monthly data
#---------------------------------------
df_month <- covid_world %>%
  mutate(cases_month = sum(new_cases, na.rm=T),
         tests_month = sum(new_tests, na.rm=T))

df_month_1day <- df_month %>%
  filter(date==end_date)

#---------------------------------------
# Merging in older estimates
#---------------------------------------

if(end_date=="2020-04-30")  {
    df_month_1day <- df_month_1day %>% mutate(estimated_cases=total_cases)
  }   else   {
    month_estimates <- readRDS(paste0(estimates_path, "estimate_", country,"_",load_date,".RDS"))
    month_estimates <- month_estimates %>%
      select(state, estimated_cases, estimated_cases_lb, estimated_cases_ub)
    df_month_1day <- merge(df_month_1day, month_estimates, by="state")
}

#---------------------------------------
# Clean data (monthly estimates)
#---------------------------------------
covid_world_1day <- df_month_1day %>% select(
  date, state, death, cases_month, tests_month, positive_rate, population, starts_with("estimated_cases"), total_cases, total_tests
)

covid_world_1day <- covid_world_1day %>%
  rename(total = tests_month, positive = cases_month) %>% 
  mutate(N_susceptible = population-estimated_cases)

#---------------------------------------
# Expected case counts
#---------------------------------------
set.seed(123)
Sys.time()
tic()
corrected_samples_world_1day = mapply(
  generate_corrected_sample,
  covid_world_1day$population,
  covid_world_1day$total,
  covid_world_1day$positive,
  covid_world_1day$estimated_cases,
  MoreArgs = list("distribution_list" = simdata),
  reps,
  unique(covid_world_1day$state)
)
toc()

colnames(corrected_samples_world_1day) = unique(covid_world_1day$state)

saveRDS(corrected_samples_world_1day, paste0(results_path, "corrected_samples_world_", Sys.Date(),
                                             "_", "reps", reps, ".RDS"))

# obtain medians
sample_medians = unlist(mclapply(1:nrow(covid_world_1day),
                                 function(x) median(corrected_samples_world_1day[,x]$exp_cases)))

sample_lb = unlist(mclapply(1:nrow(covid_world_1day),
                            function(x) quantile(corrected_samples_world_1day[,x]$exp_cases, prob=0.025,
                                                 na.rm=TRUE)))

sample_ub = unlist(mclapply(1:nrow(covid_world_1day),
                            function(x) quantile(corrected_samples_world_1day[,x]$exp_cases, prob=0.975,
                                                 na.rm=TRUE)))


covid_month_adjusted <- covid_world_1day %>% mutate(
  estimated_cases_month = sample_medians,
  estimated_cases_lb_month = sample_lb,
  estimated_cases_ub_month = sample_ub
)

covid_month_adjusted <- covid_month_adjusted %>% select(date, state, population, death, starts_with("estimated_cases"), positive, positive_rate, total_cases, total, total_tests)

#for month1 estimations, the next line of code is not necessary as the correction formulae already add the confirmed positive observations to the estimated no.

if(end_date=="2020-04-30")  {
  covid_total_adjusted <- covid_month_adjusted %>% mutate(estimated_cases = estimated_cases_month,
                                                          estimated_cases_lb = estimated_cases_lb_month,
                                                          estimated_cases_ub = estimated_cases_ub_month)
}   else   {
  covid_total_adjusted <- covid_month_adjusted %>% mutate(estimated_cases = estimated_cases + estimated_cases_month,
                                                          estimated_cases_lb = estimated_cases_lb + estimated_cases_lb_month,
                                                          estimated_cases_ub = estimated_cases_ub + estimated_cases_ub_month)
}
#returns the estimated number of infections + the positive confirmed cases until that date.

saveRDS(covid_total_adjusted, paste0(estimates_path, "estimate_", country, "_", end_date,".RDS"))

}
}


#