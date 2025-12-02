################################################################################
# Tracking the progress of the COVID-19 pandemic
################################################################################

# Aggregate and summarize results #

################################################################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library("writexl")

#---------------------------------------
# Read in all estimates
#---------------------------------------
all_estimates <- list.files(path=estimates_path, full.names = TRUE) %>% 
  lapply(readRDS) %>% 
  bind_rows 
all_estimates <- all_estimates %>% select(date, state, population, estimated_cases, estimated_cases_lb, estimated_cases_ub, starts_with("estimated"), positive, total_cases, death, total, total_tests) %>% 
  rename(country=state) 

# all_estimates_und <- list.files(path=estimates_und_path, full.names = TRUE) %>% 
#   lapply(readRDS) %>% 
#   bind_rows 
# all_estimates_und <- all_estimates_und %>% select(date, state, population, estimated_cases, estimated_cases_lb, estimated_cases_ub, starts_with("estimated"), positive, death, total, starts_with("perc")) %>% 
#   rename(country=state) 

#---------------------------------------
# Detection rates, IFR,
#---------------------------------------
all_estimates <- all_estimates %>% mutate(detection_rate = (positive/estimated_cases_month)*100,
                                          detection_rate_lb = (positive/estimated_cases_ub_month)*100,
                                          detection_rate_ub = (positive/estimated_cases_lb_month)*100)

all_estimates <- all_estimates %>% mutate(IFR = (death/estimated_cases)*100,
                                          perc_pop_infected = (estimated_cases/population)*100)

all_estimates <- as_tibble(all_estimates) %>% mutate_at(vars(starts_with("perc"), starts_with("detect")), funs(round(.,3)))

#---------------------------------------
# testing per capita - number of tests per 1000 population?, tests per day
#---------------------------------------
all_estimates <- all_estimates %>% mutate(testing_pc = total_tests/population,
                                          test_positivty = total_cases/total_tests,
                                          avg_daily_tests = total/30)
 
#---------------------------------------
# Add in income and GDP data
#---------------------------------------
gdp <- read.csv("C:/Users/Admin/Desktop/Masterarbeit/Final Estimation Model/1-data/WB GDP data.csv", fileEncoding="UTF-8-BOM")  %>%
  select(Country.Name, X2019)  %>%
  rename(country=Country.Name, GDP2019=X2019)  %>%
  filter(country=="India" | country=="Mexico" | country=="United Kingdom" | country=="United States") 
#gdp <- format(gdp,scientific=FALSE)

all_estimates <- merge(all_estimates, gdp, by="country")

#---------------------------------------
# Mutate date
#---------------------------------------
all_estimates <- all_estimates %>% mutate(gdp_pc = (GDP2019/population))



# Overall number of estimated infections in each country
end_estimates <- all_estimates %>% filter(date == "2021-02-28")



# Country-wise estimates
country_choice <- "India"
country_estimate <- all_estimates %>% filter(country == country_choice)


saveRDS(all_estimates, paste0(results_path, "all_estimates.RDS"))
saveRDS(end_estimates, paste0(results_path, "end_estimates.RDS"))


# Export as an excel table
write_xlsx(all_estimates,"C:/Users/Admin/Desktop/table.xlsx")






# Summarize results for - 
sum_res <- "Wave_1"
# Choose between Waves 1-3 or overall

start_date <- "2020-02-28"
country <- "India"

# countries <- c("United States", "India", "United Kingdom", "Mexico")
# for (country in countries)  {

load_dates <- c("2020-04-27", "2020-05-27", "2020-06-27", "2020-07-27", "2020-08-27", "2020-09-27", "2020-10-27", "2020-11-27", "2020-12-27", "2021-01-27", "2021-02-27")



df_wave_1 <- df %>% select()

# Annual estimates per country
for (load_dates in end_date) {
   df_load_dates <- readRDS(paste0(results_path,"estimate_", country, "_",end_date,".RDS"))
}

















covid_usa_state_adjusted = readRDS(paste0(results_path, "covid_usa_state_adjusted.RDS"))
covid_all_usa <- load_US_data(min_date = "2020-03-07", max_date = "2020-04-18")

state_res = covid_usa_state_adjusted %>%
  dplyr::select(state, statename, positive, total, estimated_cases, population)  %>%
  mutate(
    test_rate = total / population * 1000,
    testpos_rate = positive / total * 100,
    obs_perpop = positive / population * 1000,
    exp_perpop = estimated_cases / population * 1000,
    ratio = estimated_cases / positive
  )

us_res = covid_usa_state_adjusted %>%
  summarise(
    positive = sum(positive),
    total = sum(total),
    population = sum(population),
    estimated_cases = sum(estimated_cases),
    estimated_cases_lb = sum(estimated_cases_lb),
    estimated_cases_ub = sum(estimated_cases_ub)
    
  ) %>%
  mutate(
    test_rate = total / population * 1000,
    testpos_rate = positive / total * 100,
    obs_perpop = positive / population * 1000,
    exp_perpop = estimated_cases / population * 1000,
    ratio = estimated_cases / positive
  )

#---------------------------------------
# US total confirmed vs. estimated
#---------------------------------------
us_res
us_res$exp_perpop
us_res$ratio

#---------------------------------------
# % of infections in the US in March were undocumented
#---------------------------------------
(us_res$estimated_cases - us_res$positive) / us_res$estimated_cases

#---------------------------------------
# testing rates
#---------------------------------------
covid_all_usa = covid_all_usa %>% 
  filter(as.Date(date) %in% as.Date(c("2020-02-28", "2020-04-18"))) %>%
  mutate(testrate = total / population * 1000)
covid_all_usa$testrate

state_res %>% arrange(test_rate) %>% dplyr::select(test_rate) %>% pull() %>% min()
state_res$state[state_res$test_rate<6.068]
state_res %>% arrange(test_rate) %>% dplyr::select(test_rate) %>% pull() %>% max()
state_res$state[state_res$test_rate>30.8]

#---------------------------------------
# test positive rates
#---------------------------------------
state_res %>% dplyr::select(testpos_rate) %>% pull() %>% min()
state_res %>% dplyr::select(testpos_rate) %>% pull() %>% max()

#---------------------------------------
# total US cases obs and exp
#---------------------------------------
us_res$estimated_cases / us_res$positive
us_res$positive / us_res$population * 1000

us_res$estimated_cases_lb / us_res$positive
us_res$estimated_cases_ub / us_res$positive

#---------------------------------------
# state range cases obs and exp
#---------------------------------------
state_res %>% dplyr::select(obs_perpop) %>% pull() %>% min()
state_res %>% dplyr::select(obs_perpop) %>% pull() %>% max()

state_res %>% dplyr::select(exp_perpop) %>% pull() %>% min()
state_res %>% dplyr::select(exp_perpop) %>% pull() %>% max()

#---------------------------------------
# state range ratio
#---------------------------------------
state_res %>% dplyr::select(state, ratio) %>% arrange(ratio) %>% head()
state_res %>% dplyr::select(state, ratio) %>% arrange(ratio) %>% tail()

#---------------------------------------
# states with at least 10 times ratio
#---------------------------------------
state_res %>% filter(ratio > 10) %>% nrow()

#---------------------------------------
# testing rate among states with biggest ratio
#---------------------------------------
# largest ratio
state_res %>% arrange(ratio) %>% tail() %>%
  dplyr::select(statename, test_rate, ratio)

# smallest ratio
state_res %>% arrange(ratio) %>% head() %>%
  dplyr::select(statename, test_rate, ratio)


