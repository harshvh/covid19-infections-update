################################################################################
# Master Thesis
# Chair of Development Economics
# Tracking the progress of the COVID-19 pandemic
# 11905481
################################################################################

# Figure 2. Avgerage monthly testing rates across sample countries #

################################################################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(directlabels)
library(zoo)
library(scales)

#---------------------------------------
# Load data and mutate data
#---------------------------------------
samp_countries <- load_country_data(min_date = "2020-03-01", max_date = "2021-02-28")
samp_countries <- samp_countries %>%
  rename(state=location) 
country_abb_key = read.csv(country_abbrev_path, fileEncoding="UTF-8-BOM") %>% 
  select(state = State, statename = Abbreviation)

covid_tests <- samp_countries %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(month, year, state) %>%
  summarise(total = sum(new_tests, na.rm=T))

covid_tests <- covid_tests %>% mutate(month=as.numeric(month), year=as.numeric(year))
covid_tests$date <- as.yearmon(paste(covid_tests$year, covid_tests$month), "%Y %m")
covid_country_2 <- samp_countries %>% select(state, population, gdp_per_capita)
covid_tests <- covid_tests %>% 
  left_join(country_abb_key, by = "state") %>%
  left_join(covid_country_2, by = "state")

covid_tests <- covid_tests %>% mutate(avg_income = mean(gdp_per_capita),
                                      rich = ifelse(gdp_per_capita >= avg_income, 1, 0)) %>% 
  mutate(avg_daily_tests = total/days_in_month(date), 
         avg_daily_tests_pc = avg_daily_tests/population * 1000)
covid_tests$rich <- as.factor(covid_tests$rich)

#---------------------------------------
# Plot figure
#---------------------------------------
palette = viridis(n = 4, option = "C", begin=0, end=0.9, direction = -1)

testplot2.2 = ggplot(covid_tests, aes(x = date, y = avg_daily_tests_pc, group = state, color = state)) + 
  geom_point(aes(col = state), shape = 1, size = 0.75, stroke = 0.25, na.rm=T) +
  geom_line(aes(col = state)) +
  geom_dl(aes(label = statename), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.75), na.rm=T) +
  ylab("Avg. daily tests administered per 1,000") +
  xlab("Month") +
  scale_x_continuous(breaks = as.numeric(covid_tests$date), labels = format(covid_tests$date,"%b %Y")) + 
  scale_color_manual("Countries", values = palette) +
  theme_bw()  +
  theme(plot.title = element_text(size = 12, hjust = 0),
        plot.subtitle = element_text(size = 11)) +
  labs(title = "Average daily tests administered per 1,000 per month",
       subtitle = "in the sample countries") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=9),
        legend.title = element_text(size=11))  
testplot2.2

ggsave(testplot2.2, filename = paste0(plot_path, "fig-testrates-sample-countries.png"),
       width = 10, height = 5)

#