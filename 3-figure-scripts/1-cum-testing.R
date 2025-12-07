################################################################################
# Master Thesis
# Chair of Development Economics
# Tracking the progress of the COVID-19 pandemic
# 11905481
################################################################################

# Figure 1. Trends in cumulative test rates in the sample coutries #

################################################################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(directlabels)
library(zoo)
library(scales)

#---------------------------------------
# Load and mutate data
#---------------------------------------
country_abb_key = read.csv(country_abbrev_path, fileEncoding="UTF-8-BOM") %>% 
  select(state = State, statename = Abbreviation)

covid_world <- load_country_data(min_date = "2020-03-01", max_date = "2021-02-28")
maxdate = max(covid_world$date)

covid_world <- covid_world %>% rename(positive = total_cases, total = total_tests, state = location)

covid_country = covid_world %>%
  dplyr::select(date, state, positive, total, population, new_tests) %>%
  mutate(testrate = total/ population * 1000) 

covid_country = covid_country %>% 
  left_join(country_abb_key, by = "state")

#---------------------------------------
# Define labels, plot figure
#---------------------------------------
palette = viridis(n = 4, option = "C", begin=0, end=0.9, direction = -1)

testplot = ggplot(covid_country, aes(x = date, y = testrate, group = state, colour = state)) + 
  geom_line(aes(col = state), size = 1, na.rm=T) +
  ylab("Population tested per 1,000") +
  xlab("Date") +
  scale_y_continuous(breaks = seq(0,1500,250), labels = seq(0,1500,250)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  scale_color_manual("Countries", values = palette) +
  geom_dl(aes(label = statename), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.75), na.rm=T) +
  theme_bw()  +
  labs(title = "Total tests administered per 1,000 by February 28, 2021") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12, hjust = 0),
        legend.title = element_text(size = 11))  
testplot

ggsave(testplot, filename = paste0(plot_path, "fig-cum-testrates-samp-countries.png"),
       width = 10, height = 5)



#