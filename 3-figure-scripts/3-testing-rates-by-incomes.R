################################################################################
# Master Thesis
# Chair of Development Economics
# Tracking the progress of the COVID-19 pandemic
# 11905481
################################################################################

# Figure 3. Mean testing rates across top 20 most affected countries #

################################################################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(directlabels)
library(zoo)
library(scales)

#------------------------------------------------------
# Load data and mutate data
#------------------------------------------------------
all_countries <- load_countries_data(min_date = "2020-03-01", max_date = "2021-02-28")
all_countries <- all_countries %>%
  rename(state=location) 
country_abb_key = read.csv(country_abbrev_path, fileEncoding="UTF-8-BOM") %>% 
  select(state = State, statename = Abbreviation)

covid_tests <- all_countries %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(month, year, state) %>%
  summarise(total = sum(new_tests, na.rm=T))

covid_tests <- covid_tests %>% mutate(month=as.numeric(month), year=as.numeric(year))
covid_tests$date <- as.yearmon(paste(covid_tests$year, covid_tests$month), "%Y %m")
covid_country_2 <- all_countries %>% select(state, population, gdp_per_capita)
covid_tests <- covid_tests %>% 
  left_join(country_abb_key, by = "state") %>%
  left_join(covid_country_2, by = "state")

covid_tests <- covid_tests %>% mutate(avg_income = mean(gdp_per_capita),
                                      rich = ifelse(gdp_per_capita >= avg_income, 1, 0)) %>% 
  mutate(avg_daily_tests = total/days_in_month(date), 
         avg_daily_tests_pc = avg_daily_tests/population * 1000)
covid_tests$rich <- as.factor(covid_tests$rich)

#------------------------------------------------------
# Plot figure
#------------------------------------------------------
palette = viridis(n = 4, option = "C", begin=0, end=0.9, direction = -1)

testplot2.1 = ggplot(covid_tests, aes(x = date, y = avg_daily_tests_pc, group = rich, color = rich)) + 
  geom_point(aes(col = rich), shape = 1, size = 0.75, stroke = 0.25, na.rm=T, stat='summary', fun='mean') +
  geom_line(aes(col = rich), stat='summary', fun='mean') +
  ylab("Avg. daily tests administered per 1,000") +
  xlab("Month") +
  scale_x_continuous(breaks = as.numeric(covid_tests$date), labels = format(covid_tests$date,"%b %Y")) + 
  scale_color_manual("Income levels", labels = c("Poorer countries", "Richer countries"), values = palette) +
  theme_bw()  +
  theme(plot.title = element_text(size = 12, hjust = 0),
        plot.subtitle = element_text(size = 11)) +
  labs(title = "Average daily tests administered per 1,000 per month in rich and poor countries",
       subtitle = "in the top 20 most affected countries") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=9),
        legend.title = element_text(size=11))  
testplot2.1

ggsave(testplot2.1, filename = paste0(plot_path, "fig-testrates-countries.png"),
       width = 10, height = 5)


#