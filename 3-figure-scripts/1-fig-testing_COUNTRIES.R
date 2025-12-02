################################################################################
# Tracking the progress of the COVID-19 pandemic
################################################################################

# Figure of testing rates by country #
# change state to country?
################################################################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(directlabels)

#---------------------------------------
# Load and mutate data
#---------------------------------------
country_abb_key = read.csv(country_abbrev_path, fileEncoding="UTF-8-BOM") %>% 
  select(state = State, statename = Abbreviation)

covid_world <- load_global_data(min_date = "2020-03-01", max_date = "2021-02-28")
maxdate = max(covid_world$date)

covid_world <- covid_world %>% rename(positive = total_cases, total = total_tests, state = location)

covid_state = covid_world %>%
  dplyr::select(date, state, positive, total, population) %>%
  mutate(testrate = total/ population * 1000) 

covid_state = covid_state %>% 
  left_join(country_abb_key, by = "state")

#---------------------------------------
# Define labels, plot figure
#---------------------------------------
covid_state = covid_state %>% 
  mutate(mylabel = paste0(state, "\n",
                          date, "\n",
                          sprintf("%0.1f", testrate), " per 1,000"))

palette = viridis(n = 5, option = "C", begin=0, end=0.9, direction = -1)

testplot = ggplot(covid_state, aes(x = date, y = testrate, group = state, colour = state,  
                                   text = mylabel)) + 
  geom_point(aes(col = state), shape = 1, size = 0.75, stroke = 0.25, na.rm=T) +
  geom_line(aes(col = state)) +
  ylab("Population tested per 1,000") +
  xlab("Date") +
  scale_y_continuous(breaks = seq(0,1500,250), labels = seq(0,1500,250)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") + 
  scale_color_manual("Testing per 1,000\nby February 28, 2021", values = palette) +
  geom_dl(aes(label = statename), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.75), na.rm=T) +
  theme_bw()  +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=8))
testplot

ggsave(testplot, filename = paste0(plot_path, "fig-testrates-state.png"),
       width = 10, height = 5)

#