################################################################################
# Tracking the progress of the COVID-19 pandemic
################################################################################

# Figure showing test-positivity rate by date and country #

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
  mutate(posrate = positive/total * 100,
         testrate = total/ population * 100) 

covid_world = covid_state %>% 
  left_join(country_abb_key, by = "state")

#---------------------------------------
# Define labels, plot figure
#---------------------------------------
palette = viridis(n = 4, option = "C", begin=0, end=0.9, direction = -1)

plot_all = ggplot(covid_world, aes(x = date, y = posrate, group = state, colour = state)) +
  geom_line(aes(col = state)) +
  scale_y_continuous(limits = c(0,50)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d",
               limits = as.Date(c("2020-03-01", "2021-02-28"))) +
  scale_color_manual("Test-positivity\nrates over time", values = palette) +
  ylab("P(test+ | tested)") +
  xlab("Date")+
  geom_dl(aes(label = statename), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.75), na.rm=T) +
    theme_bw()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=8))

plot_later = ggplot(covid_world %>% filter(testrate>0.6), aes(x = date, y = posrate, group = state, text = state, colour = state)) +
  geom_line(aes(col = state)) +
  scale_y_continuous(limits = c(0,50)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d",
               limits = as.Date(c("2020-03-01", "2021-02-28"))) +
  scale_color_manual("Test-positivity\nrates over time\nwith sufficient testing", values = palette) +
  ylab("P(test+ | tested)") +
  xlab("Date")+
  geom_dl(aes(label = statename), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.75), na.rm=T) +
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=8))

ggsave(plot_all, filename = paste0(plot_path, "fig-testpos-state.png"),
       width = 7, height = 4)

ggsave(plot_later, filename = paste0(plot_path, "fig-testpos-later-state.png"),
       width = 7, height = 4)



#