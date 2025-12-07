################################################################################
# Master Thesis
# Chair of Development Economics
# Tracking the progress of the COVID-19 pandemic
# 11905481
################################################################################

# Fig. 6: Trends in detection rates #

################################################################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(directlabels)

#---------------------------------------
# Load and mutate data
#---------------------------------------
country_abb_key = read.csv(country_abbrev_path, fileEncoding="UTF-8-BOM") %>% 
  select(country = State, statename = Abbreviation)
all_estimates <- readRDS(paste0(results_path, "all_estimates.RDS"))
all_estimates <- all_estimates %>% 
  left_join(country_abb_key, by = "country")

#---------------------------------------
# Plotting graphs
#---------------------------------------
palette = viridis(n = 4, option = "C", begin=0, end=0.9, direction = -1)

# Cumulative dtection rates of true infections
detrate = ggplot(all_estimates, aes(x = date, group = country, color = country)) + 
  geom_point(aes(y = detection_rate, col = country), shape = 1, size = 0.75, stroke = 0.25, na.rm=T) +
  geom_line(aes(y = detection_rate, col = country)) +
  geom_dl(aes(y = detection_rate, label = statename), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.75), na.rm=T) +
  ylab("Detection Rate") +
  xlab("Month") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  scale_color_manual("Countries", values = palette) +
  theme_bw()  +
  theme(plot.title = element_text(size = 12, hjust = 0),
        plot.subtitle = element_text(size = 11)) +
  labs(title = "Detection rates of true infections",
       subtitle = "in the sample countries") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=10),
        legend.title = element_text(size=11)) 

detrate

ggsave(detrate, filename = paste0(plot_path, "fig-trends-det-rate.png"),
       width = 10, height = 5)




#