################################################################################
# Master Thesis
# Chair of Development Economics
# Tracking the progress of the COVID-19 pandemic
# 11905481
################################################################################

# Figure: estimated and confirmed infections per month by country #

################################################################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(directlabels)
library(grid)

#---------------------------------------
# Load and mutate data
#---------------------------------------
country_abb_key = read.csv(country_abbrev_path, fileEncoding="UTF-8-BOM") %>% 
   select(country = State, statename = Abbreviation)
all_estimates <- readRDS(paste0(results_path, "all_estimates.RDS"))
all_estimates <- all_estimates %>% 
   left_join(country_abb_key, by = "country")

#---------------------------------------
# Country-specific plots
#---------------------------------------
estimate_IN <- all_estimates %>% filter(country == "India")
ylab_IN <- c(0, 5, 10, 15, 20, 25)

countryplot_IN <- ggplot(estimate_IN, aes(x = date)) + 
   geom_line(aes(y = estimated_cases_month), col = "red1") +
   geom_ribbon(aes(ymin = estimated_cases_lb_month, ymax = estimated_cases_ub_month), linetype=2, alpha=0.1) +
   geom_line(aes(y = positive), col = "darkblue") +
   geom_point(aes(y = estimated_cases_month), shape = 1, size = 0.75, stroke = 0.25, na.rm=T, col = "red1") +
   geom_point(aes(y = positive), shape = 1, size = 0.75, stroke = 0.25, na.rm=T, col = "darkblue") +
   ylab("Number of infections (in millions)") +
   xlab("") +
   scale_y_continuous(labels = paste0(ylab_IN, "M"), breaks = 10^6 * ylab_IN) +
   scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
   theme_bw()  +
   labs(title = "Country: India") +
   theme(plot.title = element_text(size = 12, hjust = 0),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 8.5))

countryplot_IN

ggsave(countryplot_IN, filename = paste0(plot_path, "fig-estimates-IN-pm.png"),
       width = 10, height = 5)

##############################################################

estimate_MX <- all_estimates %>% filter(country == "Mexico")
ylab_MX <- c(0, 2, 4, 6, 8)

countryplot_MX <- ggplot(estimate_MX, aes(x = date)) + 
   geom_line(aes(y = estimated_cases_month), col = "red1") +
   geom_ribbon(aes(ymin = estimated_cases_lb_month, ymax = estimated_cases_ub_month), linetype=2, alpha=0.1) +
   geom_line(aes(y = positive), col = "darkblue") +
   geom_point(aes(y = estimated_cases_month), shape = 1, size = 0.75, stroke = 0.25, na.rm=T, col = "red1") +
   geom_point(aes(y = positive), shape = 1, size = 0.75, stroke = 0.25, na.rm=T, col = "darkblue") +
   ylab("") +
   xlab("") +
   scale_y_continuous(labels = paste0(ylab_MX, "M"), breaks = 10^6 * ylab_MX) +
   scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
   theme_bw()  +
   labs(title = "Country: Mexico") +
   theme(plot.title = element_text(size = 12, hjust = 0),
         axis.text = element_text(size = 8.5))

countryplot_MX

ggsave(countryplot_MX, filename = paste0(plot_path, "fig-estimates-MX-pm.png"),
       width = 10, height = 5)

##############################################################
estimate_UK <- all_estimates %>% filter(country == "United Kingdom")
ylab_UK <- c(0, 1, 2, 3, 4)

countryplot_UK <- ggplot(estimate_UK, aes(x = date)) + 
   geom_line(aes(y = estimated_cases_month), col = "red1") +
   geom_ribbon(aes(ymin = estimated_cases_lb_month, ymax = estimated_cases_ub_month), linetype=2, alpha=0.1) +
   geom_line(aes(y = positive), col = "darkblue") +
   geom_point(aes(y = estimated_cases_month), shape = 1, size = 0.75, stroke = 0.25, na.rm=T, col = "red1") +
   geom_point(aes(y = positive), shape = 1, size = 0.75, stroke = 0.25, na.rm=T, col = "darkblue") +
   ylab("Number of infections (in millions)") +
   xlab("Date") +
   scale_y_continuous(labels = paste0(ylab_UK, "M"), breaks = 10^6 * ylab_UK) +
   scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
   theme_bw()  +
   labs(title = "Country: United Kingdom") +
   theme(plot.title = element_text(size = 12, hjust = 0),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 8.5))

countryplot_UK

ggsave(countryplot_UK, filename = paste0(plot_path, "fig-estimates-UK-pm.png"),
       width = 10, height = 5)

##############################################################
estimate_US <- all_estimates %>% filter(country == "United States")
ylab_US <- c(0, 5, 10, 15, 20)

countryplot_US <- ggplot(estimate_US, aes(x = date)) + 
   geom_line(aes(y = estimated_cases_month), col = "red1") +
   geom_ribbon(aes(ymin = estimated_cases_lb_month, ymax = estimated_cases_ub_month), linetype=2, alpha=0.1) +
   geom_line(aes(y = positive), col = "darkblue") +
   geom_point(aes(y = estimated_cases_month), shape = 1, size = 0.75, stroke = 0.25, na.rm=T, col = "red1") +
   geom_point(aes(y = positive), shape = 1, size = 0.75, stroke = 0.25, na.rm=T, col = "darkblue") +
   xlab("Date") +
   ylab("") +
   scale_y_continuous(labels = paste0(ylab_US, "M"), breaks = 10^6 * ylab_US) +
   scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
   theme_bw()  +
   labs(title = "Country: United States") +
   theme(plot.title = element_text(size = 12, hjust = 0),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 8.5))

countryplot_US

ggsave(countryplot_US, filename = paste0(plot_path, "fig-estimates-US-pm.png"),
       width = 10, height = 5)

#---------------------------------------
# Combine individual plots
#---------------------------------------
bar_plot = grid.arrange(countryplot_IN, countryplot_MX, countryplot_UK, countryplot_US, ncol = 2,
                        top = textGrob("Estimated and confirmed COVID-19 cases per month", gp=gpar(fontsize=16)))

ggsave(bar_plot, filename = paste0(plot_path, "fig-est-infections-countries-monthly.png"),
       width=14, height=8)





#