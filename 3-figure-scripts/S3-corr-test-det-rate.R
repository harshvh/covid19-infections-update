################################################################################
# Master Thesis
# Chair of Development Economics
# Tracking the progress of the COVID-19 pandemic
# 11905481
################################################################################

# Supplementary fig. : Correlation between detection rate and avg daily test rate #

################################################################################
source(paste0(here::here(), "/0-config.R"))
library(grid)

#---------------------------------------
# Load and mutate data
#---------------------------------------
all_estimates <- readRDS((paste0(results_path, "all_estimates.RDS")))

estimate_IN <- all_estimates %>% filter(country == "India")
estimate_MX <- all_estimates %>% filter(country == "Mexico")
estimate_UK <- all_estimates %>% filter(country == "United Kingdom")
estimate_US <- all_estimates %>% filter(country == "United States")

#---------------------------------------
# Plot figure
#---------------------------------------
yscat_IN <- c(0, 500, 1000)
yscat_MX <- c(0, 10, 20, 30)
yscat_UK <- c(0, 200, 400, 600)
yscat_US <- c(0, 500, 1000, 1500)

scatter1.1 <- ggplot(estimate_IN, aes(detection_rate_month, avg_daily_tests)) +
  geom_point(aes(col = country), shape = 1, size = 1.75, stroke = 1.5, na.rm=T) + 
  geom_smooth(col = "black", size = 0.5, method="lm") +
  ylab("Average daily tests conducted (in thousands)") +
  scale_y_continuous(labels = paste0(yscat_IN), breaks = 1000 * yscat_IN) +
  xlab("") +
  labs(title = "Country: India",
       color = "Countries") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, hjust = 0),
        axis.title.y = element_text(size = 8.5))

scatter2.1 <- ggplot(estimate_MX, aes(detection_rate_month, avg_daily_tests)) +
  geom_point(aes(col = country), shape = 1, size = 1.75, stroke = 1.5, na.rm=T) + 
  geom_smooth(col = "black", size = 0.5, method="lm") +
  ylab("") +
  scale_y_continuous(labels = paste0(yscat_MX), breaks = 1000 * yscat_MX) +
  xlab("") +
  labs(title = "Country: Mexico",
       color = "Countries") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, hjust = 0))

scatter3.1 <- ggplot(estimate_UK, aes(detection_rate_month, avg_daily_tests)) +
  geom_point(aes(col = country), shape = 1, size = 1.75, stroke = 1.5, na.rm=T) + 
  geom_smooth(col = "black", size = 0.5, method="lm") +
  ylab("Average daily tests conducted (in thousands)") +
  scale_y_continuous(labels = paste0(yscat_UK), breaks = 1000 * yscat_UK) +
  xlab("Monthly detection rates") +
  labs(title = "Country: United Kingdom",
       color = "Countries") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 9, hjust = 0),
        axis.title.y = element_text(size = 8.5))

scatter4.1 <- ggplot(estimate_US, aes(detection_rate_month, avg_daily_tests)) +
  geom_point(aes(col = country), shape = 1, size = 1.75, stroke = 1.5, na.rm=T) + 
  geom_smooth(col = "black", size = 0.5, method="lm") +
  ylab("") +
  scale_y_continuous(labels = paste0(yscat_US), breaks = 1000 * yscat_US) +
  xlab("Monthly detection rates") +
  labs(title = "Country: United States",
       color = "Countries") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 9, hjust = 0))

scattered2 <- grid.arrange(scatter1.1, scatter2.1, scatter3.1, scatter4.1,
                           top = textGrob("Correlation between monthly detection rates and test rates",
                                          gp=gpar(fontsize=15)))

ggsave(scattered2, filename = paste0(plot_path, "scatterplot_test_det.png"),
       width = 10, height = 5)


#