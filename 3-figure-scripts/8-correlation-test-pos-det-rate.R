################################################################################
# Master Thesis
# Chair of Development Economics
# Tracking the progress of the COVID-19 pandemic
# 11905481
################################################################################

# Fig. 8: Correlation between detection rate and test positivity rate #

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
scatter <- ggplot(estimate_IN, aes(detection_rate_month, test_positivty)) +
  geom_point(aes(col = country), shape = 1, size = 1.75, stroke = 1.5, na.rm=T) + 
  geom_smooth(col = "black", size = 0.5, method="lm") +
  ylab("Test positivity rate") +
  xlab("") +
  labs(title = "Country: India",
       color = "Countries") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0))

scatter2 <- ggplot(estimate_MX, aes(detection_rate_month, test_positivty)) +
  geom_point(aes(col = country), shape = 1, size = 1.75, stroke = 1.5, na.rm=T) + 
  geom_smooth(col = "black", size = 0.5, method="lm") +
  ylab("") +
  xlab("") +
  labs(title = "Country: Mexico",
       color = "Countries") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0))

scatter3 <- ggplot(estimate_UK, aes(detection_rate_month, test_positivty)) +
  geom_point(aes(col = country), shape = 1, size = 1.75, stroke = 1.5, na.rm=T) + 
  geom_smooth(col = "black", size = 0.5, method="lm") +
  ylab("Test positivity rate") +
  xlab("Monthly detection rates") +
  labs(title = "Country: United Kingdom",
       color = "Countries") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0))

scatter4 <- ggplot(estimate_US, aes(detection_rate_month, test_positivty)) +
  geom_point(aes(col = country), shape = 1, size = 1.75, stroke = 1.5, na.rm=T) + 
  geom_smooth(col = "black", size = 0.5, method="lm") +
  ylab("") +
  xlab("Monthly detection rates") +
  labs(title = "Country: United States",
       color = "Countries") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0))


scattered <- grid.arrange(scatter, scatter2, scatter3, scatter4,
                          top = textGrob("Correlation between monthly detection rates and test positivity rates",
                                         gp=gpar(fontsize=15)))

ggsave(scattered, filename = paste0(plot_path, "scatterplot_test_pos_det.png"),
       width = 10, height = 5)


#