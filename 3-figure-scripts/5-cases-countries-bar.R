################################################################################
# Master Thesis
# Chair of Development Economics
# Tracking the progress of the COVID-19 pandemic
# 11905481
################################################################################

# Bar plot of COVID-19 cases vs.estimated number of infections #

################################################################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(viridis)
library(grid)

#---------------------------------------
# Load and mutate data
#---------------------------------------
end_estimates <- readRDS(paste0(results_path, "end_estimates.RDS"))
gnbu_colors = brewer.pal(n=6,"GnBu")[2:6]

#---------------------------------------
# compare observed vs estimated test positive rate
# by country (Panel B)
#---------------------------------------

# calculate % diff in obs vs. exp
bar_data_perdiff = end_estimates %>%
  mutate(factor_diff = estimated_cases / total_cases,
         factor_diff_lb = estimated_cases_lb / total_cases,
         factor_diff_ub = estimated_cases_ub / total_cases) %>%
  mutate(country_f = fct_reorder(country, factor_diff, max)) %>%
  mutate(label = paste0(
    country, 
    "\nConfirmed cases: ", format(round(positive, 0), scientific=F, big.mark=","),
    "\nEstimated cases: ", format(round(estimated_cases, 0), scientific=F, big.mark=","),
    "\nEstimated:confirmed ", format(round(factor_diff,0), scientific=F, big.mark=","),
    " (",format(round(factor_diff_lb,0), scientific=F, big.mark=","), ", ",
    format(round(factor_diff_ub,0), scientific=F, big.mark=","),")"))

ratio_quantiles <- quantile(bar_data_perdiff$factor_diff, c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm=TRUE)

# modify interval labels
label_interval <- function(breaks) {
  paste0(breaks[1:length(breaks) - 1], " - ", breaks[2:length(breaks)])
}

bar_data_perdiff = bar_data_perdiff %>%
  mutate(factor_diff_cat = cut(factor_diff, ratio_quantiles, include.lowest=TRUE,
                               labels = label_interval(round(ratio_quantiles, 1))))

perdiff_solo_plot = ggplot(bar_data_perdiff, aes(x = country_f, y = factor_diff, text = label))+
  geom_bar(aes(fill = factor_diff_cat), stat="identity") +
  geom_linerange(aes(ymin = factor_diff_lb, ymax = factor_diff_ub)) + 
  xlab("") +
  scale_fill_manual(values = gnbu_colors) +
  scale_y_continuous(labels = seq(0,80,5), breaks = seq(0,80,5)) +
  ylab("B) Ratio of estimated infections vs. confirmed cases") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(size=10.5),
        axis.title = element_text(size = 12)) +
  theme(legend.position = "none")

############################################################
# Bar chart of estimated counts (Panel A)
############################################################

bar_data =  bar_data_perdiff %>%
  mutate(exp = estimated_cases,
         exp_perpop = estimated_cases/population*1000,
         exp_perpop_lb = estimated_cases_lb / population*1000,
         exp_perpop_ub = estimated_cases_ub / population*1000) %>%
  dplyr::select(country, country_f,  exp_perpop,
                exp_perpop_lb, exp_perpop_ub) %>%
  melt(id.vars = c("country", "country_f", "exp_perpop_lb","exp_perpop_ub")) %>%
  mutate(variable = as.character(variable)) %>%
  mutate(variable_f = as.factor("Estimated infections")) %>%
  mutate(variable_f = factor(variable_f, "Estimated infections")) %>%
  mutate(exp_perpop_lb = exp_perpop_lb, exp_perpop_ub = exp_perpop_ub)


# create label for interactive plot
bar_labels = bar_data %>% dplyr::select(country) %>%
  left_join(bar_data %>% filter(variable == "exp_perpop") %>%
              dplyr::select(country, value), by = "country") %>%
  left_join(bar_data %>% filter(variable == "exp_perpop") %>% 
              dplyr::select(country, exp_perpop_lb, exp_perpop_ub), by = "country") %>%
  mutate(label = paste0("<b>", country, "</b>\n",
                        "Estimated Cases: ", 
                        format(value, big.mark=",", digits=1, scientific=F, trim = TRUE), 
                        " (", format(exp_perpop_lb, big.mark=",", digits=1, scientific=F, trim = TRUE), 
                        ", ", format(exp_perpop_ub, big.mark=",", digits=1, scientific=F, trim = TRUE), ")"
  )) %>%
  dplyr::select(country, label)

bar_data = bar_data %>%
  left_join(bar_labels, by = "country")

bar_data$variable_f = factor(bar_data$variable_f, "Estimated infections")


############################################################
# natural scale plot
############################################################

nat_bar_solo = ggplot(bar_data,
                      aes(x = country_f, y = value , fill = variable_f, text = label)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8) +
  geom_linerange(aes(ymin = exp_perpop_lb, ymax = exp_perpop_ub),
                 position=position_dodge(width=0.8)) +
  scale_fill_manual("", values = "#858585")  +
  theme_minimal() +
  theme(axis.text.y = element_text(size=10.5),
        axis.title = element_text(size = 12)) +
  xlab("") +
  ylab("A) Cumulative estimated COVID-19 infections per 1,000") +
  theme(legend.position = "none")+
  coord_flip()
  
nat_bar_solo

############################################################
# combine plots
############################################################
### save combined plots
bar_plot = grid.arrange(nat_bar_solo, perdiff_solo_plot, ncol = 2, 
                        top =  textGrob("Relationship between cumulative estimated COVID-19 infections and confirmed cases as of February 28, 2021", 
                                        gp=gpar(fontsize=15)))

ggsave(bar_plot, filename = paste0(plot_path, "fig-countries-cases-bar.png"),
       width=12, height=8)


#