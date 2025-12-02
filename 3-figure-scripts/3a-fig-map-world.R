################################################################################
# Tracking the progress of the COVID-19 pandemic
################################################################################

# Map of COVID-19 cases vs. estimated infections by countries #

################################################################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(rgdal)
library(rgeos)
library(leaflet)
library(maptools)
library(sf)
library(tigris)

#---------------------------------------
# Load and mutate data
#---------------------------------------

end_estimates <- readRDS(paste0(results_path, "end_estimates.RDS"))

# show cumulative cases 
end_estimates <- end_estimates %>% rename(positive = total_cases, total_cases = positive) 

covid_world_data = end_estimates %>% 
  dplyr::select(country, estimated_cases, positive, population) %>%
  rename(NAME_1 = country) %>%
  mutate(obs_label = paste0("<b>", NAME_1, "</b><br>", 
                            sprintf("%0.3f", positive/population*1000), " cases per 1,000", "<br>",
                            format(positive, big.mark=",", digits=0, scientific=F, trim = TRUE), " observed cases"),
         ratio_label = paste0("<b>", NAME_1, "</b><br>", 
                              ifelse(positive==0, "No ratio", sprintf("%0.1f", estimated_cases/positive)),
                              ifelse(positive==0, "", " estimated : observed"), "<br>",
                              format(estimated_cases, big.mark=",", digits=0, scientific=F, trim = TRUE), " estimated cases", "<br>",
                              format(positive, big.mark=",", digits=0, scientific=F, trim = TRUE), " observed cases"))

# read in US state boundaries
USA_Adm_1 <- tigris:::states(cb = TRUE)
USA_Adm_1 <- st_as_sf(USA_Adm_1)


territories = c('AS', 'VI', 'MP', 'GU')
USA_Adm_1 <- USA_Adm_1 %>% filter(!USA_Adm_1$STUSPS %in% territories)
USA_Adm_1 <- as_Spatial(USA_Adm_1)

# merge cases with shape file
USA_shp = merge(USA_Adm_1, covid_usa_data, by.x = 'NAME', by.y = 'NAME_1')

# get cases per pop
USA_shp$ratio = ifelse(USA_shp$positive==0, NA, USA_shp$estimated_cases / USA_shp$positive)
USA_shp$obs_case_perpop = USA_shp$positive / USA_shp$population * 1000

# modify interval labels
label_interval <- function(breaks) {
  paste0(breaks[1:length(breaks) - 1], " - ", breaks[2:length(breaks)])
}

ratio_quantiles <- quantile(USA_shp$ratio, c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm=TRUE)
obs_case_perpop_quantiles <- unique(quantile(USA_shp$obs_case_perpop, 
                                             c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                             na.rm=TRUE))

# create categorical variable
USA_shp$ratio = cut(USA_shp$ratio, 
                    ratio_quantiles,
                    include.lowest=TRUE,
                    labels = label_interval(round(ratio_quantiles, 0))
)

USA_shp$obs_case_perpop_cat = cut(USA_shp$obs_case_perpop, 
                                  obs_case_perpop_quantiles,
                                  include.lowest=TRUE,
                                  labels = label_interval(round(obs_case_perpop_quantiles, 1))
)


# define color palette
gnbu_colors = brewer.pal(n=6,"GnBu")[2:6]
orrd_colors = brewer.pal(n=6,"OrRd")[2:6]

exp_cases_pal = colorFactor(gnbu_colors, USA_shp$ratio)
obs_cases_pal = colorFactor(orrd_colors, USA_shp$obs_case_perpop_cat)
