setwd("/Users/anami/nss_data_science/Obesity_food_enviroment/")
options(scipen = 999)

library(tidyverse)
library(plotly)
library(dplyr)
library(tidycensus)
library(sf)
library(leaflet)
library(tmap)
library(tmaptools)
library(rmapshaper)

# usda <- readRDS("data/usda_clean.rds")

# # ## Reading  data
# 
# 
# county_shape <- st_read("./data/acs_2012_2016_county_us_B27001/acs_2012_2016_county_us_B27001.shp",stringsAsFactors = FALSE)
# #shpsimp <- simplify_shape(county_shape, fact = 0.05)
# county_shape <- append_data(county_shape,usda,key.shp="GEOID", key.data = "GEOID",ignore.duplicates = TRUE,ignore.na = TRUE)
# county_shape <- county_shape %>%
#   select(-c(3:5))
# 
# saveRDS(county_shape, file = "data/county_shape.RDS")

county_shape <- readRDS("data/county_shape.RDS")




# Create a state FIPS field 
county_shape <- mutate(county_shape, STFIPS = stringr::str_sub(GEOID, 1, 2))

# Aggregate the county layer by state 
states <- county_shape %>%
  aggregate_map(by = "STFIPS")

mycols <- c("#f0f4c3", "#dce775", "#cddc39", "#afb42b", "#827717")
var <- "POBESE"

tm <- tm_shape(county_shape,projection = 2163) +
  
  tm_fill(var, midpoint = 0,
          n=9,
          palette = "Set1",
          border.col = "black",
          title='Obesity Rate',
          border.alpha = .5,
          id = "NAME",
          textNA = 'Unreliable',
          colorNA = "grey",
          alpha= 1,
          popup.vars = c(
                         "Low access to grocery store & lowincome %" = "PACCESS_I",
                         "SNAP households, low access to grocery store %" = "PLACCESS_SNAP",
                         "White, low access to grocery store %" = "PLACCESSWHITE",
                         "Black, low access to grocery store %" = "PLACCESSBLACK",
                         "Hispanic, low access to grocery store %" = "PLACCESSHISP",
                         "Asian, low access to grocery store %" = "PLACCESSNHAASIAN",
                         "Number of Grocery stores/1,000 pop" = "GROC14",
                         "Fast-food restaurants/1,000 pop" = "FFRPTH14",
                         "Adult diabetes rate" = "PDIABETES",
                         "Recreation & fitness facilities" = "RECFACPTH14", 
                         "Median household income" = "MEDHHINC15",
                         "Poverty rate" ="POVRATE15"
                         ))+
  
  tm_polygons(var, 
              
              style = "quantile",           #bin selecion
              border.col = "white",
              border.alpha = 0.5,
              
             ) +  

  tm_legend(legend.position = c("left","bottom")) +
  tm_layout(title = "Obesity Rate per County in U.S.A",
            title.size = 1.1,
            title.position = c("center", "top"),
            inner.margins = c(0.06, 0.10, 0.10, 0.08)) + 
  tm_shape(states) +
    tm_style("natural")+
  tm_borders(lwd = 2,col = "black", alpha =.25)
  
  tmap_leaflet(tm)
  
  # STATE SPECIFIC MAP
  
  # tn <- county_shape %>% filter(State == 'TN')
  