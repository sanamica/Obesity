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



## Reading  data
usda_access <- read_csv("data/usda_access.csv")
usda_access <- usda_access %>% 
  select(c(1:3,8,13,18,20,22,27))


usda_grocery <- read_csv("data/usda_store.csv")
usda_grocery <- usda_grocery %>% 
  select(c(1:3,5,11,17))


usda_restaurant <- read_csv("data/usda_resturant.csv")
usda_restaurant <- usda_restaurant %>% 
  select(c(1:3,8,14))

##Reading Health
usda_health <- read_csv("data/usda_health.csv")
usda_health <- usda_health %>% 
  select(1:3, 5,7,13)
##Socio Economic
socioeco <-  read_csv("data/usda_socioeco.csv")
socioeco <- socioeco %>% 
  select(c(1:9,12:13))

#Using reduce to merge multiple data frames (reduce= lambda)
usda <- list(usda_access,usda_grocery,usda_restaurant,usda_health,socioeco) %>% 
  reduce(inner_join,by = c( "State", "County", "FIPS"))

usda <- usda %>% 
  filter(State != "DC") %>% 
  rename("GEOID"=FIPS)


county_shape <- st_read("./data/acs_2012_2016_county_us_B27001/acs_2012_2016_county_us_B27001.shp")
#shpsimp <- simplify_shape(county_shape, fact = 0.05)
county_shape <- append_data(county_shape,usda,key.shp="GEOID", key.data = "GEOID",ignore.duplicates = TRUE,ignore.na = TRUE)


