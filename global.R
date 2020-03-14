options(scipen = 999)

library(shinydashboard)
library(tidyverse)
library(plotly)
library(dplyr)
library(caret)
library(glmnet)
library(coefplot)
library(ggplot2)
library(corrplot)
library(ggpubr)
library(DT)
library(sf)
library(leaflet)
library(tmap)
library(tmaptools)
library(rmapshaper)
library(rsconnect)

usda <- readRDS("data/usda_clean.rds")
usda_state_w <- readRDS("data/usda_state.rds")

# Mp Work
county_shape <- readRDS("data/county_shape.rds")

# Create a state FIPS field 
county_shape <- mutate(county_shape, STFIPS = stringr::str_sub(GEOID, 1, 2))

# Aggregate the county layer by state 
states <- county_shape %>%
  aggregate_map(by = "STFIPS")

mycols <- c("#f0f4c3", "#dce775", "#cddc39", "#afb42b", "#827717")

# states <- as.data.frame(county_shape) %>% 
#   dplyr::select(State_name) %>% 
#   unique()
# 
# states <- sort(states$State_name)
choices = c("Low access to grocery store %" = "PACCESS" ,
            "Low access to grocery store & lowincome %" = "PACCESS_I",
            "Household no car & low access to grocery store %"= "PLACCESS_HHNV",
            "SNAP households, low access to grocery store %" = "PLACCESS_SNAP",
            "White, low access to grocery store %" = "PLACCESSWHITE",
            "Black, low access to grocery store %" = "PLACCESSBLACK",
            "Hispanic, low access to grocery store %" = "PLACCESSHISP",
            "Asian, low access to grocery store %" = "PLACCESSNHAASIAN",
            "American Indian or Alaska Native, low access to store %" = "PLACESSNHA",
            "Hawaiian or Pacific Islander, low access to store %" = "PACCESSPNHI",
            "Number of Grocery stores/1,000 pop" = "GROC14",
            "Supercenters & club stores/1,000 pop" = "SUPERC14",
            "Convenience stores/1,000 pop" = "CONVS14",
            "Fast-food restaurants/1,000 pop" = "FFRPTH14",
            "Full-service restaurants/1,000 pop" = "FSRPTH14",
            "Adult diabetes rate" = "PDIABETES",
            "Recreation & fitness facilities/ 1,000" = "RECFACPTH14", 
            "Median household income" = "MEDHHINC15",
            "Poverty rate" ="POVRATE15",
            "White %" = "PWHITE","Black %" = "PBLACK", "Hispanic %" = "PHISP"
)

labels <- names(choices)
names(labels) <- choices

cor_val <- corrs <- usda %>% select(c("POBESE","PACCESS","PACCESS_I","PLACCESS_HHNV","PLACCESS_SNAP","PLACCESSWHITE", "PLACCESSBLACK", "PLACCESSHISP",
                                      "PLACCESSNHAASIAN", "PLACESSNHA", "PACCESSPNHI", "GROC14","SUPERC14", "CONVS14","FFRPTH14","FSRPTH14", "PDIABETES", 
                                      "RECFACPTH14",  "MEDHHINC15","POVRATE15"))

