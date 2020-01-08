setwd("/Users/anami/nss_data_science/Obesity_food_enviroment/")
options(scipen = 999)

library(tidyverse)
library(plotly)
library(dplyr)
#library(tm) (used to delete word in column)
library(caret)
library(glmnet)
library(coefplot)
library(ggpubr)

#EXPLORATION
usda <- readRDS("data/usda_clean.rds")
  # select(-c(1))
# usda %>% select(POBSE) %>% 
#   summary()
# one missing obesity at Bedford, VA, dropped it. Thing to remember while merging with mapping dataframe)

#missing values 
# usda %>% summarise_all(funs(sum(is.na(.)))) %>% 
#   view()
# usda %>% filter(is.na(PACCESS_I)) %>% 
#   view()
#Trying to get region
usda <- usda %>% filter(!is.na(POBESE)) 
region_key <-usda %>% 
  group_by(State) %>%
  mutate(State,region = state.region)

names(state.region) <- state.abb
usda$region <- state.region[usda$State]


## Exploring Obesity
#Distribution of Obesity seems to be normally distributed
usda %>% 
  ggplot(aes(x=POBESE)) + geom_histogram(bin = 50)

# Obesity ranges from 11.80% to 47.60 county lavel, Median 31.20, Mean = 31.02
usda %>% select(POBESE) %>% 
  summary()


usda %>%
  
  summarise(
    mean_obese = mean(POBESE, na.rm = TRUE),
    min_obese = min(POBESE, na.rm = TRUE),
    max_obese = max(POBESE, na.rm = TRUE)
  )


usda <- mutate(usda,
       obesity_rate = if_else(POBESE >= 35, "High", NA_character_),
        obesity_rate = if_else(POBESE >= 25 & POBESE < 35,
                               "Medium", obesity_rate),
       obesity_rate = if_else(POBESE < 25, "Low", obesity_rate))
 
  

usda %>%
  group_by(State) %>%
  mutate(POBESE_mean= mean(POBESE, na.rm = TRUE)) %>% 
  ggplot(aes(x=fct_reorder(State, POBESE,.desc = TRUE), y="POBESE", fill = obesity_rate)) + 
  geom_bar(stat = "identity")

  # ungroup() %>%
  # filter(POBESE > POBESE_mean) %>%
  # select(State, POBESE, POBESE_mean)

## State level
usda %>% 
  group_by(State) %>% 
  summarise('Average Obesity'= mean(POBESE)) %>% 

  ggplot(aes(x=fct_reorder(State, `Average Obesity`,.desc = TRUE), y=`Average Obesity`)) + 
  geom_bar(stat = "identity")


  # geom_vline(xintercept = 91.95,color="blue")
  # view()
  # ungroup() %>%
  # filter(POBESE > POBESE_mean) %>%
  # select(State, POBESE, POBESE_mean)

#Corelation
corrs <- usda %>% select(-c("GEOID", "State", "County")) %>% 
  drop_na() %>% 
  cor()

library(corrplot)
corrplot(corrs,type = "upper",order = "hclust",tl.col = "black")#ini ranges from 0 (complete equality) to 1 (complete inequality).`) 

#grouping by state
usda%>% 
  group_by(State) %>% 
  # summarise("Median Income"=median(MEDHHINC15, na.rm = TRUE)) %>%
  ggplot(aes(x=State, y="MEDHHINC15")) +geom_col() + geom_vline(xintercept = 91.95,color="blue")

usda_race<- usda %>%
  group_by(State) %>% 
  filter(State == "AL") %>% 
  pivot_longer(PWHITE:OTHER, names_to = RACE)

usda_race <- pivot_longer(usda, PWHITE:OTHER, names_to = "RACE")

usda %>% distinct() %>% 
  count(State) %>% 
  view()

  
