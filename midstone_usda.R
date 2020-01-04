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
  rename("PACCESS" = `PCT_LACCESS_POP15`,"PACCESS_I" = `PCT_LACCESS_LOWI15`,"PLACCESS_HHNV" = `PCT_LACCESS_HHNV15`,
         "PLACCESS_SNAP" = `PCT_LACCESS_SNAP15`, "LACCESS_C"=`LACCESS_CHILD15`, "LACCESS_S"=`LACCESS_SENIORS15`, 
         "PDIABETES" = `PCT_DIABETES_ADULTS13`, "POBESE" = "PCT_OBESE_ADULTS13")
usda <- usda %>% 
   filter(State != "DC")

#EXPLORATION

usda %>% select(PCT_OBESE_ADULTS13) %>% 
  summary()
# one missing obesity at Bedford, VA, dropped it. Thing to remember while merging with mapping dataframe)
usda %>% filter(is.na(PBESE))
usda <- usda %>% filter(!is.na(POBESE)) 
#missing values 
usda %>% summarise_all(funs(sum(is.na(.)))) %>% 
  view()
usda %>% filter(is.na(PACCESS_I)) %>% 
  view()
usda <- usda %>% filter(!is.na(POBESE)) 

#Corelation
corrs <- usda %>% select(-c("FIPS", "State", "County")) %>% 
  drop_na() %>% 
  cor()

library(corrplot)
corrplot(corrs,type = "upper",order = "hclust",tl.col = "black")



#cleaning mapping america data on health
mapping_health<- read_csv("data/MappingAmerica_Health.csv",skip = 15)
mapping_health<- mapping_health %>%
  select(1,14,25)
mapping_gini<- read_csv("data/MappingAmerica_Work-Wealth-Poverty.csv",skip = 15)

mapping_gini<- mapping_gini %>%
  select(1,4)
mapping_gini <-  mapping_gini %>%
  rename("County"=X1, "Gini_Cofficient"=`Gini ranges from 0 (complete equality) to 1 (complete inequality).`) 
  mapping_gini <-  na.omit(mapping_gini) #removing empty row  
  

mapping_health <-  mapping_health %>%
  rename("County"=X1,"PPHINACTIVE"= `Percentage of adults age 20 and over reporting no leisuren/atime physical activity.`,
         "PFISECURITY"= `Percentage of the population that did not have access to a reliable source of food during the past year.`)
mapping_health <-  na.omit(mapping_health) #removing empty row

mapping <- mapping_gini %>% 
  inner_join(mapping_health, by = c("County"))

mapping_anti <- mapping_gini %>% 
  anti_join(mapping_health, by = c("County"))

mapping_health <- mapping_health%>%
  # mutate(State=unlist(lapply(strsplit(County,", "),function(x) x[2])),
  #        County=gsub(",.*","",County))
   separate(County,c("County","State"),sep=",")
mapping_health$County <- gsub(' County', '' , mapping_health$County) #Regex to remove county after space
mapping_health$State <- gsub(' ', '' , mapping_health$State)

# usda_mapping <- merge(x=usda,y=mapping_health,
#       by = "County", all.y=TRUE)

usda_mapping <- usda %>% 
  inner_join(mapping_health, by = c("County","State"))

usda_mapping_anti <- usda %>% 
  anti_join(mapping_health, by = c("County","State"))
#Getting distinct information
usda %>% distinct() %>% 
  count(County) %>% 
  view()

mapping_health %>% distinct() %>% 
  count(County) %>% 
  view()
tail(mapping_health)
tail(usda)

