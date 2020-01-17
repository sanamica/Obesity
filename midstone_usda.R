setwd("/Users/anami/nss_data_science/Obesity_food_enviroment/")
options(scipen = 999)

library(tidyverse)
#library(plotly)
library(dplyr)
#library(tm) (used to delete word in column)
library(caret)
library(glmnet)
library(coefplot)
library(plotly)
#library(ggpubr)

## Reading  data
usda_access <- read_csv("data/usda_access.csv")
usda_access <- usda_access %>% 
  
usda_access <- usda_access %>% 
  select(c(1:3,8,13,18,20,32,34,36,38,40,42))
# Total Population
usda_pop <- read_csv("data/Pop_est_county.csv")
usda_pop <- usda_pop %>% 
  select(c(1:3,9))
names(state.abb) <- state.name
usda_pop$abbrev <- state.abb[usda_pop$State]
usda_pop <- usda_pop %>% 
  select(-c(2)) %>% 
  rename('State'= abbrev)
  

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
usda <- list(usda_access, usda_pop, usda_grocery,usda_restaurant,usda_health,socioeco) %>% 
  reduce(inner_join,by = c( "State", "County", "FIPS"))
usda <- usda %>% 
  rename("GEOID"=FIPS,"PACCESS" = `PCT_LACCESS_POP15`,"PACCESS_I" = `PCT_LACCESS_LOWI15`,"PLACCESS_HHNV" = `PCT_LACCESS_HHNV15`,
         "PLACCESS_SNAP" = `PCT_LACCESS_SNAP15`, "PLACCESSWHITE" = `PCT_LACCESS_WHITE15`, "PLACCESSBLACK" = `PCT_LACCESS_BLACK15`,
         "PLACCESSHISP" =`PCT_LACCESS_HISP15`, "PLACCESSNHAASIAN" = `PCT_LACCESS_NHASIAN15`, "PLACESSNHA" =`PCT_LACCESS_NHNA15`,
         "PACCESSPNHI" = `PCT_LACCESS_NHPI15`,
         "POPE15"=`Population Estimate, 2015`, "PDIABETES" = `PCT_DIABETES_ADULTS13`, "POBESE" = `PCT_OBESE_ADULTS13`,
         "PWHITE" =`PCT_NHWHITE10`,"PBLACK"=`PCT_NHBLACK10`,"PHISP"=`PCT_HISP10`,"PNHASIAN"=`PCT_NHASIAN10`, "PNHA"= `PCT_NHNA10`,"PNHPI" = `PCT_NHPI10`)
#Creating other column for race
usda <- mutate(usda,
       OTHER=100-(PWHITE + PBLACK + PHISP + PNHASIAN + PNHA + PNHPI ))

#Changing column order moved OTHER

usda <- subset(usda,select = c(GEOID:PNHA,OTHER, MEDHHINC15:POVRATE15))

#converting state to region
#creat a list

NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")
NE.abrv <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
NE.ref <- c(NE.name,NE.abrv)

MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")
MW.abrv <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE",
             "ND","SD")
MW.ref <- c(MW.name,MW.abrv)

S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
S.abrv <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL",
            "KY","MS","TN","AR","LA","OK","TX")
S.ref <- c(S.name,S.abrv)

W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")
W.abrv <- c("AZ","CO","ID","NM","MT","UT","NV","WY","AK","CA",
            "HI","OR","WA")
W.ref <- c(W.name,W.abrv)

region.list <- list(
  Northeast=NE.ref,
  Midwest=MW.ref,
  South=S.ref,
  West=W.ref)
# Pass the list in function

usda$region <- sapply(usda$State, 
                      function(x) names(region.list)[grep(x,region.list)])
#Adding columes

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


## STATE LEVEL to create new column df$Col_name <- 
##weighted state
usda_state_w<- usda %>%
  group_by(State) %>% 
  mutate(weight=POPE15/sum(POPE15,na.rm = TRUE)) %>%
  mutate(w_POBESE = weighted.mean(POBESE,weight)) %>%
  ungroup() %>% 
  mutate(obesity_rate = if_else(w_POBESE >=29, "High", NA_character_),
         obesity_rate = if_else(w_POBESE >= 28 & w_POBESE < 29 ,"Average", obesity_rate),
         obesity_rate = if_else(w_POBESE < 28, "Low", obesity_rate))

#adding column for hoover text

usda$hover <- with(usda, paste("County:", County, '<br>', "Obesity%:", POBESE,
                               '<br>', "Obesity Rate:", obesity_rate,
                               '<br>', "Median Income:", MEDHHINC15))

#Rounding decimals 
#function
usda <- function(usda, 3) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(usda, class) == 'numeric'
  usda[numeric_columns] <-  round(usda, 3)
  usda
}

#Rounding
round_df(usda,2)

is.num <- sapply(usda, is.numeric)
usda[is.num] <- lapply(usda[is.num], round, 8)

# usda <- usda %>% 
#    filter(State != "DC") %>% 
#   filter(!is.na(POBESE))
  # write.csv(file = 'data/usda_final.csv',row.names = False) #row.names = False to not get extra row
# Adding Region column 
# region_key <-usda %>% 
#   group_by(State) %>%
#   mutate(State,region = state.region)
# 
# names(state.region) <- state.abb
# usda$region <- state.region[usda$State]

# usda_state_w<-usda_state_w %>% 
#   mutate(State,region = state.region)

# State level obesity ranges from 20.27, -35.18, Median =28.23, Mean = 28.60
# usda_state_w%>% summary(w_POBESE)

# converting to data.frame

# as.data.frame(usda_state_w)

# one missing obesity at Bedford, VA, dropped it. Thing to remember while merging with mapping dataframe)

#missing values  (17 missing on access 12 had medium rate obesity, and median income anpoverty rate 1 each )
# usda %>% summarise_all(funs(sum(is.na(.)))) %>% 
#   view()
# usda %>% filter(is.na(PACCESS_I)) %>% 
#   view()
# usda <- drop_na(usda)
#usda <- usda %>% filter(!is.na(POBESE))
# Saving files as rds

saveRDS(usda,file = "data/usda_clean.rds")
saveRDS(usda_state_w,file = "data/usda_state.rds")




usda <- readRDS("data/usda_clean.rds")
usda_state_w <- readRDS("data/usda_state.rds")


saveRDS(usda_state_w,file = "data/usda_state.rds")
usda_state_w %>% 
  filter(region== "Northeast") %>% 
ggplot(aes(x=fct_reorder(State, w_POBESE,.desc = TRUE), y=w_POBESE, fill = obesity_rate)) +
  geom_bar(stat = "identity") 



  p <- usda_state_w %>% 
    filter(region== "Northeastern") %>% 
  ggplot(aes(x=fct_reorder(State, w_POBESE,.desc = TRUE), y=w_POBESE, fill = obesity_rate)) +
    geom_bar(stat = "identity") + labs(x= "", y = "Obesity Rate")  + 
    theme(axis.text.x =element_text(angle = 90, hjust = 1, size = 14))
  ggplotly(p)



# usda %>% select(POBSE) %>% 
#   summary()
 

#Corelation
corrs <- usda %>% select(-c("GEOID", "State", "County")) %>% 
  drop_na() %>% 
  cor()

library(corrplot)
corrplot(corrs,type = "upper",order = "hclust",tl.col = "black", )#ini ranges from 0 (complete equality) to 1 (complete inequality).`) 
  
mapping_gini <-  na.omit(mapping_gini) #

#grouping by state



#cleaning mapping america data on health
mapping_health<- read_csv("data/MappingAmerica_Health.csv",skip = 15)
mapping_health<- mapping_health %>%
  select(1,14,25)
mapping_gini<- read_csv("data/MappingAmerica_Work-Wealth-Poverty.csv",skip = 15)

mapping_gini<- mapping_gini %>%
  select(1,4)
mapping_gini <-  mapping_gini %>%
  rename("County"=X1, "Gini_Cofficient"=`Gremoving empty row`)  
  

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
  count(nregion) %>% 
  view()

mapping_health %>% distinct() %>% 
  count(County) %>% 
  view()
tail(mapping_health)
tail(usda)

