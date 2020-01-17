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
# library(waffle)
# library(ggwaffle)
library(ggplot2)
library(hrbrthemes)
library(corrplot)
library(wesanderson)



#EXPLORATION
usda <- readRDS("data/usda_clean.rds")
usda_state_w <- readRDS("data/usda_state.rds")


# one missing obesity at Bedford, VA, dropped it. Thing to remember while merging with mapping dataframe)

#missing values 
# usda %>% summarise_all(funs(sum(is.na(.)))) %>% 
#   view()
# usda %>% filter(is.na(PACCESS_I)) %>% 
#   view()


## Exploring Obesity


#Distribution of Obesity seems to be normally distributed
# usda %>% 
#   ggplot(aes(x=POBESE)) + geom_histogram(bin = 50)

# Obesity ranges from 11.80% to 47.60 county lavel, Median 31.20, Mean = 31.02
# usda %>% select(POBESE) %>% 
#   summary()



#PLOTS THEME

usda_theme <- theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
                    axis.title.y = element_text(size = 14),
                    plot.title = element_text(size = 15, face = "bold"))

usda_theme1 <- theme(axis.title.y = element_text(size = 14),
                     plot.title = element_text(size = 15, face = "bold")) 


o_c <- usda %>% 
  filter(State=='TX') %>% 
  ggplot(aes(x=fct_reorder(County,POBESE, .desc = TRUE), y=POBESE, fill =obesity_rate))+geom_bar(stat = "identity") +
  labs(x= "", y = "Obesity Rate")  + theme(axis.text.x =element_text(angle = 90, hjust = 1, size = 14))
ggplotly(o_c)

## removing label
o_c <- usda %>% 
  filter(State=='TX') %>% 
  ggplot(aes(x=fct_reorder(County,POBESE, .desc = TRUE), y=POBESE, fill =obesity_rate))+geom_bar(stat = "identity") +
  labs(x= "", y = "Obesity Rate")  + theme_void()
ggplotly(o_c)




#Corelation
corrs <- usda %>%select(-c("GEOID", "State", "County", "obesity_rate","region","POPE15", "PWHITE", "PBLACK", "PHISP", "PNHASIAN" ,
                           "PNHA", "OTHER", "GROC14" )) %>%
  drop_na() %>%
  cor()


corrplot(corrs,type = "upper",order = "hclust",tl.col = "black",
       )
dev.copy(png,"corplot_matrix")
dev.off()
# ggsave('plots/Cor_matris.png', width = 12, height = 10)

#ini ranges from 0 (complete equality) to 1 (complete inequality).`)
p_cor <- usda %>%
  filter(State=="TX") %>%
  ggscatter(x="PACCESS", y = "POBESE",
            add = "reg.line", conf.int = TRUE, color="obesity_rate", pallette = "jco") +
  stat_cor(aes(color = obesity_rate), label.x = 3) +
  geom_point(alpha=.40)

ggplotly(p_cor)

# label.x =  3


# p_cor <- usda %>%
#   filter(State=="AL") %>%
#   ggscatter(x="PACCESS", y = "POBESE",
#             add = "reg.line", conf.int = TRUE, color="obesity_rate", pallette = "jco") +
#   stat_cor(aes(label = "County")) +
#   geom_point(alpha=.40)
# 
# ggplotly(p_cor)


# ggplotly Corelation
p_obesity_g <- usda %>%
  filter(State=="AL") %>%
  ggscatter(x="PACCESS", y = "POBESE", text = paste("County:",County),
          add = "reg.line", conf.int = FALSE, color="obesity_rate")

 access<- p_obesity_g+geom_point(alpha=.40)+stat_cor(aes(color=obesity_rate))


ggplotly(access)

# PLOTLY

p_obesity <- usda %>% 
  filter(State=="TX") 
fit <- lm(POBESE ~ PACCESS, data = p_obesity)
posn_j <- position_jitter(0.1,seed = 50)

P_O <- ggplot(p_obesity,aes(x=PACCESS, y=POBESE, color = obesity_rate ))+
  labs(x= "Access", y = "Obesity Rate") +
    geom_smooth(method = "lm",
              color = "red",
              se = FALSE,
              size = 1)+
  geom_point(alpha =0.7, size = 3, position = "jitter",aes(text = paste("County:",County,"Obesity%:",POBESE))) +
  theme(text = element_text(family = "sherif",size = 14),
        rect = element_blank(),
    panel.grid =element_blank(),
        title = element_text (color = "#8b0000" ),
        axis.line = element_line(color = "grey"))

ggplotly(P_O,tooltip = "text") 
# %>% config(displayModeBar = F)


  # geom_vline(intercept =global_mean, color ="grey40",linetype =3)
#  text = paste("County:",County)


 
p_obesity%>% plot_ly(x = ~PACCESS) %>% 
  add_markers(y = ~POBESE) %>% 
  add_lines(x= ~PACCESS, y = fitted(fit)) %>% 
  layout(showlegend = F) 

ggsave('plots/coorelation.png')

# Adding regressionline
fv <- p_obesity  %>% lm(POBESE ~ PACCESS,.) %>% fitted.values()

p_obesity %>% 
  plot_ly(x= ~PACCESS, y = ~POBESE, mode = "markers" ) %>% 
  add_markers(y = ~POBESE) %>% 
  add_trace(x= ~PACCESS, y= fv, mode = "lines") %>% 
  layout(showlegend = F)

  

paccess <- plot_ly(p_obesity+geom_point(alpha=.40)+stat_cor(aes(color=obesity_rate)))

# ggsave('plots/recorrelation.png')
p <- gg




# usda %>%
#   group_by(State) %>%
#   mutate(POBESE_mean= mean(POBESE, na.rm = TRUE)) %>% 
#   ggplot(aes(x=fct_reorder(State, POBESE,.desc = TRUE), y="POBESE", fill = obesity_rate)) + 
#   geom_bar(stat = "identity")

  # ungroup() %>%
  # filter(POBESE > POBESE_mean) %>%
  # select(State, POBESE, POBESE_mean)

## PLOT Themes
#COUNTY Level

o_c <- usda %>%
  filter(State=="AL") %>%
  ggplot(aes(x=fct_reorder(County,POBESE, .desc = FALSE), y=POBESE, fill =obesity_rate))+geom_bar(stat = "identity") +
  coord_flip()+aes(text = paste("County:", County, "Obesity%:", POBESE))+
  labs(x= "", y = "Obesity Rate", fill = "Obesity Rate") +
  #geom_hline(aes(yintercept = mean(POBESE))) (need to get median line)
  scale_y_continuous(limits = c(0,50), expand = c(0, 0)) +
  theme(axis.text.y =element_blank(),
        rect = element_blank())
ggplotly(o_c, tooltip = "text") %>% config(displayModeBar = F)

## STATE LEVEL ANALYSIS
#not weighted

usda_state <- usda %>% 
  group_by(State) %>% 
  #mutate('Average_Obesity' = mean(POBESE))
  summarise('Average_Obesity'= mean(POBESE))

p <- ggplot(usda_state,aes(x=fct_reorder(State, Average_Obesity,.desc = TRUE), y=Average_Obesity)) + 
  geom_bar(stat = "identity") + 
  # theme(legend.position='none')+
  labs(x = "", y ="Obesity Percent") + 
  usda_theme1
ggplotly(p)

##trying to change order in Plotly
xform <- list(categoryorder = "array",
              categoryarray = usda_state$State)
data <- usda_state (State, Average_Obesity, stringsAsFactors= TRUE)
usda_state$State <- factor(data$State, levels = unique(data$State)[order(data$Average_Obesity, decreasing = TRUE)])

p1 <- plot_ly(usda_state, x = ~State, y = ~Average_Obesity,type = 'bar') %>% 
  layout(title = "Obesity in Different States", 
         xaxis = list(title="",categoryorder = "array", categoryarray = ~State),
         yaxis = list(title = "Obesity Percent"))
p1

##weighted state
usda_state_w <- usda %>%
  group_by(State) %>% 
  mutate(weight=POPE15/sum(POPE15,na.rm = TRUE)) %>%
  summarise(w_POBESE = weighted.mean(POBESE,weight)) %>%
  ungroup() %>% 
  mutate(obesity_rate = if_else(w_POBESE >=29, "High", NA_character_),
         obesity_rate = if_else(w_POBESE >= 28 & w_POBESE < 29 ,"Average", obesity_rate),
         obesity_rate = if_else(w_POBESE < 28, "Low", obesity_rate))

# usda_state_w<-usda_state_w %>% 
#   mutate(State,region = state.region)

# State level obesity ranges from 20.27, -35.18, Median =28.23, Mean = 28.60
usda_state_w%>% summary(w_POBESE)

 ggplot(usda_state_w,aes(x=fct_reorder(State, w_POBESE,.desc = F), y=w_POBESE, fill = region)) +
  geom_bar(stat = "identity") + usda_theme1 
 
 ## Final state
 
  S_P <-  ggplot(usda_state_w,aes(x=fct_reorder(State, w_POBESE,.desc = FALSE), y=w_POBESE, fill = region)) +
   geom_bar(stat = "identity") +  coord_flip()+
   labs(x= "", y = "Obesity Rate", title = "Distribution Of Obesity Across States")  +
   scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
   theme(rect = element_blank()) +usda_theme1
  ggplotly(S_P)
   
  
 
 ggsave('plots/state_p.png', width = 10, height = 12)
 
 
 S_P <-  ggplot(usda_state_w,aes(x=fct_reorder(State, w_POBESE,.desc = FALSE), y=w_POBESE, fill = region)) +
   geom_bar(stat = "identity") +  coord_flip()+
   labs(x= "", y = "Obesity Rate", title = "Distribution Of Obesity Across States")  +
   scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
   theme(rect = element_blank()) +usda_theme1
 S_P
 
 S_P + scale_fill_hue(l=40, c=35)



# Regional level

usda_region <- usda %>%
  
  group_by(region) %>%
  mutate(weight=POPE15/sum(POPE15,na.rm = TRUE)) %>%
  mutate(w_POBESE = weighted.mean(POBESE,weight)) %>%
  ungroup()

usda_region_w <- usda_region %>% 
  group_by(region) %>% 
  # #mutate('Average_Obesity' = mean(POBESE))
  summarise('weighted_obesity'=  weighted.mean(POBESE,weight))

# Weighted obesity : Min: 24.02 , Median 29.50, Mean: 28.65, Max: 30.03
summary(usda_region)


ggplot(usda_region,aes(x=fct_reorder(region, w_POBESE,.desc = TRUE), y= w_POBESE)) + 
  geom_bar(stat = "identity")

ggplot(usda,aes(x=region, y=POBESE, fill=region)) + labs(x ="", y="Obesity Rate", 
                       title= "Distribution Of Obesity Across Regions") +
  geom_boxplot() + usda_theme1 +theme(rect = element_blank())

ggsave('plots/region_boxn.png', width = 9, height = 7)

# usda_region <- usda %>% 
#   group_by(region) %>% 
#   #mutate('Average_Obesity' = mean(POBESE))
#   summarise('Average_Obesity'= mean(POBESE))
# 
# p <- ggplot(usda_region,aes(x=fct_reorder(region, Average_Obesity,.desc = TRUE), y=Average_Obesity)) + 
#   geom_bar(stat = "identity")
# 
# ggplotly(p)

# ggsave('plots/unweightedpobese_A.S.png')






  # geom_vline(xintercept = 91.95,color="blue")
  # view()
  # ungroup() %>%
  # filter(POBESE > POBESE_mean) %>%
  # select(State, POBESE, POBESE_mean)


#grouping by state
usda%>% 
  group_by(State) %>% 
  # summarise("Median Income"=median(MEDHHINC15, na.rm = TRUE)) %>%
  ggplot(aes(x=State, y="MEDHHINC15")) +geom_col() + geom_vline(xintercept = 91.95,color="blue")

# usda_race<- usda %>%
#   group_by(State) %>% 
#   filter(State == "AL") %>% 
#   pivot_longer(PWHITE:OTHER, names_to = RACE)


usda_long <- pivot_longer(usda, PWHITE:OTHER, names_to = "RACE", values_to = "PCT") 
# %>%  filter(State == "AL")

usda_long %>% filter (State=="AL") %>% 
  barplot(xlab = "RACE", col="PCT")
  

usda_race <- usda_long %>% 
  group_by(State) %>% 
  #mutate('Average_Obesity' = mean(POBESE))
  summarise('Percentage'= mean(PCT))

ggplot(usda_race,aes(x=fct_reorder(State, Percentage,.desc = TRUE), y=RACE), fill = RACE) + 
  geom_bar(stat = "identity")


# 
# ggplot(usda_race, aes(fill = RACE, values = vallue))+
#   geom_waffle(n_rows = 10, size= 0.5, flip= TRUE)
  # scale_fill_manual(values = c("#f59cbf","#00daff")) +
  # coord_equal() +
  # theme_minimal() +
  # theme_enhance_waffle() +
  # labs(title = "Proprtion of Race/ Ethnicity ",
  #      subtitle = "Alabama",
  #      caption = "Plot made using `waffle` - R package")
  
# 
# usda %>% distinct() %>% 
#   count(State) %>% 
#   view()
usda_race %>% 
  ggplot(aes(fill=RACE, values = value)) +
  geom_waffle()

usda_race %>% 
  ggplot(aes(x=fct_reorder(RACE,value,.desc= TRUE), y=value, fill = RACE)) +
  geom_col()

  plot <- table(usda_race$value)
  waffle(plot)
  
  ##Regresion
  
  
