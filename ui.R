setwd("/Users/anami/nss_data_science/Obesity_food_enviroment/")
library(shiny)
library("shinydashboard")
sidebar <- dashboardSidebar(selectinputId ="name",
                            label ="State" ,
                            choices =usda$State)


shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Obesity and Food Enviroment', titleWidth = 300),
    dashboardSidebar(
      # selectInput("State", label = "State:", choices = State,
      #             selected = "AL"),
      "Data Source:" ,
      div(a(href = "https://www.ers.usda.gov/data-products/food-access-research-atlas/", img(src="usda.png", width = 110),
            target = "_blank")),
      sidebarMenu( collapsed = FALSE,
                  
                   menuItem("Map", tabName = "map", icon=icon("globe")),
                   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                   id="tabs",
                   menuItem("Connections", tabName = "connection", icon = icon("th"))
                   # menuItem("Map", tabName = "map", icon=icon("globe"))
                
                  
                  


      ),
      # conditionalPanel(condition ="input.tabs== 'map'",
      #                selectInput(State_1, label = "State:", choices = usda$State %>% unique(),
      #                                        selected = "TN")),
      
      conditionalPanel(condition ="input.tabs== 'dashboard'",
                       selectInput("State", label = "State:", choices = usda$State %>% unique(),
                                   selected = "TN")),
      conditionalPanel(condition ="input.tabs== 'connection'",
                       selectInput("State_2", label = "State:", choices = usda$State %>% unique(),
                                   selected = "TN"),
                       selectInput("Select",label="Select:",
                                   choices = choices))
      
    ), #dashboardSidebar

    
    dashboardBody(
      #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}")
      tabItems(
        tabItem(tabName = "map",
                box(width ="100%", height ="1000px",title ="Obesity Map For U.S.A",
                    
                    leafletOutput("obesity_map", height = 850))
                ),
        
        tabItem(tabName = "dashboard",
                tabBox( width = 12,
                        title = "",
                        height = "260px",
                        
                    tabPanel(title = "Obesity Rate per County",plotlyOutput("obesity_county")),
                    # box(selectInput("State", label = "State:", choices = usda$State,
                    #              selected = "TN")),
                    tabPanel(title = "Obesity Rate per State",plotlyOutput("obesity_state"))
                   
                    
                )
                
        ) , #tabItem
        tabItem(tabName = "connection",
                fluidRow(
                # box(selectInput("State", label = "State:", choices = usda$State,
                #                 selected = "TN")),
                
                box(width = 8,
                    title = "Relation Between Obesity and Food Enviroment",
                    
                  plotlyOutput("coorelation_plot")
                    )
                ))
        
        
      )    #dbtabitems
    )    # dashboardBody
    
  )    #dbPage
)      #shinyui     
