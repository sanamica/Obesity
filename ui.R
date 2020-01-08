setwd("/Users/anami/nss_data_science/Obesity_food_enviroment/")
library(shiny)
library("shinydashboard")


shinyUI(
    dashboardPage(
        dashboardHeader(title = 'Obesity and Food Enviroment'),
        dashboardSidebar(
            # selectInput("year", label = "Year:", choices = years,
            #             selected = 2014),
            "Data Source:" , 
            div(a(href = "https://www.ers.usda.gov/data-products/food-access-research-atlas/", img(src="usda.png", width = 110),
                  target = "_blank")),
            sidebarMenu(
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Connections", tabName = "connection", icon = icon("th"))
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "dashboard",
                        "Demography",
                        tabBox
                        (title = "Demograpgic Plots",
                            tabPanel("Tab1","Content"),
                            tabPanel("Tab2","Content2")
                            )
                        ),
                tabItem(tabName = "connection",
                        "Corelations")
                )
            )
            # fluidRow(
            #   plotOutput("scatter", width = 800)
            # ),
        #     fluidRow(
        #       # box(width = 12, status = 'primary',
        #       #     'Click on column name to sort.',
        #       #     dataTableOutput("table"))
        # )
    )
)



