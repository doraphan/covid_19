library(shiny)
library(readr)
library(shinydashboard) 
library(shinyWidgets)
library(rsconnect)
library(leaflet)
library(ggplot2)
library(dplyr)
library(plotly)


ui <- dashboardPage(
    dashboardHeader(title = "COVID 19"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(id = 'background',
    tags$head(tags$style(HTML('
     h2 {
  font-family: "Lucida Console", Courier, monospace;
  display: block;
  font-size: 20px;
  margin-top: 1em;
  margin-bottom: 1em;
  margin-left: 0;
  margin-right: 0;
  font-weight: bold;
  
     }
.main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 30px;
}
#background {
background-color: #f7f5f5;
}
                              '))),
    h2("GLOBAL SITUATION"),
    fluidRow(
        column(width = 3,
               box(width = 12,
               selectInput("map_type","Select type of map:", 
                            choices = list("Choropeth" = 'choropeth', "Bubble" = 'bubble'), selected = 1),
               radioButtons("data_type", "Choose to display:",
                            c("Cases" = "total_cases",
                              "Deaths" = "total_deaths"))),
               valueBox(format(cases_global_sum, big.mark=",", scientific=FALSE),"TOTAL CASES", width = 12, color = "purple"),
               br(),
               valueBox(format(deaths_global_sum, big.mark=",", scientific=FALSE), "TOTAL DEATHS",  width = 12, color = "blue")
        ),
        tabBox(width = 9,
        tabPanel(title = "Map",  addSpinner(plotlyOutput("map_cases", height = '365px'), spin = "bounce")),
        tabPanel(title = "Daily Statistics", 
           addSpinner(plotlyOutput("daily_cases", height = '365px'), spin = "bounce")
    )
    )),
    h2("CONTINENT SITUATION"),
    fluidRow(
    box(width = 12,
        column(width = 4, 
            addSpinner(plotlyOutput("north_america"), spin = "bounce"),
            br(),
            addSpinner(plotlyOutput("south_america"), spin = "bounce")
            
        ),
        column(width = 4, 
               addSpinner(plotlyOutput("asia"), spin = "bounce"),
               br(),
               addSpinner(plotlyOutput("europe"), spin = "bounce")
               
        ),
        column(width = 4, 
               addSpinner(plotlyOutput("oceania"), spin = "bounce"),
               br(),
               addSpinner(plotlyOutput("africa"), spin = "bounce")
               
        )
    ))
))
