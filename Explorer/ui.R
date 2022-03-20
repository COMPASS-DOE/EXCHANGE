#

library(shiny)
library(leaflet)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(

    navbarPage("EXCHANGE Explorer",
               theme = "lumen",
               tabPanel("Home"),
               tabPanel("Map",

                        leafletOutput("map", height = "calc(100vh - 80px)", width = "100%")
                        ),
               tabPanel("Data",
                        # Placeholder search criteria
                        fluidRow(
                        
                          column(3,
                                 selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                          ),
                          column(3,
                                 conditionalPanel("input.states",
                                                  selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                                 )
                          ),
                          column(3,
                                 conditionalPanel("input.states",
                                                  selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                                 )
                          )
                          ),
                        hr(),
                        DT::dataTableOutput("ziptable")
                        ),
               tabPanel("Get Involved")

    )

)
