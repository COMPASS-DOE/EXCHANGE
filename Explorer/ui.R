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
               tabPanel("Data"),
               tabPanel("Get Involved")

    )

)
