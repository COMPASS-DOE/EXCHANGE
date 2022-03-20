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

                        # tags$head(
                        #     # Include our custom CSS
                        #     includeCSS("styles.css"),
                        #     includeScript("gomap.js")
                        # ),

                        leafletOutput("map", height = "calc(100vh - 80px)", width = "100%")
                        ),
               tabPanel("Data"),
               tabPanel("Get Involved")

    )

)
