#

library(shiny)
library(leaflet)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(

    navbarPage("EXCHANGE Explorer",
               
               theme = "lumen",
               tags$head(
                 # Include our custom CSS
                 includeCSS("style.css")
               ),
               tabPanel("Home"),
               tabPanel("Map",

                        leafletOutput("map", height = "calc(100vh - 80px)", width = "100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 90, left = "auto", right = 30, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Campaign Explorer")#,
                                      
                                      # selectInput("color", "Color"),
                                      # selectInput("size", "Size"),
                                      # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                      #                  # Only prompt for threshold when coloring or sizing by superzip
                                      #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      # )
                        )
                        
                        ),
               tabPanel("Data",
                        # Placeholder search criteria
                        fluidRow(
                        
                          column(3,
                                 selectInput("states", 
                                             "States", 
                                             c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), 
                                             multiple=TRUE)
                          ),
                          column(3,
                                 conditionalPanel("input.states",
                                                  selectInput("cities", 
                                                              "Cities", 
                                                              c("All cities"=""), 
                                                              multiple=TRUE)
                                 )
                          ),
                          column(3,
                                 conditionalPanel("input.states",
                                                  selectInput("zipcodes", 
                                                              "Zipcodes", 
                                                              c("All zipcodes"=""), 
                                                              multiple=TRUE)
                                 )
                          )
                          ),
                        hr(),
                        DT::dataTableOutput("ziptable")
                        ),
               tabPanel("Get Involved")

    )

)
