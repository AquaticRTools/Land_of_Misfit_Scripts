library(shiny)
library(shinyjs)
library(leaflet)
library(mapview)
library(DT)
library(tidyverse)
library(rgdal)
library(rgeos)
library(raster)
library(measurements)
library(shinythemes)
library(leafem)
library(leafpop)


shinyUI(fluidPage(theme=shinytheme("yeti"),
                  shinyjs::useShinyjs(),
                  div(
                    id = "loading_page",
                    h1("Loading...")
                  ),
                  hidden(
                    div(
                      id = "main_content",
                      navbarPage("VDEQ EDAS Station Creation Tool",
                                 tabPanel('Enter Location',
                                          id = "resetArea",
                                          sidebarPanel(
                                            id = "resetArea1",
                                            h4('Instructions:'),
                                            p("Enter the latitude and longitude for the new station in the 
                                              appropriate fields below. Remember, we are in the western 
                                              hemisphere! After clicking the 'Create Station' button, 
                                              the map will populate with the new station such that you can
                                              double check your coordinates. The appropriate watershed, ecoregion, etc. 
                                              information will self populate below the map for you to enter into the EDAS database.
                                              
                                              For stream order and WQS information you will need to use the map to zoom to your site, 
                                              turn on the WQS layer for class information and the Virginia ProbMon Sample Frame layer
                                              for stream order information."),
                                            textInput('stationLat',strong('New Station Latitude:'),placeholder='e.g. 37.27854'),
                                            textInput('stationLng',strong('New Station Longitude:'),placeholder='e.g. -80.01876'),
                                            actionButton('plotStation',"Locate New Station",class='btn-block')),
                                          
                                          mainPanel(
                                            leafletOutput("stationMap",width="100%",height=450),
                                            h4(strong('Macroinvertebrate EDAS Information')),
                                            DT::dataTableOutput("stationDetailsMACRO"),
                                            br(),
                                            h4(strong('Fish EDAS Information')),
                                            DT::dataTableOutput("stationDetailsFISH"),
                                            actionButton("reset_input", "Reset inputs")
                                          )),
                                 
                                 
                                 
                                 tabPanel('About',
                                          h6('This tool organizes spatial information associated with various layers to
                                             simplify the Station Data entry for the EDAS databases.'),
                                          br(),br(),
                                          h6('Please contact Emma Jones for technical support: emma.jones@deq.virginia.gov'))
                                          )
                                          )))
                                 )