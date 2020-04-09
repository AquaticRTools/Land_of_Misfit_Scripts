library(shiny)
library(shinyjs)
library(shinythemes)
library(leaflet)
library(mapview)
library(DT)
library(tidyverse)
library(rgdal)
library(raster)



# Load in Data
assessmentRegions <- readOGR('data','AssessmentRegions_VA84')
vafrm99_05 <- readOGR('data','vafrm_99_05')
WQS <- readOGR('data','wqs_riverine_id305b_2013_84')
decimalgrid <- readOGR('data','decimalgrid_84')
ecoregion_L3 <- readOGR('data','VA_level3ecoregion')
ecoregion_L4 <- readOGR('data','ECOregion4_84')
basin <- readOGR('data','VAbasins')
county <- readOGR('data',"COUNTY_84")
huc8 <- readOGR('data','huc8_84')


load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}



ui <- shinyUI(fluidPage(theme=shinytheme("yeti"),
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





server <- shinyServer(function(input, output, session) {
  
  # display the loading feature until data
  load_data()
  
  # Reset information when press reset
  observeEvent(input$reset_input, {
    shinyjs::reset("resetArea1")
  })
  
  ## Map ## 
  output$stationMap <- renderLeaflet({
    leaflet()%>%
      addProviderTiles(providers$OpenMapSurfer.Roads,group='Thunderforest Outdoors')%>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addLayersControl(baseGroups=c('Thunderforest Landscape','Esri World Imagery','Open Street Map'),
                       options=layersControlOptions(collapsed=T),position='topleft')%>%
      #addMouseCoordinates(style='basic')%>%
      addMiniMap(toggleDisplay=T)%>%
      leafem::addHomeButton(extent(assessmentRegions), "Back to Full Extent")%>%setView(-78.8,37.2,zoom=9)%>%
      addMeasure(activeColor='#3D535D',completedColor='#7D4479')
  })
  
  # Don't allow user to click plotStation unless they have both a Lat and Long
  observe({
    shinyjs::toggleState('plotStation', input$stationLat !="" && input$stationLng !="" )
  })
  
  ## Make a spatial object from user lat/long
  station <- reactive({
    req(input$plotStation)
    
    lat <- as.numeric(input$stationLat)
    lng <- as.numeric(input$stationLng)
    point <- data.frame(name='incident',lat=lat,lng=lng)
    coordinates(point) <- ~lng+lat
    proj4string(point) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")       
    return(point)
  })
  
  
  ## Plot Station on map ##
  observeEvent(input$plotStation,{
    lat <- as.numeric(input$stationLat)
    lng <- as.numeric(input$stationLng)
    # make a spatial object from lat/long
    point <- data.frame(name='station',lat=lat,lng=lng)
    coordinates(point) <- ~lng+lat
    proj4string(point) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")       
    
    
    stationHUC <- assessmentRegions[station(),]
    wqs_HUC <- WQS[stationHUC,]
    vafrm_HUC <- vafrm99_05[stationHUC,]
    
    
    # Add station location and highlight HUC to map
    leafletProxy('stationMap')  %>% clearGroup("Station HUC6") %>% clearGroup('New Station') %>%clearControls() %>%
      setView(lng=lng,lat=lat,zoom=12)%>%
      addPolygons(data=stationHUC,color='blue',fill=0.02,stroke=0.1,group="Station HUC6",
                  popup=paste(sep='<br/>',
                              paste("Station HUC:",stationHUC@data$VAHU6),
                              paste("HUC6 Name:",stationHUC@data$VaName),
                              paste("Assessment Region",stationHUC@data$ASSESS_REG)))%>%#%>%hideGroup('Catchment')
      
      
      addPolylines(data=vafrm_HUC, color='black', group="Virginia ProbMon Sample Frame",
                   popup=leafpop::popupTable(vafrm_HUC))%>%hideGroup("Virginia ProbMon Sample Frame")%>%
      addPolylines(data=wqs_HUC, color='darkslategray', group="Water Quality Standards",
                   popup=leafpop::popupTable(wqs_HUC))%>%hideGroup("Water Quality Standards")%>%
      
      addCircleMarkers(data=station(),~station()$lng,~station()$lat,radius=8,
                       color=~'red',stroke=F,fillOpacity=0.5,
                       group='New Station',layerId='NewStation',popup='New Station')%>%                          
      addLayersControl(baseGroups=c('Thunderforest Landscape','Esri World Imagery','Open Street Map'),
                       overlayGroups=c('New Station',"Water Quality Standards","Virginia ProbMon Sample Frame",
                                       "Water Quality Standards",'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')
  })
  
  output$stationDetailsFISH <- DT::renderDataTable({
    req(input$plotStation)
    
    stationInformation <- data.frame(
      Basin = basin[station(),]$BASIN,
      HUC8 = substr(assessmentRegions[station(),]$HUC12,0,8),
      Watershed = assessmentRegions[station(),]$VAHU6,
      MapQuad = decimalgrid[station(),]$NAME,
      DEQregion = assessmentRegions[station(),]$ASSESS_REG,
      County = county[station(),]$DESCRIPT,
      Ecoregion = ecoregion_L3[station(),]$US_L3CODE,
      Subecoregion = ecoregion_L3[station(),]$US_L3NAME)
    
    datatable(stationInformation,extensions = 'Buttons', escape=F, rownames = F,
              options= list(scrollX = TRUE,dom='Bt',
                            buttons=list('copy')))
  })
  
  output$stationDetailsMACRO <- DT::renderDataTable({
    req(input$plotStation)
    
    stationInformation <- data.frame(
      Basin = huc8[station(),]$NAME,
      HUC8 = substr(assessmentRegions[station(),]$HUC12,0,8),
      Watershed = assessmentRegions[station(),]$VAHU6,
      MapQuad = decimalgrid[station(),]$NAME,
      DEQregion = assessmentRegions[station(),]$ASSESS_REG,
      County = county[station(),]$DESCRIPT,
      Ecoregion_Code = ecoregion_L4[station(),]$ECO,
      Ecoregion = paste(ecoregion_L3[station(),]$US_L3NAME,' (',ecoregion_L4[station(),]$ECO,')',sep=''),
      Subecoregion = paste(ecoregion_L4[station(),]$NAME,' (',ecoregion_L4[station(),]$ECO,')',sep=''))
    
    datatable(stationInformation,extensions = 'Buttons', escape=F, rownames = F,
              options= list(scrollX = TRUE,dom='Bt',
                            buttons=list('copy')))
  })
  
  
  
})





shinyApp(ui,server)


