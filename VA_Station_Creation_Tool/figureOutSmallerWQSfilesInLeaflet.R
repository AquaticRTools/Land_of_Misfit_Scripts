#point <- data.frame(name='station',lat=37.93166667,lng=-75.48972222)
point <- data.frame(name='station',lat=37.45,lng=-79)
coordinates(point) <- ~lng+lat
proj4string(point) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")       
#point_prj <- spTransform(point,CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=-8000000 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
point_prj <- spTransform(point,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 
                                           +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ")) #then can transform it to same as other shapefiles

WQS <- readOGR('data','wqs_riverine_id305b_2013_84')
WQS_prj <- readOGR('C:/HardDriveBackup/R/AssessmentTool/AssessmentShinyApp_v1.1/data','wqs_riverine_id305b_2013_albers')

identicalCRS(point,WQS)
identicalCRS(point_prj, WQS_prj)

step1 <- gBuffer(point_prj,width=1000)
plot(step1, col='blue');plot(WQS_prj, add=T)

step2 <- WQS_prj[step1,]
plot(step2)

step3 <- spTransform(step2,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") )

leaflet()%>%
  addProviderTiles(providers$Thunderforest.Landscape,group='Thunderforest Landscape')%>%
  addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
  addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
  #addPolylines(data=step3, color='blue', group="Virginia ProbMon Sample Frame",
  #             popup=popupTable(step3))%>%
  addPolygons(data=stationHUC,color='blue',fill=0.02,stroke=0.1,group="Station HUC6",
              popup=paste(sep='<br/>',
                          paste("Station HUC:",stationHUC@data$VAHU6),
                          paste("HUC6 Name:",stationHUC@data$VaName),
                          paste("Assessment Region",stationHUC@data$ASSESS_REG)))%>%#%>%hideGroup('Catchment')
  
  
  addPolylines(data=vafrm_HUC, color='black', group="Virginia ProbMon Sample Frame",
               popup=leafpop::popupTable(vafrm_HUC))%>%hideGroup("Virginia ProbMon Sample Frame")%>%
  addPolylines(data=wqs_HUC, color='darkslategray', group="Water Quality Standards",
               popup=leafpop::popupTable(wqs_HUC))%>%hideGroup("Water Quality Standards")
  
  



stationHUC <- assessmentRegions[point,]
wqs_HUC <- WQS[stationHUC,]
vafrm_HUC <- vafrm99_05[stationHUC,]
