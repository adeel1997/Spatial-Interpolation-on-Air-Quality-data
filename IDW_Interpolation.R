library(sf)
library(sp)
library(gstat)
library(rgdal)
library(raster)
library(leaflet)

# Setting the working directory
setwd("C:/Users/ACS/Desktop/Hawa_Dawa/Blog_Analysis/Spatial-Interpolation-on-Air-Quality-data")
NO2 <- read.csv("Data/NO2_collector_2020.csv")
coordinates(NO2) <- ~Longitude + Latitude
crs(NO2) 
proj4string(NO2)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# Changing the Projection
NO2_T <- spTransform(NO2, CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

# Reading the Shape file of Munich
Munich <- readOGR("Data/Munich_Raster_shape_file.shp")
crs(Munich)
Munich_T <- spTransform(Munich, CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
# Crating Grids for Munich
grdpts <- makegrid(Munich_T)
spgrd <- SpatialPoints(grdpts, proj4string = CRS(proj4string(Munich_T)))
spgrdWithin <- SpatialPixels(spgrd[Munich_T,])

#IDW Interpolation

NO2.idw_2019 <- gstat::idw(Q1_2019 ~ 1, NO2_T, newdata=spgrdWithin, idp=2.0)
NO2.idw_2020 <- gstat::idw(Q1_2020 ~ 1, NO2_T, newdata=spgrdWithin, idp=2.0)
raster_2019_Q1_IDW  <- raster(NO2.idw_2019)
raster_2020_Q1_IDW  <- raster(NO2.idw_2020)
raster_diff_Q1_IDW <- overlay(raster_2020_Q1_IDW, raster_2019_Q1_IDW, fun=function(r1, r2){return(r1-r2)})

val1 = as.numeric(c(minValue(raster_2019_Q1_IDW):maxValue(raster_2019_Q1_IDW)))
val2 = as.numeric(c(minValue(raster_2020_Q1_IDW):maxValue(raster_2020_Q1_IDW)))
val3 = as.numeric(c(minValue(raster_diff_Q1_IDW):maxValue(raster_diff_Q1_IDW)))

# Setting the Color Pallete
pal1 = colorNumeric(c("yellow", "orange", "red"),val1,na.color = "transparent")
pal2 = colorNumeric(c("yellow", "orange", "red"),as.numeric(c(0:max(60))),na.color = "transparent")
pal3 = colorNumeric(c("#008000","#00FF00","red"),val3,na.color = "transparent")

# Plotting the map using Leaflet
leaflet(data = NO2) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(popup = NO2$Station.Name) %>% 
  addRasterImage(raster_2019_Q1_IDW,colors = pal1,opacity = 0.5,group = "2019") %>%
  addRasterImage(raster_2020_Q1_IDW,colors = pal2,opacity = 0.5,group = "2020")%>%
  addRasterImage(raster_diff_Q1_IDW,colors = pal3,opacity = 0.5,group = "Difference")%>%
  addLegend('bottomleft',group = "2019",pal = pal1, values = val1, title = "NO2 Concentration Quater1 2019") %>%
  addLegend('bottomleft',group = "2020",pal = pal2, values = val2, title = "NO2 Concentration_Quater 1 2020") %>%
  addLegend('bottomleft',group = "Difference",pal = pal3, values = val3, title = "NO2 Difference") %>%
  addLayersControl(
    overlayGroups = c("2019", "2020","Difference"),
    options = layersControlOptions(collapsed = FALSE)
  )


#Validation of IDW Results
IDW.out <- vector(length = length(NO2_T))
for (i in 1:length(NO2_T)) {
  IDW.out[i] <- idw(Q1_2019 ~ 1, NO2_T[-i,], NO2_T[i,], idp=2.0)$var1.pred
}

# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ NO2_T$Q1_2019, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ NO2_T$Q1_2019), col="red", lw=2,lty=2)
abline(0,1)
par(OP)
sqrt( sum((IDW.out - NO2_T$Q1_2019)^2) / length(NO2_T))
