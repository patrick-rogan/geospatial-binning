library(maptools)
library(plyr)
library(tigris)
library(viridis)
library(ggplot2)
library(leaflet)

# Get census tracts and remove tracts not relevant to the analysis
# map of tracts: http://maps.nyc.gov/census/
nyc <- tracts(state = 'NY', county = c('New York'))
nyc <- nyc[which(nyc@data$NAMELSAD != "Census Tract 5"),]
nyc <- nyc[which(nyc@data$NAMELSAD != "Census Tract 1"),]
nyc <- nyc[which(nyc@data$NAMELSAD != "Census Tract 143"),]

queens <- tracts(state = 'NY', county = c('Queens'))
queens <- queens[which(queens@data$NAMELSAD != "Census Tract 331"),]
queens <- queens[which(queens@data$NAMELSAD != "Census Tract 1072.02"),]
queens <- queens[which(queens@data$NAMELSAD != "Census Tract 716"),]
queens <- queens[which(queens@data$INTPTLAT != min(queens@data$INTPTLAT)),]

bro <- tracts(state = 'NY', county = c('Kings'))
bro <- bro[which(bro@data$NAMELSAD != "Census Tract 702.03"),]
bro <- bro[which(bro@data$NAMELSAD != "Census Tract 702.02"),]
bro <- bro[which(bro@data$INTPTLAT != min(bro@data$INTPTLAT)),]

bronx <- tracts(state = 'NY', county = c('Bronx'))
bronx <- bronx[which(bronx@data$NAMELSAD != "Census Tract 1"),]

si <- tracts(state = 'NY', county = c('Richmond'))
si <- si[which(si@data$NAMELSAD != "Census Tract 9901"),]

nyc <- spRbind(nyc,queens)
nyc <- spRbind(nyc,bro)
nyc <- spRbind(nyc,bronx)
nyc <- spRbind(nyc,si)

nyc@data$NAME <- as.numeric(as.character(nyc@data$NAME ))
nyc@data <- nyc@data[which(nyc@data$AWATER==0),]

# Read in CSV file of complaints and save to R Data File. The resulting file size 
# is roughly a magnitude of 10 smaller. 
#df = read.csv("311_Service_Requests_from_2010_to_Present.csv")
#save(df,file="311_Service_Requests_from_2010_to_Present.Rda")
load('311_Service_Requests_from_2010_to_Present.Rda')

heat_hw = df[,c("Latitude","Longitude","Created.Date")]
  
# Create an extremely simple dataframe whose indices correspond to the unique polygon ids (pid) and the one 
# variable (ID) to our own set of indices from 1 to the number of polygons.
pid <- sapply(slot(nyc, "polygons"), function(x) slot(x, "ID")) 

nyc2.df <- data.frame(ID=1:length(nyc), row.names = pid) 

# Create a new SpatialPolygonsDataFrame, nyc2 from the list of polygons provided
# by nyc2.df and polygon objects from nyc. 
nyc2 <- SpatialPolygonsDataFrame(nyc,nyc2.df)

# Populate this with the data portion of nyc.
nyc2@data = nyc@data 

# Drop complaints that do not have lat/long values then strip out all other information
# in the dataframe with the exception of lat/long pairs.
complaintsLatLon = na.omit(heat_hw[which(grepl(".+",heat_hw$Created.Date)),])
complaintsLatLon = complaintsLatLon[,c("Longitude","Latitude")]

# Convert the dataframe into spatial points, then convert to the same projection as nyc.
coordinates(complaintsLatLon) <- ~ Longitude + Latitude
proj4string(complaintsLatLon) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 
 
# We're finally ready to bin the complaints into polygons. We use the over function which, among other
# things, assigns a census tract to each data point. The function table is then used to sum the number
# of points in each tract. We then convert this table to a dataframe of census tracts and complaint counts.
overVals <- over(complaintsLatLon,nyc2)
binnedValues <- table(overVals$NAMELSAD) 
binnedValues <- as.data.frame(binnedValues)

# Rename the binnedValues dataframe so that it shares the variable NAMELSAD with the nyc 
# Large SpatialPolygonsDataFrame. Then use join to include this data in nyc. If any values
# have not been assigned (empty bins) assign a value of 0.
colnames(binnedValues) = c("NAMELSAD",'HEAT_HW')
nyc@data = join(binnedValues, nyc@data, by = "NAMELSAD",type = "right")
nyc@data[is.na(nyc@data)] = 0

save(nyc,file="nyc.Rda")

# Create color palette using the viridis package
pal <- function(x) {colorNumeric(
  palette = rev(viridis(max(x),option="magma")),
  domain = x
  )
}

# Plot data
leaflet(nyc) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(lat = 40.7, lng = -74.0, zoom = 11) %>%
  addPolygons(data = nyc, color = pal(nyc@data$HEAT_HW)(nyc@data$HEAT_HW),stroke = TRUE, weight = 1,
              popup = paste("Heat/Hot Water",nyc@data$HEAT_HW, sep=": "), fillOpacity = 0.7) %>%
  addLegend(pal = pal(nyc@data$HEAT_HW),
            values = nyc@data$HEAT_HW,
            position = "bottomright",
            title = "Heat/Hot Water")