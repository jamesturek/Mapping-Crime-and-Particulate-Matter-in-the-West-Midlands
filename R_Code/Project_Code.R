#install/load required packages/libraries

install.packages("dplyr")
install.packages("sf")
install.packages("terra")
install.packages("tmap")
install.packages("ggplot2")
install.packages("viridis")
install.packages("RColorBrewer")
install.packages("R.utils")
install.packages("exactextractr")

#Question 1

#Import the West Midlands and England shapefile and set the CRS to the British National Grid (27700).
#I will use the British National Grid or, EPSG:27700, as my CRS as both shapefiles pertain to places within the United Kingdom. It makes most sense
#to me to use this CRS as to not potentially geographically skew any data points.

library(sf)
path_to_data <- "C:/Users/james/OneDrive/Desktop/GY476_Data/GY476_2025_GIS_Files_20250930/Formative"

West_Midlands <- read_sf(paste0(path_to_data, "/Other_Data/west-midlands.gpkg")) %>%
  st_transform(st_crs(27700))

England_shp <- read_sf("C:/Users/james/OneDrive/Desktop/GY476_Data/GY476_2025_GIS_Files_20250930/Formative/England/england_lsoa_2021_bgc.shp") %>%
  st_transform(st_crs(27700))


st_crs(West_Midlands) == st_crs(England_shp)

#Question 2 (Figure 1)

#Intersect the two shapefiles 

West_Midlands_Intersection <- st_intersection(England_shp, West_Midlands)

library(ggplot2)
ggplot() +
  geom_sf(data = West_Midlands_Intersection, fill = "lightgrey", color = "white") +
  geom_sf(data = West_Midlands_Intersection, fill = "red", alpha = 0.5) +
  theme_minimal() +
  labs(title = "The West Midlands")


# Import the West Midlands crime data and load/install any geopackages that you need

Crimes <- read.csv("C:/Users/james/OneDrive/Desktop/GY476_Data/GY476_2025_GIS_Files_20250930/Formative/2023-07-west-midlands-street.csv")


library(tmap)

#Filter the data for the crime types

criminal_damage <- Crimes[Crimes$Crime.type == "Criminal damage and arson",]
theft_person <- Crimes[Crimes$Crime.type == "Theft from the person",]

#Convert data to sf objects and remove any rows with missing lat/long data

criminal_damage_clean <- criminal_damage[!is.na(criminal_damage$Longitude) & 
                                           !is.na(criminal_damage$Latitude), ]
theft_person_clean <- theft_person[!is.na(theft_person$Longitude) & 
                                     !is.na(theft_person$Latitude), ]

#Create sf objects but with BNG (27700)

criminal_damage_sf <- st_as_sf(criminal_damage_clean, 
                               coords = c("Longitude", "Latitude"),
                               crs = 4326) %>%
  st_transform(27700)

theft_person_sf <- st_as_sf(theft_person_clean,
                            coords = c("Longitude", "Latitude"),
                            crs = 4326) %>%
  st_transform(27700)

#Use tmap function to create two maps; one showing 'Criminal damage and arson' and another showing 'Theft from the person'

Criminal_Damage_And_Arson <- tm_shape(West_Midlands_Intersection) +
  tm_borders() +
  tm_shape(criminal_damage_sf) +
  tm_dots(col = "red", size = 0.1, alpha = 0.6) +
  tm_layout(title = "Criminal Damage and Arson")

Theft_From_Person <- tm_shape(West_Midlands_Intersection) +
  tm_borders() +
  tm_shape(theft_person_sf) +
  tm_dots(fill = "blue", size = 0.1, fill_alpha = 0.6) +
  tm_title("Theft from the Person")

tmap_arrange(Criminal_Damage_And_Arson, Theft_From_Person)

#Analysis
#Looking at the maps, one can immediately see that Criminal Damage and Arson are are far more prevalent 
#than Theft From Person. Criminal Damage and Arson seem to be a problem in almost every part of the boundary.
#Theft from Person, however, seems to be more or less located within the center of the region. This could
#be due to a dense city in that area meaning more people to steal from. 

#Question 3 (Figure 2)

#Load geopackages
library(tmap)
library(dplyr)
library(sf)

#Count number of crimes by each LSOA

crime_counts <- Crimes %>%
  group_by(LSOA.code) %>%  
  summarise(crime_count = n())

#Import population data

LSOA.population <- read.csv("C:/Users/james/OneDrive/Desktop/GY476_Data/GY476_2025_GIS_Files_20250930/Formative/Other_Data/TS001_usual_residents_LSOA2021.csv")

#Execute join by combining popuation data with crime data

crime_pop <- crime_counts %>%
  left_join(LSOA.population, by = c("LSOA.code" = "mnemonic"))

#Calculate the crime rate per 10,000 people

crime_pop <- crime_pop %>%
  mutate(crime_rate = (crime_count * 10000) / total)

#Execute another join, this time combining crime_pop data with the LSOA shapefile

lsoa_crime <- England_shp %>%
  left_join(crime_pop, by = c("lsoa21cd" = "LSOA.code"))

#Plot the choropleth map

lsoa_crime_filtered <- lsoa_crime %>%
  filter(!is.na(crime_rate))

tm_shape(lsoa_crime_filtered) +
  tm_fill("crime_rate",
          palette = "YlOrRd",
          title = "Crime Rate\nper 10,000",
          style = "jenks",
          n = 5) +
  tm_borders(col = "grey", lwd = 0.5) +
  tm_layout(main.title = "Crime Rate per 10,000 People by LSOA",
            main.title.size = 1.2,
            legend.inside = TRUE,
            legend.inside.position = "right") +
  tm_compass(position = c("left", "top")) +
  tm_scale_bar(position = c("left", "bottom"))

#Judging from this choropleth map, there seems to be a spatial trend within the inner regions of the 
#West Midlands where we can see darker colors, thus depicting a higher rate of crime. You also notice
#a large swath of land towards the east with a very low crime rate. This could suggest that the area is
#dominated by agricultural pastures or crop fields. My choice in selecting the yellow/orange/and red 
#color scale is meant to show the high contrast between places with higher crime rates. Particularly,
# the more-or-les geographical center of the West Midlands is clearly shown by the deep red color as having
#the highest rates of crime in the region.

#Question 4 (Figure 3)

#Load the necessary libraries 

library(terra)
library(tmap)
library(sf)

#Import Raster data

pm25_raster <- rast("C:/Users/james/OneDrive/Desktop/GY476_Data/GY476_2025_GIS_Files_20250930/Formative/Other_Data/PM2_5_July2023.tif")
plot(pm25_raster)

#Import the West Midlands boundary shapefile in order to prepare it for processing with the PM25 Raster

west_midlands_boundary <- st_read("C:/Users/james/OneDrive/Desktop/GY476_Data/GY476_2025_GIS_Files_20250930/Formative/Other_Data/west-midlands.gpkg")

#Clip and mask the PM25 Raster to the boundary of the West Midlands

pm25_wm <- crop(pm25_raster, west_midlands_boundary)
pm25_wm <- mask(pm25_wm, west_midlands_boundary)

#Plot the continuous PM25 data 

map1 <- tm_shape(pm25_wm) +
  tm_raster(palette = "YlOrRd",
            title = "PM2.5 (µg/m³)",
            style = "cont") +
  tm_shape(west_midlands_boundary) +
  tm_borders(col = "black", lwd = 1) +
  tm_layout(main.title = "PM2.5 Concentration in the West Midlands - July 2023",
            main.title.size = 1,
            legend.outside = TRUE)
print(map1)


#Reclassify te previous map to show areas with PM2.5 concentrations above and below 6 µg/m³

rcl_matrix <- matrix(c(0, 6, 1,      # Below 6 = 1
                       6, Inf, 2),    # Above 6 = 2
                     ncol = 3, byrow = TRUE)

pm25_classified <- classify(pm25_wm, rcl_matrix)

#Now plot the reclassified map and print

map2 <- tm_shape(pm25_classified) +
  tm_raster(palette = c("lightblue", "red"),
            labels = c("Below 6 µg/m³", "Above 6 µg/m³"),
            title = "PM2.5 Classification",
            style = "cat") +
  tm_shape(west_midlands_boundary) +
  tm_borders(col = "black", lwd = 1) +
  tm_layout(main.title = "PM2.5: Above/Below 6 µg/m³ Threshold",
            main.title.size = 1,
            legend.outside = TRUE)
print(map2)

#Use tmap function to plot them side by side

tmap_arrange(map1, map2, ncol = 2)

#By looking at both the crime maps and the PM2.5 concentration maps, one can see that there does
#seem to be some correlation between high crime happens and where there is a PM2.5 concentration above the 
#6 µg/m³ threshold. The high PM2.5 concentrations looks to be happening mostly around the more densely 
#populated areas. Regarding visualization choices, I chose the yellow, orange, and red color palette
#to drive home the point that PM2.5 concentrations are high where the cities exist. For the threshold map,
#I wanted to use two distinct colors but also ones that pop out where a viewer can clearly see the discrepancy
#between areas above or below 6 µg/m³.


#' # Appendix: R Code
#' 
#+ ref.label=knitr::all_labels(), echo=TRUE, eval=FALSE
