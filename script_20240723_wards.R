rm(list=ls())

################################################################################
############################## LOAD PACKAGES ###################################
################################################################################

#install.packages("plyr")
#install.packages("dplyr")
# install.packages("reshape2")
#install.packages("plotly")
# install.packages("hrbrthemes")
# install.packages("directlabels")
#install.packages("here")
# install.packages("PNWColors")
# install.packages("wesanderson")
#install.packages("devtools")
#install.packages("rlang")
#install.packages("vegan")
#install.packages("sf")
#install.packages("igraph", type = "binary")
#install.packages("purrr")
#install.packages("patchwork")



library(devtools)
#library(PNWColors)
#library(ggConvexHull)
library(plyr)
library(dplyr)
#library(reshape2)
library(tidyverse)
library(vegan)
library(here)
library(plotly)
library(sf)
library(igraph)
library(ggplot2)
library(tidyr)
library(purrr)
library(patchwork)
library(stringr)

################################# LOAD DATA ####################################

brownfield_land <- read.csv(here('brownfield-land.csv'))
brownfield_sites <- read.csv(here('brownfield-site.csv'))
vizdata<- read.csv(here('VizData.csv'))
local_authority<- read.csv(here('local-planning-authority.csv'))
street_trees<- read.csv(here('london_street_trees_gla_20180214.csv'))
green_roofs<- st_read(here('Green Roofs'))
#biodiv_hotspots<- st_read(here('Biodiv Hotspots'))

gis_boundaries<- st_read(here('GIS Boundaries'))

OAs_2021<- st_read(here('OAs_Dec_2021'))
OA_to_LSOA <- read.csv(here('PCD_to_OA_to_LSOA.csv'))
OA_to_ward <- read.csv(here('OA_to_ward.csv'))
canopy_2024 <- read.csv(here('2024_canopy.csv'))
ward_to_westminster <- read.csv(here('Ward_to_Westminster.csv'))
CoW_stats <- read.csv(here('CoW Stats.csv'))

names(CoW_stats)[names(CoW_stats) == "Ward.Name"] <- "WD24NM"
CoW_stats$Canopy.as.percentage <- as.numeric(sub("%", "", 
                                                 CoW_stats$Canopy.as.percentage))
names(CoW_stats)[names(CoW_stats) == "Canopy.as.percentage"] <- "percentage"


street_trees<- street_trees %>%
  filter( borough == "Westminster") %>%
  select(species_name, longitude, latitude)%>%
  filter(!(is.na(species_name))) %>%
  filter(!(is.na(longitude))) %>%
  filter(!(is.na(latitude)))%>%
  mutate(longitude = as.numeric(longitude)) %>%
  mutate(latitude = as.numeric(latitude))




street_trees$mergedcolumn<- paste(street_trees$longitude, street_trees$latitude)
street_trees_coord <- (street_trees$mergedcolumn)

    
#clean up the list of oas to just remove westminster ones

OAs_2021 <- OAs_2021 %>%
  filter(str_detect(LSOA21NM, "Westminster"))


#rename to match OAs list and merge to the LSOA label is now a column in the OA data
names(OA_to_LSOA)[names(OA_to_LSOA) == "oa21cd"] <- "OA21CD"


OAs_with_LSOAs <- merge(OAs_2021, OA_to_LSOA, by = "OA21CD", all.x = TRUE)

#clean up the converison list from oa to lsoa to just include westminster

OA_to_LSOA <- OA_to_LSOA %>%
  filter(str_detect(ladnm, "Westminster"))%>%
  select(OA21CD, lsoa21cd, ladcd)%>%
  distinct()

West_OA<- as.data.frame(OA_to_LSOA$OA21CD)
names(West_OA)[names(West_OA) == "OA_to_LSOA$OA21CD"] <- "OA21CD"

Westminster_ward_OA <-  OA_to_ward %>%
  semi_join(West_OA, by = "OA21CD")%>%
  select(OA21CD:WD23NM)

names(Westminster_ward_OA)[names(Westminster_ward_OA) == "WD23NM"] <- "ward_name"

ward_to_westminster <- ward_to_westminster %>%
  filter(str_detect(LAD24NM, "Westminster"))%>%
  select(WD24CD, WD24NM, PCON24CD, PCON24NM, LAD24CD)

names(ward_to_westminster)[names(ward_to_westminster) == "WD24CD"] <- "ward_code"
names(ward_to_westminster)[names(ward_to_westminster) == "PCON24NM"] <- "ward_name"

westminster_canopy_2024 <- canopy_2024 %>%
  semi_join(ward_to_westminster, by = "ward_code")
westminster_canopy_2024 <- westminster_canopy_2024 %>%
  select(ward:percentage)
names(westminster_canopy_2024)[names(westminster_canopy_2024) == "ward"] <- "ward_name"

Ward_data<- Westminster_ward_OA %>% 
  join(westminster_canopy_2024, by = "ward_name")

Ward_data$percentage <- as.numeric(sub("%", "", Ward_data$percentage))

############################# WESTMINSTER MULTIPOLYGON #########################



CoW <- local_authority %>%
  filter(name == "Westminster LPA")
CoW<- CoW %>%
  select(. , geometry, name, point)
WestPolygon<- CoW %>%
  select(geometry)
W_multipolygon_string<-as.vector(WestPolygon$geometry)
W_multipolygon_string




extract_W_coordinates <- function(W_multipolygon_string) {
  
  # Remove "MULTIPOLYGON ((( ... )))"from the string
  W_cleaned_string <- gsub("MULTIPOLYGON \\(\\(\\((.*)\\)\\)\\)", 
                           "\\1",
                           W_multipolygon_string)
  
  # Split the string into individual coordinate pairs
  W_coordinates <- unlist(strsplit(W_cleaned_string, ", "))
  
  # Return the coordinates
  return(W_coordinates)
}

# Apply the function to each element of the vector
result <- lapply(W_multipolygon_string, extract_W_coordinates)

# Flatten the resulting list to get a single vector
W_coordinates_vector <- unlist(result)

# Print the resulting vector
print(W_coordinates_vector)

unseparated_values<- W_coordinates_vector
separated_values <- unlist(strsplit(W_coordinates_vector, ","))

separated_values
unseparated_values


# Convert the multipolygon string to a Simple Features (sf) object
multipolygon_sf <- st_as_sfc(W_multipolygon_string)


############################# Brownfield MULTIPOLYGONs #########################


AccessPoint <- read_sf(here("OS_Maps/shp/TQ_AccessPoint.shp")) %>% st_transform(4326)
Building <- read_sf(here("OS_Maps/shp/TQ_Building.shp")) %>% st_transform(4326)
RailwayTrack<- read_sf(here("OS_Maps/shp/TQ_RailwayTrack.shp")) %>% st_transform(4326)
Road<- read_sf(here("OS_Maps/shp/TQ_Road.shp")) %>% st_transform(4326)
SurfaceWaterArea<- read_sf(here("OS_Maps/shp/TQ_SurfaceWater_Area.shp")) %>% st_transform(4326)
wards_2024<- st_read(here('Ward Boundaries 2024'))
gis_boundaries<- st_read(here('GIS Boundaries'))
OAs<- read.csv(here('Paddington_Opportunity_Area.csv'))

################## create PAddington OA polygon

names(OAs)[names(OAs) == "WKT"] <- "geometry"


#acquire list of multipolygons of lsoas
OA_string<-as.vector(OAs$geometry)
OAs<- st_as_sfc(OA_string)
OAs <- st_set_crs(OAs, 4326)


################## create an outline of the wards

wards_2024 <- wards_2024 %>%
  st_transform(4326)

Wards<- wards_2024 %>%
  select(geometry)


#acquire list of multipolygons of lsoas
Wards_string<-as.vector(Wards$geometry)

extract_Wards_coordinates <- function(Wards_string) {
  
  # Remove "MULTIPOLYGON ((( ... )))"from the string
  Wards_cleaned_string <- gsub("MULTIPOLYGON \\(\\(\\((.*)\\)\\)\\)", "\\1", Wards_string)
  
  # Split the string into individual coordinate pairs
  Wards_coordinates <- unlist(strsplit(Wards_cleaned_string, ", "))
  
  # Return the coordinates
  return(Wards_coordinates)
}

# Apply the function to each element of the vector
Wards_result <- lapply(Wards_string, extract_Wards_coordinates)

# Flatten the resulting list to get a single vector
Wards_coordinates_vector <- unlist(Wards_result)
Wards_coordinates_vector

wards_unseparated_values<- Wards_coordinates_vector
wards_separated_values <- unlist(strsplit(Wards_coordinates_vector, ","))


# Plot the multipolygon
plot(Wards_string, main = "Wards Plot")

Wards_latlon <- st_transform(Wards_string, crs = 4326)
ward_coordinates <- st_coordinates(Wards_latlon)

ward_coordinates_df <- as.data.frame(ward_coordinates)

Westminster<- ward_coordinates_df %>% 
  join(CoW_stats, by = "L2")

coords <- matrix(c(-0.22, -0.11, 51.48, 51.54), 
                 byrow = TRUE, 
                 nrow = 2, 
                 ncol = 2, 
                 dimnames = list(c('x','y'),
                                 c('min','max')))

g1<- ggplot()+
  geom_sf(data = Road, fill = '#B4BFB0',color = '#B4BFB0') +
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE)+
  theme_minimal()
g1

g2<-g1+
  geom_polygon(data = Westminster,
              aes(x = X, y= Y, group = L2, 
              fill = percentage,
              alpha = 0.8)) +
  scale_fill_gradient(low = "tan", high = "chartreuse4") +
  geom_polygon(data = Westminster,
             aes(x = X, y= Y, group = L2), 
               color = "black", fill = NA,
               linewidth = 1.0)+
  theme_minimal() +
  labs(fill = "Percentage") +
  theme(legend.position = "bottom")+
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE)+
  theme_minimal()
g2 

base_plot  <- ggplot() +
  geom_sf(data = Road, fill = '#B4BFB0',color = '#B4BFB0') +
  geom_sf(data = SurfaceWaterArea, fill = '#cce6ff', color = NA) + 
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE)+
  theme_minimal()

ward_final_plot<- base_plot+
  geom_sf(data = RailwayTrack, fill = "grey", colour = "grey")+
  geom_sf(data = OAs, fill = "orange", 
          color = NA,
          alpha = 0.7) + 
  geom_polygon(data = ward_coordinates_df,
               aes(x = X, y= Y, group = L2), 
               color = "black", fill = NA,
               linewidth = 1.0
  ) +
  theme_minimal() +
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE)

ward_final_plot
ggplotly(ward_final_plot)

################################ OAs ##########################################
westminster<- gis_boundaries %>%
  filter( LAD11NM == "Westminster") 


