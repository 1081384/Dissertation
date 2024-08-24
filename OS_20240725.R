rm(list=ls())


library(devtools)
library(plyr)
library(dplyr)
library(tidyverse)
library(here)
library(plotly)
library(sf)
library(ggplot2)
library(tidyr)


AccessPoint <- read_sf(here("OS_Maps/shp/TQ_AccessPoint.shp")) %>% st_transform(4326)
#Building <- read_sf(here("OS_Maps/shp/TQ_Building.shp")) %>% st_transform(4326)
#FunctionalSite <- read_sf(here("OS_Maps/shp/TQ_FunctionalSite.shp")) %>% st_transform(4326)
GreenspaceSite <- st_read(here('GreenSpacesite')) %>% st_transform(4326)
#ImportantBuilding <- read_sf(here("OS_Maps/shp/TQ_ImportantBuilding.shp")) %>% st_transform(4326)
#MotorwayJunction <- read_sf(here("OS_Maps/shp/TQ_MotorwayJunction.shp")) %>% st_transform(4326)
#NamedPlace <- read_sf(here("OS_Maps/shp/TQ_NamedPlace.shp")) %>% st_transform(4326)
RailwayStation <- read_sf(here("OS_Maps/shp/TQ_RailwayStation.shp")) %>% st_transform(4326)
RailwayTrack<- read_sf(here("OS_Maps/shp/TQ_RailwayTrack.shp")) %>% st_transform(4326)
RailwayTunnel<- read_sf(here("OS_Maps/shp/TQ_RailwayTunnel.shp")) %>% st_transform(4326)
Road<- read_sf(here("OS_Maps/shp/TQ_Road.shp")) %>% st_transform(4326)
RoadTunnel<- read_sf(here("OS_Maps/shp/TQ_RoadTunnel.shp")) %>% st_transform(4326)
SurfaceWaterArea<- read_sf(here("OS_Maps/shp/TQ_SurfaceWater_Area.shp")) %>% st_transform(4326)
Woodland<- read_sf(here("OS_Maps/shp/TQ_Woodland.shp")) %>% st_transform(4326)
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



coords <- matrix(c(-0.22, -0.11, 51.48, 51.54), 
                 byrow = TRUE, 
                 nrow = 2, 
                 ncol = 2, 
                 dimnames = list(c('x','y'),
                                 c('min','max')))



base_plot  <- ggplot() +
  geom_sf(data = Road, fill = '#B4BFB0',color = '#B4BFB0') +
  geom_sf(data = SurfaceWaterArea, fill = '#cce6ff', color = NA) + 
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE)+
  theme_minimal()

ward_final_plot<- base_plot+
  geom_sf(data = Woodland, fill = 'darkolivegreen', color = NA) + 
  #geom_sf(data = ImportantBuilding, fill = "grey", colour = "grey")+
  #geom_sf(data = Building, fill = "grey", colour = "grey")+
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




ggsave("OS_Westminster.pdf", plot = ward_final_plot, width = 18, height = 12, dpi = 600)




#######################  with LSOAs

LSOAs<- gis_boundaries %>%
  filter(LAD11NM == "Westminster")%>%
  select(geometry)


#acquire list of multipolygons of lsoas
LSOAs_string<-as.vector(LSOAs$geometry)

extract_LSOA_coordinates <- function(LSOAs_string) {
  
  # Remove "MULTIPOLYGON ((( ... )))"from the string
  LSOAs_cleaned_string <- gsub("MULTIPOLYGON \\(\\(\\((.*)\\)\\)\\)", "\\1", LSOAs_string)
  
  # Split the string into individual coordinate pairs
  LSOA_coordinates <- unlist(strsplit(LSOAs_cleaned_string, ", "))
  
  # Return the coordinates
  return(LSOA_coordinates)
}

# Apply the function to each element of the vector
LSOA_result <- lapply(LSOAs_string, extract_LSOA_coordinates)

# Flatten the resulting list to get a single vector
LSOA_coordinates_vector <- unlist(LSOA_result)
LSOA_coordinates_vector

tec_unseparated_values<- LSOA_coordinates_vector
tec_separated_values <- unlist(strsplit(LSOA_coordinates_vector, ","))


# Plot the multipolygon
plot(LSOAs_string, main = "LSOA Plot")

LSOAs_latlon <- st_transform(LSOAs_string, crs = 4326)
coordinates <- st_coordinates(LSOAs_latlon)

coordinates_df <- as.data.frame(coordinates)

crs<- st_crs(coordinates_df)

LSOA_final_plot<- base_plot+
  geom_sf(data = Woodland, fill = '#4F9638', color = NA) + 
  geom_sf(data = RailwayTrack, fill= "grey", color = "grey")+
  geom_sf(data = RailwayStation, fill = "grey40")+
  #geom_sf(data = ImportantBuilding, fill = "grey", colour = "grey")+
  #geom_sf(data = Building, fill = "bisque", colour = NA)+
  geom_sf(data = OAs, fill = "orange", 
          color = NA,
          alpha = 0.7) + 
  geom_polygon(data = coordinates_df,
               aes(x = X, y= Y, group = L3), 
               color = "black", fill = NA,
               linewidth = 1.0
  ) +
  theme_minimal() +
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE)

LSOA_final_plot




ggsave("OS_LSOA_Westminster.pdf", plot = LSOA_final_plot, width = 18, height = 12, dpi = 600)



################With TRee Canopy

brownfield_sites <- read.csv(here('brownfield-site.csv'))
local_authority<- read.csv(here('local-planning-authority.csv'))

tile_355<- st_read(here('canopy/tile_355_mercator.kml'))
tile_377<- st_read(here('canopy/tile_377_mercator.kml'))
tile_378<- st_read(here('canopy/tile_378_mercator.kml'))
tile_379<- st_read(here('canopy/tile_379_mercator.kml'))
tile_399<- st_read(here('canopy/tile_399_mercator.kml'))
tile_400<- st_read(here('canopy/tile_400_mercator.kml'))
tile_401<- st_read(here('canopy/tile_401_mercator.kml'))
tile_402<- st_read(here('canopy/tile_402_mercator.kml'))
tile_422<- st_read(here('canopy/tile_422_mercator.kml'))
tile_423<- st_read(here('canopy/tile_423_mercator.kml'))
tile_424<- st_read(here('canopy/tile_424_mercator.kml'))
tile_445<- st_read(here('canopy/tile_445_mercator.kml'))
tile_446<- st_read(here('canopy/tile_446_mercator.kml'))

######################################CLEAN ####################################
st_crs(tile_355)<- NA
st_crs(tile_377)<- NA
st_crs(tile_378)<- NA
st_crs(tile_379)<- NA
st_crs(tile_399)<- NA
st_crs(tile_400)<- NA
st_crs(tile_401)<- NA
st_crs(tile_402)<- NA
st_crs(tile_422)<- NA
st_crs(tile_423)<- NA
st_crs(tile_424)<- NA
st_crs(tile_445)<- NA
st_crs(tile_446)<- NA

tile_355 <- st_make_valid(tile_355)
tile_377 <- st_make_valid(tile_377)
tile_378 <- st_make_valid(tile_378)
tile_379 <- st_make_valid(tile_379)
tile_399 <- st_make_valid(tile_399)
tile_400 <- st_make_valid(tile_400)
tile_401 <- st_make_valid(tile_401)
tile_402 <- st_make_valid(tile_402)
tile_422 <- st_make_valid(tile_422)
tile_423 <- st_make_valid(tile_423)
tile_424 <- st_make_valid(tile_424)
tile_445 <- st_make_valid(tile_445)
tile_446 <- st_make_valid(tile_446)

CoW <- local_authority %>%
  filter(name == "Westminster LPA")
CoW<- CoW %>%
  select(. , geometry, name, point)
WestPolygon<- CoW %>%
  select(geometry)
W_multipolygon_string<-as.vector(WestPolygon$geometry)
W_multipolygon_string
larger_multipolygon_sf <- st_as_sfc(W_multipolygon_string)

############################# Brownfield MULTIPOLYGONs #########################




BF_multipolygon_string<-as.vector(brownfield_sites$geometry)

#let's convert a character into a list
Brown_sfc <- st_as_sfc(BF_multipolygon_string, crs = 4326)

extract_BF_coordinates <- function(BF_multipolygon_string) {
  
  # Remove "MULTIPOLYGON ((( ... )))" from the string
  BF_cleaned_string <- gsub("MULTIPOLYGON \\(\\(\\((.*)\\)\\)\\)",
                            "\\1",
                            BF_multipolygon_string)
  
  # Split the string into individual coordinate pairs
  BF_coordinates <- unlist(strsplit(BF_cleaned_string, ", "))
  
  # Return the coordinates
  return(BF_coordinates)
}

# Apply the function to each element of the vector
BF_result <- lapply(BF_multipolygon_string, extract_BF_coordinates)

# Flatten the resulting list to get a single vector
BF_coordinates_vector <- unlist(BF_result)


larger_multipolygon_sf <- st_as_sfc(W_multipolygon_string)
BF_smaller_multipolygons_sf <- st_as_sfc(BF_multipolygon_string)



# Convert the larger multipolygon to a data frame for ggplot2
larger_multipolygon_df <- st_as_sf(larger_multipolygon_sf)

# Check which smaller multipolygons fall within the larger multipolygon
intersection <- st_intersection(BF_smaller_multipolygons_sf, 
                                larger_multipolygon_sf)

intersection <- st_set_crs(intersection, 4326)


# Convert the intersection to a data frame for ggplot2
intersection_df <- st_as_sf(intersection)


# Check which smaller multipolygons fall within the larger multipolygon
intersection_377 <- st_intersection(tile_377, 
                                    larger_multipolygon_sf)

intersection_378 <- st_intersection(tile_378, 
                                    larger_multipolygon_sf)

intersection_379 <- st_intersection(tile_379, 
                                    larger_multipolygon_sf)

intersection_399 <- st_intersection(tile_399, 
                                    larger_multipolygon_sf)

intersection_400 <- st_intersection(tile_400, 
                                    larger_multipolygon_sf)

intersection_401 <- st_intersection(tile_401, 
                                    larger_multipolygon_sf)

intersection_402 <- st_intersection(tile_402, 
                                    larger_multipolygon_sf)

intersection_422 <- st_intersection(tile_422, 
                                    larger_multipolygon_sf)

intersection_423 <- st_intersection(tile_423, 
                                    larger_multipolygon_sf)

intersection_424 <- st_intersection(tile_424, 
                                    larger_multipolygon_sf)

intersection_445 <- st_intersection(tile_445, 
                                    larger_multipolygon_sf)

intersection_446 <- st_intersection(tile_446, 
                                    larger_multipolygon_sf)

intersection_377 <- st_set_crs(intersection_377, 4326)
intersection_378 <- st_set_crs(intersection_378, 4326)
intersection_379 <- st_set_crs(intersection_379, 4326)
intersection_399 <- st_set_crs(intersection_399, 4326)
intersection_400 <- st_set_crs(intersection_400, 4326)
intersection_401 <- st_set_crs(intersection_401, 4326)
intersection_402 <- st_set_crs(intersection_402, 4326)
intersection_422 <- st_set_crs(intersection_422, 4326)
intersection_423 <- st_set_crs(intersection_423, 4326)
intersection_424 <- st_set_crs(intersection_424, 4326)
intersection_445 <- st_set_crs(intersection_445, 4326)
intersection_446 <- st_set_crs(intersection_446, 4326)

Canopy_Plot <- base_plot+
  geom_sf(data = Woodland, fill = '#4F9638', color = NA) + 
  geom_sf(data = RailwayTrack, fill= "grey", color = "grey")+
  geom_sf(data = RailwayStation, fill = "grey40")+
  #geom_sf(data = Building, fill = "grey", colour = "grey")+
  #geom_sf(data = ImportantBuilding, fill = "grey", colour = "grey")+
  #geom_sf(data = Building, fill = "grey", colour = "grey")+
  geom_sf(data = OAs, fill = "orange", 
          color = NA,
          alpha = 0.7) + 
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE)+
  theme_minimal()


######LSOA
LSOA_Canopy_Plot<- Canopy_Plot+  
  geom_sf(data = intersection_df,
          fill = "tan4", 
          color = "black") +
  geom_sf(data = intersection_377,
          fill = "chartreuse4")+
  geom_sf(data = intersection_378,
          fill = "chartreuse4")+
  geom_sf(data = intersection_379,
          fill = "chartreuse4")+
  geom_sf(data = intersection_399,
          fill = "chartreuse4")+
  geom_sf(data = intersection_400,
          fill = "chartreuse4")+
  geom_sf(data = intersection_401,
          fill = "chartreuse4")+
  geom_sf(data = intersection_402,
          fill = "chartreuse4")+
  geom_sf(data = intersection_422,
          fill = "chartreuse4")+
  geom_sf(data = intersection_423,
          fill = "chartreuse4")+
  geom_sf(data = intersection_424,
          fill = "chartreuse4")+
  geom_sf(data = intersection_445,
          fill = "chartreuse4")+
  geom_sf(data = intersection_446,
          fill = "chartreuse4")+   
  geom_polygon(data = coordinates_df,
               aes(x = X, y= Y, group = L3), 
               color = "black", fill = NA,
               linewidth = 0.6 ) +
  theme_minimal()+
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE)

LSOA_Canopy_Plot


ggsave("OS_LSOA_Canopy_Westminster.pdf",
       plot = LSOA_Canopy_Plot, 
       width = 18, 
       height = 12, 
       dpi = 600)

######ward

Wards_Canopy_Plot<- Canopy_Plot+  
  geom_sf(data = intersection_df,
          fill = "tan4", 
          color = "black") +
  geom_sf(data = intersection_377,
          fill = "chartreuse4")+
  geom_sf(data = intersection_378,
          fill = "chartreuse4")+
  geom_sf(data = intersection_379,
          fill = "chartreuse4")+
  geom_sf(data = intersection_399,
          fill = "chartreuse4")+
  geom_sf(data = intersection_400,
          fill = "chartreuse4")+
  geom_sf(data = intersection_401,
          fill = "chartreuse4")+
  geom_sf(data = intersection_402,
          fill = "chartreuse4")+
  geom_sf(data = intersection_422,
          fill = "chartreuse4")+
  geom_sf(data = intersection_423,
          fill = "chartreuse4")+
  geom_sf(data = intersection_424,
          fill = "chartreuse4")+
  geom_sf(data = intersection_445,
          fill = "chartreuse4")+
  geom_sf(data = intersection_446,
          fill = "chartreuse4")+   
  geom_polygon(data = ward_coordinates_df,
               aes(x = X, y= Y, group = L2), 
               color = "black", fill = NA,
               linewidth = 0.6 ) +
  theme_minimal()+
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE)

Wards_Canopy_Plot


ggsave("OS_Wards_Canopy_Westminster.pdf", 
       plot = Wards_Canopy_Plot, 
       width = 18, 
       height = 12, 
       dpi = 600)




####################### paddington OA 

HydePark<- wards_2024%>%
  filter(WD24NM == "Hyde Park")%>%
  select(geometry)

Hyde_string<-as.vector(HydePark$geometry)
HP_coordinates <- st_coordinates(st_transform(Hyde_string, crs = 4326))


# Finding the limits
HP_limits <- c(xmin = min(HP_coordinates[, "X"]), 
               xmax = max(HP_coordinates[, "X"]),
               ymin = min(HP_coordinates[, "Y"]), 
               ymax = max(HP_coordinates[, "Y"]))

HP_limits


Hyde_base <- base_plot+
  geom_sf(data = Woodland, fill = '#4F9638', color = NA) + 
  geom_sf(data = RailwayTrack, fill= "grey", color = "grey")+
  geom_sf(data = RailwayStation, fill = "grey40")+
  #geom_sf(data = Building, fill = "grey", colour = "grey")+
  #geom_sf(data = ImportantBuilding, fill = "grey", colour = "grey")+
  #geom_sf(data = Building, fill = "grey", colour = "grey")+
  geom_sf(data = OAs, fill = "orange", 
          color = NA,
          alpha = 0.7) + 
  coord_cartesian(xlim = c(HP_limits["xmin"], HP_limits["xmax"]),
                  ylim =c(HP_limits["ymin"], HP_limits["ymax"]))+
  theme_minimal()



hyde_final <- Hyde_base+
  geom_sf(data = intersection_df,
          fill = "tan4", 
          color = "black") +
  geom_sf(data = intersection_377,
          fill = "chartreuse4")+
  geom_sf(data = intersection_378,
          fill = "chartreuse4")+
  geom_sf(data = intersection_379,
          fill = "chartreuse4")+
  geom_sf(data = intersection_399,
          fill = "chartreuse4")+
  geom_sf(data = intersection_400,
          fill = "chartreuse4")+
  geom_sf(data = intersection_401,
          fill = "chartreuse4")+
  geom_sf(data = intersection_402,
          fill = "chartreuse4")+
  geom_sf(data = intersection_422,
          fill = "chartreuse4")+
  geom_sf(data = intersection_423,
          fill = "chartreuse4")+
  geom_sf(data = intersection_424,
          fill = "chartreuse4")+
  geom_sf(data = intersection_445,
          fill = "chartreuse4")+
  geom_sf(data = intersection_446,
          fill = "chartreuse4")+ 
  geom_polygon(data = ward_coordinates_df,
               aes(x = X, y= Y, group = L2), 
               color = "black", fill = NA,
               linewidth = 0.8 ) +
  theme_minimal()


hyde_final <- hyde_final+
  coord_sf(xlim = c(HP_limits["xmin"], HP_limits["xmax"]),
           ylim =c(HP_limits["ymin"], HP_limits["ymax"]))+
  labs(title = "Hyde Park Ward Green Canopy")+
  theme_minimal()

hyde_final

ggsave("Hyde_Park_Ward_Canopy.pdf", 
       plot = hyde_final, 
       width = 18, 
       height = 12, 
       dpi = 600)




POA_final <- Hyde_base+
  geom_sf(data = intersection_df,
          fill = "tan4", 
          color = "black") +
  geom_sf(data = intersection_377,
          fill = "chartreuse4")+
  geom_sf(data = intersection_378,
          fill = "chartreuse4")+
  geom_sf(data = intersection_379,
          fill = "chartreuse4")+
  geom_sf(data = intersection_399,
          fill = "chartreuse4")+
  geom_sf(data = intersection_400,
          fill = "chartreuse4")+
  geom_sf(data = intersection_401,
          fill = "chartreuse4")+
  geom_sf(data = intersection_402,
          fill = "chartreuse4")+
  geom_sf(data = intersection_422,
          fill = "chartreuse4")+
  geom_sf(data = intersection_423,
          fill = "chartreuse4")+
  geom_sf(data = intersection_424,
          fill = "chartreuse4")+
  geom_sf(data = intersection_445,
          fill = "chartreuse4")+
  geom_sf(data = intersection_446,
          fill = "chartreuse4")+ 
  geom_polygon(data = coordinates_df,
               aes(x = X, y= Y, group = L3), 
               color = "black", fill = NA,
               linewidth = 0.8 ) +
  theme_minimal()


POA_final <- POA_final+
  coord_sf(xlim = c(-0.185,-0.168),
           ylim =c(51.514, 51.522))+
  labs(title = "Hyde Park Ward Green Canopy")+
  theme_minimal()
POA_final

ggsave("PaddingtonOA_LSOA_Canopy.pdf", 
       plot = POA_final, 
       width = 18, 
       height = 12, 
       dpi = 600)


POA <- Hyde_base+
  geom_sf(data = intersection_df,
          fill = "tan4", 
          color = "black") +
  geom_sf(data = intersection_377,
          fill = "chartreuse4")+
  geom_sf(data = intersection_378,
          fill = "chartreuse4")+
  geom_sf(data = intersection_379,
          fill = "chartreuse4")+
  geom_sf(data = intersection_399,
          fill = "chartreuse4")+
  geom_sf(data = intersection_400,
          fill = "chartreuse4")+
  geom_sf(data = intersection_401,
          fill = "chartreuse4")+
  geom_sf(data = intersection_402,
          fill = "chartreuse4")+
  geom_sf(data = intersection_422,
          fill = "chartreuse4")+
  geom_sf(data = intersection_423,
          fill = "chartreuse4")+
  geom_sf(data = intersection_424,
          fill = "chartreuse4")+
  geom_sf(data = intersection_445,
          fill = "chartreuse4")+
  geom_sf(data = intersection_446,
          fill = "chartreuse4")+ 
  theme_minimal()


POA<- POA+
  coord_sf(xlim = c(-0.185,-0.168),
           ylim =c(51.514, 51.522))+
  labs(title = "Paddington Opportunity Area Green Canopy")+
  theme_minimal()
POA

ggsave("PaddingtonOA_Canopy.pdf", 
       plot = POA, 
       width = 18, 
       height = 12, 
       dpi = 600)


#######################viuctoria


Vic_base <- base_plot+
  geom_sf(data = Woodland, fill = '#4F9638', color = NA) + 
  geom_sf(data = RailwayTrack, fill= "grey", color = "grey")+
  geom_sf(data = RailwayStation, fill = "grey40")+
  #geom_sf(data = Building, fill = "grey", colour = "grey")+
  #geom_sf(data = ImportantBuilding, fill = "grey", colour = "grey")+
  #geom_sf(data = Building, fill = "grey", colour = "grey")+
  geom_sf(data = OAs, fill = "orange", 
          color = NA,
          alpha = 0.7) + 
  theme_minimal()

Vic_OA <- Vic_base+
  geom_sf(data = intersection_df,
          fill = "tan4", 
          color = "black") +
  geom_sf(data = intersection_377,
          fill = "chartreuse4")+
  geom_sf(data = intersection_378,
          fill = "chartreuse4")+
  geom_sf(data = intersection_379,
          fill = "chartreuse4")+
  geom_sf(data = intersection_399,
          fill = "chartreuse4")+
  geom_sf(data = intersection_400,
          fill = "chartreuse4")+
  geom_sf(data = intersection_401,
          fill = "chartreuse4")+
  geom_sf(data = intersection_402,
          fill = "chartreuse4")+
  geom_sf(data = intersection_422,
          fill = "chartreuse4")+
  geom_sf(data = intersection_423,
          fill = "chartreuse4")+
  geom_sf(data = intersection_424,
          fill = "chartreuse4")+
  geom_sf(data = intersection_445,
          fill = "chartreuse4")+
  geom_sf(data = intersection_446,
          fill = "chartreuse4")+ 
  geom_polygon(data = coordinates_df,
               aes(x = X, y= Y, group = L3), 
               color = "black", fill = NA,
               linewidth = 0.8 ) +
  theme_minimal()


Vic_OA <- Vic_OA+
  coord_sf(xlim = c(-0.155,-0.125),
           ylim =c(51.489, 51.5))+
  labs(title = "Victoria Opportunity Area Canopy Green Canopy")+
  theme_minimal()
Vic_OA

ggsave("Vic_OA_LSOA_Canopy.pdf", 
       plot = Vic_OA, 
       width = 18, 
       height = 12, 
       dpi = 600)



Vic_OA <- Vic_base+
  geom_sf(data = intersection_df,
          fill = "tan4", 
          color = "black") +
  geom_sf(data = intersection_377,
          fill = "chartreuse4")+
  geom_sf(data = intersection_378,
          fill = "chartreuse4")+
  geom_sf(data = intersection_379,
          fill = "chartreuse4")+
  geom_sf(data = intersection_399,
          fill = "chartreuse4")+
  geom_sf(data = intersection_400,
          fill = "chartreuse4")+
  geom_sf(data = intersection_401,
          fill = "chartreuse4")+
  geom_sf(data = intersection_402,
          fill = "chartreuse4")+
  geom_sf(data = intersection_422,
          fill = "chartreuse4")+
  geom_sf(data = intersection_423,
          fill = "chartreuse4")+
  geom_sf(data = intersection_424,
          fill = "chartreuse4")+
  geom_sf(data = intersection_445,
          fill = "chartreuse4")+
  geom_sf(data = intersection_446,
          fill = "chartreuse4")+ 
  theme_minimal() 


Vic_OA <- Vic_OA+
  coord_sf(xlim = c(-0.155,-0.125),
           ylim =c(51.489, 51.5))+
  labs(title = "Victoria Opportunity Area Canopy Green Canopy")+
  theme_minimal()
Vic_OA

ggsave("Vic_OA_Canopy.pdf", 
       plot = Vic_OA, 
       width = 18, 
       height = 12, 
       dpi = 600)

