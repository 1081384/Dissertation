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


Paddington_Habitats <- read.csv(here('GiGL_Habitats_PaddingtonOA.csv'))
Paddington_sf<- st_read(here('PaddingtonOA'))
crs<- st_crs(Paddington_sf)
crs

Paddington_Habitats<- Paddington_sf%>%
  select(geometry, Habitat, Mgmt1, shape_area, shape_leng)

Paddington_BNG<-Paddington_Habitats%>%
  as.data.frame() %>%
  select(Habitat, Mgmt1, shape_area, shape_leng)

summarized_Paddington_BNG<- Paddington_BNG %>%
  group_by(Habitat) %>%
  summarise(total_area = sum(shape_area))

summarized_Paddington_mgmt<- Paddington_BNG %>%
  group_by(Mgmt1) %>%
  summarise(total_area = sum(shape_area))

Paddington_canal <- Paddington_BNG%>%
  group_by(Habitat)%>%
  filter(Habitat == "AS0 : Standing open water and canals")%>%
  summarise(total_length = sum(shape_leng))

Paddington_Garden<- Paddington_sf%>%
  group_by(Habitat)%>%
  filter(Habitat == "UR0 : Built-up areas and gardens")%>%
  filter(Mgmt1 == "UA32 : Gardens")%>%
  summarise(total_area = sum(shape_area))

Paddington_unknown<- Paddington_sf%>%
  group_by(Habitat)%>%
  filter(Habitat == "UH0 : Unidentified habitat")


  
            

write.csv(summarized_Paddington_BNG, 
          here("Paddington_OA_BNG.csv"), 
          row.names = TRUE)

unique(Paddington_sf$P2DetQual)


P <- ggplot(Paddington_sf) +
  geom_sf(aes(fill = Habitat)) +
  scale_fill_manual(values = c("WB3 : Broadleaved woodland"   = "forestgreen", 
                               "GI0 : Improved grassland" = "khaki",                          
                               "UR0 : Built-up areas and gardens" = "darkgrey",  
                               "OV0 : Unknown terrestrial vegetation" = "gold1",
                               "LF271 : Transport corridor without associated verges" = "grey28",
                               "LF272 : Transport corridor associated verges only" = "lightgreen",   
                               "AS0 : Standing open water and canals" = "royalblue",
                               "UH1 : Untranslatable habitat (data deficient)" = "tomato",      
                               "UH0 : Unidentified habitat" = "sienna",                          
                               "UH3 : Untranslatable habitat (multi-habitat source)"="goldenrod" )) +
  theme_minimal() +
  labs(title = "Paddington Opportunity Area by Habitat Type",
       fill = "Habitat Type")

P 

ggsave("Paddington_OA_habitat.pdf", 
       plot = P, 
       width = 18, 
       height = 12, 
       dpi = 600)




Pm <- ggplot(Paddington_sf) +
  geom_sf(aes(fill = Mgmt1))+
  scale_fill_manual(values = c("GL1 : Amenity grassland"   = "lightseagreen", 
                               "LT3 : Rail-side" = "thistle",                          
                               "LT4 : Road-side" = "chartreuse4",  
                               "UA0 : Undetermined built up areas" = "darkgrey",
                               "UA32 : Gardens" = "green3",
                               "UL1 : Railway" = "chocolate4",   
                               "UL2 : Roadway" = "grey30",
                               "UL3 : Path and trackway" = "khaki",      
                               "WM0 : Undetermined woodland management" = "darkseagreen" ),
                    na.value = "honeydew2") +
  labs(title = "Paddington Opportunity Area by Management Type",
       fill = "Management Type")+
  theme_minimal()

Pm

ggsave("Paddington_OA_mgmt.pdf", 
       plot = Pm, 
       width = 18, 
       height = 12, 
       dpi = 600)

############### Victoria OA   ###############################################


Victoria_Habitats <- read.csv(here('GiGL_Habitats_VictoriaOA.csv'))
Victoria_sf<- st_read(here('VictoriaOA'))


Victoria_Habitats<- Victoria_sf%>%
  select(geometry, Habitat, Mgmt1, shape_area)

Victoria_BNG<-Victoria_Habitats%>%
  as.data.frame() %>%
  select(Habitat, Mgmt1, shape_area)


summarized_Victoria_BNG<- Victoria_BNG %>%
  group_by(Habitat) %>%
  summarise(total_area = sum(shape_area))

summarized_Victoria_mgmt<- Victoria_BNG %>%
  group_by(Mgmt1) %>%
  summarise(total_area = sum(shape_area))

write.csv(summarized_Victoria_BNG, 
          here("Victoria_OA_BNG.csv"), 
          row.names = TRUE)

unique(Victoria_sf$Habitat)

V <- ggplot(Victoria_sf) +
  geom_sf(aes(fill = Habitat)) +
  scale_fill_manual(values = c("WB3 : Broadleaved woodland"   = "forestgreen", 
                               "GI0 : Improved grassland" = "khaki",                          
                               "UR0 : Built-up areas and gardens" = "darkgrey",  
                               "OV0 : Unknown terrestrial vegetation" = "gold",
                               "LF271 : Transport corridor without associated verges" = "grey28",
                               "LF272 : Transport corridor associated verges only" = "lightgreen",   
                               "UH0 : Unidentified habitat" = "sienna",
                               "WB2 : Scrub woodland" = "darkolivegreen1")) +                   
  theme_minimal() +
  labs(title = "Victoria Opportunity Area by Habitat Type",
       fill = "Habitat Type")
V

ggsave("Victoria_OA_Habitat.pdf", 
       plot = V, 
       width = 18, 
       height = 12, 
       dpi = 600)

Victoria_Garden<- Victoria_sf%>%
  group_by(Habitat)%>%
  filter(Habitat == "UR0 : Built-up areas and gardens")%>%
  filter(Mgmt1 == "UA32 : Gardens")%>%
  summarise(total_area = sum(shape_area))


Vm <- ggplot(Victoria_sf) +
  geom_sf(aes(fill = Mgmt1))+
  scale_fill_manual(values = c("GL1 : Amenity grassland"   = "lightseagreen", 
                               "GM0 : Undetermined grassland etc. management" = "lightgreen",
                               "LT3 : Rail-side" = "thistle",                          
                               "LT4 : Road-side" = "chartreuse4",  
                               "UA0 : Undetermined built up areas" = "darkgrey",
                               "UA32 : Gardens" = "green3",
                               "UL1 : Railway" = "chocolate4",   
                               "UL2 : Roadway" = "grey30",
                               "UL3 : Path and trackway" = "khaki",      
                               "WM0 : Undetermined woodland management" = "darkseagreen" ),
                    na.value = "honeydew2") +
  labs(title = "Paddington Opportunity Area by Management Type",
       fill = "Management Type")+

  theme_minimal()

Vm

ggsave("Victoria_OA_Mgmt.pdf", 
       plot = Vm, 
       width = 18, 
       height = 12, 
       dpi = 600)
