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

LSOA_Info <-read.csv(here("LSOA_info.csv"))


LSOA_Info <- data.frame(LSOA_Info, stringsAsFactors = FALSE)
cols_to_convert <- names(LSOA_Info)[3:ncol(LSOA_Info)]
LSOA_Info[cols_to_convert] <- lapply(LSOA_Info[cols_to_convert], 
                                     function(x) as.numeric(as.character(x)))


LSOA_Info <- LSOA_Info %>%
  mutate(
    Urban.Area_sqm = Urban.Area * 1000000,  # Convert sqkm to sqm
    canopy_per_capita = canopy.cover * Urban.Area_sqm / Population
  )

############################## canopy cover vs temp diff



canopy_temp <- LSOA_Info%>%
  ggplot(aes(x = canopy.cover,
             y = temp_diff,
             colour = tes,
             size = 3))+
  geom_quantile(method = "rq",formula = y ~ x,
                quantiles = c(0.25, 0.5, 0.75),
                colour = "lightblue", 
                size = 1)+
  geom_point()+
  scale_color_gradient(low = "orange", high = "darkgreen", limits = c(58, 100)) +
  geom_hline(yintercept = 0, colour = "black", size = 1)+
  geom_vline(xintercept = 0, colour = "black", size = 1)+
  scale_x_continuous(breaks = seq(0, 40, by = 2)) +  # Add more x values
  scale_y_continuous(breaks = seq(-3, 4, by = 0.5)) +  # Add more y values
  guides(color = guide_colorbar(title = "Tree Equity Score"), size = "none") + 
  theme()+
  labs(x = "Canopy Cover (%)",
       y = "Temperature Difference",
       title = "Canopy Cover versus Temperature Difference")

canopy_temp
ggplotly(canopy_temp)

############################## canopy/capita vs temp diff logscale
LSOA_Info_filtered <- LSOA_Info %>%
  filter(canopy_per_capita > 0) 

x_values <- 10^seq(0, 5, length.out = 100)


blank_plot <- ggplot() +
  geom_blank(aes(x = x_values, y = NULL)) +  # Blank geom to set up x-axis
  scale_x_log10(
    breaks = c(1, 10, 100, 1000, 10000, 100000),  # Specify breaks at powers of 10
    labels = c("1", "10", "100", "1000", "10000", "100000")  # Labels for each break
  ) +
  theme_minimal() +
  labs(
    x = "Canopy Cover/Capita (Log Scale)",  # X axis label
    y = "Temperature Difference",  # Y axis label
    title = "Blank Plot with Logarithmic X-axis"  # Plot title
  )
blank_plot

overlay_plot <- blank_plot +
  geom_point(data = LSOA_Info_filtered, aes(x = canopy_per_capita, y = temp_diff, colour = tes),
             size = 3) +  # Example geom: points
  scale_color_gradient(low = "orange", high = "darkgreen", limits = c(58, 100)) +  # Color scale
  guides(color = guide_colorbar(title = "Tree Equity Score"), size = "none") +  # Legend
  geom_hline(yintercept = 0, colour = "black", size = 1) +  # Horizontal line at y = 0
  theme_bw() +  # Theme
  labs(
    x = "Canopy Cover/Capita (Log Scale)",  # X axis label
    y = "Temperature Difference",  # Y axis label
    title = "Canopy Cover/Capita (Log Scale) versus Temperature Difference"  # Plot title
  )
print(overlay_plot)
######################################canopy/capita vs temp diff

LSOA_Info <- LSOA_Info %>%
  mutate(
    Canopy_area = Urban.Area_sqm * (canopy.cover/100))



canopycap_plot <- LSOA_Info%>%
  ggplot((aes(x = Canopy_area,
              y = temp_diff,
              colour = tes,
              size = 2)))+
  geom_smooth(method = 'lm', se = FALSE,
              size = 2,
              colour = "lightblue")+
  geom_point()+
  scale_color_gradient(low = "orange", 
                       high = "darkgreen", 
                       limits = c(58, 100)) +  # Color scale
  guides(color = guide_colorbar(title = "Tree Equity Score"), size = "none") +  # Legend
  geom_vline(xintercept = 0, colour = "black", size = 1) +  # Horizontal line at y = 0
  geom_hline(yintercept = 0, colour = "black", size = 1)+
  theme_minimal() +  # Theme
  labs(
    x = "Canopy Area / m^2",  # X axis label
    y = "Temperature Difference / celsius",  # Y axis label
    title = "Canopy Area versus Temperature Difference"  # Plot title
  )+
  xlim(0, 120000)
print(canopycap_plot)

ggsave()


############################## canopy/capita vs inc_rank


LSOA_Info <- LSOA_Info %>%
  mutate(inc_rank_quintile = ntile(inc_rank, 5))

# Calculate average canopy_per_capita for each inc_rank quintile
avg_canopy_per_capita <- LSOA_Info %>%
  group_by(inc_rank_quintile) %>%
  summarise(avg_canopy_per_capita = mean(canopy_per_capita))

# Plot the data
ggplot(avg_canopy_per_capita, aes(x = factor(inc_rank_quintile),
                                  y = avg_canopy_per_capita)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Neighborhood Quintiles of Indices of Multiple Deprivation - Income",
       y = "Average Canopy Cover per Capita",
       title = "Neighbourhood Deprivation vs Average Canopy Cover per Capita" ) +
  scale_x_discrete(labels = paste0("Quintile ", 1:5)) +
  theme_minimal()


LSOA_Info <- LSOA_Info %>%
  mutate(emp_rank_quintile = ntile(emp_rank, 5))


# Calculate average canopy_per_capita for each inc_rank quintile
avg_canopy_per_capita <- LSOA_Info %>%
  group_by(emp_rank_quintile) %>%
  summarise(avg_canopy_per_capita = mean(canopy_per_capita))

# Plot the data
ggplot(avg_canopy_per_capita, aes(x = factor(emp_rank_quintile), 
                                  y = avg_canopy_per_capita)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Neighborhood Quintiles of Indices of Multiple Deprivation - Employment",
       y = "Average Canopy Cover per Capita",
       title = "Neighbourhood Deprivation vs Average Canopy Cover per Capita") +
  scale_x_discrete(labels = paste0("Quintile ", 1:5)) +
  theme_minimal()


