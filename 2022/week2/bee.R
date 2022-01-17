# TidyTuesday: Week 2
# Bee Colonies

# Get the Data
tuesdata <- tidytuesdayR::tt_load('2022-01-11')
colony <- tuesdata$colony
stressor <- tuesdata$stressor
# Want to produce some hexbin map  of the US
# Packages
library(tidyverse)
library(geojsonio) # Read hexagones boundaries 
library(broom) # Fortify data -> transform json to dataframe
library(rgeos) # Calculate centroid of each hex to add the labels
library(RColorBrewer) # Color pallete library
library(rgdal) # geospatial library

# Credits to R Graph Gallery on the hexbin map
# Download hex boundaries in geojson format  from: 
# https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map

# Read hex boundaries file you've just downloaded
hexb <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
hexb@data =  hexb@data %>%
    mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# 'Fortify' data to be able to show it with ggplot2 (need a data frame)
hexb_fortified <- tidy(hexb, region = "google_name")

# Calculate the centroid of each hexagon to add the label:
centers <- cbind.data.frame(data.frame(gCentroid(hexb, byid=TRUE), 
                                       id=hexb@data$iso3166_2))

hexb_fortified %>%
    left_join(. , stressor, by=c("id"="state")) %>% 
    mutate(stressor = as_factor(stressor)) %>%
    drop_na() %>%
    filter(year == 2020 | year == 2021) %>%
    ggplot() +
    geom_polygon(aes(long, lat, fill = stress_pct, group = group)) +
    geom_text(data = centers, aes(x, y, label = id)) +
    facet_wrap(.~stressor) +
    scale_fill_gradientn(colours = c("#030300",
                                     "#db8300",
                                     "#eaa001",
                                     "#f9c700",
                                     "#ffe11d"),
                         trans = "log") +
    
    theme_void() +
    coord_map() +
    labs(title = "How are bee colonies affected in different states?",
         subtitle = "We show the percent of colonies affected by stressor in 2020 and 2021",
         caption = "Source: USDA |  Viz: Carlos Passos @chpassos_") +
    theme(
        plot.title = element_text(color = "gray43", face = "bold", size = 30, margin = margin(t = 15), lineheight = .3),
        plot.subtitle = element_text(color = "gray51", face = "bold", size = 16, margin = margin(5, 0, 20, 0), lineheight = .3),
        plot.caption = element_text(color = "gray43", size = 13, hjust = .5),
        text = element_text(color = "#22211d"),
        legend.position = "bottom",
        legend.key.width = unit(2.5, "cm"),
        legend.title=element_blank(), 
        legend.direction = "horizontal",
        legend.margin=margin(20,0,20,0),
        strip.text.x = element_text(size = 15, colour = "gray51"),
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA)) 
