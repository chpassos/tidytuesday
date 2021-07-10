devtools::install_github("davidsjoberg/ggsankey")
library(tidyverse)
library(ggsankey)
library(showtext)

# Read data
animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

# Data structure
str(animal_rescues)
glimpse(animal_rescues)
animal_rescues %>% View()

# Transforming data
animal_rescues <- animal_rescues %>%
  mutate(property_type = case_when(
    str_detect(property_type, "House - single occupancy") ~ "House",
    str_detect(property_type, "Converted Flat/Maisonette") ~ "Converted Flat/Maisonette",
    str_detect(property_type, "Domestic garden") ~ "Domestic Garden",
    str_detect(property_type, "Purpose Built Flats/Maisonettes") ~ "Purpose Build Flats/Maisonette",
    TRUE ~ property_type))

# Separating most common rescued animals
common_animal <- animal_rescues %>%
  group_by(animal_group_parent) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(!(str_detect(animal_group_parent, "Unknown"))) %>%
  arrange(desc(count)) %>%
  top_n(6)

# Separating most commons places
common_places <- animal_rescues %>%
  group_by(property_type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  filter(count >= 100)

animals_property <- animal_rescues %>%
  filter(animal_group_parent %in% common_animal$animal_group_parent) %>%
  filter(property_type %in% common_places$property_type)


df <- animals_property %>%
  make_long(animal_group_parent, property_type)


# Viz -
font_add_google(name = "Noto Sans JP", family = "notosans-jp")
showtext.auto()

ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = "gray30", width = 1/12) +
  geom_sankey_label(size = 4, color = "white", fill = "gray40") +
  labs(caption = "Source: London.gov |  Viz: Carlos Passos @chpassos_",
       title = "Animal Rescue Locations",
       subtitle = "The London Government shares a collection of data about different aspects of animal rescue in the city.\n
                  Here, I've separated the most common rescued animals, along with the most common places that they are found. ") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 18) +
  theme(text = element_text(family="notosans-jp"),
        plot.title = element_text(hjust = 0.5, size = 44, margin = margin(30, 0, 20, 0)),
        plot.subtitle = element_text(hjust = 0.5, size = 14, margin = margin(10, 0, 10, 0)), 
        plot.background = element_rect(fill = "#e8e9eb"), 
        panel.background = element_rect(fill = "#e8e9eb", colour = "#e8e9eb",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = "longdash",
                                        colour = "#e8e9eb"), 
        panel.grid.minor = element_line(size = 0.25, linetype = "longdash",
                                        colour = "#e8e9eb"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
