#TidyTuesday Week 24
library(tidyverse)
library(ggstream)
library(ggthemes)
library(RColorBrewer)

# Read data
fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')

# Inspect data
glimpse(fishing)
head(fishing)

# Top production species
top_production_species <- fishing %>%
  drop_na(grand_total) %>%
  group_by(species) %>%
  summarise(production_per_species = sum(grand_total)) %>%
  arrange(desc(production_per_species))  %>%
  top_n(10) %>%
  pull(species)

p1 <- fishing %>%
  filter(species %in% top_production_species) %>%
  drop_na(grand_total) %>%
  group_by(year, lake, species) %>%
  summarise(production_amount = sum(grand_total))

# Viz--
ggplot(p1, aes(year, production_amount, fill = species)) +
  geom_stream() +
  facet_wrap(~lake, scales = "free", ncol = 1, strip.position = "left") +
  labs(x = "Year",
       y = "Amount observed",
       caption = "Source: Great Lakes DataBase |  Viz: Carlos Passos @chpassos_",
       title = "Amount of fish observed in different lakes of the Great Lake") +
  scale_fill_brewer(palette = "Spectral") +
  theme_pander() +
  theme(plot.title = element_text(hjust = 0.5, size = rel(1)),
        legend.title = element_blank(), 
        legend.position="bottom",
        legend.key.size = unit(0.2, "cm"),
        axis.text.y = element_text(angle = 45))
