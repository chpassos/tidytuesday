# TidyTuesday: Week 22 
library(tidyverse)
library(showtext)
library(patchwork)
library(ggtext)
library(ggthemes)

font_files()
font_add(family = "mariokartsans", "Mario-Kart-DS.ttf")
font_families()
showtext_auto()

# Read data
records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

# The longest-running record in MarioKart 64
p1 <- records %>%
  arrange(desc(record_duration)) %>%
  group_by(track, type) %>%
  summarise(max = max(record_duration))


# Viz -------------------
pal <- c("#049CD8", "#E52521")


p1 %>%
  ggplot(aes(reorder(track, max), max, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = max), position=position_dodge(width=1)) +
  coord_flip() +
  scale_fill_manual(values = pal, guide = FALSE) +
  labs(x = NULL, 
       y = "Record duration in days") +
  plot_annotation(title = toupper("Longest-Running record in MarioKart64"),
    subtitle = glue::glue("For how long a record can hold itself? Here, we can see the record duration, in days, for <b style='color:#049CD8;'>single laps</b> or <b style='color:#E52521;'>three laps</b> in different tracks of the game."),
    caption = "Source: MarioKart World Records |  Viz: Carlos Passos @chpassos_",
    theme = theme(
    plot.title = element_text(family = "mariokartsans", hjust = 0.5, size = 30),
    plot.subtitle = ggtext::element_markdown(hjust = 0.5, color = "#000000", size = 14))) +
  theme(axis.text.x = element_text(angle = 45, size = 13, margin = margin(10,20)),
        axis.text.y = element_text(size = 13, margin = margin(10,20))) +
  theme_pander()

