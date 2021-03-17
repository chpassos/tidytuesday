library(tidyverse)
library(waffle)
library(showtext)
library(patchwork)
library(ggtext)

font_files()
font_add(family = "necrosans", "necrosans.ttf")
font_families()
showtext_auto()

# Load data
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

crypt_df <- games %>%
  filter(gamename == "Crypt of the NecroDancer") %>%
  select(year, month, gain) %>%
  mutate(type = ifelse(gain > 0, 1, 2)) %>%
  group_by(year, type) %>%
  mutate(count = n()) %>% 
  select(year, type, count) %>%
  distinct(year, type, .keep_all = TRUE) %>%
  drop_na() %>%
  mutate(year = as.factor(year),
         type = as.character(type))
  
  ungroup() %>%
  group_by(year, type) %>%
  distinct() %>%
  drop_na() %>%
  mutate(year = as.factor(year),
         type = as.character(type))


distinct(year, type, .keep_all = TRUE)
# Viz --------------------------------

# Color pallete
pal <- c("#d22ed3", "#26db65")


crypt_df %>%
  ggplot() +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  geom_waffle(aes(values = count, fill = type), n_rows = 4, size = .45, flip = T, show.legend = F) +
  coord_equal() +
  scale_fill_manual(values = pal) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  plot_annotation(
    title = toupper("Crypt of the NecroDancer playerbase over the years"),
    subtitle = glue::glue(
      "Year by year, we can see a monthly <b style='color:#d22ed3;'>increase</b> or <b style='color:#26db65;'>decrease</b> in the number of players in the game."
    ),
    caption = "Note: Each square represents a month |  Source: Steam |  Viz: Carlos Passos @chpassos_",
    theme = theme(
      plot.title = element_text(family = "necrosans", color = "#FFFFFF", face = "bold", size = 34, margin = margin(t = 25), lineheight = .3),
      plot.caption = element_text(family = "necrosans", color = "#FFFFFF", size = 14, hjust = .5),
      plot.subtitle = ggtext::element_markdown(family = "necrosans", color = "#FFFFFF", size = 20, lineheight = .5, margin = margin(t = 10, b = -85)),
      plot.background = element_rect(fill = "#000000", color = "#000000"),
      panel.background = element_rect(fill = "#000000", color = "#000000"))) +
  theme(panel.background = element_rect(fill = "#000000", color = "#000000"),
        plot.background = element_rect(fill = "#000000", color = "#000000"),
        strip.background =element_rect(fill="#000000"),
        strip.text = element_text(family = "necrosans", colour = "#FFFFFF", size = 20))
