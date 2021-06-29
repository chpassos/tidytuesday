# TidyTuesday: Week 26
library(tidyverse)
library(hrbrthemes)
library(ggpubr)
library(ggtext)
library(patchwork)
# Get Data
parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')

str(parks)
glimpse(parks)

# Data transformation
parks <- parks %>%
  rename(investment = spend_per_resident_data) %>%
  mutate(investment = as.double(str_remove(investment, "\\$")))

rich_cities  <- c("New York", "Los Angeles", "Chicago", "San Francisco",
                 "Dallas", "Houston", "Boston", "Philadelphia", "Atlanta")


park_investment <- parks %>%
  select(year, city, investment, total_points) %>% 
  filter(city %in% rich_cities | grepl("Washing", city)) %>%
  mutate(city = case_when(
    city == "Washington, D.C." ~ "Washington, DC",
    TRUE ~ as.character(city)
  ))

tidy_df <- park_investment %>%
  filter(year == min(.$year) | year == max(.$year)) %>%
  mutate(investment = as.double(str_remove(investment, "\\$")))
  

# Viz --
pal <- c("#FF7F50", "#008080")

p1 <- ggplot(tidy_df) +
  geom_line(aes(city, investment), size = 1, color = "gray") +
  geom_point(aes(city, investment, color = as.factor(year)), size = 4) +
  coord_flip() +
  scale_color_manual(values = pal, guide = FALSE) +
  labs(y = "Spending per resident in USD",
       x = NULL) +
  theme(legend.position = "none") +
  theme_ft_rc() +
  plot_annotation(
    title = toupper("U.S. cities investment in parks"),
    subtitle = glue::glue(
      "For the wealthiest US cities, we have the investments (USD per resident) in parks between <b style='color:#FF7F50;'>2012</b> and <b style='color:#008080;'>2020</b>, and its impact on the overall points of the cities."),
    theme = theme(
      plot.title = element_text(color = "gray", face = "bold", size = 30, margin = margin(t = 15), lineheight = .3),
      plot.caption = element_text(color = "gray", size = 10, hjust = .5),
      plot.subtitle = ggtext::element_markdown(color = "gray", size = 16, lineheight = .5, margin = margin(t = 10, b = -85)),
      plot.background = element_rect(fill = "#252a32", color = "#252a32"),
      panel.background = element_rect(fill = "#252a32", color = "#252a32")))
 
p2 <- ggplot() +
  geom_point(data = subset(parks, year == 2012), aes(investment, total_points), color = "#FF7F50", size = 2) +
  geom_smooth(data = subset(parks, year == 2012), aes(investment, total_points), method = "lm", se = FALSE, color = "#FF7F50") +
  geom_point(data = subset(parks, year == 2020), aes(investment, total_points), color = "#008080", size = 2) +
  geom_smooth(data = subset(parks, year == 2020), aes(investment, total_points), method = "lm", se = FALSE, color = "#008080") +
  geom_text(data = subset(parks, year == 2012 & city %in% rich_cities), aes(investment, total_points, label = city), color = "gray", size = 4, vjust = -0.7) +
  geom_text(data = subset(parks, year == 2020 & city %in% rich_cities), aes(investment, total_points, label = city), color = "gray", size = 4, vjust = -0.7) +
  labs(x = "Spending per resident in USD", 
       y = "Total points per City",
       caption = "Source: TPL |  Viz: Carlos Passos @chpassos_") +
  theme_ft_rc()

ggarrange(p1, p2, ncol = 1, nrow = 2)
