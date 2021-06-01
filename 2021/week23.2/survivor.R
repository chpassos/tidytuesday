# TidyTuesday Week 23: SurvivoR
library(tidyverse)
library(showtext)
library(patchwork)
library(ggtext)
library(ggthemes)

# Read data
summary <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')

glimpse(summary)
summary %>% View()

# Data transformation
p1 <- summary %>%
  select(season, viewers_premier, viewers_finale, viewers_mean) %>%
  pivot_longer(c(viewers_premier, viewers_finale, viewers_mean), names_to = "audience_type", values_to = "viewers")


# Viz --
pal <- c("#F52701", "#0BA664")
ggplot(p1) +
  geom_line(data = subset(p1, audience_type != "viewers_mean"), aes(reorder(season, -season), viewers)) +
  geom_point(data = subset(p1, audience_type != "viewers_mean"), aes(reorder(season, -season), viewers, color = audience_type), size = 4) +
  coord_flip() +
  scale_color_manual(values= pal, guide = FALSE) +
  labs(x = "Season",
       y = "Number of viewers (in Millions)") +
  theme_minimal() +
  plot_annotation(title = toupper("Viewership of Survivors"),
                  subtitle = glue::glue("Season after season, we can see a decline in the audience both at the <b style='color:#F52701;'>season premier</b> and <b style='color:#0BA664;'>season finale</b> of the show."),
                  caption = "Source: SurvivoR |  Viz: Carlos Passos @chpassos_",
                  theme = theme(
                    plot.title = element_text(family = "mariokartsans", hjust = 0.5, size = 26),
                    plot.subtitle = ggtext::element_markdown(hjust = 0.5, color = "#000000", size = 14)))
