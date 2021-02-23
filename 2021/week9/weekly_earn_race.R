library(tidyverse)

# Get data
earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

# Data wrangling and plotting
earn %>%
  filter(race != "All Races") %>%
  group_by(race, year) %>%
  summarise(median_earn = median(median_weekly_earn)) %>%
  ggplot(aes(year, median_earn)) +
  geom_line(aes(group = year), size = 1.5, color = "#617677") +
  geom_point(aes(color = race), size = 10) +
  geom_text(aes(label = paste0("$",median_earn))) +
  scale_color_manual(values = c("Asian" = "#AB5498", "White" = "#5498AB", "Black or African American" = "#98AB54")) +
  scale_x_discrete(limits = 2010:2020) +
  theme_bw() +
  labs(x = "Years" ,
       y = "Median Weekly Earns (Dollar)",
       title = "Median Weekly Earning by Race in U.S.A.",
       fill = NULL) +
  theme(plot.title = element_text(family = "Trebuchet MS", color = "#545454", hjust = 0.5, size = 26, margin=margin(30,0, 30, 0)),
        axis.title.y = element_text(family = "Trebuchet MS", color = "#545454", size = 16),
        axis.title.x = element_text(family = "Trebuchet MS", color = "#545454", size = 16),
        legend.title = element_blank())
  
