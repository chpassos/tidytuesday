library(tidyverse)

#Challenge 1: Comparative Increase of White and Colored Population in Georgia
#Loading
georgia_pop <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/georgia_pop.csv')

# Tidying
georgia_pop <- georgia_pop %>%
  pivot_longer(c(Colored, White), names_to = "Type", values_to = "Pop")

# Plotting
ggplot(georgia_pop, aes(Year, Pop)) +
  geom_line(aes(linetype = Type)) +
  ggtitle("Comparative Increase of White and Colored\nPopulation of Georgia") +
  xlab(NULL) +
  ylab("Percents") +
  coord_flip() + 
  scale_y_reverse() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = '#ECDED1', colour = '#ECDED1'),
        panel.background = element_rect(fill = '#ECDED1', colour = '#ECDED1')) +
  theme(panel.grid.major = element_line("#C93211"), 
        panel.grid.minor = element_line("#C93211"),
        panel.border = element_rect(color = "#2B2B2B", fill = NA)) +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  theme(plot.margin = unit(c(1,5,1,5),"cm"))


ggsave("georgia_pop.png", 
       dpi = 120)
