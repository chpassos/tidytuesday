library(tidyverse)


# Load data
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

# First plot: gender equity
p <- movies %>% 
  select(year, title, country, binary, genre, metascore, imdb_rating, budget_2013) %>%
  mutate(country = strsplit(country, ",")) %>%
  unnest(country) %>%
  tidyr::extract(country, c("country")) %>%
  drop_na() %>%
  group_by(country, binary) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

cp <- coord_polar(theta = "y")
cp$is_free <- function() TRUE

p1 <- p %>%
  ggplot(aes(country, count, fill = binary)) +
  geom_bar(stat = 'identity', position = 'fill') +
  cp +
  scale_fill_manual(values=c("#412EA3", "#F0D722"), 
                    breaks=c("FAIL", "PASS"),
                    labels=c("FAIL", "PASS")) +
  facet_wrap(~country, scales = "free") +
  labs(x = NULL, 
       y = NULL, 
       title = "How different countries represent gender equity?\nAnd how this impacts on its budget and performance?",
       subtitle = "From the top five countries that produces movies,\n roughly half of them pass on the Bechdel test") +
  theme(
    plot.title = element_text(family = "Bodoni MT", color = "#545454", hjust = 0.5, size = 14, margin=margin(15,0, 5, 0)),
    plot.subtitle = element_text(family = "Bodoni MT", hjust = 0.5, size = 10, color = "gray25", margin=margin(0, 0, 10, 0)),
    axis.text.y = element_blank(), axis.ticks = element_blank(),
    axis.text.x = element_text(size = 8, family = "Trebuchet MS"),
    legend.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank()
    ) +
  theme(panel.background = element_blank()) +
  theme(aspect.ratio=0.5)

# Second plot: Number of films produced
b <- movies %>% 
  select(year, title, country, binary, genre, metascore, imdb_rating, budget_2013) %>%
  mutate(country = strsplit(country, ",")) %>%
  unnest(country) %>%
  tidyr::extract(country, c("country")) %>%
  drop_na() %>%
  group_by(year, country) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(country %in% c("Canada", "France", "Germany", "UK", "USA"))

p2 <- b %>%
  ggplot(aes(year, count)) +
  geom_line(aes(group = country, color = country)) +
  labs(x = "Year",
       y = "Number of films produced",
       subtitle = "And even though the USA produces significantly more films than the others...") +
  theme(
    plot.subtitle = element_text(family = "Bodoni MT", hjust = 0.5, size = 10, color = "gray25", margin=margin(0, 0, 10, 0)),
    axis.text.x = element_text(size = 8, family = "Trebuchet MS"),
    legend.title = element_blank()
  ) +
  theme(panel.background = element_blank()) +
  theme(aspect.ratio=0.5)
  


# Third plot: Investment per country
c <- movies %>% 
  select(year, title, country, binary, genre, metascore, imdb_rating, budget_2013) %>%
  mutate(country = strsplit(country, ",")) %>%
  unnest(country) %>%
  tidyr::extract(country, c("country")) %>%
  drop_na %>%
  group_by(year, country) %>%
  summarise(median = median(budget_2013)) %>%
  filter(country %in% c("Canada", "France", "Germany", "UK", "USA"))

p3 <- c %>%
  ggplot(aes(year, median)) +
  geom_line(aes(group = country, color = country)) +
  scale_y_continuous(limits=c(0, 190000000)) +
  labs(x = "Year",
       y = "Median Investment Budget",
       subtitle = "The median investment, \nalthough very different through the years, \nis similar nowadays") +
  theme(
    plot.subtitle = element_text(family = "Bodoni MT", hjust = 0.5, size = 10, color = "gray25", margin=margin(0, 0, 10, 0)),
    axis.text.x = element_text(size = 8, family = "Trebuchet MS"),
    legend.title = element_blank()
  ) +
  theme(panel.background = element_blank()) +
  theme(aspect.ratio=0.5)
  
# Foruth plot: Mean rating (IMDb and Metascore)
p4 <- movies %>%
  select(year, title, country, binary, genre, metascore, imdb_rating, budget_2013) %>%
  mutate(country = strsplit(country, ",")) %>%
  unnest(country) %>%
  tidyr::extract(country, c("country")) %>%
  drop_na() %>%
  group_by(country) %>%
  summarise(IMDb = mean(imdb_rating), Metascore = mean(metascore)) %>%
  filter(country %in% c("Canada", "France", "Germany", "UK", "USA")) %>%
  pivot_longer(c("IMDb", "Metascore"), names_to = "type", values_to = "value") %>%
  ggplot(aes(country, value)) +
  geom_point(aes(color = type), size = 3) +
  facet_wrap(~type, scale = "free") +
  labs(x = "Country",
       y = "Mean Score",
       subtitle = "However, there is a  difference in IMDb and Metascore ratings",
       caption = "Data: FiveThirtyEight\nVisualization: @chpassos_") +
  theme(
    plot.subtitle = element_text(family = "Bodoni MT", hjust = 0.5, size = 10, color = "gray25", margin=margin(0, 0, 10, 0)),
    axis.text.x = element_text(size = 8, family = "Trebuchet MS"),
    legend.title = element_blank(),
    plot.caption = element_text(size = 10, color = "gray35")
    ) +
  theme(panel.background = element_blank()) +
  theme(aspect.ratio=0.5)
 
 
# All plots together
grid.arrange(
  p1, p2, p3, p4,
  widths = c(1, 1, 1, 1),
  layout_matrix = rbind(c(NA, 1, 1, NA),
                        c(2, 2, 3, 3),
                        c(NA, 4, 4, NA))
)
