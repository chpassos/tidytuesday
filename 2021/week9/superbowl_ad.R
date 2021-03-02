library(tidyverse)

youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')

# Tidying data
tidy_tbl <- youtube %>%
  select(year, brand, funny, show_product_quickly, patriotic, celebrity, 
         danger, animals, use_sex, view_count, like_count) %>%
  pivot_longer(c(funny, show_product_quickly, patriotic, celebrity, danger, animals, use_sex), 
               names_to = "ad_type", values_to = "ad_value")

# Type of ad per brand
ad_type_per_brand <- tidy_tbl %>%
  filter(ad_value != FALSE) %>%
  group_by(brand, ad_type) %>%
  summarise(count_per_type = n()) %>%
  ungroup() %>%
  group_by(brand) %>%
  mutate(mean_type = count_per_type / sum(count_per_type))





ad_type_per_brand %>% ggplot(aes(brand, mean_type)) +
  geom_bar(aes(fill = ad_type) , stat = "identity") +
  coord_polar() +
  scale_fill_manual(values=c("#E1BBF5", "#A9F5B3", "#F2F5BC", "#F5DCA4", "#F5B0C2","#C9D7F5", "#F5ACA4"), 
                    name="Experimental\nCondition",
                    breaks=c("animals", "celebrity", "danger", "funny", "patriotic",
                             "show_product_quickly", "use_sex"),
                    labels=c("Animals", "Celebrity", "Danger", "Funny", "Patriotic",
                             "Brand showing", "Sex"))+
  labs(x = NULL, y = NULL, title = "Type of AD per Brand in SuperBowl Commercials") +
  theme(plot.title = element_text(family = "Trebuchet MS", color = "#545454", hjust = 0.5, size = 20, margin=margin(15,0, 0, 0)),
        axis.text.y = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_text(size = 8, family = "Trebuchet MS"),
        legend.title = element_blank()) +
  theme(panel.background = element_blank()) 
