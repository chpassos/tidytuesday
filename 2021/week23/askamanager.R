#TidyTuesday: Week 21
library(tidyverse)
library(RColorBrewer)
library(plotly)
library(hrbrthemes)
library(viridis)

# Read data
survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

# Keeping USD currency
usd <- survey %>%
  filter(currency == "USD")

# Mean annual salary per education, per gender (without millionaires)
mean_annual_salary <- usd %>%
  group_by(highest_level_of_education_completed, gender) %>%
  mutate(gender=replace(gender, gender=="Prefer not to answer", "Other or prefer not to answer")) %>%
  filter(!is.na(gender)) %>%
  filter(annual_salary <= 1000000) %>% 
  summarise(mean_annual_sal = mean(annual_salary))

# More transformations on data
p1 <- mean_annual_salary %>%
  filter(!is.na(highest_level_of_education_completed)) %>%
  mutate(education = as.factor(highest_level_of_education_completed)) %>%
  mutate(education = fct_relevel(education,
                                 "PhD", "Master's degree", "Professional degree (MD, JD, etc.)",
                                 "College degree", "High School", "Some college")) %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(gender = fct_relevel(gender, "Other or prefer not to answer", "Non-binary",
                              "Man", "Woman")) %>%
  ungroup() %>%
  select(gender, education, mean_annual_sal)


# Text for plotly
p1 <- p1 %>%
  mutate(text = paste0("Mean salary: ", gender, " with ", education, ": ", mean_annual_sal, " USD"))


# Viz ---
p <- ggplot(p1, aes(gender, education, fill = mean_annual_sal, text = text)) +
  geom_tile() +
  scale_fill_viridis(name = "Salary") +
  labs(x = "Level of Education",
       y = "Gender",
       title = "Salary Differences according to sex and to education",
       subtitle = "Although higher degrees seems to impact your annual earnings, it looks like you can't compete with men that holds any degree level") +
  theme(plot.title = element_text(size = 24, hjust = 0.5, margin = margin(t = 10, b = 15, r = 0, l = 0)),
        plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(t = 10, b = 15, r = 0, l = 0)),
        ) +
  theme_ft_rc()
p
# Make it interactive
ggplotly(p, tooltip="text")
