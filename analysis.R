library(ggplot2)
library(dplyr)
library(tidyr)

master_data <- read.csv("master.csv", stringsAsFactors = FALSE)

master_data <- master_data %>% 
  filter(suicides_no != "0") %>% 
  select(year, suicides_no, sex) %>% 
  arrange(year)

female_only <- master_data %>%
  filter(sex == "female") %>% 
  group_by(year) %>% 
  summarise(suicides_no = sum(suicides_no)) %>% 
  mutate(sex = "Female")
  
male_only <- master_data %>%
  filter(sex == "male") %>% 
  group_by(year) %>% 
  summarise(suicides_no = sum(suicides_no)) %>% 
  mutate(sex = "Male")

final_df <- bind_rows(male_only, female_only) %>% 
  arrange(year)

draw_plot <- function() {
  ggplot(data = final_df) +
    geom_point(mapping = aes(x = year, y = suicides_no, color = sex)) +
    ggtitle("Suicides report in the world over the last 40 years") +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Suicide Cases") +
    labs(color = "Gender: ")
}