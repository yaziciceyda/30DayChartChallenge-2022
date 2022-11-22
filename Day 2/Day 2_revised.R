# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load(2021, week = 52)

starbucks <- tuesdata$starbucks

library(tidyverse)
library(ggplot2)
library(ggtext)
library(stringr)
library(showtext)
library(emojifont)


font_add_google(name = "Noto Serif",   
                family = "ns")
showtext_auto()

fa <- fontawesome('fa-coffee')

# Data Wrangling

day2_data <- starbucks %>%
  filter(str_detect(starbucks$product_name, "Ice")) %>%
  filter(size %in% c("tall",  "grande",  "venti")) %>%
  filter(whip == 0) %>%
  mutate(size = as.factor(size),
         size = str_to_title(size),
         milk = as.factor(milk),
         whip = as.factor(whip),
         product_name = ifelse(product_name == "Iced Black tea", "Iced Black Tea", 
                               product_name),
         product_name = ifelse(product_name == "Iced Coffee with milk", 
                               "Iced Coffee with Milk", product_name),
         product_name = ifelse(product_name == "Iced Black tea Lemonade", 
                               "Iced Black Tea Lemonade", product_name)
         ) %>%
    group_by(product_name, size, milk) %>%
  summarise(mean_calories_per_service = mean(calories/serv_size_m_l),
            mean_sugar_per_service = mean(sugar_g/serv_size_m_l)) %>%
  mutate(cup = fa) %>%
  ungroup() 


# Plot

p <- ggplot(day2_data, aes(x = fct_relevel(size,     "Tall", 
                                                "Grande", 
                                                "Venti"), y = product_name)) +
  geom_text(day2_data, mapping = aes(label = cup, 
                                     size = mean_calories_per_service),
            family ='fontawesome-webfont', 
            color = "white", hjust = 0.5, show.legend = FALSE) +
  scale_size_area(max_size = 13) +
  coord_cartesian() +
     #  coord_fixed(xlim=c(0, 5), ylim=c(-1,22.2)) +
  # geom_hline(yintercept = 20.5, size = 1, color = "white") +
    scale_y_discrete(limits = rev) +
  scale_x_discrete(position = "top") + 
  labs(x = "",
       y = "",
       title = "Average Calories per Service (KCal/ml) for Iced Drinks\n", 
       caption = "Data: Starbucks Coffee Company | TidyTuesday 2021 - Week 52\n
       |#30DayChartChallenge - Day #2 |Prepared by: @Cyd_yzc") +
  theme(text = element_text(family = "ns"),
  panel.border = element_rect(colour = "#006241", fill = NA, size = 3),
  plot.title = element_text(size = 20, color = "white",
                            hjust = 0.5, vjust = -1),
  panel.background = element_rect(fill = "#006241"),
  plot.background = element_rect(fill = "#006241"),
  plot.caption = element_text(size = 12, color = "white", 
                              hjust = 1, vjust = -1),
  panel.grid = element_blank(),
  axis.ticks = element_blank(),
  axis.text.y = element_text(hjust = 0, color = "white", size = 14),
  axis.title.y = element_text(vjust = -0.5),
  axis.text.x = element_text(hjust = 0.5, color = "white", size = 14))

p

# Save the Plot

dev.off()
ggsave("Day_v2.png", p, width = 20, height = 14, dpi = 72)


 