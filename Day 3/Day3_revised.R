library(readxl)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(cowplot)
library(ggwordcloud)
library(showtext)

font_add_google(name = "Mouse Memoirs",   
                family = "mm")
showtext_auto()

# Data Import

disney_data <- read.csv("disney_plus_titles.csv") 

# Data Wrangling

day3_data <- disney_data %>%
  group_by(type, release_year) %>%
  count(release_year) %>%
  ungroup()

# First Plot

p1 <- ggplot(day3_data, aes(x = release_year, y = n, color = type)) + 
    geom_line(size = 1)+
    coord_cartesian(xlim = c(1928, 2021)) +
    labs(x = "Year",
         y = "# of Movies",
         color = "Type") +
    scale_color_manual(values = c("#ce75a7", "#006e99")) +
    geom_vline(xintercept = 1975, linetype = "dotted", color = "#006e99") +
    theme(text = element_text(family = "mm"),
      panel.background = element_rect(fill = "#f3b9cb"),
      axis.text.x = element_text(family = "mm", size = 30),
      axis.text.y = element_text(family = "mm", size = 30),
      axis.title.y = element_text(size = 35),
      axis.title.x = element_text(size = 35),
      axis.ticks = element_blank(),
      legend.title = element_text(family = "mm", size = 30),
      legend.text = element_text(family = "mm", size = 30),
      legend.key = element_rect(fill = "white", color = NA),
      panel.grid = element_blank()
      ) 
    p1

set.seed(225)

# Data Wrangling

 part1 <- disney_data %>%
  filter(release_year < 1975) %>%
  select(description) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  filter(n > 3) 

 # Second Plot
 
p2 <- ggplot(data = part1, aes(label = word, size = n, color = n)) +
  geom_text_wordcloud_area(
    mask = png::readPNG(system.file("extdata/hearth.png",
                                    package = "ggwordcloud", mustWork = TRUE
    )),
    rm_outside = TRUE
  ) +
  scale_size_area(max_size = 70) +
  scale_color_gradient(low = "#f3b9cb", high = "#ce75a7") +
  theme(panel.background = element_rect(fill = "#bff5fd"),
        text = element_text(family = "ns"))
p2

# Data Wrangling

  part2 <- disney_data %>%
  filter(release_year >= 1975) %>%
  select(description) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  filter(n > 20)

# Third Plot
  
p3 <- ggplot(data = part2, aes(label = word, size = n, color = n)) +
  geom_text_wordcloud_area(
    mask = png::readPNG(system.file("extdata/hearth.png",
                                    package = "ggwordcloud", mustWork = TRUE
    )),
    rm_outside = TRUE
  ) +
    scale_size_area(max_size = 20) +
    scale_color_gradient(low = "#f3b9cb", high = "#ce75a7") +
  theme(panel.background = element_rect(fill = "#bff5fd"))
p3
  
# Final Plot

plots <- align_plots(p2, p3,  align = 'r', axis = 'r')

title <- ggdraw() + 
  draw_label(
    "Number of Movies and TV Shows released by Disney",
    fontface = 'bold',
    fontfamily = "mm",
    hjust = 0.5,
    x = 0.5,
    size = 50,
    color = "#ce75a7"
  )  
title2 <- ggdraw() + 
  draw_label(
    "Wordclouds of the Descriptions of Movies and TV Shows for the periods 1928 - 1975 and  1976 - 2011",
    fontface = 'bold',
    fontfamily = "mm",
    hjust = 0.5,
    x = 0.5,
    y = 0.5, 
    size = 50,
    color = "#ce75a7"
  ) 
caption <- ggdraw() + 
  draw_label(
    "Data Source: Kaggle (Disney+ Movies and TV Shows) |#30DayChartChallenge - Day #3 |Prepared by: @Cyd_yzc", 
      fontface = 'bold',
      fontfamily = "mm",
    hjust = 0.5,
    x = 0.5,
    y = 0.5, 
    size = 30,
    color = "#ce75a7"
  ) 

bottom_row <- plot_grid(
  plots[[1]], plots[[2]],
  labels = "",
  rel_widths = c(1, 1), 
  nrow = 1
)
final_plot <- plot_grid(title, p1, title2, bottom_row, caption, labels = "", ncol = 1,
          rel_heights = c(0.4, 1, 0.3, 1, 0.1))

ggsave("Day3_revised.png", final_plot)


