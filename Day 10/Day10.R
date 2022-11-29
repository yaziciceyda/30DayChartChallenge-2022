library(ggkeyboard)
library(stringr)
library(ggplot2)
library(tidyverse)
library(ggtext)

# Data Wrangling

text <- "30 Day Chart Challenge 2022"
text <- str_to_upper(text)
ch <- strsplit(text, "")

table_list <- as_tibble(table(ch)) %>%
  filter(ch != " ") %>%
  mutate(color = case_when(
    n == 1 ~  "#F7B9D9",
    n == 2 ~  "#F67FBC",
    n == 3 ~  "#F90B86"
  )) %>%
  arrange(n)

# Plot

p <- ggkeyboard(mac, palette = keyboard_palette("magic"), font_size = 5) %>%
   highlight_keys(keys = table_list$ch, colour = "#F67FBC",
                 fill = "#F67FBC", alpha = 0.6) +
  ggplot2::labs(title = "Characters in #30DayChartChallenge - 2022",
                caption = "#30DayChartChallenge - Day #10 |Prepared by: @Cyd_yzc",
                subtitle = "<span>\n The characters 2 and A appear three times, 0, C, E, H and L appear twice<span><br>and the rest occur only once.</span>") +
  ggplot2::theme(plot.title = element_text(hjust = 0.5, size = 44, color = "white"),
                 plot.subtitle = element_markdown(hjust = 0.5,
                                                  color = "white", size = 25),
                 plot.caption = element_text(hjust = 1, 
                                             size = 23, color = "white"),
                 plot.background = element_rect(fill = "#F99CCC"),
                 plot.margin = unit(c(1,1,1,1), "cm"),
                 panel.border = element_rect(color = "#F99CCC", fill = NA)) 

# Save the Plot

ggsave("Day10.png", p, width = 20, height = 10, dpi = 72)              
                
