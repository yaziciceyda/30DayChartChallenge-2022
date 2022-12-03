library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(contactdata)
library(ggforce)
library(patchwork)
library(scales)
library(showtext)

# Data Import

dt <- read.csv("countries of the world.csv", dec = ",")
head(dt)

# Data Wrangling

dt2 <- dt %>%
  mutate(Region = as.factor(Region)) %>%
  mutate(Region_new = case_when(
  Region %in% c("ASIA (EX. NEAR EAST)         ", 
                "C.W. OF IND. STATES ",
                "NEAR EAST                          ")  ~ "ASIA",
  Region %in% c("BALTICS                            ",
                "EASTERN EUROPE                     ", 
                "WESTERN EUROPE                     ")  ~ "EUROPE",
  Region %in% c("LATIN AMER. & CARIB    ",
                "NORTHERN AMERICA                   ") ~ "AMERICA",
  Region %in% c("NORTHERN AFRICA                    ", 
                "SUB-SAHARAN AFRICA                 ") ~ "AFRICA",
  Region %in% c("OCEANIA                            ") ~ "OCEANIA"
)) %>%
  rename("GDP" = GDP....per.capita.,
         "Phones" = Phones..per.1000.) %>%
  group_by(Region_new) %>%
  mutate(corr = corr.test(GDP, Phones)$r)  %>%
  ungroup() %>%
  mutate(color = case_when(
         Region_new == "AFRICA" ~ "#00BA38",
         Region_new == "AMERICA" ~ "#00BFC4",
         Region_new == "ASIA" ~ "#619CFF",
         Region_new == "EUROPE" ~ "#B79F00",
         Region_new == "OCEANIA" ~ "#F564E3", 
         )) %>%
  ungroup()

dt3 <- dt2 %>%
  group_by(Region_new) %>%
  select(Region_new, corr, color) %>%
  distinct() %>%
  ungroup()
  
font_add_google("DM Serif Display", "sd")
showtext_auto()

# Plot

p1 <- ggplot(dt2, aes(x = GDP, y = Phones, 
                colour = Region_new, size = Birthrate)) +
      geom_point() +
      scale_x_continuous(label = scales::comma) +
      labs(title = 'The Number of Phones vs GDP',
           subtitle = '\nAs the GDP per capita increases the number of phones (per 1000 people) increases as expected. \nMoreover, the continents with the high level of birthrate have lower GDP and number of phones.',
           caption = 'Data: Kaggle |#30DayChartChallenge - Day #13 |Prepared by: @Cyd_yzc') +
    theme(text = element_text(family = "sd"),
    panel.background = element_rect(fill = "#F9E099"),
    panel.grid = element_blank(),
    legend.key.size = unit(1, 'cm'),
    legend.text = element_text(size = 30),
    legend.title = element_text(size = 45),
    axis.text = element_text(size = 59),
    axis.title = element_text(size = 79),
    plot.title = element_text(size = 79, hjust = 0),
    plot.subtitle = element_text(size = 45, hjust = 0),
    plot.caption = element_text(size = 35, hjust = 1)) +
  
  # Save the Plot
  
    guides(color = guide_legend(title = "Continents",override.aes = list(size = 10)))
ggsave("Day13.png", p1)

p2 <- ggplot(dt3)  +
  geom_col(aes(x = reorder(Region_new, corr), y = corr), alpha = 0.3, fill = "grey40", width = 0.5) +
  geom_point(aes(x = Region_new, y = corr + 0.01, colour = color), shape = 1, size = 22, stroke = 2) +
  geom_text(aes(x = Region_new, y = corr + 0.01, colour = color, size = 65, label = round(corr,2))) +
  geom_text(aes(x = Region_new, y = 0.25, label = Region_new, colour = color,
                size = 65, hjust = 0)) +
  coord_flip() +
  labs(x = "",
       y = "Correlation" ) +
  theme(text = element_text(family = "sd"),
        panel.background = element_rect(fill = "#F9E099"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 35),
        axis.title.x = element_text(size = 35),
        legend.position = "none")




patchwork <- p1 
patchwork + plot_annotation(
  title = 'The Number of Phones vs GDP',
  subtitle = '\nAs the GDP per capita increases the number of phones increases as expected. Moreover, the continents with the high level of birthrate have lower GDP and number of phones.',
  caption = 'Data: Kaggle |#30DayChartChallenge - Day #13 |Prepared by: @Cyd_yzc',
  theme = theme(text = element_text(family = "sd"),
                plot.title = element_text(size = 25, hjust = 0),
                plot.subtitle = element_text(size = 18, hjust = 0),
                plot.caption = element_text(size = 12, hjust = 1))) 


ggsave("Day13.png", patchwork) 
              