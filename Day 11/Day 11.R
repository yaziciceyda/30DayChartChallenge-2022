library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(grid)
library(showtext)
library(gganimate)
library(ggtext)
library(usefunc)

# Data Import

tuesdata <- tidytuesdayR::tt_load('2022-05-03')

capacity <- tuesdata$capacity
average_cost <- tuesdata$average_cost
wind <- tuesdata$wind

# Data Wrangling

wind <- wind %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  mutate(year_n = cur_group_id(),
         n_wind = n()) %>%
  select(year_n, n_wind, wind_capacity, wind_mwh) %>% 
  summarise(across(.cols = c(year_n, n_wind, wind_capacity, wind_mwh), median)) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    blade1 = round(runif(1, 0, 2 * pi), 1),
    blade2 = round(blade1 + 0.67 * pi, 1),
    blade3 = round(blade2 + 0.67 * pi, 1),
    t = list(seq(0, 2 * pi, 0.1))
  ) %>% 
  unnest(t) %>%
  mutate(year_id = case_when(
    year == 2009 ~ 1,
    year == 2010 ~ 2,
    year == 2011 ~ 3,
    year == 2012 ~ 4,
    year == 2013 ~ 5,
    year == 2014 ~ 6,
    year == 2015 ~ 7,
    year == 2016 ~ 8,
    year == 2017 ~ 9,
    year == 2018 ~ 10,
    year == 2019 ~ 11,
    year == 2020 ~ 12,
  ))

solar <- tuesdata$solar
solar <- solar %>%
  mutate(year = year(date)) %>%
  filter(year != 2021) %>% 
  group_by(year) %>%
  mutate(year_n = cur_group_id(),
         n_solar = n()) %>%
  select(year_n, n_solar, solar_capacity, solar_mwh) %>% 
  summarise(across(.cols = c(year_n, n_solar, solar_capacity, solar_mwh), median)) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    t = list(seq(0, 2 * pi, 0.1))
  ) %>% 
  unnest(t) %>%
  mutate(year_id = case_when(
    year == 2009 ~ 1,
    year == 2010 ~ 2,
    year == 2011 ~ 3,
    year == 2012 ~ 4,
    year == 2013 ~ 5,
    year == 2014 ~ 6,
    year == 2015 ~ 7,
    year == 2016 ~ 8,
    year == 2017 ~ 9,
    year == 2018 ~ 10,
    year == 2019 ~ 11,
    year == 2020 ~ 12,
  ))

all_data <- wind %>%
  bind_cols("n_solar" = solar$n_solar,
            "solar_capacity" = solar$solar_capacity,
            "solar_mwh" = solar$solar_mwh)

font_add_google("DM Serif Display", "sd")
showtext_auto()

# Plots
 ps <-  ggplot(solar, aes(x = year, y = solar_capacity, color = solar_mwh)) +
  geom_point(shape = 21, size = 3.3, fill = "white", stroke = 2) +
  geom_point(shape = 8, size = 5, stroke = 2) +
  geom_jitter(alpha = 0.8) +
  scale_color_gradient(high = "firebrick", low = "#ffce00", name = " ") +
  transition_states(t)  
  anim_save("Day11_solar.gif", ps, renderer = gifski_renderer(), 
            width = 1536, height = 512, fps = 30)

p2 <-   ggplot(all_data) +
  geom_segment(aes(x = year_id * 300, y = 0, 
                   xend = year_id * 300, yend = wind_mwh * 10),
               stat = "unique", color = "#043c9a") +
  geom_spoke(aes(x = year_id * 300, y = wind_mwh*10, 
                 angle = -log(n_wind) * t + blade1, 
                 radius = wind_capacity), colour = "#043c9a", 
             stat = "unique") + 
  geom_spoke(aes(x = year_id * 300, y = wind_mwh*10, 
                 angle = -log(n_wind) * t + blade2, 
                 radius = wind_capacity), colour = "#043c9a", 
             stat = "unique") +
  geom_spoke(aes(x = year_id * 300, y = wind_mwh*10, 
                 angle = -log(n_wind) * t + blade3, 
                 radius = wind_capacity), colour = "#043c9a", 
             stat = "unique") +
  geom_text(aes(x = year_id * 300, y = -20, label = year), 
            stat = "unique", vjust = 1, lineheight = 0.9, 
            size = 4, family = "sd") +
  geom_point(aes(x = year_id * 300, y = solar_mwh * 10, 
                 color = solar_capacity), shape = 21, 
             size = 3.3, fill = "white", stroke = 2) +
  geom_point(aes(x = year_id * 300, y = solar_mwh * 10,
                 color = solar_capacity), shape = 8, 
             size = 5, stroke = 2) +
  scale_color_gradient(high = "firebrick", low = "#ffce00", name = " ") +
  coord_fixed(clip = "off") +
  labs(x = "",
       y = "Price ($/MWh)",
       title = "Wind and Solar Energy",
       subtitle = "\nThe height of the windmills represents the median wind projected\nprice (in $/MWh) and the radius shows the median wind projected\ncapacity (in Gigawatts).The rotational speed is proportional to the\nnumber of weeks of available data.The height of the suns represents\nthe median solar projected price (in $/MWh) and the  median solar\nprojected capacity (in Gigawatts) is represented with the colour.\nIn order to prevent overlapping the heights are scaled with the same\nproportion for both type of energy.",
      caption = "Data: Berkeley Lab | TidyTuesday 2022 - Week 18 -#30DayChartChallenge - Day #11 |Prepared by: @Cyd_yzc") +
     theme(
     text = element_text(family = "sd"),
     axis.ticks = element_blank(),
     panel.grid = element_blank(),
     axis.text.x = element_blank(),
     axis.text.y = element_text(size = 15, color = "black"), 
     plot.title.position = "plot",
     plot.title = element_text(size = 25, hjust = 0),
     plot.subtitle = element_text(size = 12, hjust = 0),
     plot.caption = element_text(size = 10), 
     plot.background = element_rect(fill = "#97F09E"),
     panel.background = element_rect(fill = "#97F09E"),
     legend.background = element_rect(fill = "#97F09E"),
     legend.position = c(0.9, 0.6),
     legend.key = element_rect(fill = "#97F09E", color = "#97F09E")) +
    guides(color = guide_legend(title = "Solar\nCapacity"),
    panel.border = element_rect(color = "#97F09E",
                                       fill = NA)) +
    transition_states(t)
  
# Save the Plots

animate(p2, res = 150)
anim_save("Day11.gif", p2, renderer = gifski_renderer())
  

  
  
