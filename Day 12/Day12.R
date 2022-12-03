library(readr)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(cowplot)
library(ggimage)
library(rasterImage)
library(png)
library(grid)

# Data Import

  dt <- read.csv("HIGH_AGLINK_2021_12052022194238126.csv")
head(dt)

# Data Wrangling

dt <- dt %>%
  filter(Country %in% c("Austria",
                        "Australia", "Belgium", "Canada", "Chile", "Colombia",
                        "Costa Rica", "Czech Republic", "Denmark", "Estonia", 
                        "Finland", "France", "Germany", "Greece", "Hungary", 
                        "Iceland", "Ireland", "Israel", "Italy", "Japan", 
                        "Korea", "Latvia", "Lithuania", "Luxembourg",
                        "Mexico", "Netherlands", "New Zealand", "Norway",
                        "Poland", "Portugal", "Slovak Republic", "Slovenia",
                        "Spain", "Sweden", "Switzerland", "Turkey",
                        "United Kingdom", "United States", "European Union"),
         Variable == "Production",
         Value != 0,
         Time == 2022,
         Commodity == "Wheat") %>%
  mutate(Percent = Value / sum(Value) * 100) %>%
  arrange(desc(Percent)) %>%
  filter(Percent > 2) %>%
  mutate(Cum_Sum = cumsum(Percent)) 

img <- readPNG("wheat.png")
dt$image <- "wheat.png"

# Plot 

p <- ggplot(dt) +
  annotation_custom(rasterGrob(img, 
                               width = unit(1,"npc"),
                               height = unit(1,"npc")), 
                   xmin = 0, xmax = 100, ymin = 0, ymax = 105) +
 geom_text(dt, mapping = aes(x = 9, y = Cum_Sum + 2.1, label = Country),
           size = 13, colour = "dark blue") +
  geom_hline(yintercept = dt$Cum_Sum, linetype = 1, 
             colour = "light blue", size = 3) +
  coord_fixed(ylim = c(0, 100), xlim = c(0, 90)) +
  labs(x = "",
       y = "% of Wheat Production",
       title = "Wheat Production of\nOECD Countries (projection of 2022)",
       subtitle = "\nCountries whose wheat production is greater than 15K (tonnes) has\nan average production of 45K",
       caption = "Data: OECD |#30DayChartChallenge - Day #12 |Prepared by: @Cyd_yzc") +
 theme_economist() +
  theme(text = element_text(family = "sans"),
        plot.background = element_rect(fill = "white"),
           axis.ticks.x = element_blank(),
        plot.title = element_text(size = 48, hjust = 0.5,
                                  margin = margin(30,0,0,0)),
        plot.subtitle = element_text(size = 38, hjust = 0),
        axis.text.x = element_blank(),
        plot.caption = element_text(size = 32, hjust = 1),
        axis.line.x = element_blank(),
        axis.text.y = element_text(hjust = 1, size = 35),
        axis.title.y = element_text(size = 39)
) +
  scale_size_area(max_size = 30)
        
# red box
rect <- rectGrob(
  x = unit(2.8, "in"),
  y = unit(1, "npc"),
  width = unit(2.1, "in"),
  height = unit(0.5, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(col = "#E3120B", fill = "#E3120B", alpha = 1)
)
p2 <- ggdraw(p) +
  draw_grob(rect) 

# Save the Plot

ggsave("Day12.png", p2, width = 20, height = 20, dpi = 72)  

