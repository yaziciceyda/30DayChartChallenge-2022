library(tidyverse)
library(ggplot2)
library(dplyr)
library(showtext)
library(cowplot)

# Data Import

tuesdata <- tidytuesdayR::tt_load(2021, week = 14)

sephora <- tuesdata$sephora
ulta <- tuesdata$ulta
allShades <- tuesdata$allShades

# Data Wrangling

allShades2 <- allShades %>%
  filter(brand %in% c(
    "Anastasia Beverly Hills",
    "Charlotte Tilbury",
    "MAC",
    "NYX Professional Makeup",
    "SEPHORA COLLECTION")) 

medians_plot <- allShades2 %>%
  group_by(brand) %>%
  summarise(median_lightness = median(lightness)) %>%
  ungroup()
  
font_add_google(name = "Dancing Script",   
                family = "ds")
showtext_auto()

# Plot

p <- ggplot(allShades2, aes(brand, lightness, colour = hex)) + 
  geom_jitter(alpha = 0.8) +
  geom_vline(xintercept = 1.45, size = 2) +
  geom_vline(xintercept = 2.45, size = 2) +
  geom_vline(xintercept = 3.45, size = 2) +
  geom_vline(xintercept = 4.45, size = 2) +
  geom_vline(xintercept = 5.45, size = 20) +
  scale_colour_identity() +
  scale_shape_manual(values = "diamond") +
  scale_x_discrete(labels = c("Anastasia Beverly\n Hills",
                             "Charlotte Tilbury",
                             "MAC",
                             "NYX Professional\n Makeup",
                             "SEPHORA\n COLLECTION")) +
  labs(x = "",
       caption = "Data: The Pudding | TidyTuesday 2021 - Week 14 |#30DayChartChallenge - Day #7 |Prepared by: @Cyd_yzc") + 
       theme(text = element_text(family = "ds"),
       legend.position = "none",
       plot.caption = element_text(color = "#FDFBF4", size = 12),
       axis.ticks = element_blank(),
       axis.text = element_text(color = "#FDFBF4", size = 12),
       panel.background = element_rect(fill = "#FDFBF4"),
       plot.background = element_rect(fill = "black"),
       panel.grid = element_blank())
p

p2 <- ggdraw() +
  draw_image("https://www.maccosmetics.com/media/export/cms/products/640x600/mac_sku_SCC201_640x600_0.jpg",
             scale = 1.8, x = 0, y = 0.1, height = 0.8) +
  draw_plot(p, scale = 0.8, x =  0.12, y = 0, height = 0.9, width = 0.76)
p2

# Save the Plot
ggsave("Day7.png", p2, width = 20, height = 10, dpi = 72)  

