library(readxl)
library(tidyverse)
library(ggplot2)
library(ggimage)
library(magick)
library(showtext)
library(forcats)
library(cowplot)

# Data Import

olive <- read_xlsx("Olive.xlsx", sheet = "2020_Olive")

# Data Wrangling

olive2020 <- olive %>%
  filter(Country %in% c("Greece",
                        "Spain",
                        "Italy",
                        "Turkey")) %>%
  mutate(Country = as.factor(Country),
         Production = as.numeric(Olive_P)) %>%
  select(-Olive_P)

font_add_google(name = "Dancing Script",   
                family = "ds")
showtext_auto()

p <- ggplot(olive2020, aes(x = fct_reorder(Country, Production, 
                                       .desc = TRUE), 
                       y = Production,
                       fill = Country)) + 
 geom_bar(stat = "identity", width = 0.3) +
 scale_fill_manual(values = c("Italy" = "#8db600", 
                               "Greece" = "#808000", 
                               "Spain" = "#708238",
                               "Turkey" = "#608000")) +
labs(title = "The Amount of Olive Production (tonnes) \n for some Mediterranean Countries\nin 2020",
       x = "",
       y = "",
       caption = "Data Source: Eurostat |#30DayChartChallenge - Day #4 |Prepared by: @Cyd_yzc") +
   theme(text = element_text(family = "ds"),
         plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         plot.title = element_text(color = "#608000", hjust = 0.5, size = 42),
         plot.caption = element_text(size = 23, hjust = 1),
         panel.border = element_blank(),
         axis.line = element_blank(),
         axis.ticks = element_blank(), 
         axis.text.x =  element_text(size = 25),
         axis.text.y =  element_text(size = 25),
         legend.position = "none",
         plot.margin = unit(c(1,3,1,3), "cm"))
p
  
p2 <- ggdraw() +
  draw_plot(p) +
    draw_image("olive.png",
              scale = 0.5, x = 0.33, y = 0.20, height = 0.8) +
  annotate("text", x = 0.1, y = 0.79, label = "Production",
           family = "ds", size = 10)
    
p2

  ggsave("Day4_revised.png", p2, width = 20, height = 10, dpi = 72) 
 
  

  

       
       
       