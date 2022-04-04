library(readxl)
library(tidyverse)
library(ggplot2)
library(ggimage)
library(magick)
library(showtext)

olive <- read_xlsx("Olive.xlsx", sheet = "2020_Olive")

olive2020 <- olive %>%
  filter(Country %in% c("Greece",
                        "Spain",
                        "Italy",
                        "Turkey")) %>%
  mutate(Country = as.factor(Country))

olive <- olive2020

font_add_google(name = "Dancing Script",   
                family = "ds")
showtext_auto()
  
p <- ggplot(olive, aes(x = fct_reorder(Country, Production, 
                                       .desc = TRUE), y = Production,
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
         axis.ticks = element_blank(), 
         plot.title = element_text(color = "#608000", hjust = 0.5, size = 42),
         plot.caption = element_text(size = 23, hjust = 1),
         panel.border = element_blank(),
         axis.line = element_blank(),
         axis.text.x =  element_text(size = 30),
         axis.title.y = element_text(size = 30),
         axis.text.y =  element_text(size = 30),
         legend.position = "none",
         plot.margin = unit(c(1,3,1,3), "cm"))
  
p2 <- ggdraw() +
    draw_image("https://st4.depositphotos.com/2881739/31353/v/380/depositphotos_313535930-stock-illustration-watercolor-vector-wreath-of-olive.jpg?forcejpeg=true",
              scale = 0.7, x = 0.33, y = 0.27, height = 0.8) +
    draw_plot(p)

  ggsave("Day4.png", p2, width = 20, height = 10, dpi = 72)  
  

  
  
       
       
       