library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(showtext)

dt <- read.csv("Day6.csv", sep = ";")
head(dt)
dt_new <- dt %>%
  select(c(-2, -6)) %>%
  mutate(Total_Products = VegetalProducts + AnimalProducts) %>%
  rename("Plant protein" = "VegetalProducts",
         "Animal protein" = "AnimalProducts") 
  
annual_protein <- dt %>%
  arrange(Year) %>%
  group_by(Year) %>%
  summarise(Avg_Animal = mean(AnimalProducts),
            Avg_Vegetal = mean(VegetalProducts),
            Total_Protein = Avg_Animal + Avg_Vegetal) %>%
  ungroup()

med_countries <- dt_new %>%
  filter(Year == 2017,
         Entity %in% c("Turkey",
                       "Spain",
                       "Italy",
                       "Greece")) 

med_countries_2017 <- med_countries %>%
  select(-c(Year)) %>%
  pivot_longer(-Entity, names_to = "Type_Protein",
               values_to = "Protein") 
  
  x <-  tibble(Entity = "World",
               Type_Protein = c("Animal protein", "Plant protein", "Total_Products"),
               Protein = c(36.88372,
                           44.37895,
                           81.26267))

med_countries_2017 <- med_countries_2017 %>%
  rbind(x)

font_add_google(name = "Libre Franklin",   
                family = "lf")
showtext_auto()

p <- ggplot(subset(med_countries_2017, Type_Protein %in% c("Plant protein",
                                                           "Animal protein")), 
            aes(x = reorder(Entity, Protein), y = Protein, 
                fill = factor(Type_Protein, levels = c("Plant protein",
                                                       "Animal protein")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  geom_text(aes(label = paste0(round(Protein), " g")), position = position_stack(vjust = 0.5), size = 8) +
  geom_text(subset(med_countries_2017, Type_Protein %in% "Total_Products") ,
            mapping = aes(x = Entity, y = Protein,
                          label = paste0(round(Protein), " g"), ),
            position = position_stack(vjust = 1.05), size = 8) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                     labels = unit_format(unit = "g")) +
  scale_fill_manual(values = c("Animal protein" = "#E24B66", "Plant protein" = "#CF882E"))  +
  labs(x = "",
       y = "",
       fill = "",
       title = "Daily protein supply from animal and plant-based foods, 2017",
       subtitle = "\nDaily per capita protein supply is measured in grams per person per day. Protein\n of animal origin includes protein from all meat commodities, eggs and dairy products,\n and fish & seafood.\n",
       caption = "Data Source: Our World In Data |#30DayChartChallenge - Day #6 |Prepared by: @Cyd_yzc") +
  coord_flip() +
  theme(
    text = element_text(family = "lf"),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 18),
    plot.title = element_text(size = 35),
    plot.subtitle = element_text(size = 25, hjust = 0),
    plot.caption = element_text(size = 10),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15, hjust = 1),
    panel.grid.major.x = element_line(color = "grey80", linetype = 2),
    panel.background = element_rect(fill = "white"),
  )
ggsave("Day6.png", p, width = 20, height = 10, dpi = 72)  
