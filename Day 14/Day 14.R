remotes::install_github("tylermorganwall/rayshader")
library(rayshader)
library(ggplot2)
library(tidyverse)
library(psych)

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


p1 <- ggplot(dt2, aes(x = GDP, y = Phones, color = Birthrate)) +
  geom_point() +
  scale_colour_distiller(name = "Birthrate",
                     palette = "Reds", limits = c(7, 51)) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(title = "How does the number of phones (per\n1000 people) change according to GDP and Birthrate?",
       caption = "Data: Kaggle |#30DayChartChallenge - Day #14 |Prepared by: @Cyd_yzc") +
  theme(text = element_text(family = "sd"),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30, hjust = 0),
        plot.subtitle = element_text(size = 15, hjust = 0),
        plot.caption = element_text(size = 10, hjust = 1))

plot_gg(p1, width=3.5, multicore = TRUE, windowsize = c(1400,866), sunangle=225,
        zoom = 0.60, phi = 30, theta = 45)
render_snapshot("Day14_revised.png", clear = TRUE)
