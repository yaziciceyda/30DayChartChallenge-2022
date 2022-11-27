# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Data Import

tuesdata <- tidytuesdayR::tt_load(2021, week = 51)

studio_album_tracks <- tuesdata$studio_album_tracks

library(tidyverse)
library(ggplot2)
devtools::install_github("coolbutuseless/ggblur")
library(ggblur)
library(ggforce)
remotes::install_github("coolbutuseless/ggecho")
library(ggecho)
remotes::install_github("johnmackintosh/popthemes")
library(popthemes)
library(geomtextpath)
library(showtext)
library(ggtext)

# Data Wrangling

day5 <- studio_album_tracks %>%
  select(album_id, album_release_year, danceability,
         energy, key, loudness, speechiness,
         acousticness, instrumentalness, 
         liveness, valence, tempo) %>%
  group_by(album_release_year) %>%
  summarise(Danceability = mean(danceability),
            Energy = mean(energy),
            Speechiness = mean(speechiness),
            Acousticness = mean(acousticness),
            Liveness = mean(liveness),
            Valence = mean(valence)
            ) %>%
  pivot_longer(!album_release_year, names_to = "Category", values_to = "Values") %>%
  mutate(label = case_when(
   album_release_year == 1996 ~ "Spice",
   album_release_year == 1997 ~ "Spiceworld",
    album_release_year == 2000 ~ "Forever"
  )) %>%
  ungroup()


font_add_google(name = "Permanent Marker",   
                family = "pm")
showtext_auto()

# Plot 

p <- ggplot(data = day5, aes(x = album_release_year, y = Values,
                        group = Category, color = Category,
                        label = label)) +
  geom_line(size = 2) +
  geom_point_blur(blur_steps = 150, size = 3, aes(alpha = Values)) +
  geom_text(day5, mapping = aes(y = 0.95, label = label), size = 6,
            family = "pm", nudge_x = 0, hjust = 0.5) +
  # scale_color_spice() +
   coord_cartesian() + 
   scale_x_continuous(breaks = c(1996, 1997, 2000)) +
    labs(x = "",
       y = "", 
       title = "Average Audio Features of Spice Girls' Albums",
       subtitle = "<br> <span style='color:#00BFC4'>Liveness, 
                   <span style='color:#619CFF'>Speechiness <span style='color:white'>and 
                   <span style='color:#F8766D'>Acousticness <span style='color:white'>of the three albums are low
                   <span style='color:white'>but the <span style='color:#00BA38'>Energy, 
                   <span style='color:#F564E3'>Valence <br> <span style='color:white'>and 
                   <span style='color:#B79F00'>Danceability <span style='color:white'> are higher
                   <span style='color:white'>in all three albums. Moreover, there is a decrease in <br> the valence and danceability,  but all others increased from Spice to Spiceworld.",
       
       caption = "Data: Spotify | TidyTuesday 2021 - Week 51\n |#30DayChartChallenge - Day #5 |Prepared by: @Cyd_yzc") +
  theme(
    text = element_text(family = "pm"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    plot.title = element_text(size = 40, color = "white",
                                hjust = 0.5),
    plot.subtitle = element_markdown(size = 20, 
                                  hjust = 0),
    plot.caption = element_text(size = 20, color = "white",
                                hjust = 1),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white", size = 15),
    plot.margin = unit(c(1, 3, 1, 3), "cm"))

# Save the plot
ggsave("Day5.png", p, width = 20, height = 10, dpi = 72)  



