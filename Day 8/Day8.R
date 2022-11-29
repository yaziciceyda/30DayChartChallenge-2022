library(ggplot2) 
library(tidyverse) 
library(dplyr) 
library(ggridges)
library(showtext)
install.packages("remotes")
remotes::install_github("dill/emoGG")
library(emoGG)

# Data Import

tuesdata <- tidytuesdayR::tt_load(2020, week = 28)

data8 <- tuesdata$coffee_ratings
 table(data8$processing_method)

summary(data8$altitude_mean_meters)

data8_final <- data8 %>%
  drop_na(altitude_mean_meters,
          processing_method) %>%
  filter(altitude_mean_meters < 2000,
         unit_of_measurement == "m",
         bag_weight == "1 kg",
         !country_of_origin %in% c("Tanzania, United Republic Of",
                                   "Taiwan",
                                   "Nicaragua",
                                   "Kenya",
                                   "Japan",
                                   "El Salvador",
                                   "Cote d?Ivoire",
                                   "Brazil")) %>%
  mutate(hex_c = "#6F4E37")

font_add_google(name = "Arvo",   
                family = "ks")
showtext_auto()

fa <- fontawesome('fa-coffee')

# Plot

 p <- ggplot(data8_final, aes(y = country_of_origin)) +
   geom_density_ridges(aes(x = total_cup_points, fill = country_of_origin),   
      alpha = 0.9, color = "#6F4E37", from = 62, to = 90) +
   geom_text(aes(label = "United States", x = 65, y = 8.2), size = 7) +
   geom_text(aes(label = "Thailand", x = 65, y = 7.2), size = 7) +
   geom_text(aes(label = "Mexico", x = 65, y = 6.2), size = 7) +
   geom_text(aes(label = "Indonesia", x = 65, y = 5.2), size = 7) +
   geom_text(aes(label = "Honduras", x = 65, y = 4.2), size = 7) +
   geom_text(aes(label = "Guatemala", x = 65, y = 3.2), size = 7) +
   geom_text(aes(label = "Costa Rica", x = 65, y = 2.2), size = 7) +
   geom_text(aes(label = "Colombia", x = 65, y = 1.2), size = 7) +
   scale_fill_cyclical(values = c("#6F4E37")) +
   add_emoji(emoji = "2615") +
   coord_cartesian(clip = "off") +
   labs(x = "Total rating",
        y = "",
       title = "<span style='color:#6F4E37;'font-size:14pt>**Coffee Ratings**   
    <span>The US origin coffee got the highest ratings while Honduras and <span><br>Costa Rica got the minimum.</span>",
        caption = "\nData: Coffee Quality Institute | TidyTuesday 2020 - Week 28 - #30DayChartChallenge - Day #8 |Prepared by: @Cyd_yzc") +
         theme(
         text = element_text(family = "ks"),
         panel.background = element_rect(fill = "#EEA352"),
         panel.grid = element_blank(),
         axis.ticks = element_blank(),
         axis.text.y = element_blank(),
         axis.text.x = element_text(size = 15),
         axis.title.x = element_text(size = 15),
         plot.title = element_markdown(size = 30),
         plot.caption = element_text(size = 15))
 
 # Save the Plot
 
 ggsave("Day8.png", p, width = 20, height = 10, dpi = 72)
 
 