library(ggplot2)
library(readxl)
library(tidyverse)
library(showtext)
library(ggtext)

# Data Import

youth_data <- read_xlsx("Data_Day1.xlsx", sheet = "Data_Youth")

# Data Wrangling

youth_data <- youth_data %>%
  mutate(Percent_non = 100 - Percent) %>%
  pivot_longer(!Country, names_to = "Youth_Level", values_to = "Percent")

font_add_google("DM Serif Display", "sd")
showtext_auto()

hsize = 2

#Plot

p <- ggplot(youth_data, aes(x = hsize, y = Percent, 
                            color = Youth_Level, fill = Youth_Level)) +
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5))+
  facet_wrap(~ Country) +
  geom_col(color = "white")+
  scale_fill_manual(values = c("#E072A7", "#EE9D1A"),
                    labels = c("Living with\ntheir parents",
                     "Not living with\ntheir parents"))+
  geom_text(aes(label = paste0(Percent,"%")), size = 4,
            colour = "black", family = "sd",
            position = position_stack(vjust = 0.5)) +
  labs(title = "\n\nPercentage of Young Adults living with their Parents\n",
       subtitle = "<span style='color:#553746'>Croatia has the highest percent of <span style= 'color:#E072A7'> young adults living with<br> their parents,<span style='color:#553746'> while Denmark
       has the highest<br> percentage of them <span style='color:#EE9D1A'> not living with their parents.<span style='color:#C5FD05'> </span>",
       caption = "Data: Eurostat |#30DayChartChallenge - Day #1 |Prepared by: @Cyd_yzc") +
  theme(
    panel.border = element_rect(colour = "#B2E1DB", fill = NA, size = 3),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(), 
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-1, -0.2, -1, -0.2),"cm"),
    plot.background = element_rect(fill = "#B2E1DB"),
    panel.background = element_rect(fill = "#B2E1DB"),
    plot.subtitle = element_markdown(family = "sd", size = 18),
    plot.title = element_text(family = "sd", size = 30),
    plot.caption = element_text(family = "sd", 
                                hjust = 4, vjust = 8, size = 14),
    strip.background = element_rect(fill = "#B2E1DB"),
    strip.text = element_text(size = 14, family = "sd"),
    legend.background = element_rect(fill = "#B2E1DB"),
    legend.title = element_blank(),
    legend.text = element_text(family = "sd", size = 13, hjust = 0))


p
#Save the Plot
dev.off()
ggsave("Day1_revised.png", p, width = 15, height = 14, dpi = 72)  


