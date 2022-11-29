library(tidyverse)
library(ggplot2)
library(scales)
library(showtext)
library(ggtext)

# Data Import

tuesdata <- tidytuesdayR::tt_load('2022-03-22')
babynames <- tuesdata$babynames

# Data Wrangling

day9 <- babynames %>%
  mutate(name = str_to_lower(name)) %>%
      mutate(  decade  = case_when(
      year >= 1880 & year < 1926 ~ "Period1",
      year >= 1926 & year < 1971 ~ "Period2",
      year >= 1971 & year < 2018 ~ "Period3"
  ))

first_period <- day9 %>%
  filter(decade == "Period1")

second_period <- day9 %>%
  filter(decade == "Period2")

character_function <- function(input_name)
{
  character_vector <- c()
  for (i in 1:length(input_name))
    {
    ch <- strsplit(input_name[i], "")
    character_vector <- c(character_vector, ch[[1]])
    }
  return(character_vector)
}

first_ch_group <- character_function(first_period$name)
second_ch_group <- character_function(second_period$name)

final_ch_group <- as_tibble(table(first_ch_group))
final_ch_group <- final_ch_group %>% 
  cbind(period = "1880 - 1925") %>%
  as_tibble() %>%
  rename("Letter"= first_ch_group)

final_ch_group2 <- as_tibble(table(second_ch_group))
final_ch_group2 <- final_ch_group2 %>% 
  cbind(period = "1926- 1971") %>%
  as_tibble() %>%
  rename("Letter"= second_ch_group) 

final_data <- rbind(final_ch_group, final_ch_group2)

font_add_google("Poppins", "p")
showtext_auto()

# Plot

p <- ggplot(final_data, aes(x = Letter, y = 1, shape = Letter, size = n)) +
  geom_point(color = "white") +
  facet_grid(period~.) +
  scale_shape_manual(values=c("a" = 97, "b" = 98, "c" = 99, 
                              "d" = 100, "e" = 101,
                              "f" = 102, "g" = 103, 
                              "h" = 104, "i" = 105, 
                              "j" = 106, "k" = 107, 
                              "l" = 108, "m" = 109, 
                              "n" = 110, "o" = 111, 
                              "p" = 112, "q" = 113, 
                              "r" = 114, "s" = 115, 
                              "t" = 116, "u" = 117, 
                              "v" = 118, "w" = 119, 
                              "x" = 120, "y" = 121, 
                              "z" = 122)) +
  scale_size_area(max_size = 20, labels = comma) +
 # scale_size_continuous(labels = comma) +
  coord_cartesian() +
  labs(x = "",
       y = "",
       title = "Letters in the Babynames", 
       subtitle = "<span style='color:white;'font-size:14pt>   
    <span>The frequency of the letters especially vowels (a, e, i, o, u) show similar pattern for<span><br> the two time periods (1880 - 1925 and 1926 - 1971).</span>",
       caption = "Data from {babynames} R package | TidyTuesday 2022 - Week 12 - #30DayChartChallenge - Day #9 |Prepared by: @Cyd_yzc") +
        theme(text = element_text(family = "p"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.margin = unit(c(2,1,2,1), "cm"), 
        plot.title = element_text(color = "white", hjust = 0.5, size = 29),
        plot.subtitle = element_markdown(color = "white", 
                                         hjust = 0.5, size = 18),
        plot.caption = element_text(color = "white", hjust = 1, size = 12),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.key = element_rect(fill = "black", color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(color = "white"), 
        legend.background = element_rect(fill = "black", color = "black"),
        legend.key.width = unit(3,"cm"),
        strip.background = element_rect(fill = "black"),
        strip.text.y = element_text(color = "white", size = 18)) +
        guides(shape = F, size = guide_legend(nrow = 1, 
                                              label.position = "bottom")) 
  
# Save the Plot
                                        
ggsave("Day9.png", p, width = 20, height = 10, dpi = 72)

