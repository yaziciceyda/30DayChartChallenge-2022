tuesdata <- tidytuesdayR::tt_load('2020-03-10')

library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(maps)
library(stringr)

tuition_cost <- tuesdata$tuition_cost
tuition_income <- tuesdata$tuition_income
salary_potential <- tuesdata$salary_potential
historical_tuition <- tuesdata$historical_tuition
diversity_school <- tuesdata$diversity_school

salary2 <- salary_potential %>%
  drop_na() %>%
  mutate(state_name = str_to_lower(state_name)) %>%
  group_by(state_name) %>%
  summarise(corr1 = corr.test(make_world_better_percent, stem_percent)$r) %>%
  arrange(corr1)
  
MainStates <- map_data("state") %>%
  rename(state_name = region)

MergedStates <- inner_join(MainStates, salary2, by = "state_name")

ggplot(salary_potential, aes(x = early_career_pay, y = stem_percent)) +
  geom_point() +
  geom_jitter(alpha = 0.2)

ggplot(MergedStates) +
 geom_polygon(data=MergedStates, 
                       aes(x=long, y=lat, group=group, fill = corr1), 
                       color="white", size = 0.2) +
  scale_fill_continuous(name="Population(millions)", 
                        low = "lightgreen", high = "darkgreen",limits = c(-1, 1), 
                        breaks=c(5,10,15,20,25,30,35), na.value = "grey50")



cyd <- corr.test(salary_potential$early_career_pay,  salary_potential$stem_percent)
