# this script creates a population pyramid
# from a life table
# shelby bachman, 2020

# references:
# https://www.r-bloggers.com/animated-population-pyramids-in-r/
# https://www.statology.org/how-to-create-a-population-pyramid-in-r/

# setup
rm(list = ls())
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(gganimate)

# load data and make sex a variable
data <- as.data.frame(rbind(
  read_xlsx('~/Downloads/Lambda_ColombiaLE.xlsx', sheet = 1) %>%
    mutate(sex = 'male'), 
  read_xlsx('~/Downloads/Lambda_ColombiaLE.xlsx', sheet = 2) %>%
    mutate(sex = 'female')))

# TBA: calculate total number in population
totalSurvived <- sum(data$l)

# TBA: calculate percentage of population for each age bracket
data <- data %>%
  mutate(percent_in_pop = l/totalSurvived)

#### create pyramid for a single year

# add a base (0) variable
data <- data %>%
  mutate(base = 0)

# modify percentage of population based on sex
data <- data %>%
  mutate(percent_in_pop_mod = ifelse(sex == 'female', percent_in_pop,
                                     ifelse(sex == 'male', percent_in_pop*(-1), NA)))

p <- ggplot(data = data, # when year is added, add %>% filter(year == 1990), 
            aes(x = age, 
                ymin = base, ymax = percent_in_pop_mod, 
                fill = sex))

pyr <- p + geom_ribbon(alpha = 0.5) +
  scale_y_continuous(labels = abs, limits = max(data$percent_in_pop)*c(-1, 1)) +
  scale_x_continuous(breaks = seq(0, 85, 5)) +
  scale_fill_aaas() +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Age", y = "Percent of Population",
       title = "",
       fill = "Sex") +
  theme_pubr() +
  theme(legend.position = 'right',
        axis.text.y = element_text(size = rel(0.9)),
        axis.text.x = element_text(size = rel(0.9))) +
  coord_flip()

pyr

#### create animated pyramid
p_animated <- ggplot(data = data,
            aes(x = age, 
                ymin = base, ymax = percent_in_pop_mod, 
                fill = sex,
                frame = year))

pyr_animated <- p_animated + geom_ribbon(alpha = 0.5) +
  scale_y_continuous(labels = abs, limits = max(data$percent_in_pop)*c(-1, 1)) +
  scale_x_continuous(breaks = seq(0, 85, 5)) +
  scale_fill_aaas() +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Age", y = "Percent of Population",
       title = "",
       fill = "Sex") +
  theme_pubr() +
  theme(legend.position = 'right',
        axis.text.y = element_text(size = rel(0.9)),
        axis.text.x = element_text(size = rel(0.9))) +
  coord_flip()

gganimate(pyr_animated, filename = "~/Downloads/population-pyramid.gif",
          ani.width = 1000, ani.height = 1600, ani.res = 200)

############ using functions in "create-population-pyramid.R"

create_population_pyramid(data, age_continuous = TRUE, age_bin_width = 5, animate = 0, year_to_plot = 2008)

############ another example

# load data from chile
data_female <- read.delim('~/Downloads/Chi_Popf.txt')
data_male <- read.delim('~/Downloads/Chi_Popm.txt')

# add variable indicating sex
data_female <- data_female %>%
  mutate(sex = 'female',
         country = 'chile')
data_male <- data_male %>%
  mutate(sex = 'male',
         country = 'chile')

# bind the dataframes onto one another
data <- rbind(data_female, data_male)

# filter out rows where Age is 999
data <- data %>%
  filter(Age != 999)

# filter out rows where Year is earlier than 1930
data <- data %>%
  filter(Year >= 1930)

# create age bracket labels
data <- data %>%
  mutate(Age = ifelse(Age == 1, 0, Age)) %>%
  mutate(AgeGroup = str_c(Age, '-', Age+4, sep=''))

data$AgeGroup[data$AgeGroup == '85-89'] <- '85+'

data$AgeGroup <- factor(data$AgeGroup, 
                        levels = c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29',
                                   '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', 
                                   '60-64', '65-69', '70-74', '75-79', '80-84', '85+'))

# compute total population by year
data_yearlypop <- data %>%
  group_by(Year) %>%
  summarize(total_pop = sum(Population, na.rm = TRUE))

# join total population column to original data
data <- left_join(data, data_yearlypop, by = 'Year')

# create new variable with percentage of population 
data <- data %>%
  mutate(fraction_pop = Population/total_pop,
         percent_in_pop = 100*fraction_pop) %>%
  rename(year = Year)

### create pyramid

create_population_pyramid(data, age_continuous = FALSE, animate = FALSE, year_to_plot = 1930)
pyr_chile <- create_population_pyramid(data, age_continuous = FALSE, animate = TRUE)
anim_save(filename = '~/Downloads/test.gif', animation = pyr_chile)


data <- data %>%
  mutate(base = 0) %>%
  mutate(percent_in_pop_mod = ifelse(sex == 'female', percent_in_pop,
                                     ifelse(sex == 'male', percent_in_pop*(-1), NA)))

p <- ggplot(data = data,
            aes(x = AgeGroup, 
                y = percent_in_pop_mod,
                fill = sex))

pyr <- p + geom_col(alpha = 0.5) +
  scale_y_continuous(labels = abs, limits = max(data$percent_in_pop)*c(-1, 1)) +
  scale_fill_aaas() +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = 'Age', y = 'Percent of population', fill = 'Sex') +
  theme_pubr() +
  theme(legend.position = 'right',
        axis.text.y = element_text(size = rel(0.9)),
        axis.text.x = element_text(size = rel(0.9))) +
  coord_flip()

pyr + transition_time(year) +
  labs(title = 'Year: {frame_time}', x = 'Age group', y = 'Percent of population')

### next attempt
#https://ourworldindata.org/grapher/historic-and-un-pop-projections-by-age
