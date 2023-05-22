library(tidyverse)
library(readxl)
library(scales)
library(furrr)

################
# SEC 1 - DATA READ IN #
################

Kantar_2022 <- read_csv("data/Kantar_2022.csv")

Kantar_2021 <- read_csv("data/Kantar_2021.csv") %>% 
  mutate(`Total Calories` = str_remove_all(`Total Calories`,','),
         `Total Calories` = as.numeric(`Total Calories`))

Kantar_2020 <- read_csv("data/Kantar_2020.csv") %>% 
  mutate(`Total Calories` = str_remove_all(`Total Calories`,','),
         `Total Calories` = as.numeric(`Total Calories`))

Kantar_2019 <- read_csv("data/Kantar_2019.csv")

Kantar_2018 <- read_csv("data/Kantar_2018.csv")

Kantar_2017 <- read_csv("data/Kantar_2017.csv")

####################
# SEC 2 - EXTRACT DATA #
####################

consumption <- data.frame(
  year = c(2017,2018,2019,2020,2021,2022),
  total_kcal = c(sum(Kantar_2017$Calories_Sales,na.rm = T) * 1000,
                 sum(Kantar_2018$Calories_Sales,na.rm = T) * 1000,
                 sum(Kantar_2019$Calories_Sales,na.rm = T) * 1000,
                 sum(Kantar_2020$`Total Calories`,na.rm = T),
                 sum(Kantar_2021$`Total Calories`,na.rm = T),
                 sum(Kantar_2022$`Total Calories`,na.rm = T)),
  days = c(365,365,365,366,365,365),
  pop = c(64169395,65185724,65185724,65185724,65121729,65121729)
)

pp_daily_consumption <- consumption %>% 
  mutate(ppd = total_kcal/days/pop)

pp_daily_consumption %>% ggplot(aes(year,ppd)) +
  geom_line() +
  labs(title = 'Daily At Home Calorie Consumption',
       subtitle = 'All Ages',
       y = 'Kcal')
