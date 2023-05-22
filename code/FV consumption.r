
library(tidyverse)
library(readxl)
library(scales)
library(furrr)

Kantar_2022 <- read_csv("data/Kantar_2022.csv")

Kantar_2021 <- read_csv("data/Kantar_2021.csv")

Kantar_2020 <- read_csv("data/Kantar_2020.csv")

Kantar_2019 <- read_csv("data/Kantar_2019.csv")

Kantar_2018 <- read_csv("data/Kantar_2018.csv")

Kantar_2017 <- read_csv("data/Kantar_2017.csv")

############
# CALCULATE f&v PERCENTAGE
##############
############

NPM_calc <- function(input_data, years){
  input_data %>% 
  group_by(SECTOR) %>% 
  summarise(sales = sum(HH_SALE_VOLUME,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(tot = sum(sales)) %>% 
  mutate(percent = sales/tot) %>% 
mutate(year = years)
}

NPM_calc(Kantar_2021,2021)

datasets <- list(Kantar_2020,Kantar_2021,Kantar_2022)

years <- c(2020,2021,2022)

FandV <- map2_dfr(datasets,years,NPM_calc)

FandV %>% 
  filter(SECTOR=="Fruit+Veg+Salads") %>% 
  ggplot(aes(year,percent))+
  geom_line() +
  scale_y_continuous(limits = c(0,0.2),labels = percent) +
  labs(title = '% of Fruit and Veg Sale',
       subtitle = 'of the whole food and drink market')

FandV %>% 
  filter(SECTOR=="Fruit+Veg+Salads") %>% 
  ggplot(aes(year,sales))+
  geom_line() +
  labs(title = 'Fruit and Veg Sale')
