#CALCULATE NPM SCORE USING https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/216094/dh_123492.pdf

library(tidyverse)
library(readxl)
library(scales)
library(furrr)

Kantar_2022 <- read_csv("data/Kantar_2022.csv")

Kantar_2021 <- read_csv("data/Kantar_2021.csv")

Kantar_2020 <- read_csv("data/Kantar_2020.csv")

############
# JOIN FVN TO Kantar DATA  #
#############

# EXTRACT FVN
################

FVN_2021 <- Kantar_2021 %>%  
  select(CATEGORY,`FVN Score`) %>% 
  unique()

# JOIN FVN TO NEWER KANTAR DATA
#########################

Kantar_2022 <- left_join(Kantar_2022,
                         FVN_2021)

############
# APPLY NPM
##############
############

NPM_calc <- function(input_data, years){
  input_data %>% 
    filter(CATEGORY == 'Ambient Cakes+Pastries' &   
             SUB_CATEGORY != 'Other Dessert/Meringue'& 
             SUB_CATEGORY != 'Slices'& 
             SUB_CATEGORY != 'Trifle Sponges'|
           CATEGORY == 'Ambient Christmas Pudding'|
           CATEGORY == 'Morning Goods' & 
             SUB_CATEGORY %in% c('Bagels',
                                 'Croissants',
                                 'Crumpets/Pikelets',
                                 'Crusty', # review - bread excluded
                                 'Fruited Iced Buns',
                                 'Fruited Other Buns',
                                 'Hotcross Buns',
                                 'Lavash Bread',
                                 'Muffins', #revew - include english, not american
                                 'Pain Au Chocolate',
                                 'Pancakes/Griddle Scones',
                                 'Partbaked', #revew
                                 'Scones',
                                 'Soft', #revew
                                 'Teacakes',
                                 'Unfruited Iced Buns',
                                 'Unfruited Other Buns',
                                 'Waffles')|
           SECTOR == 'Biscuits' &  
             CATEGORY != 'Confect. & Other Exclusions'& 
             CATEGORY != 'Crackers & Crispbreads' & 
             CATEGORY != 'Savoury Biscuits'|
           CATEGORY == 'Ambient Vgtrn Products'| #revew
           CATEGORY == 'Canned Hot Meats' &  #revew
             SUB_CATEGORY %in% c('Canned Puddings',
                                 'Fillings',
                                 'Other Meat Products',
                                 'Ready Meals')|
           CATEGORY == 'Canned Pasta Products' &  #revew
             SUB_CATEGORY %in% c('Bolognese',
                                 'Macaroni',
                                 'Other Pasta',
                                 'Pasta Meal',
                                 'Ravioli',
                                 'Shaped Pasta',
                                 'Spaghetti Meals')|
           CATEGORY == 'Canned Rice Puddings'|
           CATEGORY == 'Fruit Filling'|
           CATEGORY == 'Tinned Sponge Puddings'|
           CATEGORY == 'Chilled Cakes' & 
             SUB_CATEGORY != 'Slices'& # review
             SUB_CATEGORY != 'Compote'& # review 
             SUB_CATEGORY != 'Dessert Sauce/Topping'&
             SUB_CATEGORY != 'Pavlovas/Meringues'& # review 
           CATEGORY == 'Chilled Desserts' & 
             SUB_CATEGORY != 'Terrine'| # review 
           CATEGORY == 'Chilled Pizza+Bases' & 
             SUB_CATEGORY != 'Chilled Bases'| 
           CATEGORY == 'Chilled Processed Poultry'|
           CATEGORY == 'Chilled Ready Meals' & 
             SUB_CATEGORY != 'Burger Bites'& # review
             SUB_CATEGORY != 'Hot Pies'&
             SUB_CATEGORY != 'Vegetable Accompaniments'| # review
           CATEGORY == 'Chilled Rice' & 
             SUB_CATEGORY != 'Savoury'| 
           CATEGORY == 'Chilled Vegetarian' &  
             SUB_CATEGORY %in% c('Kievs',
                                 'Other Meal Centres',
                                 'Vegetable Accompaniments')| #revew
           CATEGORY == 'Fresh Pasta' &  
             SUB_CATEGORY == 'Fresh Filled Pasta'|
           CATEGORY == 'Other Chilled Convenience' &  
             SUB_CATEGORY %in% c('Potato Skins',
                                 'Potato Wedges')| 
           CATEGORY == 'Chilled Flavoured Milk'|
           CATEGORY == 'Chilled Fruit Juice+Drink' & 
             SUB_CATEGORY != 'Chilled Pure Juice'|
           CATEGORY == 'Chilled One Shot Drinks'|
           CATEGORY == 'Fromage Frais'| #review
           CATEGORY == 'Yoghurt'| # REVIEW
           CATEGORY == 'Yoghurt Drinks And Juices'| # REVIEW
           CATEGORY == 'Chilled Prepared Fish' & 
             SUB_CATEGORY %in% c('Battered',
                                 'Breaded',
                                 'Calamari/Squid',
                                 'Fish Meals')|
           CATEGORY == 'Wet/Smoked Fish' & 
             SUB_CATEGORY %in% c('Calamari/Squid', # REVIEW
                                 'Breaded')|
           CATEGORY == 'Chilled Processed Poultry' & 
             SUB_CATEGORY %in% c('Bites', # review
                                 'Breasts+Fillets', # review
                                 'Burgers', # review
                                 'Childrens Shaped', # review
                                 'Drumstick/Thigh', # review
                                 'Goujons', # review
                                 'Non Barcoded Products', # review
                                 'Nuggets',
                                 'Portions',
                                 'Wings')| # review
           CATEGORY == 'Frozen Confectionery' & 
             SUB_CATEGORY != 'Danish Products'& # review
             SUB_CATEGORY != 'Frozen Fruit' & 
             SUB_CATEGORY != 'Frozen Pastry' & 
             SUB_CATEGORY != 'Other Bakery Products'| # review
           CATEGORY == 'Total Ice Cream'|
           CATEGORY == 'Frozen Fish' & 
             SUB_CATEGORY %in% c('Battered Fish',
                                 'Breaded Fillet',
                                 'Breaded Steaks',
                                 'Fish Cakes',
                                 'Fish Fingers',
                                 'Fish Ready/Main Meals',
                                 'Other Fish Products', # review
                                 'Speciality Fish')| # review
           CATEGORY == 'Frozen Flavoured Meats'|
           CATEGORY == 'Frozen Bread' & 
             SUB_CATEGORY %in% c('Croissants',
                                 'Pain Au Chocolate',
                                 'OPartbaked', # review
                                 'Soft')| # review
           CATEGORY == 'Frozen Meat Products' & 
             SUB_CATEGORY %in% c('Defined Choppd+Shapd Prds', # review
                                 'Defined Oth Mt/Pltry Prod', # review
                                 'Faggots')|
           CATEGORY == 'Frozen Pizzas'|
           CATEGORY == 'Frozen Potato Products'|
           CATEGORY == 'Frozen Processed Poultry' & 
             SUB_CATEGORY %in% c('Bites', # review
                                 'Breasts+Fillets', # review
                                 'Burgers', # review
                                 'Childrens Shaped', # review
                                 'Drumstick', # review
                                 'Drumstick/Thigh', # review
                                 'Goujons', # review
                                 'Grills', # review
                                 'Nuggets', 
                                 'Portions', # review
                                 'Steaks', # review
                                 'Thighs', # review
                                 'Wings')| # review
           CATEGORY == 'Frozen Ready Meals'|
           CATEGORY == 'Frozen Vegetarian Prods' & 
             SUB_CATEGORY %in% c('Miscellaneous Svry Prods', # review
                                  'Other Frozen Foods')| # review
           CATEGORY == 'Other Frozen Foods' &
             SUB_CATEGORY == 'Other Frozen Foods'| #REVIEW
           CATEGORY == 'Food Drinks' & 
             SUB_CATEGORY != 'Cocoa'& # review
             SUB_CATEGORY != 'Low Calorie Instant Choc'|
           CATEGORY == 'Breakfast Cereals'|
           CATEGORY == 'Toaster Pastries'|
           CATEGORY == 'Complete Dry/Ambient Mls' & 
             SUB_CATEGORY != 'Ambient Meals'|
           CATEGORY == 'Sweet+Savoury Mixes' & 
             SUB_CATEGORY %in% c('Biscuit',
                                 'Cake Mixes',
                                 'Crumble Mix',
                                 'Dessert Cakes')|
           CATEGORY == 'Ambient Sponge Puddings'|
           CATEGORY == 'Milkshake Mixes'|
           CATEGORY == 'Powd Desserts&Custard(B)' & 
             SUB_CATEGORY %in% c('Blancmanges',
                                 'Cold Instant Desserts',
                                 'Custard Powder',
                                 'Hot Instant Desserts',
                                 'Instant Custard',
                                 'Trifle')|
           CATEGORY == 'R.T.S. Custard'|
           CATEGORY == 'RTS Desserts Long Life'|
           CATEGORY == 'Table+Quick Set Jellies'|
           SECTOR == 'Take Home Confectionery'|
           SECTOR == 'Take Home Savouries' &
             CATEGORY != 'Nuts'|
           SECTOR == 'Take Home Soft Drinks' &
             SUB_CATEGORY != 'Ambient Pure Juice' &
             SUB_CATEGORY != 'Yoghurt Drinks And Juices' &
             SUB_CATEGORY != 'Low Calorie' &
             SUB_CATEGORY != 'Diet' &
             CATEGORY != 'Mineral Water' &
             CATEGORY != 'Soda Water') %>% 
    filter(is.na(`As Consumed`)) %>% # REMOVE PRODUCTS WITH AS CONSUMED NUTRITIONAL PRODUCTS
    filter(`Unit of Measure`!='Servings') %>%  # REMOVE PRODUCTS WITH  NUTRITIONAL INFORMATION PER SERVING
  mutate(energy_score = case_when(Kj_100 <= 335 ~ 0, # ENERGY SCORE
                                  Kj_100 > 335 & Kj_100 <= 670 ~ 1,
                                  Kj_100 > 670 & Kj_100 <= 1005 ~ 2,
                                  Kj_100 > 1005 & Kj_100 <= 1340 ~ 3,
                                  Kj_100 > 1340 & Kj_100 <= 1675 ~ 4,
                                  Kj_100 > 1675 & Kj_100 <= 2010 ~ 5,
                                  Kj_100 > 2010 & Kj_100 <= 2345 ~ 6,
                                  Kj_100 > 2345 & Kj_100 <= 2680 ~ 7,
                                  Kj_100 > 2680 & Kj_100 <= 3015 ~ 8,
                                  Kj_100 > 3015 & Kj_100 <= 3350 ~ 9,
                                  Kj_100 > 3350 ~ 10)) %>% 
  mutate(satfat_score = case_when(Saturates_100 <= 1 ~ 0,
                                  Saturates_100 > 1 & Saturates_100 <= 2 ~ 1,
                                  Saturates_100 > 2 & Saturates_100 <= 3 ~ 2,
                                  Saturates_100 > 3 & Saturates_100 <= 4 ~ 3,
                                  Saturates_100 > 4 & Saturates_100 <= 5 ~ 4,
                                  Saturates_100 > 5 & Saturates_100 <= 6 ~ 5,
                                  Saturates_100 > 6 & Saturates_100 <= 7 ~ 6,
                                  Saturates_100 > 7 & Saturates_100 <= 8 ~ 7,
                                  Saturates_100 > 8 & Saturates_100 <= 9 ~ 8,
                                  Saturates_100 > 9 & Saturates_100 <= 10 ~ 9,
                                  Saturates_100 > 10 ~ 10)) %>% 
  mutate(tsugar_score = case_when(Sugar_100 <= 4.5 ~ 0,
                                  Sugar_100 > 4.5 & Sugar_100 <= 9 ~ 1,
                                  Sugar_100 > 9 & Sugar_100 <= 13.5  ~ 2,
                                  Sugar_100 > 13.5 & Sugar_100 <= 18  ~ 3,
                                  Sugar_100 > 18& Sugar_100 <= 22.5 ~ 4,
                                  Sugar_100 > 22.5 & Sugar_100 <= 27 ~ 5,
                                  Sugar_100 > 27 & Sugar_100 <= 31 ~ 6,
                                  Sugar_100 > 31  & Sugar_100 <= 36 ~ 7,
                                  Sugar_100 > 36  & Sugar_100 <= 40 ~ 8,
                                  Sugar_100 > 40  & Sugar_100 <= 45 ~ 9,
                                  Sugar_100 > 45 ~ 10)) %>% 
  mutate(Sodium_100 = Sodium_100 * 1000,
         sodium_score = case_when(Sodium_100 <= 90 ~ 0, 
                                  Sodium_100 > 90 & Sodium_100 <= 180 ~ 1,
                                  Sodium_100 > 180 & Sodium_100 <= 270 ~ 2, 
                                  Sodium_100 > 270 & Sodium_100 <= 360 ~ 3, 
                                  Sodium_100 > 360 & Sodium_100 <= 450 ~ 4, 
                                  Sodium_100 > 450 & Sodium_100 <= 540 ~ 5, 
                                  Sodium_100 > 540 & Sodium_100 <= 630 ~ 6, 
                                  Sodium_100 > 630 & Sodium_100 <= 720 ~ 7, 
                                  Sodium_100 > 720 & Sodium_100 <= 810 ~ 8, 
                                  Sodium_100 > 810 & Sodium_100 <= 900 ~ 9, 
                                  Sodium_100 > 900 ~ 10)) %>% 
  mutate(protein_score = case_when(Protein_100 <= 1.6 ~ 0, 
                                   Protein_100 > 1.6 & Protein_100 <= 3.2 ~ 1, 
                                   Protein_100 > 3.2 & Protein_100 <= 4.8 ~ 2, 
                                   Protein_100 > 4.8 & Protein_100 <= 6.4 ~ 3, 
                                   Protein_100 > 6.4 & Protein_100 <= 8 ~ 4, 
                                   Protein_100 > 8 ~ 5)) %>% 
  mutate(fiber_score = case_when(Fibre_100 <= 0.9 ~ 0, 
                                 Fibre_100 > 0.9 & Fibre_100 <= 1.9 ~ 1, 
                                 Fibre_100 > 1.9 & Fibre_100 <= 2.8 ~ 2, 
                                 Fibre_100 > 2.8 & Fibre_100 <= 3.7 ~ 3, 
                                 Fibre_100 > 3.7 & Fibre_100 <= 4.7 ~ 4, 
                                 Fibre_100 > 4.7 ~ 5)) %>% 
  mutate(A_score = satfat_score + sodium_score + tsugar_score + energy_score) %>% 
  mutate(C_score = if_else(A_score < 11, # CALCULATE C SCORE APPLYING PROTIEN CAP
                           protein_score + fiber_score + `FVN Score`,
                           if_else(`FVN Score` == 5,
                                   protein_score + fiber_score + `FVN Score`,
                                   fiber_score + `FVN Score`))) %>% 
           mutate(NPM_score = A_score - C_score) %>% # CALCULATE NPM SCORE
  mutate(HFSS = if_else(SECTOR %in% c('Alcohol',
                                      'Chilled Drinks',
                                      'Hot Beverages',
                                      'Take Home Soft Drinks'),
                        if_else(NPM_score >= 1, 'yes', 'no'),
                        if_else(NPM_score >= 4, 'yes', 'no'))) %>% # DEFINE AAS HFSS/NON-HFSS
  filter(!is.na(HFSS)) %>% 
  mutate(year = years) %>% 
  select(year,HH_SALE_VOLUME,satfat_score,tsugar_score,sodium_score,energy_score,A_score,protein_score,fiber_score,`FVN Score`,C_score,NPM_score,HFSS)
}

datasets <- list(Kantar_2020,Kantar_2021,Kantar_2022)

years <- c(2020,2021,2022)

is_HFSS <- map2_dfr(datasets,years,NPM_calc)


#############################
### CALCULATE HFSS PERCENTAGE
#############################
############################

# USING SALES WEIGHTED AVERAGE (BY VOLUME SOLD)
#---------------

NPM <- is_HFSS %>% 
  group_by(year,HFSS) %>% 
  summarise(tot = sum(HH_SALE_VOLUME,na.rm = T), num = n()) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(p=tot/sum(tot,na.rm = T),
         num_prod = sum(num)) %>% 
  filter(HFSS == 'yes')

#write_csv(NPM,'results/NPM-leg categories.csv')

NPM %>% 
  filter(HFSS == 'yes') %>% 
  ggplot(aes(year,p,fill=HFSS)) +
  geom_line() +
  scale_y_continuous(limits = c(0,1),labels = percent) +
  labs(title = 'Percentage of GB grocary products classed as HFSS',
       subtitle = 'in legislative categories',
       caption = 'Source: Kantar sales weighted average',
       y = 'Percent')
