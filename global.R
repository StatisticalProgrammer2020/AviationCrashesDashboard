library(rsconnect) # connect to shinyapps
library(shinydashboard) # loads shiny dashboard packages
library(shinyWidgets) # loads widgets
library(tidyverse) # data-science functions
library(lubridate) # date functions
library(maps) # load maps
library(RColorBrewer) # color of maps
library(forcats) # map color-coding intervals
library(TTR) # for loading SMA function
library(readxl) # loading the excel file

crashes <- read_excel("Plane Crashes.xlsx") # loads the file

# imputes missing numeric values to zero
crashes_17_22 <- crashes %>% select(17:22) %>% mutate_all(~replace_na(.,0))
crashes[, 17:22] <- crashes_17_22

# creates a list of unique values for the dashboard inputs
unique_vals <- list(Region = sort(unique(crashes$Region)), 
                    Country = sort(unique(crashes$Country)), 
                    Operator = sort(unique(crashes$Operator)), 
                    Aircraft = sort(unique(crashes$Aircraft)), 
                    Registration = sort(unique(crashes$Registration)),
                    `Crash cause` = sort(unique(crashes$`Crash cause`)), 
                    `Crash site` = sort(unique(crashes$`Crash site`)), 
                    `Flight phase` = sort(unique(crashes$`Flight phase`)), 
                    `Flight type` = sort(unique(crashes$`Flight type`)), 
                    Survivors = sort(unique(crashes$Survivors)))

# loads the world map data
world_map <- map_data(map = "world")

# checks which countries in the loaded file are not in the world map dataset
missing_countries <- c()
for(country in sort(unique(crashes$Country))){
  if(!(country %in% world_map$region))
    missing_countries <- c(country, missing_countries)
}
missing_countries <- sort(missing_countries[!(is.na(missing_countries))])

# checks which countries in the world map have no matches with the one in the loaded file
unmatched_countries <- c()
for(country in unique(world_map$region)){
  if(!(country %in% sort(unique(crashes$Country))))
    unmatched_countries <- c(country, unmatched_countries)
}

sorted_missing_countries <- sort(missing_countries)
sorted_unmatched_countries <- sort(unmatched_countries)

# creates a vector of valid country names based on the world map dataset
sorted_filled_countries <- c("Virgin Islands",
                             "Cape Verde",
                             "Comoros",
                             "Republic of Congo",
                             "Democratic Republic of the Congo",
                             "Dutch Antilles",
                             "Micronesia",
                             "Fiji",
                             "French Guiana",
                             "Guam",
                             "Guinea-Bissau",
                             "Reunion",
                             "North Macedonia",
                             "Maldives",
                             "Saint Barthelemy",
                             "Saint Kitts",
                             "Saint Vincent",
                             "El Salvador",
                             "Samoa",
                             "Sao Tome and Principe",
                             "Timor-Leste",
                             "Trinidad",
                             "Tuvalu",
                             "UK",
                             "USA",
                             "Virgin Islands",
                             "World")

missing_countries_df <- data.frame(cbind(missing_countries, sorted_filled_countries))
colnames(missing_countries_df)[1] <- "id"

invalid_countries <- c()
for(country in sorted_filled_countries){
  if(!(country %in% sorted_unmatched_countries))
    invalid_countries <- c(country, invalid_countries)
}
