
library(rsconnect)
# install.packages("shinyWidgets")
#pacman::p_load("openxlsx", "rJava", "xlsx", "xlsxjars", "zip")
# rsconnect::setAccountInfo(name='abeceasarperez', token='737D7AF42F6023A8308E278EF49E4F6B', secret='FWYCj6Vcwvm7phapZQssQXl/FLiwc3gq+5ctQC6q')
#rsconnect::deployApp("C:\\Users\\Abe\\Desktop\\Dataquest modules\\Datasets for Practice + Codes Used for Analysis\\aircraft_incidents")


library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(maps)
library(RColorBrewer)
library(forcats)
library(gridExtra)
library(TTR)
library(readxl)
library(shiny)

crashes <- read_excel("Plane Crashes.xlsx")
crashes_17_22 <- crashes %>% select(17:22) %>% mutate_all(~replace_na(.,0))
crashes[, 17:22] <- crashes_17_22

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

world_map <- map_data(map = "world")


missing_countries <- c()
for(country in sort(unique(crashes$Country))){
  if(!(country %in% world_map$region))
    missing_countries <- c(country, missing_countries)
}

missing_countries <- sort(missing_countries[!(is.na(missing_countries))])

unmatched_countries <- c()
for(country in unique(world_map$region)){
  if(!(country %in% sort(unique(crashes$Country))))
    unmatched_countries <- c(country, unmatched_countries)
}

sorted_missing_countries <- sort(missing_countries)
sorted_unmatched_countries <- sort(unmatched_countries)

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
