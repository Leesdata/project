#########################################
#R project - scraping table
#########################################

#setwd("C:/Users/LEE KO/Desktop/DPP2_R/final project")
library(tidyverse)
library(tidytext)
library(tidyr)
library(stringr)
library(rvest)


#####################
#1. State energy table
#####################
url_1 <- "https://neo.ne.gov/programs/stats/inf/120.htm"
request <- read_html(url_1)

#table: state energy consumption by state
table_st_energy <- html_table(request, fill = TRUE)

View(table_st_energy[[1]])

#extract table
table_1 <- table_st_energy[[1]]

#remove the total row
table_1 <- table_1 %>% filter(State != "United States Total")

table_1

write.csv(table_1, "data/energy_consumption_by_state.csv", row.names=FALSE)


#####################
#2. State - Division match table
#####################

url_2 <- "https://www.eia.gov/consumption/residential/terminology.php"
request_2 <- read_html(url_2)

#able_html <- html_nodes(request_2, ".zebra , td, th")
#table_content <- html_text(table_html)

#request_2 %>% html_elements(".zebra , td, th")

#table 2: 
table_st_division <- html_table(request_2, fill = TRUE)

View(table_st_division[[1]])

#extract table
table_2 <- table_st_division[[1]]

table_2

#fill
#https://dplyr.tidyverse.org/reference/na_if.html
table_2 <-table_2 %>% 
  mutate(Region = na_if(Region, "")) %>%
  fill(Region)

table_2 <- table_2 %>% head (-1)

#Separate rows
#https://stackoverflow.com/questions/44922612/split-strings-into-values-in-long-dataframe-format
table_2 <- table_2 %>%
  separate_rows(States, sep=",") %>%
  mutate(Division = str_replace(Division, "\\*", "")) %>%
  mutate(States = str_replace(States, " and", "")) %>%
  mutate(States = str_replace(States, "the", "")) %>%
  mutate(States = str_trim(States)) %>%
  mutate (Division_num = case_when(
    Division == "New England" ~ 1,
    Division == "Middle Atlantic" ~ 2,
    Division == "East North Central" ~3,
    Division == "West North Central" ~ 4,
    Division == "South Atlantic" ~ 5,
    Division == "East South Central" ~6,
    Division == "West South Central" ~7,
    Division == "Mountain" ~ 8,
    Division == "Pacific" ~9
    
  ))


#Add division numbers to align with AHS data coding
table_2 




write.csv(table_2, "data/state_division_match.csv", row.names=FALSE)
