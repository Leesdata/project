############################################################
#American Housing Survey (AHS) - analysis and creating map
############################################################


#large file ('ahs2015n.csv')
#downloadable from
#https://drive.google.com/file/d/1dzgglr8T3qz56GYg0_ryz2AmhUpVQGuv/view?usp=share_link
#save the file into the 'data' folder




rm(list=ls())
#.rs.restartR()
library(tidyverse)
library(readr)
library(data.table)
library(ggplot2)
library(fauxnaif) #https://cran.r-project.org/web/packages/fauxnaif/readme/README.html
library(sf)
library(spData)
library(scales)

#setwd("C:/Users/LEE KO/Desktop/DPP2_R/final project/AHS data")
setwd("~/GitHub/final-project-xiaoqi-ellie-lee")

      
###########################
#1. Cleaning data
###########################

columns_to_use = c("CONTROL",
                   "OMB13CBSA", "BLD", "DIVISION",
                   "YRBUILT", 
                   "TOTROOMS",
                   "NUMPEOPLE",
                   "FINCP", "HINCP",
                   "ELECAMT","GASAMT", "OILAMT", "OTHERAMT",
                   "HMRENEFF",
                   "HEATFUEL", "HEATTYPE", "ACPRIMARY"
)


#large file downloaded from google drive & saved into 'data' folder
#https://drive.google.com/file/d/1dzgglr8T3qz56GYg0_ryz2AmhUpVQGuv/view?usp=share_link
#supposing that the file is in the 'data' folder
ahs <- fread(file.path("data/ahs2015n.csv"), 
             select = columns_to_use)


#cleaning
#mutate_at or lapply with sub
#https://stackoverflow.com/questions/56963214/how-can-i-use-gsub-in-multiple-specific-column-in-r
#For convenience, turn all values into numeric

ahs_cleaned <- ahs %>%
  mutate_at(vars(columns_to_use), ~ str_replace_all(., "'", "")) %>%
  mutate_all(as.numeric)

#Replace with NA
#M or 9: not reported (M is not showing here)
#N or 6: not applicable (N is not showing here)
ahs_cleaned <- ahs_cleaned %>%
  mutate(across(where(is.numeric), ~ na_if_in(., c(-6, -9))))

ahs_cleaned <- ahs_cleaned %>%
  mutate_at(vars(c("HEATFUEL", "HEATTYPE", "ACPRIMARY", "DIVISION")), as.factor) 

ahs_cleaned %>% head(5)



###########################
#2. Drawing division map
###########################

path <- "C:/Users/LEE KO/Documents/GitHub/final-project-xiaoqi-ellie-lee"

us_division <- st_read(file.path(path, "data/cb_2018_us_division_500k/cb_2018_us_division_500k.shp"))

us_state <- st_read(file.path(path, "data/cb_2021_us_state_500k/cb_2021_us_state_500k.shp"))


#The following work is to 
#1. use the scraped file with info on 'which states belong to which division'
#2 to add division information to the state-level shape file
#and do some imputation due to incomplete scraped file (double white space)

#Using the scraped file
#with info on the relationship between state and division
state_division_match <- read_csv(file.path(path, "data/state_division_match.csv"))

#change the column formut before joining
us_division <- us_division %>%
  mutate(GEOID = as.numeric(GEOID))

#Link which state belongs to which division
#But geo is a duplicated info here
state_division_joined <- state_division_match %>%
  left_join(us_division, by=c("Division_num" = "GEOID"))

#state-level joined
#Some rows are not matched because of
#1 islands (Guam, Common wealth, Puerto Rico etc)
#2 double white space... (Rhode  Island, North  Carolina)
#need to impute manually
state_division_detail<- us_state %>%
  left_join(state_division_match, by=c("NAME" = "States"))

#impute missing values (not able to generalize)
#redo the joining
state_division_detail$Division_num[state_division_detail$NAME == "North Carolina"] <- 5
state_division_detail$Division_num[state_division_detail$NAME == "Rhode Island"] <- 1
state_division_detail$Division_num[state_division_detail$NAME == "South Dakota"] <- 4


matching_table <- state_division_match %>%
  distinct(Region, Division, Division_num)


#integrated dataset that includes state and division level info
state_division_detail <- state_division_detail%>%
  left_join(matching_table, by="Division_num") %>%
  select(-c(Region.x, Division.x)) %>%
  rename (Region = Region.y, Division = Division.y)


#Considering the objective of the project,
#filter the region/islands that are not assigned division
state_division_clean <- state_division_detail %>%
  filter(!is.na(Division_num))


#clean, integrated dataframe for drawing maps
state_division_clean
#write.csv(state_division_clean, "data/state_shapefile_with_division_info.csv", row.names=FALSE)


#use shift_geometry to move Alaska and Hawaii for visual purpose
#https://rdrr.io/cran/tigris/man/shift_geometry.html

#install.packages("tigris)
library(tigris)

state_division_clean <- state_division_clean %>%
  shift_geometry()


division_map <- ggplot() +
  geom_sf(data = state_division_clean, aes(fill = Division)) + 
  labs(title = "US Division map",
       subtitle = "Division definition aligns with Census",
       caption = "(Note) Alaska, Hawaii, Puerto Rico are shifted for convenience. Other US territories are filtered") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 10),
        title = element_text(size = 15),
        plot.caption = element_text(size=8)) 

division_map

png("images/map_division.png")
print(division_map)
dev.off()


#########################################
#3. Refine AHS data to be presented on map 
#(i.e. creating summary table at DIVISION level)
#########################################

#install.packages("DescTools")
library(DescTools)

ahs_cleaned %>%
  group_by(DIVISION) %>%
  summarise(frequent = Mode(YRBUILT))

#HMRENEFF (whether go grhough energy efficient improvement): recode 2 (NO) to 0
#make summary table: pick whether to use Mode or mean depending on each var
division_summary <- ahs_cleaned %>%
  mutate(HMRENEFF = ifelse(HMRENEFF==2, 0, HMRENEFF)) %>%
  group_by(DIVISION) %>%
  summarise(YRBUILT = Mode(YRBUILT),
            BLD = Mode(BLD),
            TOTROOMS = Mode(TOTROOMS),
            NUMPEOPLE = Mode(NUMPEOPLE, na.rm=T),
            HEATFUEL = Mode(HEATFUEL, na.rm=T),
            HEATTYPE = Mode(HEATTYPE, na.rm=T),
            ACPRIMARY = Mode(ACPRIMARY, na.rm=T),
            ELECAMT = mean(ELECAMT, na.rm=T),
            GASAMT = mean(GASAMT, na.rm=T),
            OILCAMT = mean(OILAMT, na.rm=T),
            OTHERAMT = mean(OTHERAMT, na.rm=T),
            HMRENEFF = mean(HMRENEFF, na.rm=T)
  ) %>%
  mutate(DIVISION = as.numeric(DIVISION))

division_summary %>% head()





#########################################
# 4. Add AHS info on map (division level)
#########################################

state_division_clean_ahs <- state_division_clean %>%
  left_join(division_summary, by=c("Division_num" = "DIVISION"))

#map by the most frequent builtyear
builtyear <- state_division_clean_ahs %>%
  ggplot() +
  geom_sf(aes(fill = YRBUILT)) + 
  labs(title = "Average built year of household",
       subtitle = "Categorized by division. Division definition aligns with Census",
       caption = "(Note) Alaska, Hawaii, Puerto Rico are shifted for convenience. Other US territories are filtered") +
  scale_fill_continuous(name = "Average built year") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 10),
        title = element_text(size = 15),
        legend.title=element_text(size=10),
        legend.text = element_text(size=6),
        plot.caption = element_text(size=8),
        plot.subtitle = element_text(size=11)) 



builtyear

png("images/map_builtyear.png")
print(builtyear)
dev.off()

#map by the most widely used air conditioning type
#rename labels: https://www.statology.org/ggplot-legend-labels/
ac <- state_division_clean_ahs %>%
  ggplot() +
  geom_sf(aes(fill = ACPRIMARY)) + 
  labs(title = "Map of primary AC system type",
       subtitle = "Colored by Division (not state level). Division definition aligns with Census",
       caption = "(Note) Alaska, Hawaii, Puerto Rico are shifted for convenience. Other US territories are filtered") +
  scale_fill_discrete(name = "Type of AC",
                      labels = c("1.Electric powered central AC ", "12. No AC ")) +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 10),
        title = element_text(size = 15),
        legend.title=element_text(size=10),
        legend.text = element_text(size=8),
        plot.caption = element_text(size=8),
        plot.subtitle = element_text(size=11)) 

ac

png("images/map_ac.png")
print(ac)
dev.off()


#map by average monthly electricity consumption expenditure
elec <- state_division_clean_ahs %>%
  ggplot() +
  geom_sf(aes(fill = ELECAMT)) + 
  labs(title = "Map of Monthly Electricity Consumption Bill",
       subtitle = "Colored by Division (not state level). Division definition aligns with Census",
       caption = "(Note) Alaska, Hawaii, Puerto Rico are shifted for convenience. Other US territories are filtered") +
  scale_fill_continuous(name = "Monthly electiricy \nconsumption ($)") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 10),
        title = element_text(size = 15),
        legend.title=element_text(size=10),
        legend.text = element_text(size=6),
        plot.caption = element_text(size=8),
        plot.subtitle = element_text(size=11)) 

elec

png("images/map_elec.png")
print(elec)
dev.off()


#map by the percentage of households went though energy efficiency improvement
#gradient color for the label
#https://stackoverflow.com/questions/23534938/setting-breaks-and-labels-in-ggplot
energy_efficient_home <- state_division_clean_ahs %>%
  ggplot() +
  geom_sf(aes(fill = HMRENEFF)) + 
  labs(title = "Percentage of Energy Efficient Household",
       subtitle = "Division map of the percentage of household went through \nenergy efficient improvement in last two years",
       caption = "(Note) Alaska, Hawaii, Puerto Rico are shifted for convenience. Other US territories are filtered") +
  scale_fill_gradient(name = "Average percentage of \n energy efficient home",
                      high = "springgreen4", low= "grey90")+
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 10),
        title = element_text(size = 15),
        legend.title=element_text(size=10),
        legend.text = element_text(size=6),
        plot.caption = element_text(size=8),
        plot.subtitle = element_text(size=11)) 


energy_efficient_home

png("images/map_energy_efficient_home.png")
print(energy_efficient_home)
dev.off()



#########################################
# 5. (Supplmentary) AHS statistical analysis
#########################################

ahs_cleaned_2 <- ahs_cleaned %>% 
  mutate(DIVISION = as.numeric(DIVISION)) %>%
  left_join(matching_table, by=c("DIVISION" = "Division_num"))

#1 builtyear line graph
builtyear_line <- ahs_cleaned_2 %>%
  group_by(DIVISION, Division, YRBUILT) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=YRBUILT, y=count, color = Division)) +
  geom_line() +
  labs(title = "Count of housing built years by division",
       subtitle = "South Atlantic has bigger # of house built in relatively recent years",
       x="House built year", 
       y="count of observations") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 10),
        title = element_text(size = 15),
        legend.title=element_text(size=10),
        legend.text = element_text(size=6),
        plot.caption = element_text(size=8),
        plot.subtitle = element_text(size=11)) 



builtyear_line

# print(builtyear_line)
# ggsave("images/builtyear_line.png")


png("images/builtyear_line.png")
print(builtyear_line)
dev.off()

#2 regression line built year ~ electricity bill amount
# Slopes and data points are not meaningful as they are imputed, estimated values
reg_builtyear_elect <- ahs_cleaned_2 %>% ggplot(aes(x=YRBUILT, y =ELECAMT, color = Division)) +
  geom_smooth(method = "lm", fill=NA)+
  labs(title = "Relationship between house built year \nand electricity consumption",
       caption = "(Note) No individual data. This is drawn from estimates from regression") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 10),
        title = element_text(size = 15),
        legend.title=element_text(size=10),
        legend.text = element_text(size=6),
        plot.caption = element_text(size=8),
        plot.subtitle = element_text(size=11)) 

reg_builtyear_elect


png("images/reg_builtyear_elect.png")
print(reg_builtyear_elect)
dev.off()


#3 Regression - not so meaningful without meaningful control. negative intercepts
#unable to add meaningful control as they are all in division-level means
#https://stackoverflow.com/questions/53627306/looping-regression-model-grouped-by-two-columns-in-r-using-lapply
library(broom)
library(purrr)

reg_table <- ahs_cleaned_2 %>%
  group_by(Division) %>%
  nest() %>%
  mutate(fit=map(data, ~lm(ELECAMT ~ YRBUILT, data=.)), results = map(fit, tidy)) %>%
  unnest(results)

print(reg_table)


