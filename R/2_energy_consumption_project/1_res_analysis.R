library(tidyverse)
library(ggplot2)
library(sf)
library(tigris)

setwd("/Users/zhouxiaoqi/Documents/GitHub/final-project-xiaoqi-ellie-lee")
res_data <- read.csv("recs2015_public_v4.csv")
division <- st_read("cb_2018_us_division_500k/cb_2018_us_division_500k.shp")

### Part I -- Regression Analysis ###

# X's to use
#DIVISION - Census Division
#UATYP10 - Census 2010 Urban Type
#CLIMATE_REGION_PUB - Building America Climate Zone
#TYPEHUQ - Type of housing unit
#YEARMADERANGE - Range when housing unit was built
#TOTROOMS - Total number of rooms in the housing unit, excluding bathrooms
#NHSLDMEM - Number of household members
#KOWNRENT - Own or rent
#ELPAY - Who pays for electricity
#TOTSQFT_EN - Total square footage (used for publication)

# Y's to use -- different energy types
#KWH - Total site electricity usage, in kilowatthours, 2015
#CUFEETNG - Total natural gas usage, in hundred cubic feet, 2015
#GALLONLP - Total propane usage, in gallons, 2015
#BTUFO - Total fuel oil/kerosene usage, in thousand Btu, 2015

# select the variables that are useful for analysis
res_data <- res_data %>% 
  select(DIVISION, REGIONC, UATYP10, CLIMATE_REGION_PUB, TYPEHUQ, 
         YEARMADERANGE, TOTROOMS, NHSLDMEM, TOTSQFT_EN, 
         KOWNRENT, ELPAY, NGPAY, LPGPAY, FOPAY,
         KWH, CUFEETNG, GALLONLP, BTUFO)

# change the following x-variables including DIVISION (geographic division), 
# TYPEHUQ, ELPAY, ..., into factors,
# because the number values assigned under each do not follow logical order,
# just for categorizing purpose

res_data_factored <- res_data %>% 
  mutate(DIVISION = as.factor(DIVISION),
         REGIONC = as.factor(REGIONC),
         TYPEHUQ = as.factor(TYPEHUQ),
         ELPAY = as.factor(ELPAY),
         NGPAY = as.factor(NGPAY),
         FOPAY = as.factor(FOPAY),
         LPGPAY = as.factor(LPGPAY),
         UATYP10 = as.factor(UATYP10),
         CLIMATE_REGION_PUB = as.factor(CLIMATE_REGION_PUB))

# make the names of values more meaningful

res_data_factored <- res_data_factored %>% 
  mutate(
    DIVISION = case_when(
      DIVISION == "1" ~ "East North Central",
      DIVISION == "2" ~ "East South Central",
      DIVISION == "3" ~ "Middle Atlantic",
      DIVISION == "4" ~ "Mountain North",
      DIVISION == "5" ~ "Mountain South",
      DIVISION == "6" ~ "New England",
      DIVISION == "7" ~ "Pacific",
      DIVISION == "8" ~ "South Atlantic",
      DIVISION == "9" ~ "West North Central",
      DIVISION == "10" ~ "West South Central"
    ), 
    UATYP10 = case_when(
      UATYP10 == "C" ~ "Urban cluster",
      UATYP10 == "R" ~ "Rural area",
      UATYP10 == "U" ~ "Urban area"
    ),
    REGIONC = case_when(
      REGIONC == "1" ~ "Northeast",
      REGIONC == "2" ~ "Midwest",
      REGIONC == "3" ~ "South",
      REGIONC == "4" ~ "West"
    ),
    YEARMADERANGE = case_when(
      YEARMADERANGE == "1" ~ "* to 1950",
      YEARMADERANGE == "2" ~ "1950s",
      YEARMADERANGE == "3" ~ "1960s",
      YEARMADERANGE == "4" ~ "1970s",
      YEARMADERANGE == "5" ~ "1980s",
      YEARMADERANGE == "6" ~ "1990s",
      YEARMADERANGE == "7" ~ "2000s",
      YEARMADERANGE == "8" ~ "2010 to 2015"
    ))

# run regression:
# Electricity 
rg_factored_elec <- lm(KWH ~ DIVISION + UATYP10 + CLIMATE_REGION_PUB + 
                  TYPEHUQ + TOTROOMS + NHSLDMEM + KOWNRENT +
                  YEARMADERANGE + ELPAY, 
                data = res_data_factored)
summary(rg_factored_elec)

# Both methods shows that
# Relevant: DIVISION, UATYP10, CLIMATE_REGION_PUB, TYPEHUQ, 
#           TOTROOMS, NHSLDMEM, YEARMADERANGE
# Irrelevant: KOWNRENT, ELPAY

# Natural gas
rg_factored_naturalgas <- lm(CUFEETNG ~ DIVISION + UATYP10 + CLIMATE_REGION_PUB + 
                    TYPEHUQ + TOTROOMS + NHSLDMEM + KOWNRENT +
                    YEARMADERANGE + NGPAY, 
                  data = res_data_factored)
summary(rg_factored_naturalgas)

# Propane
rg_factored_propane <- lm(GALLONLP ~ DIVISION + UATYP10 + CLIMATE_REGION_PUB + 
                    TYPEHUQ + TOTROOMS + NHSLDMEM + KOWNRENT +
                    YEARMADERANGE + LPGPAY, 
                  data = res_data_factored)
summary(rg_factored_propane)

# Fuel oil/kerosene
rg_factored_fueloil <- lm(BTUFO ~ DIVISION + UATYP10 + CLIMATE_REGION_PUB + 
                    TYPEHUQ + TOTROOMS + NHSLDMEM + KOWNRENT +
                    YEARMADERANGE + FOPAY, 
                  data = res_data_factored)
summary(rg_factored_fueloil)


### Part II -- Plotting ###
# note: we notice that size of house can have a large impact on 
# energy usage, so in order to make the message clear, we standardize 
# energy use by dividing the square footage of a house

res_data_factored <- res_data_factored %>%
  mutate(
    elec_per_sqft = KWH / TOTSQFT_EN,
    ntrlgas_per_sqft = CUFEETNG / TOTSQFT_EN,
    propane_per_sqft = GALLONLP / TOTSQFT_EN,
    fueloil_per_sqft = BTUFO / TOTSQFT_EN
    ) 

# electricity 
res_data_factored %>% 
  group_by(DIVISION) %>%
  summarise(avg_elec_use = mean(elec_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = DIVISION, y = avg_elec_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Census division", 
       y = "Average electricity usage per sqft in KWH",
       title = "Pacific, New England, and Mountain South consume more electricity") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# natural gas
res_data_factored %>% 
  group_by(DIVISION) %>%
  summarise(avg_ng_use = mean(ntrlgas_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = DIVISION, y = avg_ng_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Census division", 
       y = "Average natural gas usage per sqft in hundred cubic feet",
       title = "South Atlantic and Mid Atlantic consume more natural gas") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# propane
res_data_factored %>% 
  group_by(DIVISION) %>%
  summarise(avg_propane_use = mean(propane_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = DIVISION, y = avg_propane_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Census division", 
       y = "Average propane usage per sqft in gallons",
       title = "East North Central consume more propane") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# fuel oil/kerosene
res_data_factored %>% 
  group_by(DIVISION) %>%
  summarise(avg_fueloil_use = mean(fueloil_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = DIVISION, y = avg_fueloil_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Census division", 
       y = "Average fuel oil/kerosene usage per sqft in thousand Btu",
       title = "East North Central consume more fuel oil") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# four energy types all together 
res_data_factored_long <- res_data_factored %>% 
  pivot_longer(
    cols = c(`elec_per_sqft`, `ntrlgas_per_sqft`, `propane_per_sqft`, `fueloil_per_sqft`),
    names_to = "energy_type",
    values_to = "usage"
  ) 

res_data_factored_long <- res_data_factored_long %>%
  mutate(
    energy_type = case_when(
      energy_type == "elec_per_sqft" ~ "electricity (KWH)",
      energy_type == "ntrlgas_per_sqft" ~ "natural gas (hundred cubic feet)",
      energy_type == "propane_per_sqft" ~ "propane (gallons)",
      energy_type == "fueloil_per_sqft" ~ "fuel oil/kerosene (thousand Btu)"
    ))

res_data_factored_long %>% 
  group_by(DIVISION, energy_type) %>%
  summarise(avg_usage = mean(usage)) %>%
  ggplot(aes(x = DIVISION, y = avg_usage, fill = energy_type)) + 
  geom_col(position="dodge") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Census division", 
       y = "Average energy usage per sqft",
       title = "Consumption varies by division. Substitute effects exist between energies.") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 6),
        title = element_text(size = 8)) 

# A division that use one energy type most does not necessarily use other 
# energy types very much. There are some substitute effects between energy types

## same set of plots but grouped by region ##

# electricity 
res_data_factored %>% 
  group_by(REGIONC) %>%
  summarise(avg_elec_use = mean(elec_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = REGIONC, y = avg_elec_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Region", 
       y = "Average electricity usage per sqft in KWH",
       title = "South consumes more electricity on average") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# natural gas
res_data_factored %>% 
  group_by(REGIONC) %>%
  summarise(avg_ng_use = mean(ntrlgas_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = REGIONC, y = avg_ng_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Region", 
       y = "Average natural gas usage per sqft in hundred cubic feet",
       title = "Midwest consumes more natural gas on average") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# propane
res_data_factored %>% 
  group_by(REGIONC) %>%
  summarise(avg_propane_use = mean(propane_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = REGIONC, y = avg_propane_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Region", 
       y = "Average propane usage per sqft in gallons",
       title = "Northeast consumes more propane") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# fuel oil/kerosene
res_data_factored %>% 
  group_by(REGIONC) %>%
  summarise(avg_fueloil_use = mean(fueloil_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = REGIONC, y = avg_fueloil_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Region", 
       y = "Average fuel oil/kerosene usage per sqft in thousand Btu",
       title = "Northeast consumes more fuel oil") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

res_data_factored_long %>% 
  group_by(REGIONC, energy_type) %>%
  summarise(avg_usage = mean(usage)) %>%
  ggplot(aes(x = REGIONC, y = avg_usage, fill = energy_type)) + 
  geom_col(position="dodge") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Region", 
       y = "Average energy usage per sqft",
       title = "Consumption varies by regions. Substitute effects exist between energies.") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 


## same set of plots but grouped by climate types ##

# electricity 
res_data_factored %>% 
  group_by(CLIMATE_REGION_PUB) %>%
  summarise(avg_elec_use = mean(elec_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = CLIMATE_REGION_PUB, y = avg_elec_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Climate type", 
       y = "Average electricity usage per sqft in KWH",
       title = "Electricity Usage vs Climate Type",
       subtitle = "Hot-humid climate consumes more electricity on average") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 12)) 

# natural gas
res_data_factored %>% 
  group_by(CLIMATE_REGION_PUB) %>%
  summarise(avg_ng_use = mean(ntrlgas_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = CLIMATE_REGION_PUB, y = avg_ng_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Climate type", 
       y = "Avg ntrl gas usage per sqft in hundred cubic feet",
       title = "Natural Gas Usage vs Climate Type",
       subtitle = "Cold climate consumes more natural gas on average") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 12)) 

# propane
res_data_factored %>% 
  group_by(CLIMATE_REGION_PUB) %>%
  summarise(avg_propane_use = mean(propane_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = CLIMATE_REGION_PUB, y = avg_propane_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Climate type", 
       y = "Average propane usage per sqft in gallons",
       title = "Propane Usage vs Climate Type",
       subtitle = "Cold climate consumes more propane") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 12)) 

# fuel oil/kerosene
res_data_factored %>% 
  group_by(CLIMATE_REGION_PUB) %>%
  summarise(avg_fueloil_use = mean(fueloil_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = CLIMATE_REGION_PUB, y = avg_fueloil_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Climate type", 
       y = "Avg fuel oil usage per sqft in thousand Btu",
       title = "Fuel Oil Usage vs Climate Type",
       subtitle = "Cold climates consumes more fuel oil") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 12)) 

res_data_factored_long %>% 
  group_by(CLIMATE_REGION_PUB, energy_type) %>%
  summarise(avg_usage = mean(usage)) %>%
  ggplot(aes(x = CLIMATE_REGION_PUB, y = avg_usage, fill = energy_type)) + 
  geom_col(position="dodge") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Climate type", 
       y = "Average energy usage per sqft",
       title = "Hot-humid prefers electricity while cold areas prefer fuel oil and natural gas") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 



## Cross effects by division and Urban/Rural on energy consumption ##

# electricity per sqft -- DIVISION + Urban/Rural segments 

res_data_factored %>% 
  group_by(DIVISION, UATYP10) %>%
  summarise(avg_elec_use = mean(elec_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = DIVISION, y = avg_elec_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_wrap(vars(UATYP10), nrow = 3) +
  labs(x = "Census Division and Urban/Rural", 
       y = "Average Electricity Use per Square Ft in KWH",
       title = "Rural areas generally use more electricity independent of geographic divison") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# natural gas per sqft -- DIVISION + Urban/Rural segments 

res_data_factored %>% 
  group_by(DIVISION, UATYP10) %>%
  summarise(avg_ntrlgas_use = mean(ntrlgas_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = DIVISION, y = avg_ntrlgas_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_wrap(vars(UATYP10), nrow = 3) +
  labs(x = "Census Division and Urban/Rural", 
       y = "Average natural gas usage per sqft in hundred cubic feet",
       title = "Rural areas generally use less natural gas independent of geographic divison") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# propane
res_data_factored %>% 
  group_by(DIVISION, UATYP10) %>%
  summarise(avg_propane_use = mean(propane_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = DIVISION, y = avg_propane_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_wrap(vars(UATYP10), nrow = 3) +
  labs(x = "Census Division and Urban/Rural", 
       y = "Average propane usage per sqft in gallons",
       title = "Rural areas generally use more propane independent of geographic divison") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# fuel oil/kerosene
res_data_factored %>% 
  group_by(DIVISION, UATYP10) %>%
  summarise(avg_fueloil_use = mean(fueloil_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = DIVISION, y = avg_fueloil_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_wrap(vars(UATYP10), nrow = 3) +
  labs(x = "Census Division and Urban/Rural", 
       y = "Average fuel oil/kerosene usage per sqft in thousand Btu",
       title = "Urban areas generally use less fuel oil independent of geographic divison") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 


## Cross effects by climate type and Urban/Rural on energy consumption ##

# electricity per sqft -- CLIMATE_REGION_PUB + Urban/Rural segments 

res_data_factored %>% 
  group_by(CLIMATE_REGION_PUB, UATYP10) %>%
  summarise(avg_elec_use = mean(elec_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = CLIMATE_REGION_PUB, y = avg_elec_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_wrap(vars(UATYP10), nrow = 3) +
  labs(x = "Climate Type and Urban/Rural", 
       y = "Average Electricity Use per Square Ft in KWH",
       title = "Rural areas generally use more electricity independent of climate type") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# natural gas per sqft -- CLIMATE_REGION_PUB + Urban/Rural segments 

res_data_factored %>% 
  group_by(CLIMATE_REGION_PUB, UATYP10) %>%
  summarise(avg_ntrlgas_use = mean(ntrlgas_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = CLIMATE_REGION_PUB, y = avg_ntrlgas_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_wrap(vars(UATYP10), nrow = 3) +
  labs(x = "Climate Type and Urban/Rural", 
       y = "Average natural gas usage per sqft in hundred cubic feet",
       title = "Rural areas generally use less natural gas independent of climate type") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# propane per sqft -- CLIMATE_REGION_PUB + Urban/Rural segments 
res_data_factored %>% 
  group_by(CLIMATE_REGION_PUB, UATYP10) %>%
  summarise(avg_propane_use = mean(propane_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = CLIMATE_REGION_PUB, y = avg_propane_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_wrap(vars(UATYP10), nrow = 3) +
  labs(x = "Climate Type and Urban/Rural", 
       y = "Average propane usage per sqft in gallons",
       title = "Rural areas generally use more propane independent of climate type") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# fuel oil/kerosene per sqft -- CLIMATE_REGION_PUB + Urban/Rural segments 
res_data_factored %>% 
  group_by(CLIMATE_REGION_PUB, UATYP10) %>%
  summarise(avg_fueloil_use = mean(fueloil_per_sqft)) %>%
  ggplot() + 
  geom_col(aes(x = CLIMATE_REGION_PUB, y = avg_fueloil_use)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_wrap(vars(UATYP10), nrow = 3) +
  labs(x = "Climate Type and Urban/Rural", 
       y = "Average fuel oil/kerosene usage per sqft in thousand Btu",
       title = "Climate affects fuel oil usage more than Rural/Urban") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 


### Additional Discussion ###
## correlation between house age and energy consumption ##

# electricity consumption vs house age

ggplot(res_data_factored) + 
  geom_boxplot(aes(x = YEARMADERANGE, y = elec_per_sqft)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Time when housing unit was built", 
       y = "Electricity usage per Square Ft in KWH",
       title = "Electricity usage varies slightly with house built-year") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# investigate outlier -- what regions do they fall in

res_data_factored %>% 
  group_by(REGIONC) %>%
  ggplot() + 
  geom_point(aes(x = YEARMADERANGE, y = elec_per_sqft, color = REGIONC)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Time when housing unit was built", 
       y = "Electricity use per Square Ft in KWH",
       title = "The households with really high electricity usage locate in South or West") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# investigate outlier -- are they urban or rural?

res_data_factored %>% 
  group_by(UATYP10) %>%
  ggplot() + 
  geom_point(aes(x = YEARMADERANGE, y = elec_per_sqft, color = UATYP10)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Time when housing unit was built", 
       y = "Electricity use per Square Ft in KWH",
       title = "The most salient outlier on upper right locate in rural area")  + 
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# investigate outlier -- are climate types important?

res_data_factored %>% 
  group_by(CLIMATE_REGION_PUB) %>%
  ggplot() + 
  geom_point(aes(x = YEARMADERANGE, y = elec_per_sqft, color = CLIMATE_REGION_PUB)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Time when housing unit was built", 
       y = "Electricity use per Square Ft in KWH",
       title = "The most salient outlier on upper right is in Hot-humid climate")  + 
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 8),
        title = element_text(size = 8)) 

# Findings: the most salient outlier on upper right is in rural area in South 
# where the climate is hot-humid.

## Potential Issues and Limitation ##
# optional: cut outliers? 


## investigate why Northeast has a significantly higher avg fuel oil usage ##

fueloil_user_NE <- 
  (res_data_factored %>%
     filter(REGIONC == "Northeast") %>% 
     filter(fueloil_per_sqft > 0) %>% 
     nrow()) / 
  (res_data_factored %>%
     filter(REGIONC == "Northeast") %>%
     nrow())
# Percentage of household which uses fuel oil in Northeast: 0.283375

fueloil_user_S <- 
  (res_data_factored %>%
     filter(REGIONC == "South") %>% 
     filter(fueloil_per_sqft > 0) %>% 
     nrow()) / 
  (res_data_factored %>%
     filter(REGIONC == "South") %>%
     nrow())
# Percentage of household which uses fuel oil in South: 0.023383

fueloil_user_W <- 
  (res_data_factored %>%
     filter(REGIONC == "West") %>% 
     filter(fueloil_per_sqft > 0) %>% 
     nrow()) / 
  (res_data_factored %>%
     filter(REGIONC == "West") %>%
     nrow())
# Percentage of household which uses fuel oil in West: 0.007073


fueloil_user_MW <- 
  (res_data_factored %>%
     filter(REGIONC == "Midwest") %>% 
     filter(fueloil_per_sqft > 0) %>% 
     nrow()) / 
  (res_data_factored %>%
     filter(REGIONC == "Midwest") %>%
     nrow())
# Percentage of household which uses fuel oil in Midwest: 0.008289

# Percentage of household which uses fuel oil is much higher in Northeast, 
# which is 0.283 >> 0.023 > 0.008 > 0.007.
# People in Northeast are generally likely to be a fuel-oil user. Fuel oil is 
# more suitable for the climate type in Northeast. And people need to consume 
# much more fuel oil in quantity to live in its cold climate. 


### Part III -- Plot Map for Shiny App ###

# check the columns we will use to merge the shapefile and energy usage data
unique(division$NAME)
unique(res_data_factored$DIVISION)

# Since names of division differ a little bit, i.e., the division shapefile
# categorizes both "Mountain North" and "Mountain South" as "Mountain",
# we need to mutate some values under res_data_factored$DIVISION:

res_data_for_merge <- res_data_factored %>% 
  mutate(DIVISION = case_when(
    DIVISION == "Mountain North" ~ "Mountain",
    DIVISION == "Mountain South" ~ "Mountain",
    TRUE ~ DIVISION)) 
# check if the name change works
unique(res_data_for_merge$DIVISION) 

# select useful columns only 
res_data_for_merge <- res_data_for_merge %>% 
  select(DIVISION, elec_per_sqft, ntrlgas_per_sqft, 
         fueloil_per_sqft, propane_per_sqft)

# summarize the average for each division 
res_data_for_merge <- res_data_for_merge %>%
  group_by(DIVISION) %>%
  summarise(avg_elec_per_sqft = mean(elec_per_sqft),
            avg_ntrlgas_per_sqft = mean(ntrlgas_per_sqft), 
            avg_fueloil_per_sqft = mean(fueloil_per_sqft), 
            avg_propane_per_sqft = mean(propane_per_sqft)) 

division <- division %>% shift_geometry()

# merge division shapefile and energy usage data
res_sf <- left_join(
  division, 
  res_data_for_merge, 
  by = c("NAME" = "DIVISION")
)

# plot choropleth 
# electricity
ggplot() +
  geom_sf(data = res_sf, aes(fill = avg_elec_per_sqft)) +
  labs(title = "Electricity Usage vs Census Division", 
       subtitle = "Pacific and New England consume more electricity (KWH) per sqft in household",
       fill = element_blank(),
       caption = "Source: RES 2015 data") +
  theme_void() +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 10),
        title = element_text(size = 15),
        legend.title=element_text(size=8),
        legend.text = element_text(size=6),
        plot.caption = element_text(size=8),
        plot.subtitle = element_text(size=8)) 

# natural gas
ggplot() +
  geom_sf(data = res_sf, aes(fill = avg_ntrlgas_per_sqft)) +
  labs(title = "Natural Gas Usage vs Census Division",
       subtitle = "South Atlantic, Mid Atlantic use more natural gas (hundred cubic ft) per sqft", 
       fill = element_blank(),
       caption = "Source: RES 2015 data") +
  theme_void() +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 10),
        title = element_text(size = 15),
        legend.title=element_text(size=8),
        legend.text = element_text(size=6),
        plot.caption = element_text(size=8),
        plot.subtitle = element_text(size=8)) 

# fuel oil
ggplot() +
  geom_sf(data = res_sf, aes(fill = avg_fueloil_per_sqft)) +
  labs(title = "Fuel Oil Usage vs Census Division",
       subtitle = "East North Central consume more fuel oil (thousand Btu) per sqft in household", 
       fill = element_blank(),
       caption = "Source: RES 2015 data") +
  theme_void() +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 10),
        title = element_text(size = 15),
        legend.title=element_text(size=8),
        legend.text = element_text(size=6),
        plot.caption = element_text(size=8),
        plot.subtitle = element_text(size=8)) 

# propane
ggplot() +
  geom_sf(data = res_sf, aes(fill = avg_propane_per_sqft)) +
  labs(title = "Propane Usage vs Census Division",
       subtitle = "East North Central consume more propane (gallons) per sqft in household", 
       fill = element_blank(),
       caption = "Source: RES 2015 data") +
  theme_void() +
  theme_classic() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "grey95"),
        axis.text = element_text(size = 10),
        title = element_text(size = 15),
        legend.title=element_text(size=8),
        legend.text = element_text(size=6),
        plot.caption = element_text(size=8),
        plot.subtitle = element_text(size=8)) 



### render cleaned csv data -- final version of the data-frame I built ###
write.csv(res_data_factored,
          file.path("res_cleaned.csv"),
          row.names = FALSE)

