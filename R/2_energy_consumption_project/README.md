# [Autumn 2022 30536] Data and Programming for Public Policy II - Final Project

## Author: Lee Ko, Xiaoqi Zhou, Ellie Chen

### 0. Major links and note

- Github Repo name: datasci-harris/final-project-xiaoqi-ellie-lee
- Shiny app
  * Link to the Shiny app: https://elliechen-question3-shiny.shinyapps.io/final_project/ 
  * Shiny.R file is in *Shiny* folder
- Google drive link to download (ahs2015n.csv) : https://drive.google.com/file/d/1dzgglr8T3qz56GYg0_ryz2AmhUpVQGuv/view?usp=share_link

### 1. Introduction

Energy efficiency has became an important feature of newly built homes. As reported by the Energy Information Administration (EIA), homes built in the United States after the year 2000 are more energy efficient than those built before 2000. Household energy consumption is influenced by several factors, building characteristics, housing condition, the willingness to invest in energy efficiency measures, the way individuals inhabit their dwellings. Climate type also plays an important role in determining the energy consumption pattern. This project provides meaningful analysis to help understand the EIA's conclusion on the relationship between housing age and energy consumption, and take climate type into consideration to further evaluate the consumption pattern.

### 2. Residential Energy Consumption Survey data - Fitting OLS Model
To investigate the factors affecting energy consumption of a household, we conduct analysis based on the 2015 RECS (Residential Energy Consumption Survey) data.

Datasets used - included in the 'data' folder:
-   recs2015_public_v4.csv
-   cb_2018_us_division_500k.shp

A cleaned version of dataframe used for analysis:
-   res_cleaned.csv

Result images - included in the 'images' folder: 
- division_electricity.png
- division_gas.png
- division_oil.png
- division_propane.png
- climate_electricity.png
- climate_gas.png
- climate_oil.png
- climate_propane.png
- energies_by_climate.png
- energies_by_division.png
- energies_by_region.png
- cross_effect_climate_rural_elec.png
- cross_effect_climate_rural_gas.png
- cross_effect_climate_rural_oil.png
- cross_effect_climate_rural_propane.png
- cross_effect_division_rural_elec.png
- cross_effect_division_rural_gas.png
- cross_effect_division_rural_oil.png
- cross_effect_division_rural_propane.png
- correlation_elec_vs_builtyear.png

For the regression model, the independent variables we use are,
- DIVISION - census division
- UATYP10 - census 2010 urban type
- CLIMATE_REGION_PUB - climate zone
- TYPEHUQ - type of housing unit
- YEARMADERANGE - range when housing unit was built
- TOTROOMS - total number of rooms in the housing unit excluding bathroom
- NHSLDMEM - number of household members
- KOWNRENT - own or rent
- ELPAY - who pays for electricity
- NGPAY - who pays for natural gas
- LPGPAY -- who pays for propane
- FOPAY -- who pays for fuel oil

The outcome variables we are interested in are,
- KWH - total site electricity usage in kilowatthours
- CUFEETNG - total natural gas usage in hundred cubic feet
- GALLONLP - total propane usage in gallons
- BTUFO - total fuel oil/kerosene usage in thousand Btu

By selecting these X variables, we assume that household energy consumption depends on the geographic region, location in urban area or rural area, climate type, household type (mobile home, house, or apartment), year built, number of rooms, number of residents, and potential moral hazard if the house is rent or if the bill is not paid by the resident. We factorized the non-ordinal number values so that the different number values simply reflect the different categories and then renamed values under some variables to straightforwardly show their meaning if necessary. Such factorization enabled us to fit a fixed effects regression model based on census division, household type, climate type, urban/rural category, and who pays the bill in our regression. For each type of energy, i.e., electricity, natural gas, fuel oil, and propane, we fit:\
CONSUMPTION \~ DIVISION + UATYP10 + CLIMATE_REGION_PUB + TYPEHUQ + TOTROOMS + NHSLDMEM + KOWNRENT + YEARMADERANGE + \*\*PAY.

We generated a set of plots to illustrate the relationship between consumption of different energy types and multiple factors such as regions, climate types, urban/rural areas. We standardized energy consumption by square footage of a house. Furthermore, we also examined the cross effects of geographic or climate regions and urban/rural location on energy consumption. We presented the results by adding facets on the plots. This part is presented in Shiny.

### 3. American Housing Survey data - Choropleth Mapping

**1.  Context of AHS analysis**

We used the 2015 American Housing Survey (AHS) data in order to conduct additional analysis on the relationship between household characteristics -- most notably household age -- and energy consumption. As AHS collects specific household characteristics not limited to energy consumption expenditure from the sampled households representative of the US, analyzing AHS data provides supplemental information that matches the objective of the study while also providing a chance to check the alignment of results with the previous analyses.

**2.  Data**

First 4 datasets are used to build an integrated data frame (state_shapefile_with_division_info.csv). All files are in the 'data' folder:

-   state_shapefile_with_division_info.csv
-   cb_2018_us_division_500k.csv
-   cb_2021_us_state_500k.csv
-   ahs2015n.csv - **uploaded on Google drive** (https://drive.google.com/file/d/1dzgglr8T3qz56GYg0_ryz2AmhUpVQGuv/view?usp=share_link)
-   state_division_match.csv : cleaned version with information added

R scripts used for scraping tables and analyzing AHS data:

-   scraping_table.R: automatically downloads state-division matching info, saves the data into csv (state_division_match.csv in A.d)
-   ahs_analysis_supplementary.R

Result images - All in the 'images' folder: 

- map_division.png
- map_builtyear.png
- map_ac.png
- map_elect.png
- map_energy_efficient_home.png
- builtyear_line.png
- reg_builtyear_elect.png

Since census data does not provide state and division categories within a single dataset, we used scraped table with division-state matching information to link state-level and division-level shape files. Then we added the division-level analysis from ahs data to the shape file to present information in a map format. American Housing Survey data (ahs2015n.csv) is uploaded on Google drive, and codes are written assuming it is located in the 'data' folder.

**3. Key findings**

1. Regarding the average built year of households, there are some disparities among divisions, with the oldest houses concentrated in the New England and Middle Atlantic region (which are called Northeast at the region level). East North Central division shows the second oldest average household age. Compared to other divisions, those in the Mountain and West South/North Central region have been built comparatively recently. 
2. Major air conditioning systems used across states are electricity-powered central AC. But the Pacific division was exceptional, as most states reported not having an AC system. 
3. The monthly electricity consumption bill is highest in the West North/South Central division (i.e., South region). As illustrated earlier, electricity consumption primarily represents the use of AC (not heating), which aligns with the general understanding of the warmer climate in the South. 
4. On average, 40% of households in the New England and Middle Atlantic division (i.e., Northeast region) went through house improvements to make their home energy efficient. The division with the second highest participation rate is West North Central. East South Central division is least participating in the efforts to make household energy efficient. As there would be some policy-relevant factors in addition to the effects of climate, it is hard to conclude what affects the disparities in efforts to increase household energy efficiency.

Overall, the result is not conclusive. But it is meaningful in showing that AHS data aligns with the results drawn from RECS data. That the northeast region consumes the largest amount of fuel energy corresponds with the information that it has a relatively cold climate and older building age, thus inducing more household energy efficiency improvement work. Additional ggplot graphs (builtyear_line.png, reg_builyear_elect.png) are provided to show the difference in the average building age and electricity bill by division. However, the specificity of the analyses is limited due to identification by division.

### 4. Annual Energy Outlook reports by EIA - Sentiment, Keyword Analysis

EIA releases its Annual Energy Outlook each year to provide updated projections of U.S. energy markets. Before 2017, the Annual Energy Outlook provides modeled projections of domestic energy markets through 2040. After 2017, it projected through 2050, including cases with different assumptions about macroeconomic growth, world oil prices, and technological progress. We conducted sentiment evaluation from 2013 to 2022 to analyze whether there would be a visible sentimental change for future projections from the nation's most authoritative institute in energy market.

We discovered that except for a relatively pronounced positivity in 2018, the Annual Energy Outlook maintained a consistent sentiment in the past ten years. It is expected in authoritative reports issued by states agencies. (Results shown in nlp.R and Shiny)

### 5. Limitation, addtional notes, and direction of future research

A major limitation of our study comes from the limited specificity of the analysis due to the lack of individual state-level data. All analyses are conducted on the division level. Although the division level provides a useful tool for understanding the group of states, it could be an overly broad category for running models and estimating coefficients to track relationships between variables. Using state or neighborhood-level data would be useful to conduct valid research that represents the trend in the data. 

Furthermore, the data of RECS is of year 2015 since data of year 2021 has not been finalized and published. So to make the whole analysis up to date, we suggest researchers using the newest data once it becomes available. 

Moreover, our group members ended up finding relevant datasets from the same (or mutually linked) data source. It could be partly due to our project's topic of energy consumption, but integrating data from a commercial entity or data source other than a federal government agency could also reveal interesting trends. 

Additionally, in terms of outliers, we did not eliminate data points with very high energy consumption. In our RECS analysis, we noticed that there are some outliers in electricity consumption, thus doing further analysis on these outliers. We found that the most salient outlier with high electricity usage turns out to be in rural areas in the South where the climate is hot-humid. This is consistent with our overall analysis, so we decide to keep such data points. Researchers may study these outliers further to decide whether to erase them. 

Lastly, readers may notice that Northeast has a significantly higher average fuel oil usage than other regions do in our RECS analysis, so we managed to find a reasonable explanation. We checked the percentage of households which use fuel oil in each region, and we found that the probability of being a fuel oil user is much higher in the Northeast than in other regions. This is because fuel oil is a more suitable energy type for residents in the Northeast considering the cold climate there. Residents in the Northeast need to consume much more fuel oil in quantity to get through the cold weather. This is only one explanation and the real situation may be more complicated. 
