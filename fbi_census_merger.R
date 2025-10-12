library(dplyr)
library(tidyr)
library(sf)

crime_data <- readRDS("data/crime_data.rds")
mapping <- readRDS("data/mapping.rds")

vars <- readRDS("data/vars.rds")
acs_1yr_ts <- readRDS("data/acs_1yr_ts.rds")

acs_5yr_ts <- readRDS("data/acs_5yr_ts.rds")


### final crime data file with all variables ###
crime_data <- crime_data|>
  mutate(across(c(arson, assaults, burglary, gta, homicides, larceny, rape, robbery), ~ifelse(is.na(.x), 0, .x)))|> # set NA values to 0
  left_join(mapping, by = join_by(ori), relationship = "many-to-many")|> # join in mapping variables
  left_join(acs_1yr_ts, by = join_by(GEOID, year))|> # join to acs1 data by county and year
  left_join(acs_5yr_ts, by = join_by(GEOID, year), suffix = c("_acs1", "_acs5"))|> # join to acs5 data by county and year
  distinct(series, month, .keep_all = TRUE)|> # drop some duplicates left over
  group_by(month)|>
  drop_na(homicides)|> # drop na homicide years, primarily from smaller depts early on in the data
  ungroup()


### collapse main crime data for mapping acs_5 vs acs_1 vs homicides by county ###
mapping_yearly_data <- crime_data |>
  
  # summarize crimes and officer counts by county and year to condense from monthly data
  group_by(county, year)|>
  mutate(across(c(arson, assaults, burglary, gta, homicides, larceny, rape, robbery), sum),
         officers = mean(officers, na.rm = TRUE))|>
  select(-series, -month, -ori, -type)|>
  distinct()|>
  
  ungroup()|>
  group_by(county)|>
  
  # add rolling averages of crime stats (last 4 years incl. current)
  # this converts crime stats to the same 5 yr rolling avg that is used by acs5
  arrange(county, year) |>
  mutate(across(c(arson, assaults, burglary, gta, homicides, larceny, rape, robbery), ~ 
                  slider::slide_dbl(.x, mean, .before = 4, .complete = TRUE))
  ) |>
  
  # find total crime rates using the aggregated columns
  mutate(crime_rate =    (arson+assaults+burglary+gta+homicides+larceny+rape+robbery)/total_population_acs5,
         homicide_rate = homicides/total_population_acs5)|>
  
  st_as_sf() # save as sf object for mapping


### create final df for modeling using acs 1 data ###

acs5_vars <- paste(names(vars),"_acs5", sep = "")
acs1_vars <- paste(names(vars),"_acs1", sep = "")


acs_1_yearly_data <- crime_data |>
  
  # summarize crimes and officer counts by county and year to condense from monthly data
  group_by(county, year)|>
  mutate(across(c(arson, assaults, burglary, gta, homicides, larceny, rape, robbery), sum),
         officers = mean(officers, na.rm = TRUE))|>
  select(-series, -month, -ori, -type)|>
  distinct()|>
  
  # linear interpolation to fill missing values for 2020 years and 2018 education estimates for geoid 27019  
  ungroup()|>
  group_by(county)|>
  arrange(year, .by_group = TRUE)|>
  mutate(across(all_of(names(acs1_vars)), ~ zoo::na.approx(., x = year, na.rm = FALSE))) |>
  
  # find total crime rates
  mutate(crime_rate = (arson+assaults+burglary+gta+homicides+larceny+rape+robbery)/total_population_acs1,
         homicide_rate = homicides/total_population_acs1)|>
  
  # lag 1 for crime rate
  mutate(crime_rate_lag1 = lag(crime_rate, 1))|>
  
  ungroup()|>
  
  # calculate aggregate stats for final model
  mutate(pct_lessthan_5kincome = (households_income_lt5k_acs1 / households_total_acs1)* 100,
         
         pct_highschool_or_greater = (edu_hs_grad_acs1 + edu_some_college_lt1yr_acs1 + 
                                        edu_some_college_gt1yr_acs1 + edu_assoc_degree_acs1 + 
                                        edu_bachelors_acs1 + edu_masters_acs1 + 
                                        edu_professional_degree_acs1 + edu_doctorate_acs1) / edu_total_25plus_acs1* 100,
         
         pct_children_missing_parents = (children_w_mother_only_acs1 + children_w_father_only_acs1 + children_no_parents_acs1) / children_total_acs1* 100,
         
         periods = year - min(acs_1yr_ts$year),
         
         post_covid = ifelse(year>=2020, 1, 0),
         
         msp_main_counties = ifelse(county %in% c("RAMSEY", "HENNEPIN"), 1, 0),
         
         pct_non_white = (1 - (white_population_acs1 / total_population_acs1))* 100,
         
         pct_young_males = ((male_15_19_acs1 + male_20_24_acs1)/total_population_acs1)* 100,
         
         persons_per_household = total_population_acs1/households_total_acs1,
         
         persons_per_m2 = total_population_acs1/area_m2)|>
  
  # final cleaning
  filter(county != "CLAY")|>     # first acs1 year for clay is 2020, interpolation for 2020 wont work 
  drop_na(total_population_acs1)|>  # drop years that acs1 doesn't cover
  select(-acs5_vars, -geometry)  # remove acs5 pop estimates and geometry



### acs 5 year data to compare 2023 acs5 to 2018 acs5 ###
acs_5_yearly_data <- crime_data |>
  
  # summarize crimes and officer counts by county and year to condense from monthly data
  group_by(county, year)|>
  mutate(across(c(arson, assaults, burglary, gta, homicides, larceny, rape, robbery), sum),
         officers = mean(officers, na.rm = TRUE))|>
  select(-series, -month, -ori, -type)|>
  distinct()|>
  
  ungroup()|>
  group_by(county)|>
  
  # add rolling averages of crime stats (last 4 years incl. current)
  # this converts crime stats to the same 5 yr rolling avg that is used by acs5
  arrange(county, year) |>
  mutate(across(c(arson, assaults, burglary, gta, homicides, larceny, rape, robbery), ~ 
                  slider::slide_dbl(.x, mean, .before = 4, .complete = TRUE))
  ) |>
  
  # find total crime rates using the aggregated columns
  mutate(crime_rate =    (arson+assaults+burglary+gta+homicides+larceny+rape+robbery)/total_population_acs5,
         homicide_rate = homicides/total_population_acs5)|>
  
  ungroup()|>
  
  # calculate aggregate stats for final model
  mutate(pct_lessthan_5kincome = (households_income_lt5k_acs5 / households_total_acs5)* 100,
         
         pct_highschool_or_greater = (edu_hs_grad_acs5 + edu_some_college_lt1yr_acs5 + 
                                        edu_some_college_gt1yr_acs5 + edu_assoc_degree_acs5 + 
                                        edu_bachelors_acs5 + edu_masters_acs5 + 
                                        edu_professional_degree_acs5 + edu_doctorate_acs5) / edu_total_25plus_acs5* 100,
         
         pct_children_missing_parents = (children_w_mother_only_acs5 + children_w_father_only_acs5 + children_no_parents_acs5) / children_total_acs5* 100,
         
         post_covid = ifelse(year>=2020, 1, 0),
         
         msp_main_counties = ifelse(county %in% c("RAMSEY", "HENNEPIN"), 1, 0),
         
         pct_non_white = (1 - (white_population_acs5 / total_population_acs5))* 100,
         
         pct_young_males = ((male_15_19_acs5 + male_20_24_acs5)/total_population_acs5)* 100,
         
         persons_per_household = total_population_acs5/households_total_acs5,
         
         persons_per_m2 = total_population_acs5/area_m2)|>
  
  # final cleaning
  drop_na(total_population_acs5)|>  # drop years that acs1 doesn't cover
  select(-acs1_vars, -geometry)  # remove acs1 pop estimates and geometry


saveRDS(acs_5_yearly_data, "data/acs_5_yearly_data.rds")
saveRDS(acs_1_yearly_data, "data/acs_1_yearly_data.rds")
saveRDS(mapping_yearly_data, "data/mapping_yearly_data.rds")
saveRDS(crime_data, "data/crime_data_merged.rds")