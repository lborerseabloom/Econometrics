library(dplyr)
library(tidyr)
library(sf)

crime_data <- readRDS("data/crime_data.rds")
mapping <- readRDS("data/mapping.rds")

vars_1 <- readRDS("data/vars_1.rds")
acs_1yr_ts <- readRDS("data/acs_1yr_ts.rds")

acs_5yr_ts <- readRDS("data/acs_5yr_ts.rds")


### final crime data file with all variables ###
crime_data <- crime_data|>
  left_join(mapping, by = join_by(ori), relationship = "many-to-many")|> # join in mapping variables
  left_join(acs_1yr_ts, by = join_by(GEOID, year))|> # join to acs1 data by county and year
  left_join(acs_5yr_ts, by = join_by(GEOID, year))|> # join to acs5 data by county and year
  distinct(series, month, .keep_all = TRUE)|> # drop some duplicates left over
  group_by(month)|>
  drop_na(homicides)|> # drop na homicide years, primarily from smaller depts early on in the data
  ungroup()


### collapse main crime data for mapping acs_5 vs acs_1 vs homicides by county ###
mapping_yearly_data <- crime_data |>
  group_by(county, year)|>
  mutate(across(c(arson, assaults, burglary, gta, homicides, larceny, rape, robbery), sum))|>
  select(-series, -month, -ori, -type)|>
  distinct()|>
  st_as_sf() # save as sf object for mapping


### create final df for modeling using acs 1 data ###
acs_1_yearly_data <- crime_data |>
  
  # summarize crimes and officer counts by county and year to condense from monthly data
  group_by(county, year)|>
  mutate(across(c(arson, assaults, burglary, gta, homicides, larceny, rape, robbery), sum),
         officers = mean(officers, na.rm = TRUE))|>
  select(-series, -month, -ori, -type)|>
  distinct()|>
  
  # linear interpolation to fill missing values for 2020 years and 2018 education estimates for geoid 27019  
  ungroup(year)|>
  arrange(year)|>
  mutate(across(all_of(names(vars_1)), ~ zoo::na.approx(., x = year, na.rm = FALSE))) |>
  
  # calculate aggregate stats for final model
  ungroup()|>
  mutate(pct_lessthan_5kincome = (households_income_lt5k / households_total) * 100,
         
         pct_highschool_or_greater = (edu_hs_grad + edu_some_college_lt1yr + 
                                        edu_some_college_gt1yr + edu_assoc_degree + 
                                        edu_bachelors + edu_masters + 
                                        edu_professional_degree + edu_doctorate) / edu_total_25plus * 100,
         
         pct_children_missing_parents = (children_w_mother_only + children_w_father_only + children_no_parents) / children_total * 100,
         
         periods = year - min(acs_1yr_ts$year),
         
         post_covid = ifelse(year>=2020, 1, 0),
         
         msp_main_counties = ifelse(county %in% c("RAMSEY", "HENNEPIN"), 1, 0),
         
         pct_non_white = 1 - (white_population / total_population_1))|>
  
  # final cleaning
  filter(county != "CLAY")|>     # first acs1 year for clay is 2020, interpolation for 2020 wont work 
  drop_na(total_population_1)|>  # drop years that acs1 doesn't cover
  select(-total_population_5, -geometry)  # remove acs5 pop estimates and geometry


saveRDS(acs_1_yearly_data, "data/acs_1_yearly_data.rds")
saveRDS(mapping_yearly_data, "data/mapping_yearly_data.rds")
saveRDS(crime_data, "data/crime_data.rds")


