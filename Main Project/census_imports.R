library(dplyr)
library(tidyr)
library(purrr)
library(tidycensus)
library(readr)

# cache shape files to avoid slow downloads
options(tigris_use_cache = TRUE)

### define variables to pull from census surveys ###
vars <- c(
  total_population          = "B01003_001",  # Total population
  white_population          = "B02001_002",  # White population
  
  # young male population
  male_15_19 = "B01001_006",  
  male_20_24 = "B01001_007",
  
  # Income
  median_household_income   = "B19013_001",  # Median household income (dollars)
  households_total          = "B19001_001",  # Total households
  households_income_lt5k    = "B19001_002",  # Households with income < $5,000
  
  # Education - only available from 2008 on
  edu_total_25plus          = "B15003_001",  # Total population 25 years and over
  edu_hs_grad               = "B15003_017",  # High school graduate
  edu_some_college_lt1yr    = "B15003_018",
  edu_some_college_gt1yr    = "B15003_019",
  edu_assoc_degree          = "B15003_020",
  edu_bachelors             = "B15003_021",
  edu_masters               = "B15003_022",
  edu_professional_degree   = "B15003_023",
  edu_doctorate             = "B15003_024",
  
  # Children living arrangements
  children_total            = "B09001_001",  # Total population under 18 years
  children_w_mother_only    = "B09001_004",  # Living with mother only
  children_w_father_only    = "B09001_005",  # Living with father only
  children_no_parents       = "B09001_006"   # Not living with either parent
)

vars_dec <- c(
  total_population      = "P1_001N"      # Total population
)


### 5 year acs estimates ###
# Pull ACS data for all variables at county level for MN
years = c(2018, 2023) # Want to compare pre covid 2014-18 vs 2019-23 
acs_5yr_ts <- map_df(years, function(y) {
  get_acs(
    geography = "county",
    survey = "acs5",
    variables = vars,
    state = "MN",
    year = y,
    geometry = TRUE
  ) |>
    select(GEOID, variable, estimate) |>
    mutate(
      GEOID = as.character(GEOID),
      year = y
    ) |>
    pivot_wider(names_from = variable, values_from = estimate)
})


### 1 year acs estimates ###
# 2020 estimates https://www.census.gov/programs-surveys/acs/data/experimental-data/1-year.html
years = setdiff(2008:2024, 2020) # acs 1 with 2020 missing due to covid bias
acs_1yr_ts <- map_df(years, function(y) {
  get_acs(
    geography = "county",
    survey = "acs1",
    variables = vars,
    state = "MN",
    year = y,
    geometry = FALSE
  ) |>
    select(GEOID, variable, estimate) |>
    mutate(
      GEOID = as.character(GEOID),
      year = y
    ) |>
    pivot_wider(names_from = variable, values_from = estimate)
})


### decennial true counts from April 1 2020 to replace missing year of 2020 acs1 data ###
# wont have other variables, but does have TRUE pop
decennial_pop <- get_decennial(
  geography = "county",
  variables = vars_dec,
  year = 2020,
  geometry = FALSE,
  state = "MN"
)|>
  select(GEOID, variable, value) |>
  mutate(
    GEOID = as.character(GEOID),
    year = 2020
  ) |>
  pivot_wider(names_from = variable, values_from = value)

# add in decennial pop as a sub for 2020 counts
acs_1yr_ts <- bind_rows(acs_1yr_ts, decennial_pop)|>
  add_count(GEOID) |>       # count how many times each GEOID appears
  filter(n > 1) |>           # remove counties not in acs 1 survey
  select(-n)                  # drop the helper column


### bring in and join ai generated csvs, they look pretty accurate ###
# just some small refactoring to join the data frames
mapping <- read.csv("data/ai_mappings.csv") |> 
  mutate(GEOID = as.character(GEOID))

alc_effects <- read.csv("data/ai_alcohol_effects.csv")|>
  rename(county = Location)|>
  mutate(county = toupper(county))

# pull out geometry for mapping from the 5 year acs data
county_geometry <- acs_5yr_ts |>
  select(GEOID, geometry) |>
  distinct(GEOID, .keep_all = TRUE)|>
  mutate(area_m2   = as.numeric(st_area(geometry))) # extract area

# drop geometry to avoid duplicate columns
acs_5yr_ts <- acs_5yr_ts|> 
  sf::st_drop_geometry()

# join all of the data needed for mapping onto the main crime data
mapping <- left_join(mapping, county_geometry)|>
  left_join(alc_effects)

# use RDS to pass geometry without breaking write_csv
saveRDS(vars, "data/vars.rds")
saveRDS(mapping, "data/mapping.rds")
saveRDS(acs_1yr_ts, "data/acs_1yr_ts.rds")
saveRDS(acs_5yr_ts, "data/acs_5yr_ts.rds")