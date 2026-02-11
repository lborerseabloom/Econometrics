library(dplyr)
library(stringr)
library(purrr)
library(readxl)

files <- list.files("data/MN Liquor", full.names = TRUE)

# bind all files together
alc_tax <- map_dfr(files, function(file_path){
  # use the two separate read methods in readxl
  if (grepl(".xlsx$", file_path, ignore.case = TRUE)) {
    read_xlsx(file_path)
  } else {
    read_xls(file_path)
  }
})
# drop useless columns
alc_tax <- alc_tax[,1:3] 
alc_tax <- rename(alc_tax, 
                  year=YEAR, 
                  county=`COUNTY LIQUOR SALES & TAX (ON & OFF-SALE)`, 
                  sales=`LIQUOR SALES`)|>
  drop_na(year)|>
  filter(! county %in% c("NON-MINNESOTA CO","MN UNKNOWN COUNTY"))
# alc_tax should now have years*counties of rows, 16*87=1392 in this case
# since 2024 isn't out yet we have to impute here

# need linear interpolation for 2014, so many years it should be fine to impute here
ungroup()|>
  group_by(county)|>
  arrange(year, .by_group = TRUE)|>
  mutate(across(all_of(names(acs1_vars)), ~ zoo::na.approx(., x = year, na.rm = FALSE))) |>