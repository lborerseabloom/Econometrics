library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readxl)
library(tidyverse)

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
#alc_tax <- alc_tax[,1:4] 
alc_tax <- rename(alc_tax, 
                  year=YEAR, 
                  county=`COUNTY LIQUOR SALES & TAX (ON & OFF-SALE)`, 
                  sales=`LIQUOR SALES`,
                  alc_taxes=`LIQUOR GROSS RECEIPTS TAX (AT 2.5%)`)|>
  drop_na(year)|>
  filter(! county %in% c("NON-MINNESOTA CO","MN UNKNOWN COUNTY"))|>
  mutate(county = ifelse(county == "MCLEOD", "MC LEOD", county),
         county = ifelse(county == "ST LOUIS", "ST. LOUIS", county))|>
  mutate(year = as.integer(year))|>
  # alc_tax should now have years*counties of rows, 16*87=1392 in this case
  group_by(county)|>
  complete(year = min(year):2024) |>
  arrange(year, .by_group = TRUE)|>
  # linear interpolation for 2024, using lm to predict 2024 from all years
  # mutate(
  #   sales = {
  #     # fit the model
  #     model <- lm(sales ~ year, data = pick(year), na.action = na.exclude)
  #     # if na fill with models prediction
  #     ifelse(
  #       is.na(sales),
  #       predict(model, newdata = data.frame(year = year)),
  #       sales
  #     )
  #   }
  # )
  # carry forward 2022-2023 change for slope as linear interpolation isn't accurate
  mutate(
    # calculate slope
    slope = alc_taxes - lag(alc_taxes),
    
    # avg slope over normally trended years
    avg_slope = mean(
      slope[(year %in% 2011:2019) | (year == 2023)],
      na.rm = TRUE),
    
    # replace 2024 with that average + 2023
    alc_taxes = if_else(
      year == 2024 & is.na(alc_taxes),
      alc_taxes[year == 2023] + avg_slope,
      alc_taxes
    )
  ) |>
  ungroup() |>
  select(-slope, -avg_slope)

#alc alc_taxes and sales are almost perfectly correlated
alc_tax|>
  group_by(year)|>
  summarise(sales = sum(sales))|>
  ggplot(aes(x = year, y=sales))+
  geom_point()

alc_tax|>
  group_by(year)|>
  summarise(alc_taxes = sum(alc_taxes))|>
  ggplot(aes(x = year, y=alc_taxes))+
  geom_point()

# drop sales before saving to pass on up the line
alc_tax <- alc_tax[,c(1,2,4)]

saveRDS(alc_tax, "data/alc_data.rds")


