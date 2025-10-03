library(dplyr)
library(readr)

files <- list.files("downloads", full.names = TRUE)
crime_files <- files[!grepl("_officers", files)]
officer_files <- files[grepl("_officers", files)]

crime_data <- tibble(series = c(), month = c(), arson = c(), assaults = c(), 
                     burglary = c(), gta = c(), homicides = c(), larceny = c(), ori = c())

crime_data <- map_dfr(crime_files, function(file_path) {
  crime_type <- str_split(file_path, "[/._]")[[1]][3]
  ori <- str_split(file_path, "[/._]")[[1]][2]
  
  read.csv(file_path, colClasses = "character") |>   # force all as character
    slice(1) |> # only use actual counts row
    pivot_longer(
      cols = starts_with("X"), 
      names_to = "month", 
      values_to = "count"
    ) |>
    mutate(
      count = ifelse(is.na(count), 0, as.integer(count)), # turn back into numbers NA as 0
      crime_type = crime_type,
      ori = ori,
      month = my(sub("^X", "", month)),   # convert month string to Date
      year = year(month)
    )
})

# pivot crimes to columns 
crime_data <- crime_data |>
  pivot_wider(
    names_from = crime_type,
    values_from = count,
    id_cols = c(series, month, ori, year)
  )

officer_counts <- tibble(ori = c(), year = c(), officers = c())
for(file_path in officer_files){
  temp_file <- read.csv(file_path)|>
    pivot_longer(cols = where(is.numeric)|where(is.logical), names_to = "year", values_to = "officers")|>
    mutate(ori =  str_split(file_path, "[/._]")[[1]][2])|>
    mutate(year = as.numeric(sub("^X", "", year)))|>
    mutate(officers = if_else(officers == 0, NA, officers))|>
    select(ori, year, officers)
  # add in temp_file to total file
  officer_counts <- bind_rows(officer_counts, temp_file)
}

crime_data <- left_join(crime_data, officer_counts, by = join_by(ori, year))

write_csv(crime_data, "crime_data.csv")
