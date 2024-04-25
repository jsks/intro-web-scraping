#!/usr/bin/env Rscript

library(dplyr)
library(mapview)
library(sf)

florida <- readRDS("./florida_house_candidates.rds") |>
    mutate(last_name = sub(",.*$", "", Candidate)) |>
    select(last_name, Party, District)

liabilities <- readRDS("./liabilities.rds") |>
    left_join(florida)

aggregated.df <- group_by(liabilities, District) |>
    summarise(lower = log(mean(lower) + 1),
              upper = log(mean(upper) + 1))

districts <- st_read(dsn = "./districts/tlgdb_2021_a_us_legislative.gdb",
                     layer = "Current_Congressional_Districts") |>
    filter(substring(GEOID, 1, 2) == "12") |>
    mutate(District = substring(GEOID, 3, 4))

# Finally, after a long and painful journey we are ready to create a map
full.df <- full_join(aggregated.df, districts) |> st_as_sf()
mapview(full.df, zcol = "lower")
