#!/usr/bin/env Rscript

library(dplyr)
library(readxl)

# These are the official results for the 2020 election
df <- read_xlsx("./federalelections2020.xlsx", sheet = 13) |>
    select(State = `STATE ABBREVIATION`, District = DISTRICT,
           Candidate = `CANDIDATE NAME`, Party = PARTY,
           Results = `GENERAL %`, Winner = `GE WINNER INDICATOR`)

# Grab only candidates from Florida and ignore write-ins
florida <- filter(df, State == "FL", !is.na(Candidate), !is.na(Results),
                  Party != "W") |>
    mutate(Winner = ifelse(!is.na(Winner) & Winner == "W", "W", "L") |> as.factor())

saveRDS(florida, "florida_house_candidates.rds")
