#!/usr/bin/env Rscript
#
# Simply webscraping script that fetches the current members of the
# Norwegian parliament and collects their party affiliation, electoral
# region, and seniority in parliament.
###

library(lubridate)
library(rvest)

# We'll ignore members who are technically on leave currently
clean_text <- \(s) gsub("[\r\n]", "", s) |> sub("\\(.*$", "", x = _) |> trimws()

# Grab the full list of members of the Norwegian parliament
html <- read_html("https://www.stortinget.no/en/In-English/Members-of-the-Storting/current-members-of-parliament/")

names <- html_elements(html, "tbody td:nth-child(2)") |> html_text() |> clean_text()
parties <- html_elements(html, "tbody td:nth-child(3)") |> html_text() |> clean_text()

parliament <- data.frame(name = names, party = parties)

# Now we'll iterate over the profile page for each member
profiles <- html_elements(html, "tbody td:nth-child(2) a:first-child") |> html_attr("href")
parliament$region <- NA_character_
parliament$seniority <- as.duration(NA)

for (i in seq_along(profiles)) {
    paste("Fetching profile data for", names[i]) |> print()
    profile <- read_html(paste0("https://www.stortinget.no", profiles[i]))

    region <- html_element(profile, "dd span") |> html_text()
    seniority <- html_element(profile, "dd:last-child") |> html_text() |> duration()

    parliament$region[i] <- region
    parliament$seniority[i] <- seniority

    Sys.sleep(runif(1))
}

###
# "Analysis"
summary(parliament$seniority)
as.numeric(parliament$seniority, "days") |> hist()

# Who is the longest serving member?
filter(parliament, !is.na(seniority), seniority == max(seniority, na.rm = T))

# Who is the shortest serving member?
filter(parliament, !is.na(seniority), seniority == min(seniority, na.rm = T))

# Average seniority by party
group_by(parliament, Party) |>
    summarise(AvgSeniority = as.numeric(seniority, "days") |> mean(na.rm = T))

# Average seniority by region
group_by(parliament, region) |>
    summarise(AvgSeniority = as.numeric(seniority, "days") |> mean(na.rm = T))

# Save, save, save!
saveRDS(parliament, "norwegian_parliament.rds")
