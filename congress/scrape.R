#!/usr/bin/env Rscript
#
# Download financial disclosure forms for all Florida House candidates
# in 2020.
###

library(dplyr)
library(httr)
library(rvest)

last_name <- \(name) sub(",.*$", "", name)

fetch <- function(url, body, incumbent = T) {
    resp <- POST(url, body = body, content_type("application/x-www-form-urlencoded"))
    html <- content(resp)

    rows <- html_elements(html, "tr[role=row]")
    candidates <- html_elements(rows, "a")
    type <- html_elements(rows, "td[data-label=Filing]") |> html_text()
    if (isTRUE(incumbent)) {
        year <- html_elements(rows, "td[data-label='Filing Year']")
    } else {
        year <- html_elements(rows, "td[data-label='Election Year']")
    }

    data.frame(candidate = html_text(candidates) |> trimws(),
               year = html_text(year),
               link = html_attr(candidates, "href"),
               type = type,
               pdf = NA_character_)
}

dir.create("disclosures", showWarnings = F)

florida <- readRDS("./florida_house_candidates.rds") |>
    mutate(last_name = last_name(Candidate))

###
# Non-incumbent candidates
candidate_search <- "https://disclosures-clerk.house.gov/FinancialDisclosure/ViewCandidateSearchResult"
candidate_body <- "LastName=&ElectionYear=2020&State=FL&District=&__RequestVerificationToken=CfDJ8BIvSIJhk-pEju6u01PS0NI5lWtTVnzCEcFgvuOhITnJhPtXFRPfqfBxojJeNfua9yHbXTlbRRztn3NzP_4dzZT9qIl5bQ5-1Fb-h8m3COB5FDjiXZR3TEyY8eSdim6pwnOSoihb7giwyjMueOpozEk"

candidates <- fetch(candidate_search, candidate_body, incumbent = F) |>
    mutate(year = sub("^.*/(\\d{4})/.*$", "\\1", link)) |>
    filter(type == "Candidate/Misc") |>
    group_by(candidate) |>
    filter(n() == 1 | year == 2020)

###
# Sitting members of the House of Representatives
member_search <- "https://disclosures-clerk.house.gov/FinancialDisclosure/ViewMemberSearchResult"

# Some sitting members did not file for 2020, so also check 2019. This
# would be the case for when an incumbent lost the 2020 election for
# example.
member_body <- sprintf("LastName=&FilingYear=%d&State=FL&District=&__RequestVerificationToken=CfDJ8BIvSIJhk-pEju6u01PS0NIcR_Pg7ukEHbOsY6aG0g-uxvBbDyAE_-W1innfDiuIyjwfH5pV6DtPxr9HIwM27UwHI-HQhc797AjQti2xxveHpPpbkFJ6XPDXITk2nsrWC_bjhcBALRhiczZYlz5UCRc", c(2019, 2020))

members <- lapply(member_body, \(body) fetch(member_search, body)) |>
    bind_rows() |>
    filter(type == "FD Original")

###
# Join both incumbent and non-incumbent lists together and filter out
# primary challengers
full.df <- group_by(members, candidate) |>
    filter(year == max(year)) |>
    bind_rows(candidates) |>
    mutate(last_name = last_name(candidate)) |>
    filter(last_name %in% florida$last_name)

missing <- setdiff(florida$last_name, full.df$last_name)
sprintf("Finished with %d candidates, and %d missing", nrow(full.df), length(missing)) |>
    print()

###
# Loop through each PDF link and store a local copy
for (i in 1:nrow(full.df)) {
    candidate <- full.df$candidate[i]
    resource <- full.df$link[i]

    paste("Downloading disclosure form for", candidate) |> print()

    url <- paste0("https://disclosures-clerk.house.gov/", resource)
    resp <- GET(url)

    full.df$pdf[i] <- file.path("disclosures", basename(resource))
    writeBin(content(resp), full.df$pdf[i])

    Sys.sleep(runif(1))
}

# Finally, let's also add in the districts for each candidate
final.df <- select(florida, last_name, District) |>
    right_join(full.df)
stopifnot(!is.na(final.df$District))

saveRDS(final.df, "scraped_candidate_list.rds")
