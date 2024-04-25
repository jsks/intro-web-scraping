#!/usr/bin/env Rscript
#
# Parse PDF financial disclosure forms for each candidate.
###

library(dplyr)
library(pdftools)

# There's a lot that can go wrong here, so let's be strict about
# warnings
options(warn = 2)

results <- list()
candidates <- readRDS("./scraped_candidate_list.rds")

for (i in 1:nrow(candidates)) {
    print(paste("Parsing financial disclosure form for", candidates$candidate[i]))
    file <- candidates$pdf[i]

    txt <- pdf_text(file)  |>
        strsplit(split = '\n') |>
        unlist() |>
        trimws() |>
        tolower()

    if (length(txt) == 0) {
        print(paste("Error parsing", basename(file)))
        next
    }

    start <- sapply(txt, \(line) grepl("schedule d|s\\s+d:", line)) |> which()
    end <- sapply(txt, \(line) grepl("schedule e|s\\s+e:", line)) |> which()

    tbl <- txt[(start+1):(end-1)]
    entries <- split(tbl, cumsum(nchar(tbl) == 0) + 1)

    amounts <- sapply(entries, function(row) {
        line <- paste(row, collapse = " ")
        if (grepl("\\$", line))
            sub("^.*\\$([0-9,]+).*\\$([0-9,]+).*$", "\\1 - \\2", line)
        else
            ""
    })

    if (any(nchar(amounts) > 0)) {
        ll <- sapply(amounts[nchar(amounts) > 0], \(entry) gsub(",", "", entry)) |>
            strsplit("-")

        df <- sapply(ll, as.numeric) |> t() |> as.data.frame()
        colnames(df) <- c("lower", "upper")
        df$candidate <- candidates$candidate[i]
    } else {
        df <- data.frame(candidate = candidates$candidate[i],
                         lower = 0,
                         upper = 0)
    }

    results[[i]] <- df
}

final.df <- bind_rows(results) |>
    mutate(last_name = sub(",.*$", "", candidate))

saveRDS(final.df, "liabilities.rds")
