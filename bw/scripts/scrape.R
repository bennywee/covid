library(tidyverse)
library(lubridate)
library(here)

setwd('/Users/benjaminwee/Documents/projects/covid/bw/')
# Get the latest data------------------------------------------------------------------------

## BW: Made some adjustments to this
get_ecdc_csv <- function(url = "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                         date = lubridate::today(), 
                         writedate = lubridate::today(), 
                         fname = "ecdc-cumulative-",
                         ext = "csv") {
  
  target <- url
  message("target: ", target)
  
  destination <- fs::path(paste0(getwd()), paste0(fname, writedate), ext = ext)
  message("saving to: ", destination)
  q
  tf <- tempfile(fileext = ext)
  curl::curl_download(target, tf)
  fs::file_copy(tf, destination, overwrite = TRUE)
  
  janitor::clean_names(readr::read_csv(tf))
}  

# Look up tables for countries and codes----------------------------------------------------------
iso3_cnames <- read_csv("data/countries_iso3.csv")
iso2_to_iso3 <- read_csv("data/iso2_to_iso3.csv")
cname_table <- left_join(iso3_cnames, iso2_to_iso3, by = 'iso3')

# Scrape raw table
covid_raw <- get_ecdc_csv()

covid <- covid_raw %>%
  mutate(date = lubridate::dmy(date_rep),
         iso2 = geo_id)

covid <- left_join(covid, cname_table)

cname_xwalk <- read_csv("data/ecdc_to_iso2_xwalk.csv",
                        na = "")
# Coalesce nulls-------------------------------------------------------
coalesce_join <- function(x, y, 
                          by = NULL, 
                          suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- dplyr::union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}

covid1 <- coalesce_join(covid, 
                       cname_xwalk, 
                       by = "geo_id", 
                       join = dplyr::left_join)

focus_cn <- c("CHN", "DEU", "GBR", "USA", "IRN", "JPN",
              "KOR", "ITA", "FRA", "ESP", "CHE")


# Create main dataset
cov_case_curve <- covid %>%
  select(date, cname, iso3, cases, deaths) %>%
  drop_na(iso3) %>%
  group_by(iso3) %>%
  arrange(date) %>%
  mutate(cu_cases = cumsum(cases), 
         cu_deaths = cumsum(deaths)) %>%
  filter(cu_cases > 99) %>%
  mutate(days_elapsed = date - min(date),
         end_label = ifelse(date == max(date), cname, NA),
         end_label = recode(end_label, `United States` = "USA",
                            `Iran, Islamic Republic of` = "Iran", 
                            `Korea, Republic of` = "South Korea", 
                            `United Kingdom` = "UK"),
         cname = recode(cname, `United States` = "USA",
                        `Iran, Islamic Republic of` = "Iran", 
                        `Korea, Republic of` = "South Korea", 
                        `United Kingdom` = "UK"))

# Write to CSV
write.csv(cov_case_curve, '/Users/benjaminwee/Documents/projects/covid/bw/data/cov_case_curve2.csv')
