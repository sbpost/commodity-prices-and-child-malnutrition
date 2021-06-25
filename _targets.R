library(targets)

## Source all scripts containing functions
lapply(list.files(path = "./R/", full.names = TRUE), source)

## Load libraries:
options(tidyverse.quiet = TRUE)

library(tidyverse)
library(haven) # Read .dta
library(countrycode)

list(
  ## Read World Bank country meta-data
  tar_target(
    wb_country_meta_file,
    "./data/world_bank_country_metadata.csv",
    format = "file"
  ),

  tar_target(
    wb_country_meta_raw,
    read_csv(wb_country_meta_file)
  ),

  ## Read + clean commodity prices
  tar_target(
    commodity_prices_file,
    "./data/us_commodityprice_m_02372543400211.csv",
    format = "file"
  ),

  tar_target(
    commodity_prices_raw,
    read_csv(commodity_prices_file, skip = 2)
  ),

  tar_target(
    commidity_prices_tbl,
    clean_commidity_prices(commodity_prices_raw)
  ),

  ## Read + clean exports
  tar_target(
    exports_file,
    "./data/country_hsproduct4digit_year.dta",
    format = "file"
  ),

  tar_target(
    exports_raw,
    read_dta(exports_file)
  ),

  tar_target(
    ssa_exports_tbl,
    clean_exports(exports_raw, wb_country_meta_raw)
  ),

  ## Create figures:
  tar_target(
    export_charts,
    create_export_charts(ssa_exports_tbl)
  )


)
