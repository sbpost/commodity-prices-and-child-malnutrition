library(targets)

## Source all scripts containing functions
lapply(list.files(path = "./R/", full.names = TRUE), source)

## Load libraries:
options(tidyverse.quiet = TRUE)

library(tidyverse)
library(haven) # Read .dta
library(readxl) # Read .xls(x)
library(lubridate) # handling dates
library(ggthemes) # to get the Stata theme

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


  ## Read + clean UNCTAD commodity prices
  tar_target(
    unctad_commodity_prices_file,
    "./data/us_commodityprice_m_02372543400211.csv",
    format = "file"
  ),

  tar_target(
    unctad_commodity_prices_raw,
    read_csv(unctad_commodity_prices_file, skip = 2)
  ),

  tar_target(
    unctad_commodity_prices_tbl,
    clean_unctad_commidity_prices(unctad_commodity_prices_raw)
  ),
  ## Read + clean WB commodity prices
  tar_target(
    wb_commodity_prices_file,
    "./data/CMO-Historical-Data-Monthly.csv",
    format = "file"
  ),
  tar_target(
    wb_commodity_prices_raw,
    read_csv(wb_commodity_prices_file)
  ),
  tar_target(
    wb_commodity_prices_tbl,
    clean_wb_commidity_prices(wb_commodity_prices_raw)
  ),


  ## Read export code descriptions
  tar_target(
    hs_product_meta_file,
    "./data/UN Comtrade Commodity Classifications.xlsx",
    format = "file"
  ),

  tar_target(
    hs_product_meta_raw,
    read_excel(hs_product_meta_file)
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
    clean_exports(
      exports_raw,
      wb_country_meta_raw,
      hs_product_meta_raw
    )
  ),

  ## Create figures:
  tar_target(
    tza_export_chart,
    create_export_charts(
      ssa_exports_tbl
    )
  ),

  tar_target(
    wb_price_chart_ls,
    create_wb_charts(
      wb_tbl = wb_commodity_prices_tbl
    )
  ),

  tar_target(
    unctad_price_chart_ls,
    create_unctad_charts(
      unctad_prices_tbl = unctad_commodity_prices_tbl
    )
  ),

  tar_target(
    selected_commodity_chart_ls,
    create_selected_commodity_charts(
      unctad_prices_tbl = unctad_commodity_prices_tbl,
      wb_prices_tbl = wb_commodity_prices_tbl
    )
  ),

  ## Write charts
  tar_target(
    figs2file,
    write_figures(
      wb_chart_ls = wb_price_chart_ls,
      unctad_chart_ls = unctad_price_chart_ls,
      selected_commodity_charts_ls = selected_commodity_chart_ls,
      tza_exports_chart = tza_export_chart
    )
  )
)
