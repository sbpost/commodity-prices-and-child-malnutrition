#' Function that cleans the product-level export data and attaches country-
#' regions and product descriptions (names).
#' @param exports_raw Dataframe containing exports data from UN COMTRADE
#' (supplied by Harvard Dataverse).
#' @param country_meta Dataframe containing country meta data from the World
#' Bank. This dataframe is used just to filter countries in SSA.
#' @param product_meta Dataframe containing product descriptions of the product
#' in the export data. The original exports data only has product codes, not
#' human-readable names.
#' @return Dataframe containing product-level exports from countries in SSA.

clean_exports <- function(exports_raw, country_meta, product_meta) {

  country_meta <-
    country_meta %>%
    select(`Country Code`, Region)

  hs92_4digit_descriptions <-
    product_meta %>%
    ## Grab only HS88/92
    filter(Classification == "H0") %>%
    ## grab only 4 digit codes
    filter(Level == "4") %>%
    select(
      code = Code,
      hs_product_description = Description
    )

  ## Add region definitions to exports:
  exports_raw <-
  exports_raw %>%
    left_join(country_meta, by = c("location_code" = "Country Code"))

  ## Add product descriptions to exports
  exports_raw <-
    exports_raw %>%
    left_join(hs92_4digit_descriptions, by = c("hs_product_code" = "code"))

  ## Remove service-exports:
  service_exports <- c(
    "XXXX", # unspecified
    "unspecified", # ditto
    "travel",
    "transport",
    "ict",
    "financial",
    "9999" # unspecified
  )

  exports_raw <-
    exports_raw %>%
    filter(!hs_product_code %in% service_exports)

  ## Get export intensity = how much of total exports for a country is contributed
  ## by a given product in a given year
  total_exports_tbl <-
    exports_raw %>%
    group_by(location_code, year) %>%
    summarise(country_total_export = sum(export_value))

  exports_tbl <-
    exports_raw %>%
    ## add total exports in country-year
    left_join(total_exports_tbl, by = c("year", "location_code")) %>%
    mutate(
      product_export_share = export_value / country_total_export
    ) %>%
    select(
      location_code,
      year,
      hs_product_code,
      hs_product_description,
      export_value,
      region = Region,
      product_export_share,
      country_total_export
    )

  ssa_exports_tbl <-
  exports_tbl %>%
  filter(region == "Sub-Saharan Africa")

  return(ssa_exports_tbl)
}
