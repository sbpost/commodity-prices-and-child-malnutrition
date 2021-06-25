
# exports_raw <- tar_read("exports_raw")
# country_meta <- tar_read(wb_country_meta_raw)

clean_exports <- function(exports_raw, country_meta) {

  country_meta <-
    country_meta %>%
    select(`Country Code`, Region)

  ## Add region definitions to exports:
  exports_raw <-
  exports_raw %>%
    left_join(country_meta, by = c("location_code" = "Country Code"))

  ## Get export intensity = how much of total exports for a country is contributed
  ## by a given product
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
      location_code, year, hs_product_code, export_value, region = Region, product_export_share, country_total_export
    )

  ssa_exports_tbl <-
  exports_tbl %>%
    filter(region == "Sub-Saharan Africa")

  return(ssa_exports_tbl)
}
