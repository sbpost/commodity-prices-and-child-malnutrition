clean_unctad_commidity_prices <- function(unctad_prices_raw) {

  ## Format UNCTAD prices ----------------------------------
  unctad_prices_tbl <-
    unctad_prices_raw %>%
    rename("commodity" = PERIOD) %>%
    ## Remove empty rows (due to format in raw data)
    filter(!commodity %in% c("COMMODITY")) %>%
    ## Reformat to long data (so periods are not columns, but rows)
    pivot_longer(-commodity, names_to = "original_period", values_to = "price") %>%
    ## change the period-column formating to date-type (months) rather than string
    ## This requires a bit of hacking around due to the poor formating (sometimes a . after months, someimes not + sometimes four-letter months, sometimes three)
    mutate(
      day = "01",
      month = str_sub(original_period, 1, 3),
      year = str_sub(original_period, -4, -1)
    ) %>%
    mutate(
      period = dmy(str_glue("{day}/{month}/{year}"))
    ) %>%
    ## Change the encoding to Latin1 in order to get all the string-formatting looking right (+ enable filtering)
    ## Again, who on earth prepped this UN data?
    mutate(
      commodity = parse_character(commodity, locale = locale(encoding = "Latin1"))
    ) %>%
    ## Remove all the non-numeric price values (sometimes missing is listed at "_", "..", "-")
    ## Replace with NA
    mutate(
      price_numeric = if_else(price %in% c("_", "..", "-"), NA_real_, as.numeric(price))
    )

  ## To see that the only prices that give price_numeric == NA, uncomment:
   ## unctad_prices_tbl %>%
     ## filter(is.na(price_numeric)) %>%
     ## pull(price) %>%
     ## unique()

  return(unctad_prices_tbl)
}

clean_wb_commidity_prices <- function(wb_prices_raw) {

  wb_prices_tbl <-
    wb_prices_raw %>%
    filter(!is.na(Period)) %>%
    pivot_longer(-Period, names_to = "commodity", values_to = "price") %>%
    filter(!is.na(price)) %>%
    mutate(
      price_numeric = ifelse(price == "..", NA_character_, price) %>% as.numeric()
    ) %>%
    mutate(
      period = str_replace(Period, "M", "/"),
      period = str_glue("{period}/01"),
      period = ymd(period)
    ) %>%
    rename(original_period = Period)

  return(wb_prices_tbl)

}
