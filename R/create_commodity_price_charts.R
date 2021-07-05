## Create WB Commodity price charts:
## wb_tbl <- tar_read(wb_commodity_prices_tbl)
## unctad_prices_tbl <- tar_read(unctad_commodity_prices_tbl)

create_unctad_charts <- function(unctad_prices_tbl) {
  ## Return-list that contains figures
  collection_ls <- list()

  ## Coffee
  tropical_beverages <-
    c(
      "Cocoa beans, average daily prices New York/London (¢/lb.)",
      "Coffee, Brazilian and other natural Arabicas, ex-dock USA (¢/lb.)",
      "Coffee, Colombian mild Arabicas, ex-dock US (¢/lb.)",
      "Coffee, other mild Arabicas, ex-dock EU (¢/lb.)",
      "Coffee, other mild Arabicas, ex-dock US (¢/lb.)",
      "Coffee, Robustas, ex-dock EU (¢/lb.)",
      "Coffee, Robustas, ex-dock US (¢/lb.)",
      "Tea, Kenya Mombasa/Nairobi, auction price ($/kg)"
    )

  sisal <-
    c(
      "Sisal, Tanzania/Kenya, n° 2 & 3 long, FOB ($/t)",
      "Sisal, Tanzania/Kenya, n° 3 & UG, FOB ($/t)"
    )

  food_oils <-
    c("Palm kernel oil, in bulk, Malaysia/Indonesia, CIF Rotterdam ($/t)",
      "Palm oil, in bulk, Malaysia/Indonesia, 5% FFA, CIF ($/t)",
      "Sunflower oil, in bulk, European Union, FOB N.W. European ports ($/t)",
      "Coconut oil, in bulk, Philippines/Indonesia, CIF Rotterdam ($/t)",
      "Groundnut oil, in bulk, any origin, CIF Rotterdam ($/t)")

  tobacco <- c("Tobacco, unmanufactured, US import unit value  ($/t)")

  mixed_food_crops <- c("Maize, Argentina, Rosario, Up River, FOB ($/t)",
                        "Maize, United States, n° 3 yellow, FOB Gulf ($/t)",
                        "Wheat, Argentina, Trigo Pan, Up River, FOB ($/t)",
                        "Wheat, United States, n° 2 Hard Red Winter (ordinary), FOB Gulf ($/t)",
                        "Bananas, Central and South America, FOT, US import price ($/kg)",
                        "Bananas, Central America, Main Brands (average of US East and West Coast prices), FOR (US$/kg)",
                        "Rice, Thailand, white milled, 5% broken, export price, FOB ($/t)")

  logging <-  c(
    "Plywood, Africa & SE Asia, Lauan, 3-ply, 91cmx182cmx4mm, wholesale Tokyo (¢/sheet)",
    "Tropical logs, Okoume (60% CI, 40% CE, 20% CS), West Africa, FOB ($/m3)",
    "Tropical logs, Sapele, high quality (loyal and marchand), FOB, Cameroon ($/m3)")
  ## Create charts on absolute prices: -----------------------------------------

  ## Beverage prices faceted:
  collection_ls$`tropical_beverages` <-
    unctad_prices_tbl %>%
    filter(commodity %in% tropical_beverages) %>%
    basic_facet_plot()

  ## sisal
  collection_ls$`sisal` <-
    unctad_prices_tbl %>%
    filter(commodity %in% sisal) %>%
    basic_facet_plot()

  collection_ls$`food_oils` <-
    unctad_prices_tbl %>%
    filter(commodity %in% food_oils) %>%
    basic_facet_plot()

  collection_ls$`other_tobacco_logging` <-
    unctad_prices_tbl %>%
    filter(commodity %in% c(logging, tobacco)) %>%
    basic_facet_plot()

  collection_ls$`mixed_food_crops` <-
    unctad_prices_tbl %>%
    filter(commodity %in% mixed_food_crops) %>%
    basic_facet_plot()

  return(collection_ls)

}



create_wb_charts <- function(wb_tbl) {

  ## Return-list that contains figures
  collection_ls <- list()

  ## Create df to put in survey dates
  field_work_tbl <- get_field_work_table()

  ## Filter data
  plotting_df <-
    wb_tbl %>%
    filter(commodity %in% c(
                            "Copper ($/mt)",
                            "Gold ($/troy oz)",
                            "Cotton, A Index ($/kg)",
                                        # "Coconut oil ($/mt)",
                            "Tea, Mombasa ($/kg)",
                            "Cocoa ($/kg)",
                            "Coffee, Arabica ($/kg)",
                            "Coffee, Robusta ($/kg)",
                            "Tobacco, US import u.v. ($/mt)"
                          ))


  ## CREATE FIGURES: ---------------------------------------  -
  figure_1_tbl <-
    plotting_df %>%
    filter(period >= ymd("1990/01/01")) %>%
    group_by(commodity) %>%
    arrange(desc(period)) %>%
    mutate(
      price_as_z = (price_numeric - mean(price_numeric)) / sd(price_numeric),
      diff_from_prev = price_numeric - lag(price_numeric),
      diff_from_prev_cat = ifelse(diff_from_prev >= 0, "positive", "negative")
    ) %>%
    ungroup()

  ## 1.1: With price as Y, colored by change from previous
  collection_ls$figure_1.1 <-
    ggplot() +
    geom_rect(
      data = field_work_tbl,
      aes(
        xmin = start,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "gray",
      alpha = 0.4,
      color = NA
    ) +
    geom_line(
      data = figure_1_tbl,
      aes(
        x = period,
        y = price_numeric,
        group = commodity,
        color = diff_from_prev_cat
      )
    ) +
    facet_wrap(vars(commodity), scales = "free") +
    ylab("Price (nominal USD)") +
    xlab("Time") +
    scale_color_manual(
      name = "",
      breaks = c("negative", "positive"),
      values = c("blue", "red"),
      labels = c("Price increase", "Price decrease")
    ) +
    theme_bw() +
    theme(legend.position = "bottom")

  ## 1.2: With price as Y, colored by Z
  collection_ls$figure_1.2 <-
    ggplot() +
    geom_rect(
      data = field_work_tbl,
      aes(
        xmin = start,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "gray",
      alpha = 0.4,
      color = NA
    ) +
    geom_line(
      data = figure_1_tbl,
      aes(
        x = period,
        y = price_numeric,
        group = commodity,
        color = price_as_z
      )
    ) +
    facet_wrap(vars(commodity), scales = "free") +
    ylab("Price (nominal USD)") +
    xlab("Time") +
    labs(color = "Price as Z-score") +
    theme_bw() +
    theme(legend.position = "bottom") +
  globals()$color_scale

  ## 1.3: With Z-score as Y, colored by Z
  collection_ls$figure_1.3 <-
    ggplot() +
    geom_rect(
      data = field_work_tbl,
      aes(
        xmin = start,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "gray",
      alpha = 0.4,
      color = NA
    ) +
    geom_line(
      data = figure_1_tbl,
      aes(
        x = period,
        y = price_as_z,
        group = commodity,
        color = price_as_z
      )
    ) +
    facet_wrap(vars(commodity), scales = "free") +
    ylab("Price as Z-score") +
    xlab("Time") +
    labs(color = "Price as Z-score") +
    theme_bw() +
    theme(legend.position = "bottom") +
    globals()$color_scale

  ## CREATE PRICE DEVIATION PLOT: ----------------------------------------
  ## This section creates plots where thel ine is colored by how much a given period's price
  ## is deviating from mean price in the previous X months.
  periods <- plotting_df$period %>% unique()
  period_prices_6 <- map_dfr(periods, get_interval_mean_prices, df = plotting_df, interval_length = 6)
  period_prices_12 <- map_dfr(periods, get_interval_mean_prices, df = plotting_df, interval_length = 12)
  period_prices_24 <- map_dfr(periods, get_interval_mean_prices, df = plotting_df, interval_length = 24)


  six_months_period_plotting_df <- prep_period_prices(plotting_df, period_prices_6)
  twelve_months_period_plotting_df <- prep_period_prices(plotting_df, period_prices_12)
  twentyfour_months_period_plotting_df <- prep_period_prices(plotting_df, period_prices_24)

  collection_ls$figure_2.1 <- plot_period_price(six_months_period_plotting_df)
  collection_ls$figure_2.2 <- plot_period_price(twelve_months_period_plotting_df)
  collection_ls$figure_2.3 <- plot_period_price(twentyfour_months_period_plotting_df)

  collection_ls$figure_2.4 <- plot_period_deviation(six_months_period_plotting_df)
  collection_ls$figure_2.5 <- plot_period_deviation(twelve_months_period_plotting_df)
  collection_ls$figure_2.6 <- plot_period_deviation(twentyfour_months_period_plotting_df)

  return(collection_ls)

}


prep_period_prices <- function(plotting_df, period_price_df) {
  plotting_df %>%
    left_join(period_price_df, by = c("commodity", "period" = "reference_period")) %>%
    mutate(
      ## price_as_z_score = (price_numeric - mean_price_in_interval) / sd_price_in_interval
      ## Indexed difference from mean in period (where mean is index 100)
      indexed_difference = price_numeric / mean_price_in_interval * 100
    ) %>%
    filter(period >= ymd("1990-01-01"))

}

plot_period_price <- function(plotting_period_price_df) {
  field_work_tbl <- get_field_work_table()

  ggplot() +
    geom_rect(
      data = field_work_tbl,
      aes(
        xmin = start,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "gray",
      alpha = 0.4,
      color = NA
    ) +
    geom_line(
      data = plotting_period_price_df,
      aes(
        x = period,
        y = price_numeric,
        group = commodity,
        color = indexed_difference
      )
    ) +
    facet_wrap(vars(commodity), scales = "free") +
    globals()$color_scale +
            labs(color = "Difference from previous period mean (index = 100)") +
            theme_bw() +
            ylab("Price (nominal USD)") +
            xlab("Time") +
            theme(legend.position = "bottom")
}

plot_period_deviation <- function(plotting_period_price_df) {
  field_work_tbl <- get_field_work_table()

  ggplot() +
    geom_rect(
      data = field_work_tbl,
      aes(
        xmin = start,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "gray",
      alpha = 0.4,
      color = NA
    ) +
    geom_line(
      data = plotting_period_price_df,
      aes(
        x = period,
        y = indexed_difference,
        group = commodity,
        color = indexed_difference
      )
    ) +
    facet_wrap(vars(commodity)) +
    labs(color = "Difference from previous period mean (index = 100)") +
    ylab("Difference from previous period mean (index = 100)") +
    xlab("Time") +
    theme_bw() +
    theme(legend.position = "bottom") +
    globals()$color_scale
}


get_interval_mean_prices <- function(current_period, df, interval_length) {
  ## I'm interested in quantifying how "large" a given price fluctuation is.
  ## One way is to take the deviation from the mean price across the period.
  ## However, this might mis-estimate the importance of price changes after
  ## a price hits a "new normal". For instance, if a price have been at certain
  ## level for a year, the population might have "gotten used" to this new price,
  ## and adjusted accordningly. When the price then changes, this new price
  ## might be closer to the overall mean price, but be very significant still
  ## for the population in question, because their income changes drastically.
  ## Using only the mean-deviation would underestimate the importance of such
  ## a period.

  ## This function calculates the mean price of the previous 'interval_length'
  ## months. First the

  ## This function is mapped unto the original data-frame and a range of periods.
  ## It returns a 1-row data frame for each iteration. This gives a list of
  ## 1-row data frames which is then reduced in the "outer" function to one
  ## dataframe containing all of the rows.

  ## I want the mean price of the previous X (interval_length) month NOT including the current period
                                        # interval_end <- ymd(current_period) - months(1)
  interval_end <- ymd(current_period) - months(1)
  interval_start <- interval_end - months(interval_length)

  mean_price_tbl <-
    df %>%
    ## Grab only periods within interval
    filter(period < current_period & period > interval_start) %>%
    ## Get the average price in the period
    group_by(commodity) %>%
    summarize(
      mean_price_in_interval = mean(price_numeric),
      ) %>%
    ## Add the reference period in order to join to main data afterwards
    mutate(reference_period = current_period) %>%
    ungroup()

  ## Return mean prices
  return(mean_price_tbl)
}

get_field_work_table <- function() {
  field_work_tbl <-
    tribble(
      ~ survey, ~ start, ~ end,
      ## https://microdata.worldbank.org/index.php/catalog/76
      2008, "2008-10-01", "2009-09-01",
      ## https://microdata.worldbank.org/index.php/catalog/1050
      2010, "2010-10-01", "2011-09-01",
      ## https://microdata.worldbank.org/index.php/catalog/2252
      2012, "2012-10-01", "2013-10-01"
    ) %>%
    mutate(
      start = ymd(start),
      end = ymd(end)
    )

  return(field_work_tbl)

}

basic_facet_plot <- function(df) {
  df %>%
    ggplot(aes(x = period,
               y = price_numeric,
               group = commodity,
               color = commodity)) +
    geom_line() +
    facet_wrap(vars(commodity), scales = "free") +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    ylab("Price") +
    xlab("Time")
}
