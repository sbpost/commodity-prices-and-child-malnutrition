 # ssa_exports_tbl <- tar_read(ssa_exports_tbl)

create_export_charts <- function(ssa_exports_tbl) {

  ## 1: Create figure of Tanzania's most important exported products.
  ## A) Get the products that on avaerage (in the period - 1995-2018) have the
  ## highest share of the total exports in TZ.

  ## B) Create a dataframe with only these most important products.
  ## This data is used to create the colored and named lines in the figure.

  ## C) Add the rest of the observations for the non-colored lines.

  ## D) Add lines that signal the survey years.

  tz_exports_tbl <-
    ssa_exports_tbl %>%
    filter(location_code == "TZA") %>%
    filter(year %in% 2000:2015)

  field_work_tbl <- get_field_work_table()
  ## A -----------------------------------------------------
  ## Get average export share in the period
  mean_tz_export_share <-
    tz_exports_tbl %>%
    group_by(hs_product_code) %>%
    summarize(
      mean_exp_share = mean(product_export_share)
    ) %>%
    ungroup()

  ## Grab the 10 most important products
  most_tz_exported_products <-
    mean_tz_export_share %>%
    arrange(desc(mean_exp_share)) %>%
    slice(1:10)

  ## B, C, D -------------------------------------------------
  most_tza_exported_tbl <-
    tz_exports_tbl %>%
    semi_join(most_tz_exported_products, by = "hs_product_code")

colors <-  c(
    "#a6cee3",
    "#1f78b4",
    "#b2df8a",
    "#33a02c",
    "#fb9a99",
    "#e31a1c",
    "#fdbf6f",
    "#ff7f00",
    "#cab2d6",
    "#6a3d9a"
  )

  tza_export_chart <-
  ggplot() +
    ## Add bacgkround lines of all other products
    geom_line(
      data = anti_join(
        tz_exports_tbl,
        most_tz_exported_products,
        by = "hs_product_code"
      ),
      aes(
        x = year,
        y = export_value,
      group = hs_product_description
        ),
      color = "gray",
      alpha = 0.5,
    ) +
    ## Add lines for survey years
    geom_vline(data = field_work_tbl, aes(xintercept = survey), size = 1.2, color = "#594949") +
    ## Add colored lines of most exported products
    geom_line(
      data = most_tza_exported_tbl,
      aes(
        x = year,
        y = export_value,
        group = hs_product_description,
        color = hs_product_description,
        )
    ) +
    geom_point(
      data = most_tza_exported_tbl,
      aes(
        x = year,
        y = export_value,
        group = hs_product_description,
        color = hs_product_description
      )
    ) +
    coord_cartesian(ylim=c(0, 5.0e+08)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    xlab("Year") +
    ylab("Export value (current USD)") +
    scale_color_manual(
      name = "",
      values = colors) +
    theme_bw()


return(tza_export_chart)

}
