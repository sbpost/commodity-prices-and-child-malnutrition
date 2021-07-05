#' Function that creates
#'
#' @param ssa_exports_tbl Dataframe containing product-level exports
#' for SSA.
#' @return A figure showing yearly product exports of Tanzania.
#' The ten most important products between 2000-2015 are highlighed.

create_export_charts <- function(ssa_exports_tbl) {

  ## A) Get the products that on average (in the period 2000-2015) have the
  ## highest share of total exports in TZA.

  ## B) Create a dataframe with only these most important products.
  ## This data is used to create the colored and named lines in the figure.

  ## C) Add the rest of the observations for the non-colored lines.

  ## D) Add lines that signal the survey years.

  ## Filter exports:
  tz_exports_tbl <-
    ssa_exports_tbl %>%
    filter(location_code == "TZA") %>%
    filter(year %in% 2000:2015)

  ## Grab the field-work table (used to create lines for survey period)
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

  ## Filter product-year export data to only the ten most
  ## important products in the period.
  most_tza_exported_tbl <-
    tz_exports_tbl %>%
    semi_join(most_tz_exported_products, by = "hs_product_code")

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
    ## Add colored lines + points for most exported products
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
    coord_cartesian(ylim=c(0, 5.0e+08)) + ## Zoom plot
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    xlab("Year") +
    ylab("Export value (current USD)") +
    scale_color_manual(
      name = "",
      values = globals()$colors) +
    theme_bw()

  return(tza_export_chart)

}
