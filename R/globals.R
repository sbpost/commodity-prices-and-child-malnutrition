#' A very simple function that is used as a central place to keep some global
#' settings, like a common color scale or a custom theme for charts.
#' @return A named list containing the settings. For instance, to access colors
#' call globals()$colors.
globals <- function() {

  gl_ls <- list(
    color_scale = scale_color_gradient(low = "blue", high = "red"),
    colors = c(
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
  )

  return(gl_ls)
}

#' Simple convenience function that produces a dataframe with the periods
#' the panel-survey (in TZA) conducted field work.
#' @return Dataframe containing the start- and end-dates of survey fieldwork.
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
