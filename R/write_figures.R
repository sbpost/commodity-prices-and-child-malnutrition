#' A simple convenience function that writes all the figures to file.
#'
#' @param wb_chart_ls List containing the World Bank commodity price charts.
#' @param unctad_chart_ls List containing the UNCTAD commodity price charts.
#' @param selected_commodity_charts_ls List containing the commodity price charts with 5 selected prices.
#' @param tza_exports_chart The figure (ggplot object) containing Tanzanian
#' export chart.
#' @return A simple string ("OK") for dependency tracking.
write_figures <- function(wb_chart_ls, unctad_chart_ls, selected_commodity_charts_ls, tza_exports_chart) {

  wb_chart_path <- "./figures/wb-price-charts/"
  unctad_chart_path <- "./figures/unctad-price-charts/"
  selected_commodity_path <- "./figures/selected-commodities/"
  export_chart_path <- "./figures/"

  ## Write selected commodity charts (after email-instructions)
  sc_figures <- names(selected_commodity_charts_ls)

  map(
    sc_figures,
    write_wb_figs_to_file,
    figure_ls = selected_commodity_charts_ls,
    chart_path = selected_commodity_path
  )

  ## Write WB figures
  wb_figures <- names(wb_chart_ls)

  map(
    wb_figures,
    write_wb_figs_to_file,
    figure_ls = wb_chart_ls,
    chart_path = wb_chart_path
  )

  ## Write UN figures
  unctad_figures <- names(unctad_chart_ls)

  map(
    unctad_figures,
    write_unctad_figs_to_file,
    figure_ls = unctad_chart_ls,
    chart_path = unctad_chart_path
    )

  ## Save export chart
  ggsave(
    filename = str_glue("{export_chart_path}/tza_exports.png"),
    plot = tza_exports_chart + theme_bw(),
    height = 25,
    width = 40,
    units = "cm",
    type = "cairo",
    dpi = 300
    )

  return("OK")
}

#' Convenience function to standardize how World Bank commodity price figures
#' are written.
#'
#' @param fig A string with the name of the figure in the supplied `figure_ls`.
#' This is the igure_ls[[fig]] is the figure that is written.
#' @param figure_ls A named list of figures.
#' @param chart_path A string with the path to the folder that the figures will
#' be saved in.
write_wb_figs_to_file <- function(fig, figure_ls, chart_path) {
  ggsave(
    filename = str_glue("{chart_path}{fig}.png"),
    plot = figure_ls[[fig]],
    height = 20,
    width = 50,
    units = "cm",
    type = "cairo",
    dpi = 300
  )
}

#' Convenience function to standardize how UNCTAD commodity price figures
#' are written.
#'
#' @param fig A string with the name of the figure in the supplied `figure_ls`.
#' This is the igure_ls[[fig]] is the figure that is written.
#' @param figure_ls A named list of figures.
#' @param chart_path A string with the path to the folder that the figures will
#' saved in.
write_unctad_figs_to_file <- function(fig, figure_ls, chart_path) {
  ggsave(
    filename = str_glue("{chart_path}{fig}.png"),
    plot = figure_ls[[fig]],
    height = 20,
    width = 50,
    units = "cm",
    type = "cairo",
    dpi = 300
  )
}
