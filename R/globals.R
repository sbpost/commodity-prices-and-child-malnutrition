#' A very simple function that is used as a central place to keep some global
#' settings, like a common color scale or a custom theme for charts.
#' @return A named list containing the settings. For instance, to access colors
#' call globals()$colors.

globals <- function() {

  gl_ls <- list(
    color_scale = scale_color_gradient(low = "blue", high = "red")
  )

  return(gl_ls)
}
