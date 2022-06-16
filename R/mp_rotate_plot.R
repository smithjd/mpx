#' mp_rotate_plot
#'
#' @param expanded_lp_list a ggplot glob produced by the mp_plot function
#'
#' @return vp
#' @export mp_rotate_plot
#'
#' @examples mp_rotate_plot(expanded_lp_list)
mp_rotate_plot <- function(expanded_lp_list){
  rotation <- 45
  p_rot <- expanded_lp_list +
    ggplot2::theme(
      legend.position = "none",
      axis.title.y = ggplot2::element_text(angle = -90)
    )
  grid::grid.newpage()

  vp <- grid::viewport(name = "rotate", angle = rotation, width = 0.7, height = 0.7)
  grid::pushViewport(vp)
  print(p_rot, vp = "rotate", newpage = FALSE)

  ## Warning in grid.Call.graphics(C_setviewport, vp, TRUE): cannot clip to rotated
  ## viewport

  ## Warning in grid.Call.graphics(C_setviewport, vp, TRUE): cannot clip to rotated
  ## viewport

  vp <- grid::viewport(
    x = 0.15, y = 0.8, width = 0,
    height = 0
  )
# vp <- pushViewport(vp)
}
