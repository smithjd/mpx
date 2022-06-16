#' plot expanded medpol output from mp_fit
#'
#' @param expanded_lp_list augmented output from medpolish returned from the mp_fit function
#'
#' @return p
#' @export mp_plot
#' @return The dataframe with new mean and sum columns
#'
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 arrow
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#'
#' @examples mp_plot(expanded_lp_list)
#'
mp_plot <- function(expanded_lp_list) {
  lp <- expanded_lp_list
  p <- lp$long_fit_resids %>%
    dplyr::mutate(resid_color = case_when(
      resids < 0 ~ "blue",
      TRUE ~ "red"
    )) %>%
    ggplot2::ggplot(aes(x = col_fit, y = row_fit)) +
    # geom_point() +
    ggplot2::geom_segment(aes(y = row_grid_start, x = col_fit,
                     yend = row_grid_end, xend = col_fit)) +
    ggplot2::geom_segment(aes(y = row_fit, x = col_grid_start,
                     xend = col_grid_end, yend = row_fit)) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::geom_segment(aes(
      x = col_fit, y = row_fit,
      xend = col_fit_end, yend = row_fit_end,
      color = resid_color
    ),
    arrow = ggplot2::arrow(type = "closed", length = unit(.1, "cm"))
    ) +
    ggplot2::geom_text(
      data = lp$col_label_tibble,
      aes(col_label_x, col_label_y,
        label = col_label_label,
        show.legend = NULL
      ),
      angle = -45,
      nudge_y = -20
    ) +
    ggplot2::geom_text(
      data = lp$row_label_tibble,
      aes(row_label_y, row_label_x,
        label = row_label_label,
        show.legend = NULL
      ),
      angle = -45,
      nudge_x = 20
    ) +
    ggplot2::labs(fill = NULL) +
    ggplot2::theme(legend.position = "none")

  p
}
