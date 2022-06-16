mp_plot <- function(expanded_lp_list){
  lp <- expanded_lp_list
  p <- lp$long_fit_resids %>%
    mutate(resid_color = case_when(
      resids < 0 ~ "red",
      TRUE ~ "blue"
    )) %>%
    ggplot(aes(x = col_fit, y = row_fit)) +
    # geom_point() +
    geom_segment(aes(y = row_grid_start, x = col_fit, yend = row_grid_end, xend =   col_fit)) +
    geom_segment(aes(y = row_fit, x = col_grid_start, xend = col_grid_end, yend =   row_fit)) +
    coord_fixed(ratio = 1) +
    geom_segment(aes(
      x = col_fit, y = row_fit,
      xend = col_fit_end, yend = row_fit_end,
      color = resid_color
    ),
    arrow = arrow(type = "closed", length = unit(.1, "cm"))
    ) +
    geom_text(
      data = lp$col_label_tibble,
      aes(col_label_x, col_label_y,
          label = col_label_label
      ),
      angle = -45,
      nudge_y = -20
    ) +
    geom_text(
      data = lp$row_label_tibble,
      aes(row_label_y, row_label_x,
          label = row_label_label
      ),
      angle = -45,
      nudge_x = 20
    ) +
    labs(fill = NULL)

  p
}
