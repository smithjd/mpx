#' Extend median polish output
#'
#' @param long_df
#' @param measure
#' @param row_name
#' @param col_name
#'
#' @return lp
#' @export
#'
#' @examples
mp_fit <- function(long_df, measure, row_name, col_name) {
  my_df <- long_df
  my_measure <- my_df %>% select({{measure}}) %>% unlist()
  my_row_name <- my_df %>% select({{row_name}}) %>% unique() %>% unlist()
  my_col_name <- my_df %>% select({{col_name}}) %>% unique() %>% unlist()
  my_row_length <- my_row_name %>% unlist() %>% length()
  my_col_length <- my_col_name %>% unlist() %>% length()
  my_array <- array(my_measure, c(my_row_length, my_col_length),
                    dimnames = list(my_row_name, my_col_name))

  lp <- medpolish(my_array)

  lp_row_tibble <- tibble(columns = my_row_name)
  fit <- as_tibble(lp$overall + outer(lp$row, lp$col, "+"))
  fit <- bind_cols(lp_row_tibble, fit) # add row-names back to the fit df

  residuals <- bind_cols(lp_row_tibble, lp$residuals)

  max_row_fit <- max(lp$row) + lp$overall
  min_row_fit <- min(lp$row) + lp$overall
  max_col_fit <- max(lp$col) + lp$overall
  min_col_fit <- min(lp$col) + lp$overall

  # Store augmented residuals data frame back in the lp list
  # lp$residuals <- residuals

  long_fit <- pivot_longer(fit,
                           cols = where(is.numeric),
                           names_to = "cols",
                           values_to = "raw_count",
                           names_repair = "unique"
  )

  long_resid <- residuals %>%
    pivot_longer(
      cols = names(lp$col), names_to = "treat",
      values_to = "resids"
    )

  overall_fit <- rep(lp$overall, (length(lp$row * length(lp$col))))

  long_fit <- tibble(
    col_fit = (rep(lp$col, times = length(lp$row)) + overall_fit),
    row_fit = (rep(lp$row, each = length(lp$col)) + overall_fit)
  )

  long_fit_resids <- bind_cols(long_fit, long_resid)

  # names(long_fit_resids)

  max_row_fit <- max(c((lp$row + lp$overall)))
  min_row_fit <- min(c((lp$row + lp$overall)))
  max_col_fit <- max(c((lp$col + lp$overall)))
  min_col_fit <- min(c((lp$col + lp$overall)))
  # min_fit <- min(c(lp$row, lp$col))

  table_length <- length(lp$row) * length(lp$col)

  long_fit_resids <- long_fit_resids %>%
    mutate(
      sign = ifelse(resids >= 0, 1, -1),
      seg_length = sqrt((resids**2) / 2),
      col_fit_end = col_fit + (sign * seg_length),
      row_fit_end = row_fit + (sign * seg_length),
      row_grid_start = rep(min_row_fit, table_length),
      row_grid_end = rep(max_row_fit, table_length),
      col_grid_start = rep(min_col_fit, table_length),
      col_grid_end = rep(max_col_fit, table_length),
    ) %>%
    select(-sign)

  row_label_tibble <- tibble(
    row_label_label = names(lp$row),
    row_label_x = (lp$row + lp$overall),
    row_label_y = (rep(max_col_fit, length(lp$row)))
  )

  col_label_tibble <- tibble(
    col_label_label = names(lp$col),
    col_label_x = (lp$col + lp$overall),
    col_label_y = (rep(min_row_fit, length(lp$col)))
  )

  lp$long_fit_resids <- long_fit_resids
  lp$row_label_tibble <- row_label_tibble
  lp$col_label_tibble <- col_label_tibble
  lp
}
