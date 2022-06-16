#' fit median polish to a long form table
#'
#' @param long_df a long data frame with a numeric 'measure', and 'row_name' and 'col_name' classifiers
#' @param measure a numeric variable in long_df
#' @param row_name a variable in long_df
#' @param col_name a variable in long_df
#'
#' @return lp
#' @export mp_fit
#'
mp_fit <- function(long_df, measure, row_name, col_name) {
  my_df <- long_df
  my_measure <- my_df |> dplyr::select({{measure}}) |> unlist()
  my_row_name <- my_df |> dplyr::select({{row_name}}) |> unique() |> unlist()
  my_col_name <- my_df |> dplyr::select({{col_name}}) |> unique() |> unlist()
  my_row_length <- my_row_name |> unlist() |> length()
  my_col_length <- my_col_name |> unlist() |> length()
  my_array <- array(my_measure, c(my_row_length, my_col_length),
                    dimnames = list(my_row_name, my_col_name))

  lp <- stats::medpolish(my_array)

  lp_row_tibble <- tibble::tibble(columns = my_row_name)
  fit <- tibble::as_tibble(lp$overall + outer(lp$row, lp$col, "+"))
  fit <- dplyr::bind_cols(lp_row_tibble, fit) # add row-names back to the fit df

  residuals <- dplyr::bind_cols(lp_row_tibble, lp$residuals)

  max_row_fit <- max(lp$row) + lp$overall
  min_row_fit <- min(lp$row) + lp$overall
  max_col_fit <- max(lp$col) + lp$overall
  min_col_fit <- min(lp$col) + lp$overall

  # Store augmented residuals data frame back in the lp list
  # lp$residuals <- residuals

  long_fit <- tidyr::pivot_longer(fit,
                           cols = where(is.numeric),
                           names_to = "cols",
                           values_to = "raw_count",
                           names_repair = "unique"
  )

  long_resid <- residuals |>
    tidyr::pivot_longer(
      cols = names(lp$col), names_to = "treat",
      values_to = "resids"
    )

  overall_fit <- rep(lp$overall, (length(lp$row * length(lp$col))))

  long_fit <- tibble::tibble(
    col_fit = (rep(lp$col, times = length(lp$row)) + overall_fit),
    row_fit = (rep(lp$row, each = length(lp$col)) + overall_fit)
  )

  long_fit_resids <- dplyr::bind_cols(long_fit, long_resid)

  # names(long_fit_resids)

  max_row_fit <- max(c((lp$row + lp$overall)))
  min_row_fit <- min(c((lp$row + lp$overall)))
  max_col_fit <- max(c((lp$col + lp$overall)))
  min_col_fit <- min(c((lp$col + lp$overall)))
  # min_fit <- min(c(lp$row, lp$col))

  table_length <- length(lp$row) * length(lp$col)

  long_fit_resids <- long_fit_resids |>
    dplyr::mutate(
      sign = ifelse(resids >= 0, 1, -1),
      seg_length = sqrt((resids**2) / 2),
      col_fit_end = col_fit + (sign * seg_length),
      row_fit_end = row_fit + (sign * seg_length),
      row_grid_start = rep(min_row_fit, table_length),
      row_grid_end = rep(max_row_fit, table_length),
      col_grid_start = rep(min_col_fit, table_length),
      col_grid_end = rep(max_col_fit, table_length),
    ) |>
    dplyr::select(-sign)

  row_label_tibble <- tibble::tibble(
    row_label_label = names(lp$row),
    row_label_x = (lp$row + lp$overall),
    row_label_y = (rep(max_col_fit, length(lp$row)))
  )

  col_label_tibble <- tibble::tibble(
    col_label_label = names(lp$col),
    col_label_x = (lp$col + lp$overall),
    col_label_y = (rep(min_row_fit, length(lp$col)))
  )

  lp$long_fit_resids <- long_fit_resids
  lp$row_label_tibble <- row_label_tibble
  lp$col_label_tibble <- col_label_tibble
  lp
}
