#' Display a Styled HTML Table with Two Headers
#'
#' This function takes a data frame and returns a style HTML table with two header rows.
#'
#' @param data A data frame to be displayed in the styled HTML table.
#' @return A styled HTML table object with two header rows.
#' @import kableExtra
#' @export
#'
display_styled_table_w_2_headers <- function(data) {
  # Bold certain cells prior to giving to kableExtra
  html_tbl <- data
  html_tbl[[1]] <- ifelse(
    grepl("Total|Mean\u00B1SD|N, %", html_tbl[[1]]),
    kableExtra::cell_spec(html_tbl[[1]], bold = TRUE),
    html_tbl[[1]]
  )

  # Generate HTML table
  html_tbl_formatted <- kableExtra::kbl(
    html_tbl,
    row.names = FALSE,
    col.names = NULL,
    escape = FALSE,
    align = "c"
  ) |>
    kableExtra::kable_styling(full_width = TRUE) |>
    # Make sure no wrapping of text
    kableExtra::column_spec(
      1:ncol(html_tbl),
      extra_css = "white-space:nowrap;"
    ) |>
    # Make top two rows bold and have borders
    kableExtra::row_spec(
      1,
      bold = TRUE,
      extra_css = "border-top:2px solid black;"
    ) |>
    kableExtra::row_spec(
      2,
      bold = TRUE,
      extra_css = "border-bottom:2px solid black;"
    ) |>
    # Add border to bottom of table
    kableExtra::row_spec(
      nrow(html_tbl),
      extra_css = "border-bottom:2px solid black;"
    )

  return(html_tbl_formatted)
}
