#' Display a Styled HTML Table with Two Headers
#'
#' This function takes a data frame and returns a style HTML table with two
#' header rows.
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
  )

  html_tbl_formatted <- kableExtra::kable_styling(
    html_tbl_formatted,
    full_width = TRUE
  )

  html_tbl_formatted <- kableExtra::column_spec(
    html_tbl_formatted,
    seq_len(ncol(html_tbl)),
    extra_css = "white-space:nowrap;"
  )

  html_tbl_formatted <- kableExtra::row_spec(
    html_tbl_formatted,
    1,
    bold = TRUE,
    extra_css = "border-top:2px solid black;"
  )

  html_tbl_formatted <- kableExtra::row_spec(
    html_tbl_formatted,
    2,
    bold = TRUE,
    extra_css = "border-bottom:2px solid black;"
  )

  html_tbl_formatted <- kableExtra::row_spec(
    html_tbl_formatted,
    nrow(html_tbl),
    extra_css = "border-bottom:2px solid black;"
  )

  html_tbl_formatted
}
