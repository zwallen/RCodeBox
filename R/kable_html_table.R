#' Create a styled HTML table with kableExtra
#'
#' @description
#' Renders a centered, scroll-friendly HTML table from a data frame using
#' `kableExtra` (via `kbl()`), with a fixed header, bolded leading columns,
#' compact header styling, and sectioning borders for summary rows that contain
#' `"N (%)"` or `"Mean±SD"` in the first column. Columns are given a minimum CSS
#' width based on the maximum string length observed in each column, and all
#' body text is set to a small, consistent font size for compact display.
#'
#' @param df
#' Input table data. The first column is used to detect section headers for
#' border styling via a regex match on `"N (%)"` or `"Mean±SD"`.
#' @param bold_columns
#' Column indices to render in bold (e.g., `1` or `1:2`). Use `NULL` or
#' `integer(0)` to skip bolding. (default: `1`)
#' @param caption
#' Caption to give the table.
#'
#' @return
#' A `kableExtra` HTML table object (class typically including `knitr_kable`)
#' that can be printed in HTML contexts (R Markdown/Quarto, Shiny).
#'
#' @importFrom kableExtra kbl kable_styling column_spec row_spec scroll_box
#' @export
#'
kable_html_table = function(df, bold_columns = 1, caption = NULL) {
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package 'kableExtra' is required.")
  }

  # Begin table
  tbl = kbl(
    df,
    row.names = FALSE,
    align = "c", # Center align all columns
    caption = caption
  ) |>
    # Begin styling, freezing first row
    kable_styling(fixed_thead = TRUE, bootstrap_options = "none") |>
    # Add scrolling capability
    scroll_box(height = "500px", width = "100%") |>
    # Bold first N columns
    column_spec(column = bold_columns, bold = TRUE) |>
    # Specify font size and borders of header row
    row_spec(
      row = 0,
      extra_css = "font-size: 9pt; border-top: 2px solid #000000ff; border-bottom: 2px solid #000000ff;"
    ) |>
    # Add border to top of rows with "N, %" or "Mean±SD"
    row_spec(
      row = grep("N \\(%\\)|Mean±SD", df[[1]]),
      extra_css = "border-top: 1px solid #000000ff;"
    ) |>
    # Add border to bottom of table
    row_spec(
      row = nrow(df),
      extra_css = "border-bottom: 2px solid #000000ff;"
    ) |>
    # Add borders to left of columns with "N="
    column_spec(
      column = grep("N=", colnames(df)),
      extra_css = "border-left: 1px solid #000000ff;"
    )

  # Add border to left of column with "Coef" if only one exists
  if (sum(grepl("Coef \\[", colnames(df))) == 1) {
    tbl = tbl |>
      column_spec(
        column = grep("Coef \\[", colnames(df)),
        extra_css = "border-left: 1px solid #000000ff;"
      )
  }

  # For each column, make minimum width the max character length in column and
  # specify font size for remaining values
  for (col in seq_len(ncol(df))) {
    tbl = tbl |>
      column_spec(
        column = col,
        width_min = paste0(max(nchar(as.character(df[[col]]))) + 2, "ch"),
        extra_css = "font-size: 9pt;"
      )
  }

  return(tbl)
}
