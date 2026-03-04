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
#' border styling via a regex match.
#' @param bold_columns
#' Column indices to render in bold (e.g., `1` or `1:2`). Use `NULL` or
#' `integer(0)` to skip bolding. (default: `1`)
#' @param row_border_pattern
#' Pattern to search for in the first column to put a horizontal border across
#' the row. For example, if you have `N (%)`` next to categorical variable names and
#' `Mean±SD`` next to numeric variable names in the first column you can specify
#' the pattern `"N \\(%\\)|Mean±SD"` to place horizontal borders separating these
#' variables. (default: `"N \\(%\\)|Mean±SD"`)
#' @param col_border_pattern
#' Same as `row_border_pattern`, but searching for patterns in column names to
#' put a left border on the column (e.g., if column names have `N=` in the name
#' you can specify `"N="` as the pattern to place left border on column).
#' (default: `"N="`)
#' @param stat_pattern
#' Pattern that would map to statistics columns (e.g., OR, beta, coef, etc.).
#' Used for detecting what columns are the statistics columns. (default: `"Coef \\["`)
#' @param caption
#' Caption to give the table.
#'
#' @return
#' A `kableExtra` HTML table object (class typically including `knitr_kable`)
#' that can be printed in HTML contexts (R Markdown/Quarto, Shiny).
#'
#' @importFrom kableExtra kbl kable_styling column_spec row_spec
#' @export
#'
kable_html_table = function(
  df, 
  bold_columns = 1, 
  row_border_pattern = "N \\(%\\)|Mean±SD",
  col_border_pattern = "N=",
  stat_pattern = "Coef \\[",
  caption = NULL
) {
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package 'kableExtra' is required.")
  }

  # Begin table
  tbl = kableExtra::kbl(
    df,
    row.names = FALSE,
    align = "c", # Center align all columns
    caption = caption
  )

  # Begin styling, freezing first row
  tbl = kableExtra::kable_styling(
    tbl,
    fixed_thead = TRUE,
    bootstrap_options = "none"
  )

  # Bold first N columns
  tbl = kableExtra::column_spec(tbl, column = bold_columns, bold = TRUE)

  # Specify font size and borders of header row
  tbl = kableExtra::row_spec(
    tbl,
    row = 0,
    extra_css = paste0(
      "font-size: 9pt;",
      "border-top: 2px solid #000000ff;",
      "border-bottom: 2px solid #000000ff;",
      "background-color: #d5d5d5ff;"
    )
  )

  # Add border to top of rows with specified pattern
  tbl = kableExtra::row_spec(
    tbl,
    row = grep(row_border_pattern, df[[1]]),
    extra_css = "border-top: 1px solid #000000ff;"
  )

  # Add border to bottom of table
  tbl = kableExtra::row_spec(
    tbl,
    row = nrow(df),
    extra_css = "border-bottom: 2px solid #000000ff;"
  )

  # Add borders to left of columns with specified pattern
  tbl = kableExtra::column_spec(
    tbl,
    column = grep(col_border_pattern, colnames(df)),
    extra_css = "border-left: 1px solid #000000ff;"
  )

  # Add border to left of column with "Coef" if only one exists
  if (sum(grepl("Coef \\[", colnames(df))) == 1) {
    tbl = kableExtra::column_spec(
      tbl,
      column = grep("Coef \\[", colnames(df)),
      extra_css = "border-left: 1px solid #000000ff;"
    )
  }

  # For each column, make minimum width the max character length in column and
  # specify font size for remaining values
  for (col in seq_len(ncol(df))) {
    tbl = kableExtra::column_spec(
      tbl,
      column = col,
      width_min = paste0(max(nchar(as.character(df[[col]]))) + 5, "ch"),
      extra_css = "font-size: 9pt;"
    )
  }

  return(tbl)
}
