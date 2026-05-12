#' Export a styled data frame to an Excel worksheet
#'
#' @description
#' Writes a data frame to an `.xlsx` file with publication-style formatting using
#' `openxlsx`. The function bolds the header row, center-aligns all cells,
#' draws thin top borders above rows whose first column contains `"N (%)"` or
#' `"Mean\eqn{\pm}{+/-}SD"`, and adds a medium border at the top and bottom of the table.
#' Columns are auto-sized to fit content, and the workbook is saved to `out_path`.
#'
#' @param df
#' Dataframe to export to file. The first column is used to detect summary header rows
#' for adding thin top borders (rows matching `"N (%)"` or `"Mean\eqn{\pm}{+/-}SD"`).
#' @param out_path
#' File path for the output `.xlsx`. Existing files at this path are overwritten.
#' @param sheet
#' Name of the worksheet to create within the workbook.
#' @param numeric_columns
#' Name(s) of numeric columns in the data.frame that need to be coerced to
#' numeric type in the workbook.
#' @param row_border_pattern
#' Pattern to search for in the first column to put a horizontal border across
#' the row. For example, if you have `N (%)`` next to categorical variable names and
#' `Mean\eqn{\pm}{+/-}SD`` next to numeric variable names in the first column you can specify
#' the pattern `"N \\(%\\)|Mean\eqn{\pm}{+/-}SD"` to place horizontal borders separating these
#' variables. (default: `"N \\(%\\)|Mean\eqn{\pm}{+/-}SD"`)
#' @param col_border_pattern
#' Same as `row_border_pattern`, but searching for patterns in column names to
#' put a left border on the column (e.g., if column names have `N=` in the name
#' you can specify `"N="` as the pattern to place left border on column).
#' (default: `"N="`)
#' @param stat_pattern
#' Pattern that would map to statistics columns (e.g., OR, beta, coef, etc.).
#' Used for detecting what columns are the statistics columns. (default: `"Coef \\["`)
#'
#' @return
#' Invisibly returns `TRUE` on successful write (value from `openxlsx::saveWorkbook()`),
#' and has the side effect of creating/overwriting the Excel file at `out_path`.
#'
#' @export
#'
export_to_xlsx <- function(
  df,
  out_path,
  sheet,
  numeric_columns = NULL,
  row_border_pattern = "N \\(%\\)|Mean\u00B1SD",
  col_border_pattern = "N=",
  stat_pattern = "Coef \\["
) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required.")
  }

  # Get boundaries of table
  row_limit <- nrow(df)
  col_limit <- ncol(df)

  # Load or create workbook
  if (file.exists(out_path)) {
    wb <- openxlsx::loadWorkbook(out_path)
  } else {
    wb <- openxlsx::createWorkbook()
  }

  # Add worksheet or rewrite if already present
  if (sheet %in% wb[["sheet_names"]]) {
    openxlsx::removeWorksheet(wb, sheet)
    openxlsx::addWorksheet(wb, sheet)
  } else {
    openxlsx::addWorksheet(wb, sheet)
  }

  # Write data
  openxlsx::writeData(
    wb,
    sheet,
    df,
    startRow = 1,
    startCol = 1,
    headerStyle = openxlsx::createStyle(textDecoration = "bold")
  )

  # Center align output and make table background white
  openxlsx::addStyle(
    wb,
    sheet,
    style = openxlsx::createStyle(
      halign = "center",
      valign = "center",
      fgFill = "white"
    ),
    rows = 1:(row_limit + 1),
    cols = 1:col_limit,
    gridExpand = TRUE,
    stack = TRUE
  )

  # Add borders to rows with variable names containing specified pattern
  openxlsx::addStyle(
    wb,
    sheet,
    style = openxlsx::createStyle(
      border = "top",
      borderColour = "black",
      borderStyle = "thin"
    ),
    rows = grep(row_border_pattern, df[[1]]) + 1,
    cols = 1:col_limit,
    gridExpand = TRUE,
    stack = TRUE
  )

  # Add borders to left of columns with specified pattern
  openxlsx::addStyle(
    wb,
    sheet,
    style = openxlsx::createStyle(
      border = "left",
      borderColour = "black",
      borderStyle = "thin"
    ),
    rows = 1:(row_limit + 1),
    cols = grep(col_border_pattern, colnames(df)),
    gridExpand = TRUE,
    stack = TRUE
  )

  # Add border to left of column with stat pattern if only one exists
  if (sum(grepl(stat_pattern, colnames(df))) == 1) {
    openxlsx::addStyle(
      wb,
      sheet,
      style = openxlsx::createStyle(
        border = "left",
        borderColour = "black",
        borderStyle = "thin"
      ),
      rows = 1:(row_limit + 1),
      cols = grep(stat_pattern, colnames(df)),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  # Add thicker border around table and under first row
  openxlsx::addStyle(
    wb,
    sheet,
    style = openxlsx::createStyle(
      border = "top",
      borderStyle = "medium",
      borderColour = "black"
    ),
    rows = 1,
    cols = 1:col_limit,
    gridExpand = TRUE,
    stack = TRUE
  )
  openxlsx::addStyle(
    wb,
    sheet,
    style = openxlsx::createStyle(
      border = "bottom",
      borderStyle = "medium",
      borderColour = "black"
    ),
    rows = c(1, row_limit + 1),
    cols = 1:col_limit,
    gridExpand = TRUE,
    stack = TRUE
  )
  openxlsx::addStyle(
    wb,
    sheet,
    style = openxlsx::createStyle(
      border = "left",
      borderStyle = "medium",
      borderColour = "black"
    ),
    rows = 1:(row_limit + 1),
    cols = 1,
    gridExpand = TRUE,
    stack = TRUE
  )
  openxlsx::addStyle(
    wb,
    sheet,
    style = openxlsx::createStyle(
      border = "right",
      borderStyle = "medium",
      borderColour = "black"
    ),
    rows = 1:(row_limit + 1),
    cols = col_limit,
    gridExpand = TRUE,
    stack = TRUE
  )

  # Make sure supplied numeric columns are numeric in output
  if (!is.null(numeric_columns)) {
    for (col in which(colnames(df) %in% numeric_columns)) {
      for (row in seq_len(row_limit)) {
        cell_value <- as.numeric(as.character(df[row, col]))
        if (!is.na(cell_value)) {
          openxlsx::writeData(
            wb,
            sheet,
            cell_value,
            startCol = col,
            startRow = row + 1
          )
          openxlsx::addStyle(
            wb,
            sheet,
            style = openxlsx::createStyle(numFmt = "SCIENTIFIC"),
            rows = row + 1,
            cols = col,
            gridExpand = TRUE,
            stack = TRUE
          )
        }
      }
    }
  }

  # Auto-width columns
  openxlsx::setColWidths(wb, sheet, cols = 1:col_limit, widths = "auto")

  # Output styled data to excel sheet
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
}
