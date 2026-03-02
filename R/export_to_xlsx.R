#' Export a styled data frame to an Excel worksheet
#'
#' @description
#' Writes a data frame to an `.xlsx` file with publication-style formatting using
#' `openxlsx`. The function bolds the header row, center-aligns all cells,
#' draws thin top borders above rows whose first column contains `"N (%)"` or
#' `"Mean±SD"`, and adds a medium border at the top and bottom of the table.
#' Columns are auto-sized to fit content, and the workbook is saved to `out_path`.
#'
#' @param df
#' Dataframe to export to file. The first column is used to detect summary header rows
#' for adding thin top borders (rows matching `"N (%)"` or `"Mean±SD"`).
#' @param out_path
#' File path for the output `.xlsx`. Existing files at this path are overwritten.
#' @param sheet
#' Name of the worksheet to create within the workbook.
#' @param numeric_columns
#' Name(s) of numeric columns in the data.frame that need to be coerced to
#' numeric type in the workbook.
#'
#' @return
#' Invisibly returns `TRUE` on successful write (value from `openxlsx::saveWorkbook()`),
#' and has the side effect of creating/overwriting the Excel file at `out_path`.
#'
#' @importFrom openxlsx loadWorkbook createWorkbook removeWorksheet addWorksheet writeData createStyle addStyle setColWidths saveWorkbook
#' @export
#'
export_to_xlsx = function(df, out_path, sheet, numeric_columns = NULL) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required.")
  }

  # Get boundaries of table
  row_limit = nrow(df)
  col_limit = ncol(df)

  # Load or create workbook
  if (file.exists(out_path)) {
    wb = loadWorkbook(out_path)
  } else {
    wb = createWorkbook()
  }

  # Add worksheet or rewrite if already present
  if (sheet %in% wb$sheet_names) {
    removeWorksheet(wb, sheet)
    addWorksheet(wb, sheet)
  } else {
    addWorksheet(wb, sheet)
  }

  # Write data
  writeData(
    wb,
    sheet,
    df,
    startRow = 1,
    startCol = 1,
    headerStyle = createStyle(textDecoration = "bold")
  )

  # Center align output and make table background white
  addStyle(
    wb,
    sheet,
    style = createStyle(halign = "center", valign = "center", fgFill = "white"),
    rows = 1:(row_limit + 1),
    cols = 1:col_limit,
    gridExpand = TRUE,
    stack = TRUE
  )

  # Add borders to rows with variable names containing "N (%)" or "Mean±SD"
  addStyle(
    wb,
    sheet,
    style = createStyle(
      border = "top",
      borderColour = "black",
      borderStyle = "thin"
    ),
    rows = grep("N \\(%\\)|Mean±SD", df[[1]]) + 1,
    cols = 1:col_limit,
    gridExpand = TRUE,
    stack = TRUE
  )

  # Add borders to left of columns with "N="
  addStyle(
    wb,
    sheet,
    style = createStyle(
      border = "left",
      borderColour = "black",
      borderStyle = "thin"
    ),
    rows = 1:(row_limit + 1),
    cols = grep("N=", colnames(df)),
    gridExpand = TRUE,
    stack = TRUE
  )

  # Add border to left of column with "Coef [95%CI]" if only one exists
  if (sum(grepl("Coef \\[", colnames(df))) == 1) {
    addStyle(
      wb,
      sheet,
      style = createStyle(
        border = "left",
        borderColour = "black",
        borderStyle = "thin"
      ),
      rows = 1:(row_limit + 1),
      cols = grep("Coef \\[", colnames(df)),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  # Add thicker border around table and under first row
  addStyle(
    wb,
    sheet,
    style = createStyle(
      border = "top",
      borderStyle = "medium",
      borderColour = "black"
    ),
    rows = 1,
    cols = 1:col_limit,
    gridExpand = TRUE,
    stack = TRUE
  )
  addStyle(
    wb,
    sheet,
    style = createStyle(
      border = "bottom",
      borderStyle = "medium",
      borderColour = "black"
    ),
    rows = c(1, row_limit + 1),
    cols = 1:col_limit,
    gridExpand = TRUE,
    stack = TRUE
  )
  addStyle(
    wb,
    sheet,
    style = createStyle(
      border = "left",
      borderStyle = "medium",
      borderColour = "black"
    ),
    rows = 1:(row_limit + 1),
    cols = 1,
    gridExpand = TRUE,
    stack = TRUE
  )
  addStyle(
    wb,
    sheet,
    style = createStyle(
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
        cell_value = as.numeric(as.character(df[row, col]))
        if (!is.na(cell_value)) {
          writeData(wb, sheet, cell_value, startCol = col, startRow = row + 1)
          addStyle(
            wb,
            sheet,
            style = createStyle(numFmt = "GENERAL"),
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
  setColWidths(wb, sheet, cols = 1:col_limit, widths = "auto")

  # Output styled data to excel sheet
  saveWorkbook(wb, out_path, overwrite = TRUE)
}
