#' Export a styled Excel worksheet with two header rows
#'
#' This function takes a data frame with two header rows and exports it to an Excel
#' file with specific styling, including custom column widths and various cell
#' alignments and borders.
#'
#' @param df A data frame to be exported to Excel.
#' @param ws_title A character string specifying the worksheet title.
#' @param filename A character string specifying the output Excel file name.
#'
#' @return None. Exports provided data frame to a file.
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Creates a new workbook and worksheet
#'   \item Writes the data frame to the worksheet without column names
#'   \item Sets custom column widths
#'   \item Applies left alignment to all cells, except for the first column
#'   \item Applies bold formatting to the first two rows
#'   \item Adds medium borders to the top of table, row 2, and bottom of table
#'   \item Adds thin borders below rows containing "Total", "N, %", or "Mean<c2><b1>SD"
#'   in the first column
#'   \item Converts string representations of numbers back to numeric values where
#'   possible
#' }
#'
#' @note This function requires the \code{openxlsx} package.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   Variable = c("Header 1", "Header 2", "Row 1", "Row 2", "Total"),
#'   Value1 = c("", "", "10", "20", "30"),
#'   Value2 = c("", "", "1.5", "2.5", "4.0")
#' )
#' export_styled_xlsx_w_2_headers(df, "My Worksheet", "output.xlsx")
#' }
#' 
#' @importFrom openxlsx createWorkbook addWorksheet writeData setColWidths addStyle
#' createStyle saveWorkbook
#'
#' @export

export_styled_xlsx_w_2_headers <- function(df, ws_title, filename) {
  requireNamespace("openxlsx", quietly = TRUE)

  # Create workbook and worksheet
  wb <- createWorkbook()
  addWorksheet(wb, ws_title)

  # Write data.frame to worksheet (no column names)
  writeData(wb, sheet = ws_title, x = df, colNames = FALSE, rowNames = FALSE)

  # Set column widths
  col_widths <- c(c(55, 5, 15), rep(c(5, 15, 20, 10), sum(df[1, ] != "") - 1))
  setColWidths(wb, sheet = ws_title, cols = seq_len(ncol(df)), widths = col_widths)
  
  # Apply left alignment to all cells
  addStyle(
    wb,
    sheet = ws_title,
    style = createStyle(halign = "left", valign = "center"),
    rows = 1:nrow(df),
    cols = 1:ncol(df),
    gridExpand = TRUE
  )

  # Apply center alignment to cells in first column
  addStyle(
    wb,
    sheet = ws_title,
    style = createStyle(halign = "center", valign = "center"),
    rows = 1:nrow(df),
    cols = 1,
    gridExpand = TRUE
  )

  # Apply bold to first two rows
  addStyle(
    wb,
    sheet = ws_title,
    style = createStyle(textDecoration = "bold"),
    rows = 1:2,
    cols = 1:ncol(df),
    gridExpand = TRUE
  )

  # Apply medium border to top of row 1
  addStyle(
    wb,
    sheet = ws_title,
    style = createStyle(border = "top", borderStyle = "medium"),
    rows = 1,
    cols = 1:ncol(df),
    gridExpand = TRUE
  )

  # Apply medium border to bottom of row 2 and last row
  addStyle(
    wb,
    sheet = ws_title,
    style = createStyle(border = "bottom", borderStyle = "medium"),
    rows = 2,
    cols = 1:ncol(df),
    gridExpand = TRUE
  )
  addStyle(
    wb,
    sheet = ws_title,
    style = createStyle(border = "bottom", borderStyle = "medium"),
    rows = nrow(df),
    cols = 1:ncol(df),
    gridExpand = TRUE
  )

  # Apply thin border below major variable rows and left align
  if (length(grep("Total|N, %|Mean<c2><b1>SD", df[, 1])) > 0) {
    addStyle(
      wb,
      sheet = ws_title,
      style = createStyle(
        border = "top",
        borderStyle = "thin",
        halign = "left",
        valign = "center"
      ),
      rows = grep("Total|N, %|Mean<c2><b1>SD", df[, 1]),
      cols = 1:ncol(df),
      gridExpand = TRUE
    )
  }

  # Convert numbers from strings back to numbers where possible
  for (row in 3:nrow(df)) {
    for (col in 1:ncol(df)) {
      cell_value <- df[row, col]
      if (is.character(cell_value)) {
        # Try to convert to numeric
        tryCatch(
          {
            if (grepl("\\.", cell_value)) {
              numeric_value <- as.numeric(cell_value)
              if (!is.na(numeric_value)) {
                df[row, col] <- numeric_value
              }
            } else {
              integer_value <- as.integer(cell_value)
              if (!is.na(integer_value)) {
                df[row, col] <- integer_value
              }
            }
          },
          error = function(e) {
            # Do nothing and keep original value if conversion fails
          }
        )
      }
    }
  }

  # Rewrite the data with converted values
  writeData(wb, sheet = ws_title, x = df, colNames = FALSE, rowNames = FALSE)

  # Save workbook
  saveWorkbook(wb, filename, overwrite = TRUE)
}
