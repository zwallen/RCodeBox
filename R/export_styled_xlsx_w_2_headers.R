#' Export a data frame to a styled Excel file with two header rows (major and sub-header).
#'
#' @param data The data frame to export. The first two rows should be header and sub-header.
#' @param ws_title The worksheet title.
#' @param filename The output .xlsx filename.
#' @return None. Writes a styled Excel file to disk.
#' @export
#'
export_styled_xlsx_w_2_headers <- function(data, ws_title, filename) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required.")
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, ws_title)
  openxlsx::writeData(wb, ws_title, data, colNames = FALSE, rowNames = FALSE)
  col_widths <- c(
    max(nchar(as.character(data[[1]])), na.rm = TRUE) + 5,
    5,
    15,
    rep(c(5, 15, 20, 10), sum(data[1, ] != "") - 1)
  )
  openxlsx::setColWidths(
    wb,
    ws_title,
    cols = 1:length(col_widths),
    widths = col_widths
  )
  align_left <- openxlsx::createStyle(halign = "left", valign = "center")
  align_center <- openxlsx::createStyle(halign = "center", valign = "center")
  bold <- openxlsx::createStyle(textDecoration = "bold")
  border_top <- openxlsx::createStyle(border = "top", borderStyle = "medium")
  border_bottom <- openxlsx::createStyle(
    border = "bottom",
    borderStyle = "medium"
  )
  border_thin_top <- openxlsx::createStyle(border = "top", borderStyle = "thin")
  openxlsx::addStyle(
    wb,
    ws_title,
    align_left,
    rows = 1:nrow(data),
    cols = 1:ncol(data),
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb,
    ws_title,
    align_center,
    rows = 1:nrow(data),
    cols = 1,
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb,
    ws_title,
    bold,
    rows = 1:2,
    cols = 1:ncol(data),
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb,
    ws_title,
    border_top,
    rows = 1,
    cols = 1:ncol(data),
    gridExpand = TRUE,
    stack = TRUE
  )
  openxlsx::addStyle(
    wb,
    ws_title,
    border_bottom,
    rows = 2,
    cols = 1:ncol(data),
    gridExpand = TRUE,
    stack = TRUE
  )
  openxlsx::addStyle(
    wb,
    ws_title,
    border_bottom,
    rows = nrow(data),
    cols = 1:ncol(data),
    gridExpand = TRUE,
    stack = TRUE
  )
  grep_rows <- function(pattern) {
    which(grepl(pattern, as.character(data[[1]]), perl = TRUE))
  }
  border_rows <- unique(c(
    grep_rows("Total"),
    grep_rows("N, %"),
    grep_rows("Mean\u00B1SD")
  ))
  for (row in border_rows) {
    openxlsx::addStyle(
      wb,
      ws_title,
      border_thin_top,
      rows = row,
      cols = 1:ncol(data),
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  for (col in seq_len(ncol(data))) {
    for (row in 3:nrow(data)) {
      val <- data[row, col]
      if (is.character(val) || is.factor(val)) {
        num <- suppressWarnings(as.numeric(as.character(val)))
        if (!is.na(num)) {
          openxlsx::writeData(
            wb,
            ws_title,
            num,
            startCol = col,
            startRow = row,
            colNames = FALSE,
            rowNames = FALSE
          )
        }
      }
    }
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}
