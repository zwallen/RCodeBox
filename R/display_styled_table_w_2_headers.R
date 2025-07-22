#' Display a styled table with two header rows and pagination
#'
#' This function takes a data frame with two header rows and displays it as a styled
#' HTML table with pagination functionality. It is designed to work in both Jupyter
#' notebooks and R markdown environments.
#'
#' @param df A data frame to be displayed as a styled table.
#' @param rows An integer specifying the number of rows to display per page.
#'   Default is 50.
#'
#' @return None. Displays provided data frame as HTML table.
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Extracts the first two rows as header rows
#'   \item Creates JavaScript for pagination and cell styling
#'   \item Builds an HTML table structure with header and data rows
#'   \item Adds pagination controls
#'   \item Applies CSS styling for table layout and appearance
#'   \item Displays the styled HTML table
#' }
#'
#' The resulting table features:
#' \itemize{
#'   \item Two fixed header rows
#'   \item Paginated data rows
#'   \item Responsive design with horizontal scrolling if necessary
#'   \item Styled cells with no text wrapping
#'   \item Bold formatting for specific cells in the first column
#'   \item Previous and Next buttons for navigation
#' }
#'
#' @note This function requires the \code{htmltools} and \code{uuid} packages.
#'
#' @examples
#' df <- data.frame(
#'   Variable = c("Header 1", "Header 2", "Row 1", "Row 2", "Total"),
#'   Value1 = c("", "", "10", "20", "30"),
#'   Value2 = c("", "", "1.5", "2.5", "4.0")
#' )
#' display_styled_table_w_2_headers(df, rows = 10)
#'
#' @importFrom htmltools HTML
#' @importFrom IRdisplay display_html
#' @importFrom uuid UUIDgenerate
#'
#' @export

display_styled_table_w_2_headers <- function(df, rows = 50) {
  requireNamespace("htmltools", quietly = TRUE)
  requireNamespace("IRdisplay", quietly = TRUE)
  requireNamespace("uuid", quietly = TRUE)

  # Extract header rows (first 2 rows) and data rows
  header_rows <- df[1:2, ]
  data_rows <- if (nrow(df) > 2) {
    df[3:nrow(df), ]
  } else {
    data.frame()
  }

  # Calculate total number of pages
  total_pages <- if (nrow(data_rows) > 0) {
    ceiling(nrow(data_rows) / rows)
  } else {
    1
  }

  # Generate unique ID for this table instance
  table_id <- substr(UUIDgenerate(), 1, 8)

  # Create JavaScript for pagination and cell styling
  js_script <- paste0(
    "
  <script>
  document.addEventListener('DOMContentLoaded', function() {
    let currentPage_",
    table_id,
    " = 1;
    const totalPages_",
    table_id,
    " = ",
    total_pages,
    ";
    const rowsPerPage_",
    table_id,
    " = ",
    rows,
    ";

    // Function to bold specific cells in column 1
    function styleFirstColumn_",
    table_id,
    "() {
      const rows = document.querySelectorAll('#table_",
    table_id,
    " tr');
      rows.forEach(row => {
        const firstCell = row.querySelector('td:first-child');
        if (firstCell) {
          const cellText = firstCell.textContent || firstCell.innerText;
          if (cellText.includes('Total') || cellText.includes('(N, %)') || cellText.includes('(Mean<c2><b1>SD)')) {
            firstCell.style.fontWeight = 'bold';
          }
        }
      });
    }

    // Function to show specific page
    function showPage_",
    table_id,
    "(pageNum) {
      // Hide all data rows
      const dataRows = document.querySelectorAll('#table_",
    table_id,
    " .data-row');
      dataRows.forEach(row => row.style.display = 'none');

      // Show rows for current page
      const startIdx = (pageNum - 1) * rowsPerPage_",
    table_id,
    ";
      const endIdx = startIdx + rowsPerPage_",
    table_id,
    ";

      for (let i = startIdx; i < endIdx && i < dataRows.length; i++) {
        dataRows[i].style.display = 'table-row';
      }

      // Update page info
      document.getElementById('page-info_",
    table_id,
    "').textContent =
        `Page ${pageNum} of ${totalPages_",
    table_id,
    "}`;

      // Update button states
      document.getElementById('prev-btn_",
    table_id,
    "').disabled = (pageNum === 1);
      document.getElementById('next-btn_",
    table_id,
    "').disabled = (pageNum === totalPages_",
    table_id,
    ");

      currentPage_",
    table_id,
    " = pageNum;

      // Style first column after showing page
      styleFirstColumn_",
    table_id,
    "();
    }

    // Event listeners for pagination buttons
    document.getElementById('prev-btn_",
    table_id,
    "').addEventListener('click', function() {
      if (currentPage_",
    table_id,
    " > 1) {
        showPage_",
    table_id,
    "(currentPage_",
    table_id,
    " - 1);
      }
    });

    document.getElementById('next-btn_",
    table_id,
    "').addEventListener('click', function() {
      if (currentPage_",
    table_id,
    " < totalPages_",
    table_id,
    ") {
        showPage_",
    table_id,
    "(currentPage_",
    table_id,
    " + 1);
      }
    });

    // Show first page initially
    showPage_",
    table_id,
    "(1);
  });
  </script>
  "
  )

  # Create HTML table structure
  html_parts <- c()

  # Add header rows (these will always be visible)
  for (i in 1:nrow(header_rows)) {
    row_class <- "header-row"
    cells <- paste0(
      sapply(header_rows[i, ], function(cell) paste0("<td>", cell, "</td>")),
      collapse = ""
    )
    html_parts <- c(
      html_parts,
      paste0('<tr class="', row_class, '">', cells, "</tr>")
    )
  }

  # Add data rows (these will be shown/hidden based on pagination)
  if (nrow(data_rows) > 0) {
    for (i in 1:nrow(data_rows)) {
      row_class <- "data-row"
      cells <- paste0(
        sapply(data_rows[i, ], function(cell) paste0("<td>", cell, "</td>")),
        collapse = ""
      )
      html_parts <- c(
        html_parts,
        paste0('<tr class="', row_class, '">', cells, "</tr>")
      )
    }
  }

  # Combine all rows
  table_rows <- paste(html_parts, collapse = "\n")

  # Create pagination controls
  pagination_controls <- paste0(
    '
  <div class="pagination-controls" style="margin: 10px 0; text-align: center;">
    <button id="prev-btn_',
    table_id,
    '" style="margin: 0 10px; padding: 5px 10px;"><e2><86><90> Previous</button>
    <span id="page-info_',
    table_id,
    '" style="margin: 0 10px; font-weight: bold;">Page 1 of ',
    total_pages,
    '</span>
    <button id="next-btn_',
    table_id,
    '" style="margin: 0 10px; padding: 5px 10px;">Next <e2><86><92></button>
  </div>
  '
  )

  # Full CSS: Remove wrapping, ensure column expansion, remove width restrictions
  styled_html <- paste0(
    js_script,
    "
  <style>
  /* Remove Quarto's max-width */
  .main-container {
    max-width: none !important;
  }
  /* Style the table */
  #table_",
    table_id,
    " {
    width: 100%;
    border-collapse: collapse;
    table-layout: auto;
  }
  /* Style each cell to prevent wrapping */
  #table_",
    table_id,
    " td {
    white-space: nowrap;
    padding: 8px 12px;
    font-size: 14px;
    border: 1px solid #ddd;
  }
  /* Make header rows bold */
  #table_",
    table_id,
    " .header-row td {
    font-weight: bold;
  }
  /* Add medium border under 2nd header row */
  #table_",
    table_id,
    ' .header-row:nth-child(2) td {
    border-bottom: 2px solid #333;
  }
  /* Make table horizontally scrollable if necessary */
  .table-wrapper {
    overflow-x: auto;
  }
  /* Style pagination controls */
  .pagination-controls button {
    background-color: #f8f9fa;
    border: 1px solid #dee2e6;
    border-radius: 4px;
    cursor: pointer;
  }
  .pagination-controls button:hover:not(:disabled) {
    background-color: #e9ecef;
  }
  .pagination-controls button:disabled {
    background-color: #e9ecef;
    color: #6c757d;
    cursor: not-allowed;
  }
  </style>
  <div class=\'table-wrapper\'>
    <table id="table_',
    table_id,
    '">
      ',
    table_rows,
    "
    </table>
  </div>
  ",
    pagination_controls
  )

  # Display the styled HTML table
  if (requireNamespace("IRdisplay", quietly = TRUE)) {
    # For Jupyter notebooks
    display_html(styled_html)
  } else {
    # For RStudio or other environments
    cat(styled_html)
  }
}
