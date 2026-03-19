#' Nicely format a string to title case
#'
#' @description
#' Formats a string to title case while keeping some strings like roman numerals
#' and user specified patterns capitalized. Also ignores standard words that are
#' usually not capitalized in a title (e.g., and, or, the, in, etc.).
#'
#' @param string
#' The string to format.
#' @param keep_caps
#' String patterns within the main string to make sure and keep capitalized
#' (e.g., abbreviations).
#'
#' @return
#' A formatted character string.
#'
#' @importFrom stringr str_split str_to_title
#' @export
#'
format_string = function(string, keep_caps = NULL) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required.")
  }

  nocaps = "^and$|^but$|^or$|^at$|^in$|^of$|^the$|^for$|^by$|^to$|^with$|^Mean\u00B1SD$"
  alwayscaps = paste0(
    "^II$|^III$|^IV$|^V$|^VI$|^VII$|^VIII$|^VIIII$|^X$",
    ifelse(!is.null(keep_caps), "|", ""),
    paste(keep_caps, collapse = "|")
  )
  paste(
    sapply(unlist(stringr::str_split(string, " ")), function(x) {
      ifelse(
        grepl(nocaps, x, ignore.case = TRUE),
        x,
        ifelse(
          grepl(alwayscaps, x, ignore.case = TRUE),
          toupper(x),
          stringr::str_to_title(x)
        )
      )
    }),
    collapse = " "
  )
}
