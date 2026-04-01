#' Plot a volcano plot of coefficients and corresponding p-values
#'
#' @description
#' Creates a volcano plot from a data frame with variable, coefficient, and
#' p-value columns. Significant points are colored (red for positive, blue for
#' negative) and non-significant points are grey. Significant points are
#' labeled with their variable name.
#'
#' @param df
#' Dataframe with columns to be plotted.
#' @param variable
#' Name of column containing variable names in `df`.
#' @param coef
#' Name of column containing coefficients in `df`.
#' @param pvalue
#' Name of column containing p-values in `df`.
#' @param alpha
#' Significance threshold (default: 0.05).
#' @param transform_pvalue
#' Whether to transform the p-value using `-log10()`. Set to `FALSE` if p-values
#' are already transformed in some way. (default: TRUE)
#'
#' **NOTE:** if you are not supplying untransformed p-values and setting this to
#' `TRUE`, make sure what you set as `alpha` is also already transformed in the
#' same manner as the supplied p-values.
#'
#' @param first_group_name
#' Of the two groups tested, what is the name of the first group? (i.e., the
#' reference group). This is used to add to the legend labels to make the
#' results more clear. (default is to not include a name)
#' @param second_group_name
#' Of the two groups tested, what is the name of the second group? (i.e., the
#' non-reference group that dictates what the effect direction is of the
#' coefficient). This is used to add to the legend labels to make the results
#' more clear. (default is to not include a name)
#' @param top_n
#' How many labels to show of the top N results on both ends of the volcano plot.
#' (default: 5)
#' @param ylab
#' Title for the y-axis. (default is to use `-log10 p-value`)
#' @param xlab
#' Title for the x-axis. (default is to use `Coefficient`)
#' @param color_list
#' Vector of R recognized color strings of length 3 for coloring significantly
#' enriched or depleted points and non-significant points. (default is to color
#' enriched points `red`, depleted points `blue`, and non-significant points
#' `grey`)
#' @param save
#' Whether to save the image to file. (default: FALSE)
#' @param figwidth
#' Width of the output image file in pixels (`px`).
#' @param figheight
#' Height of the output image file in pixels (`px`).
#' @param out_path
#' File path for the output image file. Existing files at this path are overwritten.
#' Whatever extension included in the image file name (e.g., jpg, png, pdf) will
#' be the format that is outputted.
#'
#' @return
#' A `ggplot2` figure object. If save is `TRUE`, also exports the image to file.
#'
#' @import ggplot2
#' @importFrom ggrepel geom_label_repel
#' @importFrom grid unit
#' @export
#'
volcano_plot = function(
  df,
  variable,
  coef,
  pvalue,
  alpha = 0.05,
  transform_pvalue = TRUE,
  first_group_name = NULL,
  second_group_name = NULL,
  top_n = 5,
  ylab = "-log10 p-value",
  xlab = "Coefficient",
  color_list = NULL,
  save = FALSE,
  figwidth = 1000,
  figheight = 1000,
  out_path = "volcano_plot.jpg"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Package 'ggrepel' is required.")
  }

  # Perform a few data checks
  if (!(variable %in% colnames(df))) {
    stop("ERROR: variable name column was not found in column names of df")
  }
  if (!(coef %in% colnames(df))) {
    stop("ERROR: name of coefficient column was not found in df")
  }
  if (!(pvalue %in% colnames(df))) {
    stop("ERROR: name of p-value column was not found in df")
  }
  if (!(is.factor(df[[variable]]) | is.character(df[[variable]]))) {
    stop("ERROR: variable name column is not a factor or character variable")
  }
  if (!(is.numeric(df[[coef]]))) {
    warning("WARNING: coefficient column was not numeric and will be converted")
    df[[coef]] = as.numeric(df[[coef]])
  }
  if (!(is.numeric(df[[pvalue]]))) {
    warning("WARNING: p-value column was not numeric and will be converted")
    df[[pvalue]] = as.numeric(df[[pvalue]])
  }
  if (sum(df[[pvalue]] == 0) > 0) {
    stop(paste0(
      "ERROR: p-value column contains zeros, try manually calculating ",
      "p-values as some functions in R give zero p-values"
    ))
  }
  if (!is.null(color_list)) {
    if (length(color_list) != 3) {
      stop("ERROR: color list should have 3 colors in it")
    }
  }

  # Add column for designating significant results
  coef_mid = ifelse(min(df[[coef]], na.rm = TRUE) < 0, 0, 1)
  df[["Result"]] = ifelse(
    df[[coef]] < coef_mid & df[[pvalue]] < alpha,
    ifelse(
      !is.null(first_group_name),
      paste0(
        "Enriched in ",
        first_group_name,
        " (N=",
        sum(df[[coef]] < coef_mid & df[[pvalue]] < alpha),
        ", ",
        round(
          sum(df[[coef]] < coef_mid & df[[pvalue]] < alpha) / nrow(df) * 100,
          1
        ),
        "%)"
      ),
      paste0(
        "Depleted (N=",
        ", ",
        round(
          sum(df[[coef]] < coef_mid & df[[pvalue]] < alpha) / nrow(df) * 100,
          1
        ),
        "%)"
      )
    ),
    ifelse(
      df[[coef]] > coef_mid & df[[pvalue]] < alpha,
      ifelse(
        !is.null(second_group_name),
        paste0(
          "Enriched in ",
          second_group_name,
          " (N=",
          sum(df[[coef]] > coef_mid & df[[pvalue]] < alpha),
          ", ",
          round(
            sum(df[[coef]] > coef_mid & df[[pvalue]] < alpha) / nrow(df) * 100,
            1
          ),
          "%)"
        ),
        paste0(
          "Enriched (N=",
          sum(df[[coef]] > coef_mid & df[[pvalue]] < alpha),
          ", ",
          round(
            sum(df[[coef]] > coef_mid & df[[pvalue]] < alpha) / nrow(df) * 100,
            1
          ),
          "%)"
        )
      ),
      paste0(
        "Not Significant (N=",
        sum(df[[pvalue]] >= alpha),
        ", ",
        round(sum(df[[pvalue]] >= alpha) / nrow(df) * 100, 1),
        "%)"
      )
    )
  )

  # Add label column for labeling significant results and mask all but the
  # top N significant results
  df[["label"]] = ifelse(df[[pvalue]] < alpha, df[[variable]], NA)
  df = df[order(df[[pvalue]]), ]
  df[["label"]][df[[coef]] < coef_mid][-c(1:top_n)] = NA
  df[["label"]][df[[coef]] > coef_mid][-c(1:top_n)] = NA

  # Create color vector for plotting if one was not provided
  if (is.null(color_list)) {
    color_list = c("blue", "red", "grey")
  }
  names(color_list) = c(
    unique(df[["Result"]][df[[coef]] < coef_mid & df[[pvalue]] < alpha]),
    unique(df[["Result"]][df[[coef]] > coef_mid & df[[pvalue]] < alpha]),
    unique(df[["Result"]][df[[pvalue]] >= alpha])
  )

  # Transform p-value if specified
  if (transform_pvalue) {
    df[[pvalue]] = -log10(df[[pvalue]])
  }

  # Perform plotting
  set.seed(1234)
  g = ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data[[coef]], y = .data[[pvalue]])
  ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data[["Result"]]),
      size = 2,
      alpha = 0.5
    ) +
    ggplot2::geom_vline(xintercept = coef_mid, linetype = "dashed") +
    ggplot2::geom_hline(
      yintercept = ifelse(transform_pvalue, -log10(alpha), alpha),
      linetype = "dashed"
    ) +
    ggrepel::geom_label_repel(
      ggplot2::aes(label = .data[["label"]]),
      force_pull = 0,
      min.segment.length = 0.1,
      box.padding = 0.2,
      max.overlaps = Inf,
      color = "black",
      segment.color = "black"
    ) +
    ggplot2::scale_color_manual(
      labels = stringr::str_wrap(
        sapply(unique(df[["Result"]]), function(x) x),
        width = 20
      ),
      values = color_list
    ) +
    ggplot2::labs(
      x = ifelse(is.null(xlab), "-log10 p-value", xlab),
      y = ifelse(is.null(ylab), "Coefficient", ylab)
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = "black", linewidth = 0.5),
      legend.key.spacing.y = grid::unit(0.2, "lines")
    )

  # Save plot to file if requested
  if (save) {
    ggplot2::ggsave(
      out_path,
      g,
      width = figwidth,
      height = figheight,
      units = "px"
    )
  }

  return(g)
}
