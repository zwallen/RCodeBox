#' Plot a longtail (non-normalized stacked) barplot
#'
#' @description
#' This function creates a non-normalized stacked bar plot reporting frequencies of
#' a categorical variable grouped by a specified grouping variable including total
#' frequencies for each group of the grouping variable.
#'
#' @param df
#' Dataframe containing the grouping variable and categorical variable of interest.
#' Should be in a "long" format where each row is an observation (e.g., what
#' you get when reshaping a data.frame using `tidyr::pivot_longer()`,
#' `reshape2::melt()`, `reshape()`, etc.).
#' @param groups
#' Name of the categorical variable in `df` to group by (i.e., what each bar of
#' the plot will represent).
#' @param column
#' Name of the categorical variable in `df` to be plotted (i.e., how each bar of
#' the plot will be broken up).
#' @param negative_value
#' Name of the value in `column` that represents a "negative" or "absent" result.
#' (e.g., `0` for a binary column or `NULL`). Can be an actual value or `NA`.
#' @param ylab
#' Title for the y-axis. (default is to use `Frequency (%)`)
#' @param xlab
#' Title for the x-axis. (default is to use name given to `groups`)
#' @param legendlab
#' Title for the legend. (default is to use name given to `column`)
#' @param color_list
#' Vector of R recognized color strings the length of the number of groups in
#' the variable provided to `column`.
#' @param flip_plot
#' Whether to flip the plot so bars are now horizontal. (default: FALSE)
#' @param keep_caps
#' Vector of character strings to make sure to keep capitalized. The function
#' automatically tries to keep roman numerals capitalized, but any other string
#' needs to be provided here.
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
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#' @importFrom grid unit
#' @export
#'
longtail_barplot = function(
  df,
  groups,
  column,
  negative_value,
  ylab = NULL,
  xlab = NULL,
  legendlab = NULL,
  color_list = NULL,
  flip_plot = FALSE,
  keep_caps = NULL,
  save = FALSE,
  figwidth = 1000,
  figheight = 1000,
  out_path = "composition_barplot.jpg"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required.")
  }
  if (!requireNamespace("tidyselect", quietly = TRUE)) {
    stop("Package 'tidyselect' is required.")
  }

  # Perform a few data checks
  if (!(groups %in% colnames(df))) {
    stop("ERROR: groups variable name was not found in column names of df")
  }
  if (!(column %in% colnames(df))) {
    stop("ERROR: column variable name was not found in df")
  }
  if (!(negative_value %in% names(table(df[[column]], exclude = FALSE)))) {
    stop(
      "ERROR: negative value provided was not found in the variable given to column"
    )
  }
  if (!(is.factor(df[[groups]]) | is.character(df[[groups]]))) {
    stop("ERROR: groups variable is not a factor or character variable")
  }
  if (!(is.factor(df[[column]]) | is.character(df[[column]]))) {
    stop("ERROR: column variable is not a factor or character variable")
  }

  # Remove any missing observations
  plot_df = df[rowSums(is.na(df[, c(groups, column)])) == 0, ]

  # Mask specified "negative" values
  plot_df[[column]] = as.character(plot_df[[column]])
  plot_df[[column]][plot_df[[column]] == negative_value] = NA

  # Calculate overall frequencies for groups
  n = sort(
    table(plot_df[[groups]][!is.na(plot_df[[column]])]),
    decreasing = TRUE
  )
  perc = round(n / table(plot_df[[groups]]), 3)
  overall_freq = data.frame(perc)

  # Calculate within group frequencies
  n = table(plot_df[[column]], plot_df[[groups]])
  perc = data.frame(sapply(1:ncol(n), function(x) {
    n[, x] / table(plot_df[[groups]])[x]
  }))
  colnames(perc) = colnames(n)
  plot_df = tidyr::pivot_longer(
    perc,
    cols = tidyselect::everything(),
    names_to = groups,
    values_to = "Freq"
  )
  plot_df[[column]] = unlist(sapply(
    rownames(n),
    function(x) rep(x, ncol(n)),
    simplify = FALSE
  ))

  # Make group levels be sorted by overall frequency
  cat_order = overall_freq[["Var1"]]
  plot_df[[groups]] = factor(plot_df[[groups]], levels = cat_order)

  # Make sure levels of column are the same as input
  plot_df[[column]] = factor(
    plot_df[[column]],
    levels = names(table(df[[column]]))[
      names(table(df[[column]])) != negative_value
    ]
  )

  # Create color vector for plotting if one was not provided
  if (is.null(color_list)) {
    color_list = RColorBrewer::brewer.pal(
      length(levels(plot_df[[column]])),
      "Set2"
    )
  }

  # Perform plotting
  ymax = max(
    overall_freq[["Freq"]] +
      nchar(paste0(overall_freq[["Freq"]] * 100, "%")) / 100,
    na.rm = TRUE
  )
  g = ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      y = .data[["Freq"]],
      x = .data[[groups]],
      fill = .data[[column]],
      color = .data[[column]]
    )
  ) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::geom_bar(
      inherit.aes = FALSE,
      data = overall_freq,
      stat = "identity",
      ggplot2::aes(y = .data[["Freq"]], x = .data[["Var1"]]),
      alpha = 0,
      color = "black"
    ) +
    ggplot2::geom_text(
      inherit.aes = FALSE,
      data = overall_freq,
      ggplot2::aes(
        y = .data[["Freq"]],
        x = .data[["Var1"]],
        label = paste0(.data[["Freq"]] * 100, "%")
      ),
      angle = ifelse(flip_plot, 0, 90),
      hjust = -0.1
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      breaks = seq(0, 1, 0.1),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::scale_fill_manual(
      labels = stringr::str_wrap(
        sapply(levels(plot_df[[column]]), function(x) {
          format_string(x, keep_caps)
        }),
        width = 20
      ),
      values = color_list
    ) +
    ggplot2::scale_color_manual(
      labels = stringr::str_wrap(
        sapply(levels(plot_df[[column]]), function(x) {
          format_string(x, keep_caps)
        }),
        width = 20
      ),
      values = color_list
    ) +
    ggplot2::coord_cartesian(ylim = c(0, ymax + 0.05), clip = "off") +
    ggplot2::labs(
      x = ifelse(is.null(xlab), format_string(groups, keep_caps), xlab),
      y = ifelse(is.null(ylab), "Frequency (%)", ylab),
      fill = stringr::str_wrap(
        ifelse(is.null(legendlab), format_string(column, keep_caps), legendlab),
        width = 15
      )
    ) +
    ggplot2::guides(color = "none") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5) #,
      #panel.border = ggplot2::element_rect(color = "black", linewidth = 0.5)
    )

  # Flip plot if specified
  if (flip_plot) {
    g = g +
      ggplot2::scale_x_discrete(limits = rev) +
      ggplot2::coord_flip(ylim = c(0, ymax + 0.05), clip = "off") +
      ggplot2::theme(panel.spacing = grid::unit(1, "lines"))
  }

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
