#' Plot a stratified violin-boxplot with or without pairwise statistical testing
#'
#' @description
#' This function creates a violin-box plot with data points and means/stds for a
#' numeric variable grouped by a specified groups variable. It also can test for
#' significant differences between groups using a t-test or Wilcoxon rank-sum
#' test, annotating significant comparisons in the plot.
#'
#' @param df
#' Dataframe containing the grouping variable and numeric variable of interest.
#' @param groups
#' Name of the categorical variable in `df` to group by.
#' @param column
#' Name of the numeric variable in `df` to be plotted.
#' @param subgroups
#' Name of a categorical variable `df` to further subgroup each group by.
#' (default is to not subgroup)
#' @param ylab
#' Title for the y-axis. (default is to use name given to `column`)
#' @param xlab
#' Title for the x-axis. (default is to use name given to `groups`)
#' @param legendlab
#' Title for the legend. Only applicable if `subgroups` is specified. 
#' (default is to use name given to `subgroups`)
#' @param colors
#' Vector of R recognized color strings the length of the number of groups in
#' the variable provided to `subgroups`. Only applicable if `subgroups` is
#' specified.
#' @param test
#' Which test to use for group comparison: `t.test`, `wilcox.test`, or `NULL`
#' (no testing performed). (default: `wilcox.test`)
#' @param alpha
#' P-value threshold for significance. (default: 0.05)
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
#' A `ggplot2` figure object. If export is `TRUE`, also exports the image to file.
#'
#' @import ggplot2
#' @importFrom stats fisher.test chisq.test
#' @importFrom utils combn
#' @export
#'
stratified_violin_boxplot = function(
  df,
  groups,
  column,
  subgroups,
  ylab = NULL,
  xlab = NULL,
  legendlab = NULL,
  colors = NULL,
  test = "wilcox.test",
  alpha = 0.05,
  save = FALSE,
  figwidth = 5,
  figheight = 5,
  out_path = "stratified_barplot"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }

  # Prepare data
  x <- df[[groups]]
  y <- df[[column]]

  # Get group names and number of groups
  group_names <- names(table(x))
  n_groups <- length(group_names)

  # Calculate means and standard deviations for each group
  means <- tapply(y, x, mean, na.rm = TRUE)
  stds <- tapply(y, x, sd, na.rm = TRUE)

  # Calculate mean and standard deviation for all cases
  mean_all <- mean(y, na.rm = TRUE)
  std_all <- sd(y, na.rm = TRUE)

  # Create blank figure
  fig <- plot_ly()

  # Add "all cases" plot
  y_all <- y[!is.na(y)]
  # Violin
  fig <- add_trace(
    fig,
    type = "violin",
    x = rep(-1, length(y_all)),
    y = y_all,
    name = "",
    box = list(visible = FALSE),
    meanline = list(visible = FALSE),
    points = FALSE,
    line = list(color = "black"),
    fillcolor = "lightgrey",
    opacity = 1,
    width = 0.75,
    showlegend = FALSE,
    spanmode = "hard"
  )
  # Boxplot
  fig <- add_trace(
    fig,
    type = "box",
    x = rep(-1, length(y_all)),
    y = y_all,
    name = "",
    line = list(color = "black"),
    fillcolor = "white",
    opacity = 1,
    width = 0.3,
    whiskerwidth = 0,
    showlegend = FALSE,
    boxpoints = FALSE
  )
  # Points
  fig <- add_trace(
    fig,
    type = "scatter",
    mode = "markers",
    x = -1 + runif(length(y_all), -0.05, 0.05),
    y = y_all,
    name = "",
    marker = list(
      color = "black",
      size = 12,
      symbol = "circle",
      opacity = 0.75
    ),
    showlegend = FALSE,
    text = if (sum(grepl("id", names(df))) > 0) {
      id_col <- grep("id", names(df), value = TRUE)[1]
      df[[id_col]][!is.na(y)]
    } else {
      NULL
    },
    hovertemplate = if (sum(grepl("id", names(df))) > 0) {
      id_col <- grep("id", names(df), value = TRUE)[1]
      paste0(id_col, ": %{text}<br>Value: %{y}<extra></extra>")
    } else {
      "Value: %{y}<extra></extra>"
    }
  )
  # Mean + error bar
  fig <- add_trace(
    fig,
    type = "scatter",
    mode = "markers",
    x = -1,
    y = mean_all,
    name = "",
    marker = list(color = "red", size = 12, symbol = "circle"),
    error_y = list(
      type = "data",
      array = std_all,
      color = "red",
      thickness = 3
    ),
    showlegend = FALSE,
    hovertemplate = paste0(
      "Mean: ",
      round(mean_all, 1),
      "<br>Std Dev: ",
      round(std_all, 1),
      "<extra></extra>"
    )
  )

  # Create plots for each group
  for (i in seq_along(group_names)) {
    x_i <- group_names[i]
    y_i <- y[x == x_i]

    # Add violin
    fig <- add_trace(
      fig,
      type = "violin",
      x = rep(i - 1, length(y_i)),
      y = y_i,
      name = "",
      box = list(visible = FALSE),
      meanline = list(visible = FALSE),
      points = FALSE,
      line = list(color = "black"),
      fillcolor = "lightgrey",
      opacity = 1,
      width = 0.75,
      showlegend = FALSE,
      spanmode = "hard"
    )

    # Add boxplot
    fig <- add_trace(
      fig,
      type = "box",
      x = rep(i - 1, length(y_i)),
      y = y_i,
      name = "",
      line = list(color = "black"),
      fillcolor = "white",
      opacity = 1,
      width = 0.3,
      whiskerwidth = 0,
      showlegend = FALSE,
      boxpoints = FALSE
    )

    # Add individual points with jitter
    set.seed(1234)
    jitter_strength <- 0.05
    jitter <- runif(length(y_i), -jitter_strength, jitter_strength)
    fig <- add_trace(
      fig,
      type = "scatter",
      mode = "markers",
      x = (i - 1) + jitter,
      y = y_i,
      name = "",
      marker = list(
        color = "black",
        size = 12,
        symbol = "circle",
        opacity = 0.75
      ),
      showlegend = FALSE,
      text = if (sum(grepl("id", names(df))) > 0) {
        id_col <- grep("id", names(df), value = TRUE)[1]
        df[x == x_i, id_col]
      } else {
        NULL
      },
      hovertemplate = if (sum(grepl("id", names(df))) > 0) {
        id_col <- grep("id", names(df), value = TRUE)[1]
        paste0(id_col, ": %{text}<br>Value: %{y}<extra></extra>")
      } else {
        "Value: %{y}<extra></extra>"
      }
    )

    # Add mean points and error bars
    mean_val <- means[x_i]
    std_val <- stds[x_i]
    fig <- add_trace(
      fig,
      type = "scatter",
      mode = "markers",
      x = i - 1,
      y = mean_val,
      name = "",
      marker = list(color = "red", size = 12, symbol = "circle"),
      error_y = list(
        type = "data",
        array = std_val,
        color = "red",
        thickness = 3
      ),
      showlegend = FALSE,
      hovertemplate = paste0(
        "Mean: ",
        round(mean_val, 1),
        "<br>Std Dev: ",
        round(std_val, 1),
        "<extra></extra>"
      )
    )
  }

  # Statistical testing and annotation
  if (n_groups == 2) {
    # Get data for testing
    group1 <- group_names[1]
    group2 <- group_names[2]
    y1 <- y[x == group1]
    y2 <- y[x == group2]

    # Perform testing with requested test
    if (test == "t.test") {
      pval <- tryCatch(t.test(y1, y2)$p.value, error = function(e) NA)
    } else {
      pval <- tryCatch(wilcox.test(y1, y2)$p.value, error = function(e) NA)
    }

    # Generate significance label for significant associations
    signif_label <- if (!is.na(pval) && pval < alpha) {
      if (pval < 0.001) {
        "***"
      } else if (pval < 0.01) {
        "**"
      } else if (pval < 0.05) {
        "*"
      } else {
        "ns"
      }
    } else {
      "ns"
    }

    # Add annotation line and text if significant
    if (signif_label != "ns") {
      y_max <- max(y, na.rm = TRUE)
      y_annot <- y_max + 0.01 * (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))

      fig <- add_trace(
        fig,
        type = "scatter",
        mode = "lines",
        name = "",
        x = c(1, 1, 2, 2),
        y = c(y_annot - 0.01, y_annot, y_annot, y_annot - 0.01),
        line = list(color = "black", width = 2),
        showlegend = FALSE,
        hovertemplate = paste0(
          "p=",
          format(pval, scientific = TRUE, digits = 2)
        )
      )
      fig <- add_annotations(
        fig,
        x = 1.5,
        y = y_annot + 0.01 * (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)),
        text = signif_label,
        showarrow = FALSE,
        font = list(size = 16, color = "black"),
        align = "center"
      )
    }
  } else if (n_groups > 2) {
    # Get combination of groups and number of comparisons
    combn_idx <- utils::combn(seq_along(group_names), 2)
    y_range <- range(y, na.rm = TRUE)
    y_span <- y_range[2] - y_range[1]
    n_comparisons <- ncol(combn_idx)

    # Perform pairwise comparisons
    for (j in seq_len(n_comparisons)) {
      # Get data for testing
      idx1 <- combn_idx[1, j]
      idx2 <- combn_idx[2, j]
      group1 <- group_names[idx1]
      group2 <- group_names[idx2]
      y1 <- y[x == group1]
      y2 <- y[x == group2]

      # Perform testing with requested test
      if (test == "t.test") {
        pval <- tryCatch(t.test(y1, y2)$p.value, error = function(e) NA)
      } else {
        pval <- tryCatch(wilcox.test(y1, y2)$p.value, error = function(e) NA)
      }

      # Generate significance label for significant associations
      signif_label <- if (!is.na(pval) && pval < alpha) {
        if (pval < 0.001) {
          "***"
        } else if (pval < 0.01) {
          "**"
        } else if (pval < 0.05) {
          "*"
        } else {
          "ns"
        }
      } else {
        "ns"
      }

      # Add annotation lines and text if significant
      if (signif_label != "ns") {
        # Stagger annotation heights
        y_annot <- y_range[2] + (0.01 + 0.06 * (j - 1)) * y_span

        fig <- add_trace(
          fig,
          type = "scatter",
          mode = "lines",
          name = "",
          x = c(idx1, idx1, idx2, idx2) - 1,
          y = c(y_annot - 0.01, y_annot, y_annot, y_annot - 0.01),
          line = list(color = "black", width = 2),
          showlegend = FALSE,
          hovertemplate = paste0(
            "p=",
            format(pval, scientific = TRUE, digits = 2)
          )
        )
        fig <- add_annotations(
          fig,
          x = mean(c(idx1, idx2)) - 1,
          y = y_annot + 0.01 * y_span,
          text = signif_label,
          showarrow = FALSE,
          font = list(size = 16, color = "black"),
          align = "center"
        )
      }
    }
  }

  # Adjust labels and formatting
  fig <- layout(
    fig,
    title = NULL,
    margin = list(t = 20),
    xaxis = list(
      title = ifelse(!is.null(xlab), xlab, groups),
      tickmode = "array",
      tickvals = c(-1, 0:(length(group_names) - 1)),
      ticktext = c("All cases", group_names),
      ticks = "outside",
      tickcolor = "black",
      showline = TRUE,
      linecolor = "black",
      linewidth = 2,
      zeroline = FALSE,
      mirror = FALSE,
      showgrid = FALSE
    ),
    yaxis = list(
      title = ifelse(!is.null(ylab), ylab, column),
      ticks = "outside",
      tickcolor = "black",
      showline = TRUE,
      linecolor = "black",
      linewidth = 2,
      zeroline = FALSE,
      mirror = FALSE,
      showgrid = FALSE
    ),
    font = list(
      color = "black",
      size = 16
    ),
    plot_bgcolor = "white"
  )

  return(fig)
}
