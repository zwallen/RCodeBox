#' Plot a Stratified Barplot Using Plotly
#'
#' This function creates a bar plot reporting frequencies of a categorical variable
#' grouped by a specified strata variable. It can also test for significant differences
#' between groups using Fisher's exact test or Chi-squared test, annotating significant
#' comparisons in the plot.
#'
#' @param data The data frame containing the variables of interest.
#' @param var The name of the categorical variable in `data` to be plotted.
#' @param strata The name of the categorical variable in `data` to group by.
#' @param ylab The title for the y-axis (default is to use `Frequency (%)`).
#' @param xlab The title for the x-axis (default is to use name given to `strata`).
#' @param legendlab The title for the legend (default is to use name given to `var`).
#' @param colors A vector of R recognized color strings the length of the number of groups
#' in the variable provided to `var` (vector).
#' @param test Which test to use for group comparison: `fisher.test` or `chisq.test`
#' (default: `fisher.test`).
#' @param alpha P-value threshold for significance (default: 0.05).
#' @return A `plotly` figure object.
#' @import plotly
#' @importFrom stats fisher.test chisq.test
#' @importFrom utils combn
#' @export
#'
stratified_barplot <- function(
    data,
    var,
    strata,
    ylab = NULL,
    xlab = NULL,
    legendlab = NULL,
    colors = NULL,
    test = c("fisher.test", "chisq.test"),
    alpha = 0.05) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required.")
  }
  test <- match.arg(test)

  # Prepare data
  x <- data[[strata]]
  y <- data[[var]]

  # Get group names and number of groups
  group_names <- unique(as.character(x))
  n_groups <- length(group_names)

  # Define color vector for plotting
  if (!is.null(colors)) {
    col_vec <- colors[1:length(unique(y))]
  } else {
    col_vec <- RColorBrewer::brewer.pal(length(unique(y)), "Greys")
  }

  # Get contingency table of counts
  counts <- table(x, y)
  percents <- prop.table(counts, 1) * 100

  # Create blank figure
  fig <- plot_ly()

  # Add an "all cases" bar
  fig <- fig |>
    add_trace(
      type = "bar",
      x = "All cases",
      y = colSums(counts) / sum(counts) * 100,
      name = "All",
      marker = list(
        color = col_vec,
        line = list(color = "black", width = 2)
      ),
      width = 0.75,
      text = paste0(
        round(colSums(counts) / sum(counts) * 100, 1), "%, N=", colSums(counts)
      ),
      textposition = "outside",
      textfont = list(size = 50 / (n_groups + 1)),
      hoverinfo = "skip"
    )

  # Create plots for each group
  for (i in seq_along(colnames(counts))) {
    y_i <- colnames(counts)[i]

    # Add bar plot
    fig <- fig |>
      add_trace(
        type = "bar",
        x = rownames(counts),
        y = percents[, y_i],
        name = y_i,
        marker = list(
          color = col_vec[i],
          line = list(color = "black", width = 2)
        ),
        width = 0.75,
        text = ifelse(
          counts[, y_i] > 0,
          paste0(round(percents[, y_i], 1), "%, N=", counts[, y_i]),
          ""
        ),
        textposition = "outside",
        textfont = list(size = 50 / (n_groups + 1)),
        hoverinfo = "skip"
      )
  }

  # Statistical testing and annotation
  if (n_groups == 2) {
    # Get data for testing
    group1 <- group_names[1]
    group2 <- group_names[2]
    tbl <- counts[c(group1, group2), , drop = FALSE]

    # Perform testing with requested test
    if (test == "fisher.test") {
      pval <- tryCatch(fisher.test(tbl)$p.value, error = function(e) NA)
      test_name <- "Fisher's exact"
    } else {
      pval <- tryCatch(chisq.test(tbl)$p.value, error = function(e) NA)
      test_name <- "Chi-squared"
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
      y_max <- max(rowSums(percents), na.rm = TRUE)
      y_annot <- 100 +
        0.1 * (max(percents, na.rm = TRUE) - min(percents, na.rm = TRUE))

      fig <- fig |>
        add_trace(
          type = "scatter",
          mode = "lines",
          name = "",
          x = c(group1, group1, group2, group2),
          y = c(y_annot - 0.01, y_annot, y_annot, y_annot - 0.01),
          line = list(color = "black", width = 2),
          showlegend = FALSE,
          hovertemplate = paste0(
            "p=",
            format(pval, scientific = TRUE, digits = 2)
          )
        ) |>
        add_annotations(
          x = 0.5,
          y = y_annot +
            0.01 * (max(percents, na.rm = TRUE) - min(percents, na.rm = TRUE)),
          text = signif_label,
          showarrow = FALSE,
          font = list(size = 16, color = "black"),
          align = "center"
        )
    }
  } else if (n_groups > 2) {
    # Get combination of groups and number of comparisons
    combn_idx <- combn(seq_along(group_names), 2)
    y_range <- range(percents, na.rm = TRUE)
    y_span <- y_range[2] - y_range[1]
    n_comparisons <- ncol(combn_idx)

    # Perform pairwise testing
    for (j in seq_len(n_comparisons)) {
      # Get data for testing
      idx1 <- combn_idx[1, j]
      idx2 <- combn_idx[2, j]
      group1 <- group_names[idx1]
      group2 <- group_names[idx2]
      tbl <- counts[c(group1, group2), , drop = FALSE]

      # Perform testing with requested test
      if (test == "fisher.test") {
        pval <- tryCatch(fisher.test(tbl)$p.value, error = function(e) NA)
        test_name <- "Fisher's exact"
      } else {
        pval <- tryCatch(chisq.test(tbl)$p.value, error = function(e) NA)
        test_name <- "Chi-squared"
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
        y_annot <- 100 + (0.1 + 0.06 * (j - 1)) * y_span

        fig <- fig |>
          add_trace(
            type = "scatter",
            mode = "lines",
            name = "",
            x = c(group1, group1, group2, group2),
            y = c(y_annot - 0.01, y_annot, y_annot, y_annot - 0.01),
            line = list(color = "black", width = 2),
            showlegend = FALSE,
            hovertemplate = paste0(
              "p=",
              format(pval, scientific = TRUE, digits = 2)
            )
          ) |>
          add_annotations(
            x = mean(c(idx1, idx2)),
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
  fig <- fig |>
    layout(
      barmode = "stack",
      title = NULL,
      margin = list(t = 20),
      xaxis = list(
        title = ifelse(!is.null(xlab), xlab, strata),
        tickmode = "array",
        tickvals = c("All cases", rownames(counts)),
        ticktext = c("All cases", rownames(counts)),
        ticks = "outside",
        tickcolor = "black",
        showline = TRUE,
        linecolor = "black",
        linewidth = 2,
        zeroline = FALSE,
        mirror = TRUE,
        showgrid = FALSE
      ),
      yaxis = list(
        title = ifelse(!is.null(ylab), ylab, "Frequency (%)"),
        ticks = "outside",
        tickcolor = "black",
        tickvals = seq(0, 100, by = 20),
        ticktext = seq(0, 100, by = 20),
        showline = TRUE,
        linecolor = "black",
        linewidth = 2,
        zeroline = FALSE,
        mirror = TRUE,
        showgrid = FALSE
      ),
      font = list(
        color = "black",
        size = 16
      ),
      plot_bgcolor = "white",
      legend = list(
        title = list(text = ifelse(!is.null(legendlab), legendlab, var))
      )
    )

  return(fig)
}
