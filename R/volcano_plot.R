#' Plot a Volcano Plot Using Plotly
#'
#' Creates a volcano plot from a data frame with variable, coefficient, and p-value columns.
#' Significant points are colored (red for positive, blue for negative) and non-significant
#' points are grey. Significant points are labeled with their variable name.
#'
#' @param data Data frame with columns to be plotted.
#' @param var Name of variable column.
#' @param coef Name of coefficient column.
#' @param pvalue Name of p-value column.
#' @param alpha Significance threshold (default: 0.05).
#' @param top_n Number of most significant associations to show labels for on both
#' negative and positive ends (default: 5).
#' @param ylab Title for y-axis (defaults to "-log10(p-value)").
#' @param xlab Title for x-axis (defaults to "Coefficient").
#' @return A `plotly` figure object.
#' @import plotly
#' @export
#'
volcano_plot <- function(
  data,
  var,
  coef,
  pvalue,
  alpha = 0.05,
  top_n = 5,
  ylab = "-log10(p-value)",
  xlab = "Coefficient"
) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required.")
  }

  # Prepare data
  data[["neglog10p"]] <- -log10(data[[pvalue]])
  data[["sig"]] <- data[[pvalue]] < alpha

  # Determine coefficient midpoint
  coef_midpoint <- ifelse(
    min(data[[coef]]) < 0 & max(data[[coef]]) > 0,
    0,
    ifelse(
      min(data[[coef]]) < 1 & max(data[[coef]]) > 1,
      1,
      stop("No midpoint detected for range of coefficients given")
    )
  )

  # Color assignment
  data[["color"]] <- "lightgrey"
  data[["color"]][data[["sig"]] & data[[coef]] > coef_midpoint] <- "#f7758c"
  data[["color"]][data[["sig"]] & data[[coef]] < coef_midpoint] <- "#3a5ce9"

  # Create columns to help with sorting
  data[["coef_direction"]] <- ifelse(data[[coef]] < coef_midpoint, 0, 1)

  # Isolate top n negative association labels
  data[["label"]] <- ""
  data <- data[order(data[["coef_direction"]], data[[pvalue]]), ]
  data[["label"]][1:top_n] <- data[[var]][1:top_n]

  # Isolate top n negative association labels
  data <- data[order(-xtfrm(data[["coef_direction"]]), data[[pvalue]]), ]
  data[["label"]][1:top_n] <- data[[var]][1:top_n]

  # Generate main volcano plot
  fig <- plot_ly(
    data = data,
    x = ~ get(coef),
    y = ~neglog10p,
    type = "scatter",
    mode = "markers+text",
    name = "",
    text = ~label,
    textposition = "top middle",
    marker = list(
      color = ~color,
      size = 12,
      line = list(color = "black", width = 1)
    ),
    hovertemplate = paste0(
      var,
      ": ",
      data[[var]],
      "<br>",
      "Coef: %{x}<br>",
      "p-value: ",
      format(data[[pvalue]], scientific = TRUE, digits = 2)
    ),
    showlegend = FALSE
  )

  # Add reference lines at x-axis midpoint and alpha on the y-axis
  fig <- layout(
    fig,
    shapes = list(
      list(
        type = "line",
        x0 = coef_midpoint,
        x1 = coef_midpoint,
        y0 = 0,
        y1 = 1,
        xref = "x",
        yref = "paper",
        opacity = 0.5,
        line = list(color = "black", dash = "dash"),
        layer = "below"
      ),
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        y0 = -log10(alpha),
        y1 = -log10(alpha),
        xref = "paper",
        yref = "y",
        opacity = 0.5,
        line = list(color = "black", dash = "dash"),
        layer = "below"
      )
    )
  )

  # Adjust labels and formatting
  fig <- layout(
    fig,
    title = NULL,
    margin = list(t = 20),
    xaxis = list(
      title = ifelse(!is.null(xlab), xlab, coef),
      tickmode = "array",
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
    plot_bgcolor = "white"
  )

  return(fig)
}
