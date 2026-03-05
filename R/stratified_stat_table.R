#' Create a stratified summary table with regression-based comparisons
#'
#' @description
#' Builds a publication-style table of summary statistics for selected columns,
#' including totals and group-stratified estimates, and performs regression-based
#' tests of difference between groups. Categorical variables are summarized as
#' counts and percentages and compared using Firth's penalized logistic
#' regression (one-vs-category with a group indicator); numeric variables are
#' summarized as mean \eqn{\pm}{+/-} SD and compared using linear regression. Optional
#' covariates can be included to adjust the models.
#'
#' @param df
#' Input dataframe containing the grouping variable, columns to summarize,
#' and any optional covariates.
#' @param groups
#' Name of the grouping variable in `df`. Each unique value defines a group
#' for stratified summaries and serves as the binary indicator (vs. all others)
#' in the regression models.
#' @param columns
#' Names of columns in `df` to summarize and test.
#' @param rename_dict
#' Named list with optional mapping from original column names to display labels.
#' If provided, `rename_dict[col]` is used in the "Variable" column of the output;
#' otherwise, the original name is used. (Function expects that all `columns`
#' exist as names in `rename_dict` when provided.)
#' @param covariates
#' Optional names of additional columns in `df` to include as covariates
#' in the regression models for between-group comparisons.
#'
#' @return
#' A `data.frame` where rows correspond to categories (for categorical variables)
#' or a single row (for numeric variables) per input variable, and columns include
#' the variable label, categories, total summary, stratified group summaries, and
#' regression results (Coefficient and P-value).
#'
#' @importFrom stringr str_split str_to_title
#' @importFrom logistf logistf
#' @importFrom stats lm confint pnorm sd na.omit
#' @export
#'
stratified_stat_table = function(
  df,
  groups,
  columns,
  rename_dict = NULL,
  covariates = NULL
) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required.")
  }
  if (!requireNamespace("logistf", quietly = TRUE)) {
    stop("Package 'logistf' is required.")
  }

  # Perform a few data checks
  if (!(groups %in% colnames(df))) {
    stop("ERROR: groups variable name was not found in column names of df")
  }
  if (sum(columns %in% colnames(df)) != length(columns)) {
    stop("ERROR: some column names were not found in df")
  }
  if (!(is.factor(df[[groups]]) | is.character(df[[groups]]))) {
    stop("ERROR: groups variable is not a factor or character variable")
  }
  for (col in columns) {
    if (
      !(is.factor(df[[col]]) | is.character(df[[col]]) | is.numeric(df[[col]]))
    ) {
      stop(paste0(
        "ERROR: ",
        col,
        " is not a factor, character, or numeric variable"
      ))
    }
  }

  # Create a small function to capitalize categories more aesthetically
  format_string = function(x) {
    nocaps = "^and$|^or$|^at$|^in$|^of$|^the$|^for$|^by$|^to$|^with$|^Mean\u00B1SD$"
    alwayscaps = "^II$|^III$|^IV$|^V$|^VI$|^VII$|^VIII$|^VIIII$|^X$"
    paste(
      sapply(unlist(stringr::str_split(x, " ")), function(y) {
        ifelse(
          grepl(nocaps, y, ignore.case = TRUE),
          y,
          ifelse(
            grepl(alwayscaps, y, ignore.case = TRUE),
            toupper(y),
            stringr::str_to_title(y)
          )
        )
      }),
      collapse = " "
    )
  }

  # For each column of interest, calculate summary statistics and
  # and test for differences between groups
  results = data.frame()
  for (col in columns) {
    col_res = data.frame()

    if (is.factor(df[[col]]) | is.character(df[[col]])) {
      # Calculate summary statistics for total cases
      n = table(df[[col]])
      perc = round(n / sum(n) * 100, 1)
      n_perc = paste0(n, " (", perc, "%)")

      # Add to results
      col_res = rbind(
        col_res,
        data.frame(
          Variable = sapply(
            gsub(
              "_",
              " ",
              c(
                ifelse(
                  !is.null(rename_dict),
                  paste0(rename_dict[col], ", N (%)"),
                  paste0(col, ", N (%)")
                ),
                rep("", length(n) - 1)
              )
            ),
            function(x) format_string(x)
          ),
          Categories = sapply(names(n), function(x) format_string(x)),
          Total = n_perc
        )
      )

      # Add total N to total case column name
      colnames(col_res)[ncol(col_res)] = paste0("Total (N=", nrow(df), ")")

      # Calculate summary statistics stratified by grouping variable and
      # perform statistical testing with Firth's penalized logistic regression
      if (col != groups) {
        for (group in names(table(df[[groups]]))) {
          group_res = data.frame()

          for (cat in names(table(df[[col]]))) {
            # Create dummy variables
            y = ifelse(df[[col]] == cat, 1, 0)
            x = ifelse(df[[groups]] == group, 1, 0)

            # Calculate summary statistics
            n = table(y, x)
            perc = apply(n, 2, function(x) round(x / sum(x) * 100, 1))
            n_perc = data.frame(matrix(
              paste0(n, " (", perc, "%)"),
              nrow = nrow(n),
              ncol = ncol(n)
            ))

            # Perform Firth's penalized logistic regression
            if (!is.null(covariates)) {
              x = cbind(x, df[, covariates])
            }
            fit = logistf::logistf(y ~ ., data = data.frame(x))
            coef = paste0(
              round(exp(fit$coefficients[2]), 2),
              " [",
              round(exp(fit$ci.lower[2]), 2),
              "; ",
              round(exp(fit$ci.upper[2]), 2),
              "]"
            )
            pval = fit$prob[2]

            # If p-value is 0, manually calculate
            if (pval == 0) {
              zstat = fit$coefficients[2] / sqrt(diag(fit$var))[2]
              pval = 2 * pnorm(abs(zstat), lower.tail = FALSE)
            }

            # Add results for group at current category
            group_res = rbind(
              group_res,
              data.frame(
                n_perc[2, 2],
                `Coef [95%CI]` = coef,
                P = formatC(pval, digits = 2, format = "e"),
                check.names = FALSE
              )
            )
          }

          # Remove one p-value if column only has 2 categories
          if (nrow(group_res) == 2) {
            group_res[1, "P"] = ""
          }

          # Add group name with total N
          colnames(group_res)[1] = paste0(
            format_string(group),
            " (N=",
            table(df[[groups]])[group],
            ")"
          )

          # Add group results
          col_res = cbind(col_res, group_res)
        }
      }
    }

    if (is.numeric(df[[col]])) {
      # Calculate summary statistics for total cases
      avg = round(mean(df[[col]], na.rm = TRUE), 1)
      std = round(sd(df[[col]], na.rm = TRUE), 1)
      avg_std = paste0(avg, "\u00B1", std)

      # Add to results
      col_res = rbind(
        col_res,
        data.frame(
          Variable = sapply(
            gsub(
              "_",
              " ",
              ifelse(
                !is.null(rename_dict),
                paste0(rename_dict[col], ", Mean\u00B1SD"),
                paste0(col, ", Mean\u00B1SD")
              )
            ),
            function(x) format_string(x)
          ),
          Categories = "-",
          Total = avg_std
        )
      )

      # Add total N to total case column name
      colnames(col_res)[ncol(col_res)] = paste0("Total (N=", nrow(df), ")")

      # Calculate summary statistics stratified by grouping variable and
      # perform statistical testing with linear regression
      for (group in names(table(df[[groups]]))) {
        group_res = data.frame()

        # Create dummy variables
        y = df[[col]]
        x = ifelse(df[[groups]] == group, 1, 0)

        # Calculate summary statistics
        avg = round(mean(y[x == 1], na.rm = TRUE), 1)
        std = round(sd(y[x == 1], na.rm = TRUE), 1)
        avg_std = paste0(avg, "\u00B1", std)

        # Perform linear regression
        if (!is.null(covariates)) {
          x = cbind(x, df[, covariates])
        }
        fit = lm(y ~ ., data = data.frame(x))
        ci = confint(fit)[2, ]
        coef = paste0(
          round(fit$coefficients[2], 2),
          " [",
          round(ci[1], 2),
          "; ",
          round(ci[2], 2),
          "]"
        )
        pval = summary(fit)$coefficients[2, 4]

        # Add results for group at current category
        group_res = data.frame(
          avg_std,
          `Coef [95%CI]` = coef,
          P = formatC(pval, digits = 2, format = "e"),
          check.names = FALSE
        )

        # Add group name with total N
        colnames(group_res)[1] = paste0(
          format_string(group),
          " (N=",
          table(df[[groups]])[group],
          ")"
        )

        # Add group results
        col_res = cbind(col_res, group_res)
      }
    }

    # Add current column results to full results
    results = rbind(results, col_res)
  }

  # If group variable only has two levels, remove first set of results
  # (it's only the reciprocal of the second results)
  if (length(unique(na.omit(df[[groups]]))) == 2) {
    results = results[, -grep("Coef |P$", colnames(results))[c(1, 2)]]
  }

  return(results)
}
