# RCodeBox

[![R-CMD-check](https://github.com/zwallen/RCodeBox/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zwallen/RCodeBox/actions/workflows/R-CMD-check.yaml)

A collection of R functions for commonly performed actions in computational biology and data science projects.

The most recent release of the package can be installed from GitHub:

```r
install.packages("devtools")
devtools::install_github("zwallen/RCodeBox@*release")
```

And imported in your R scripts as follows:

```r
library(RCodeBox)
```

> [!NOTE]
> See [RCodeBox Examples](https://zwallen.github.io/RCodeBox/examples.html) for a vignette of function usage examples.

> [!IMPORTANT]
> As these are personal convenience functions wrapping existing functions and packages:
>
> * **Compatibility:** They may or may not work for your purposes as they were designed in response to specific workflows and analyses.
> * **Support:** Limited support is provided - use at your own risk and test thoroughly with your data.
> * **Citation:** Citation of the package is not necessary if you decide to use any of the functions written here (but glad if they were able to help!).

## Issues and Contributing

This is primarily a personal package, but if you encounter bugs or have suggestions:

* Open an issue on GitHub for bug reports
* Pull requests are welcome for bug fixes
* For questions, contact: zachary.d.wallen@gmail.com

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Changelog

Version 0.2.0
* Started using Claude Code to assist in package maintaining,
* Added following functions
    - `pie_chart`: function for plotting a pie chart with labels
    - `us_map_scatterpie`: function for plotting scatter pie charts overlaid on the US map (including AK and HI)
    - `generate_color_palette`: function for creating color palettes with automatically handling of variables with really large Ns (used within other plotting functions)
* Replaced portions of existing functions with `generate_color_palette`
* Put most packages in `Suggests` (was getting warning message about too many imports)
* General tweaks to overall code

Version 0.1.9
* Fixed handling of data when `groups` is `NULL` in the `stratified_stat_table` function
* Modified `format_string` function so that it capitalizes only the first word of a string (i.e., sentence case)
* Fixed sorting of groups in `composition_barplot` function when flipping plot to stay in same order as when plot is horizontal
* Made a few other minor fixes to multiple functions

Version 0.1.8
* Added `digits` parameter to `stratified_violin_boxplot` function to control number of digits for means and standard deviations
* Added `gene_matrix_sort` function

Version 0.1.7
* Fixed issue with `stratified_violin_boxplot` when `test = NULL`
* Added `drop_all_cases` parameter to `stratified_violin_boxplot` to give the option to drop the "All Cases" group when it does not make sense to have it
* Converted underlying computation in `pairwise_similarity` function to be performed in C++

Version 0.1.6
* Added function `pairwise_similarity` to calculate pairwise proportions of overlap between rows of a data.frame based on the features (columns) in the data.frame

Version 0.1.5
* Added parameters `drop_all_cases` and `drop_level` to `stratified_barplot` function to allow the "All Cases" group and certain levels to be dropped from plotting
* Removed the calls to `format_string` and `keep_caps` parameters for plotting functions
* Added parameter `digits` to `longtail_barplot` function to allow control of number of digits in labels
* Added parameters `flip_subrow_labels` and `flip_subcol_labels` to `composition_barplot` function for when needing to rotate facet labels 90 degrees
* Overall plotting performance improvements for barplot functions

Version 0.1.4
* Added better handling of simple barplotting (i.e., when `groups = NULL`) to the `stratified_barplot` function
* Increased vertical spacing between legend elements of barplot functions
* Added the `sort_groups` and `add_labels` parameters to the `composition_barplot` function

Version 0.1.3
* Added code to the `stratified_barplot` function that makes sure to remove levels with frequency of 0
* Added a catch and fix for statistical testing errors using `simulate.p.value = TRUE` in `stratified_barplot` function
* Removed `chisq.test` as an option for statistical testing in `stratified_barplot` (was having issues with 0 count cells and `fisher.test` can handle contingency tables >2x2 anyway)

Version 0.1.2
* Added `keep_caps` parameters to plotting functions
* Extended `ymax` for `stratified_barplot` by multiplying max character N by 1.5

Version 0.1.1
* Created a standalone function for formatting strings (`format_string`) to title cases and removed the creation of that function within other functions
* Added parameter `keep_caps` to the `stratified_stat_table` function in order to allow specifying certain strings to keep capitalized in the output table (e.g., abbreviations)

Version 0.1.0
* Initializing package repository