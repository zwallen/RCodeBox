# RCodeBox

[![R-CMD-check](https://github.com/zwallen/RCodeBox/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zwallen/RCodeBox/actions/workflows/R-CMD-check.yaml)

A collection of R functions for commonly performed actions in computational biology and data science projects.

Releases of the package can be installed from GitHub:

```r
install.packages("devtools")
devtools::install_github("zwallen/RCodeBox")
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

Version 0.1.0 (current)
* Initializing package repository