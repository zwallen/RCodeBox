% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/darken_color.R
\name{darken_color}
\alias{darken_color}
\title{Darken colors}
\usage{
darken_color(colors, amount = 0.2)
}
\arguments{
\item{colors}{A character vector of color specifications (e.g., hex codes, color
names).}

\item{amount}{A numeric value between 0 and 1 specifying the amount of darkening.
Default is 0.2.}
}
\value{
A character vector of darkened colors in hex format.
}
\description{
This function takes one or more colors and darkens them by a specified amount.
}
\details{
The function performs the following operations:
\enumerate{
\item Converts input colors to RGB values
\item Applies darkening by reducing brightness
\item Converts darkened RGB values to hex color codes
}
}
\note{
This function requires the \code{grDevices} package for color conversions.
}
\examples{
darken_color("red")
darken_color(c("#FF0000", "#00FF00", "#0000FF"), amount = 0.3)

}
