% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color-spaces.R
\name{hsv_to_lab}
\alias{hsv_to_lab}
\title{Convert HSV to Lab}
\usage{
hsv_to_lab(hsv, transformation = "sRGB", linear_func = NULL)
}
\arguments{
\item{hsv}{A dataframe or matrix with H, S and V colour channels located in the columns 1 to 3, respectively. H in degrees in [0, 360], S and L in [0, 1]}

\item{transformation}{An option in \code{c("sRGB", "Adobe")} for a built-in transformation or, alternatively, a custom 3x3 transformation matrix.}

\item{linear_func}{A function to convert RGB colour space into linear RGB space. Used only if a custom transformation matrix is provided. Transformation skips if no function is provided under a user-defined transformation matrix. See: https://en.wikipedia.org/wiki/SRGB.}
}
\value{
A \code{tibble} of L, a and b colour space values.
}
\description{
Convert HSV to Lab
}
