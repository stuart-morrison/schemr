% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color-spaces.R
\name{rgb_to_hex}
\alias{rgb_to_hex}
\title{Convert RGB colour channels to hex colour codes.}
\usage{
rgb_to_hex(rgb)
}
\arguments{
\item{rgb}{A dataframe or matrix with red, green and blue colour channels located in the columns 1 to 3, respectively. Colour channel values should be between 0 and 255, inclusive.}
}
\value{
A character vector with hex representations of RGB colour channels.
}
\description{
Convert RGB colour channels to hex colour codes.
}
\examples{
red <- sample(x = 1:255, size = 10, replace = TRUE)
green <- sample(x = 1:255, size = 10, replace = TRUE)
blue <- sample(x = 1:255, size = 10, replace = TRUE)
rgb_to_hex(data.frame(r = red, g = green, b = blue))
}
