% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-colours.R
\name{image_to_pallette}
\alias{image_to_pallette}
\title{Develop a usable colour palette form an image.}
\usage{
image_to_pallette(
  image_path,
  resize_factor = NULL,
  colour_space = "sRGB",
  rgb_to_linear_func = NULL,
  rgb_to_nonlinear_func = NULL,
  method = "slic",
  superpixel = 200,
  compactness = 20,
  verbose = TRUE,
  s = negDistMat(r = 2),
  summary_method = mean,
  ...
)
}
\arguments{
\item{image_path}{A character path to the image to cluster. Reads images of type .png, .jpeg, .jpg, .tiff.}

\item{resize_factor}{A numeric scalar that reduces (or increases) the size of the image before any processing.}

\item{colour_space}{The colour space of the original image. The clustering is undertaken in the Lab space. This is an an option in \code{c("sRGB", "Adobe")} for a built-in transformation or, alternatively, a custom 3x3 transformation matrix.}

\item{rgb_to_linear_func}{The clustering is undertaken in the Lab space. This is a function to convert RGB colour space into linear RGB space. Used only if a custom transformation matrix is provided. Transformation skips if no function is provided under a user-defined transformation matrix. See: https://en.wikipedia.org/wiki/SRGB.}

\item{rgb_to_nonlinear_func}{The clustering is undertaken in the Lab space. This is a function to convert linear RGB colour space into non-linear RGB space. Used only if a custom transformation matrix is provided. Transformation skips if no function is provided under a user-defined transformation matrix. See: https://en.wikipedia.org/wiki/SRGB.}

\item{method}{From \code{OpenImageR::superpixels}. A character string specifying the method to use. Either "slic" or "slico".}

\item{superpixel}{From \code{OpenImageR::superpixels}. A numeric value specifying the number of superpixels to use.}

\item{compactness}{From \code{OpenImageR::superpixels}. A numeric value specifying the compactness parameter. The compactness parameter is needed only if method is "slic". The "slico" method adaptively chooses the compactness parameter for each superpixel differently.}

\item{verbose}{From \code{OpenImageR::superpixels}. A boolean. If TRUE then information will be printed in the R session.}

\item{s}{From \code{apcluster::apcluster}. An l x l similarity matrix or a similarity function either specified as the name of a package-provided similarity function as character string or a user provided function object. s may also be a sparse matrix according to the Matrix package. Internally, apcluster uses the dgTMatrix class; all other sparse matrices are cast to this class (if possible, otherwise the function quits with an error). If s is any other object of class Matrix, s is cast to a regular matrix internally (if possible, otherwise the function quits with an error).}

\item{summary_method}{Function to summarise colours in clustered superpixels. Defaults to \code{mean}.}

\item{...}{Other arguments to be passed to the apcluster algorithm. For the methods with signatures character,ANY and function,ANY, all other arguments are passed to the selected similarity function as they are; for the methods with signatures Matrix,missing and sparseMatrix,missing, further arguments are passed on to the apcluster methods with signatures Matrix,missing and dgTMatrix,missing, respectively.}
}
\value{
A \code{schemr} object containing colour scheme colours and image properties and clusters.
}
\description{
Develop a usable colour palette form an image.
}
