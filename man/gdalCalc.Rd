% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gdal_raster_functions.R
\name{gdalCalc}
\alias{gdalCalc}
\title{Generic function for using gdal_calc}
\usage{
gdalCalc(
  inpath,
  outpath,
  calc_fun = "sum",
  numpy = TRUE,
  na.value = NULL,
  overwrite = TRUE,
  return.raster = TRUE,
  quiet = FALSE
)
}
\arguments{
\item{inpath}{file path of inpath file to change.}

\item{outpath}{file path of output file to generate.}

\item{calc_fun}{A text function that can be used from numpy, some examples include: "sum", "average", "std", "max", "min"}

\item{return.raster}{if TRUE raster will be returned from function call.}
}
\description{
Generic function for using gdal_calc
}
