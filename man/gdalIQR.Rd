% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gdal_raster_functions.R
\name{gdalIQR}
\alias{gdalIQR}
\title{Calculate inter quartile range using gdal_calc}
\usage{
gdalIQR(inpath, outpath, overwrite = TRUE, return.raster = TRUE, quiet = FALSE)
}
\arguments{
\item{inpath}{file path of inpath file to change.}

\item{outpath}{file path of output file to generate.}

\item{overwrite}{Overwrite the existing raster?}

\item{return.raster}{if TRUE raster will be returned from function call.}

\item{quiet}{Do processing quietly?}
}
\description{
Calculate inter quartile range using gdal_calc
}
