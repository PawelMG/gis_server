% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_funcs.R
\name{extent_margin}
\alias{extent_margin}
\title{Creates a bounding box with a margin around the network object.}
\usage{
extent_margin(ext_map, margin)
}
\arguments{
\item{ext_map}{a spatial extent object provided by the get_background
function.}

\item{margin}{a percentage measure of extent expansion versus the plotted object (0.1 is 10\%).}
}
\value{
ext_map an object of the spatial class 'extent'.
}
\description{
The \code{extent_margin} transforms the bounding box of the 
desired map to include a margin around the network object.
}
\keyword{internal}
