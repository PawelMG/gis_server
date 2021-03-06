% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_funcs.R
\name{check_map}
\alias{check_map}
\title{Checks whether the desired map background is available.}
\usage{
check_map(db, ext_map, options_list = NULL, sel = FALSE)
}
\arguments{
\item{db}{a SQLiteConnection object that specifies the connection to the database.}

\item{ext_map}{a spatial object of class 'extent' from the 'raster' package
holding the data on the extreme coordinates of the input object.}

\item{options_list}{a list of graphical and other parameters used by the rendering engine.}
}
\value{
mapl     a data frame holding one record from the map database.
}
\description{
The \code{check_map} queries the map database to check for available maps.
The parameters taken into account are the bounding box (extent of the spatial object)
and the desired scale of the map.
}
\keyword{internal}
