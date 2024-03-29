% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_factory_shape.R
\name{factory_geom_shape}
\alias{factory_geom_shape}
\title{Create an alternative color aesthetic for ggforce::geom_shape}
\usage{
factory_geom_shape(aes_name)
}
\arguments{
\item{aes_name}{string skalar, the name of the new geom}
}
\description{
Create an alternative color aesthetic for ggforce::geom_shape
}
\details{
The newly created aesthetics are then \code{test_c} (color) and \code{test_f} (fill).
}
\examples{
factory_geom_shape('var3')

}
