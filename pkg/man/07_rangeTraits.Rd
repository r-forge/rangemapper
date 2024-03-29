\name{rangeTraits}
\alias{rangeTraits}
\title{ 
A container of functions to apply on a \code{\link{SpatialPolygons} object} 
}
\description{
This is a convenience function returning a named \code{\link{list}} of functions. 
}
\usage{
	rangeTraits(\dots, use.default)
}
\arguments{
	\item{\dots}{functions, given as myfun = FUN, to apply on a \code{\link{SpatialPolygons}} object}
	\item{use.default}{If \code{TRUE}, the default, the output list contains functions to extract Area, Median, Min and Max extent of the \code{\link{SpatialPolygons}} object. This option is ignored if no functions are given. }
}
\value{
Returns a named list containing extractor functions to apply on \code{\link{SpatialPolygons}} objects. 
}

\details{
The function returns a named list so any additional functions should be given as 
rangeTraits(funName1 = FUN1, funName2 = FUN2) where FUN1, FUN2 are \code{\link{SpatialPolygons}} extractor functions.
}

\seealso{
\code{\link{processRanges}}
\code{\link{rangeMapper}}.
}
\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\keyword{SpatialPolygons}

\examples{

summary(rangeTraits(use.default = F))

f = system.file(package = "rangeMapper", "extdata", "wrens", "vector")
troaed = selectShpFiles(f, ogr = TRUE, 
	polygons.only = TRUE)[71, ] # path to Troglodytes_aedon

r = readOGR(troaed$dsn, troaed$layer)

# Beware of the value returned for Area!
sapply(rangeTraits(), function(x) x(r) )

# Define an extra function to compute correct Area
Area2 = function(x) {
x = spTransform(x, 
CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") 
	)

sum(sapply(slot(x, "polygons"), function(x) slot(x, "area") ))
}

sapply(rangeTraits(Area_sqm = Area2), function(x) x(r) )

}












