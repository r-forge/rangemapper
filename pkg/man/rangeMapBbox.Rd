
\name{global.bbox}
\alias{global.bbox}
\alias{global.bbox.save}
\alias{global.bbox.fetch}
\title{Global bounding box}
\description{
Computes, sets or retrives the global spatial bounding box.
}

\details{
	\link{global.bbox.save}{
		saves the global bounding box and the proj4 string to the \code{rangeMapper}'s \sQuote{metadata} table.}
	\link{global.bbox.fetch}{
		retrieves the global bounding box from a \code{rangeMapper}'project as as a \code{\link{spatialPolygonsDataFrame}}. }
}


\arguments{
	  \item{Dir}{ranges file directory}
	  \item{con}{An \code{"SQLiteConnection"} object pointing to a \code{rangeMapper} project}
	}	

\usage{
	global.bbox.save(Dir, con)
	global.bbox.fetch(con)
	}


\examples{
dbcon = rangeMap.start(file = "temp.sqlite", overwrite = TRUE, dir = tempdir() )
f= system.file(package="rangeMapper", "extdata", "wrens", "vector")
global.bbox.save(f, dbcon) 
summary(global.bbox.fetch(dbcon))
}
	
	
\seealso{
	\code{\link{rangeMapper}} for a complete example.
	\code{\linkS4class{rangeMapBbox}}
	\code{\link{proj4string}}
	\code{\link{bbox}} 
	}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\keyword{spatial}

