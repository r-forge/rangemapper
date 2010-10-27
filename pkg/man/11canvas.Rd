\name{canvas.save}
\alias{canvas.save}
\alias{canvas.fetch}
\alias{canvasFetch-methods}
\alias{canvasSave,rangeMap-method}

\title{Project's canvas}

\description{
	The canvas is a  regular grid of a given resolution used to perform grid interpolation.}

\usage{
canvas.save(con)
canvas.fetch(con)
}

\arguments{
	  \item{con}{An sqlite connection pointing to a valid \code{rangeMapper} project. }

\value{
\item{\code{canvas.fetch}}{Returns a \code{\link{SpatialPixelsDataFrame}} object}  
}  

	

\examples{
	dbcon = rangeMap.start(file = "temp.sqlite", overwrite = TRUE, dir = tempdir() )
	f= system.file(package="rangeMapper", "extdata", "wrens", "vector")
	global.bbox.save(f, dbcon) 
	gridSize.save(dbcon)  
	
	#  save canvas to database
	canvas.save(dbcon) 
	#... and check 
	summary(canvas.fetch(dbcon) )
}
	
	
	
\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\seealso{
	\code{\link{rangeMapper}} for a complete example.\cr
	\code{\link{gridSize}} 
	}

\keyword{spatial}
\keyword{sqlite}

