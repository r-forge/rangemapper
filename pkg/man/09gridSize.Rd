\name{gridSize}
\alias{gridSize}
\alias{gridSize.save}
\alias{gridSize.fetch}

\title{Save or retrieve the grid size from an \code{rangeMapper} project.}
\description{
Save or retrieve the grid size from the active sqlite database.
}
\usage{
gridSize.save(con, gridSize) 
gridSize.fetch(con)
}

\arguments{
	  \item{con}{A connection pointing to a valid \code{rangeMapper} project.}
	  \item{gridSize}{A numeric vector of one unit length.}
	}

\value{
	\item{\code{gridSize.fetch}}{Returns a numeric vector of one unit length containing the grid size previously saved by \code{gridSize.save}}
}	

\examples{
	dbcon = rangeMap.start(file = "temp.sqlite", overwrite = TRUE, dir = tempdir() )
	f= system.file(package="rangeMapper", "extdata", "wrens", "vector")
	global.bbox.save(f, dbcon) 

	# upload grid size using the default
	gridSize.save(dbcon)  
	gridSize.fetch(dbcon)
}


\seealso{
\code{\link[rangeMapper]{rangeMapper}} for a complete example.
\code{\link[rangeMapper]{global.bbox}}
}


\author{Mihai Valcu \email{valcu@orn.mpg.de}}
\keyword{spatial}
\keyword{sqlite}









