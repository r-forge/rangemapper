
\name{rangeMap.start}
\alias{rangeMap.start}
\alias{rangeMap.open}

\title{Initiate/open a new rangeMapper project}

\description{
Initiate/open a new rangeMapper project using an object of class \code{\link{rangeMapStart-class}} 
}

\usage{
rangeMap.start(\dots)
rangeMap.open(path, verbose)
}

\arguments{
	  \item{path}{Character vector; a path to a valid rangeMapper project}
	  \item{verbose}{Character vector; if \code{TRUE} the project's summary is printed }
	  \item{\dots}{Arguments to be passed to \code{\link{rangeMapStart-class}} }
}

\value{
An sqlite connection is returned.
}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\seealso{
	\code{\link{rangeMapper}} for a complete example.\cr
	\code{\link{rangeMapStart-class}} 
}


\examples{
td = setwd(tempdir())

dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = tempdir() )
summary(dbcon)
summary(new("rangeMap", CON = dbcon) )

dbcon = rangeMap.open(path = "test.sqlite")
summary(dbcon)
setwd(td)

}

\keyword{spatial}

















