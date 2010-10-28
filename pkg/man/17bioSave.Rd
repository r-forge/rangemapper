
\name{bio.save}
\alias{bio.save}
\alias{metadata2bio}
\alias{bioSaveFile-class}
\alias{bioSave,bioSaveFile-method}


\title{Import \sQuote{BIO} tables}
\description{
Import tables (e.g. life history data) to a \code{rangeMapper} project.
}
	
\usage{
	bio.save(con, loc, overwrite, ...)
}



\arguments{
	\item{con}{An sqlite connection pointing to a valid \code{rangeMapper} project. }
	\item{loc}{file location or data.frame name}
	\item{tableName}{if missing, the name of the file or data.frame is used}
	\item{ID}{This is the column corresponding to the names of the range files: the ID makes the link between }
	\item{...}{Other arguments to pass to methods of \code{\link{bioSaveFile-class}} class}
}

\value{
A \sQuote{BIO} table is created in the corresponding \code{rangeMapper} project. 
A \sQuote{BIO} table contains the following fields:
% tabulate %.....

}


\examples{
	dbcon = rangeMap.start(file = "temp.sqlite", overwrite = TRUE, dir = tempdir() )
	f= system.file(package="rangeMapper", "extdata", "wrens", "vector")
	global.bbox.save(f, dbcon) 
	gridSize.save(dbcon)  
	canvas.save(dbcon) 
	processRanges(dir = f, con =  dbcon, metadata = TRUE)

	# convert ranges_metadata to BIO table
	metadata2bio(dbcon)
	data(wrens)
	head(wrens)
	bio.save(con = dbcon, loc = wrens,  ID = "species_name")
	# the same table imported directly
	bio.save(con = dbcon, loc =  system.file(package="rangeMapper", "data", "wrens.csv"),  ID = "species_name")
} 
 

\seealso{
\code{\link{rangeMapper}} for a complete example.
\code{\link{bioSaveDataFrame-class}}
\code{\link{bioSaveFile-class}}
\code{\link{wrens}}
}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\keyword{import}














