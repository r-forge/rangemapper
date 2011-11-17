
\name{bio.save}
\alias{bio.save}
\alias{metadata2bio}
\alias{bio.merge}
\alias{bioSave-methods}
\alias{bioSave,bioSaveDataFrame-method}
\alias{bioSave,bioSaveFile-method}

\title{Import \sQuote{BIO} tables to  a \code{rangeMapper} project.}
\description{
Import tables (e.g. life history data) to an active \code{rangeMapper} project.
}
	
\usage{
	bio.save(con, loc, tableName, \dots)
	metadata2bio(con, \dots)
	bio.merge(con, tableName, \dots)
}



\arguments{
	\item{con}{An sqlite connection pointing to a valid \code{rangeMapper} project. }
	\item{loc}{file location or data.frame name}
	\item{tableName}{if missing, the name of the file or data.frame is used}
	\item{\dots}{Other arguments to pass to methods of \code{\link{bioSaveFile-class}} class, e.g. the ID, the column corresponding to the names of the range files}
}

\value{
A \sQuote{BIO} table is created in the corresponding \code{rangeMapper} project. 

}



\seealso{
\code{\link{rangeMapper}} for a complete example.
\code{\link{bioSaveDataFrame-class}}
\code{\link{bioSaveFile-class}}
\code{\link{wrens}}
}

\references{
Valcu, M., Dale, J. and Kempenaers, B. (in prep) rangeMapper: A platform for the study of macroecology of life history traits.
}


\examples{
require(rangeMapper)
wd = tempdir()
r = readOGR(system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined"), "wrens", verbose = FALSE)
dbcon = rangeMap.start(file = "wrens.sqlite", overwrite = TRUE, dir = wd )
global.bbox.save(con = dbcon, bbox = r)
gridSize.save(dbcon)  
canvas.save(dbcon) 
processRanges(spdf = r, con =  dbcon, metadata = rangeTraits(), ID = "sci_name" )

# Upload BIO tables
data(wrens)
Troglodytes  = wrens[grep("Troglodytes", wrens$sci_name), c(2, 5)]
bio.save(con = dbcon, loc = Troglodytes,  ID = "sci_name")
bio.save(con = dbcon, loc = system.file(package = "rangeMapper", "data", "wrens.csv"),  ID = "sci_name")
bio.merge(dbcon, "wrensNew")
metadata2bio(dbcon)

summary(new("rangeMap", CON = dbcon) )$BIO_tables

}



\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\keyword{import}














