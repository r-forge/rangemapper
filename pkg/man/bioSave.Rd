
\name{bio.save}
\alias{bio.save}
\alias{metadata2bio}
\alias{bio.merge}
\alias{bioSaveFile-class}
\alias{bioSave,bioSaveFile-method}


\title{Import \sQuote{BIO} tables}
\description{
Import tables (e.g. life history data) to a \code{rangeMapper} project.
}
	
\usage{
	bio.save(con, loc, overwrite, ...)
	metadata2bio(con)
	bio.merge(con, tableName, ...)
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


\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\keyword{import}














