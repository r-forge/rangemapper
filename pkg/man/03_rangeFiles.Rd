
\name{selectShpFiles}
\alias{selectShpFiles}


\title{Select (recursively) shape files}
\description{
Returns the file path to all \sQuote{.shp} files in a directory.
}
\usage{
selectShpFiles(dir, \dots)
}

\arguments{
	  \item{dir}{character string specifying the directory containing .shp files.}	
	  \item{\dots}{Other arguments to pass to methods of class \code{\link{rangeFiles-class}}  }	
	  
}

\value{
Either a \code{\link{data.frame}} or a character vector is returned.
}	

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\seealso{
	\code{\link{rangeMap.save}}.\cr
	\code{\link{rangeFiles-class}} 
}

\references{
Valcu, M., Dale, J. and Kempenaers, B. (in press)  rangeMapper: A platform for the study of macroecology of life history traits.
}
	
	
\examples{
f= system.file(package="rangeMapper", "extdata", "wrens", "vector")
res = selectShpFiles(f, ogr = TRUE, polygons.only = TRUE)
head(res)

}

\keyword{misc}











