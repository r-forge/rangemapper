
\name{selectShpFiles}
\alias{selectShpFiles}


\title{Select (recursively) shape files}
\description{
Returns the file path to all \sQuote{.shp} files in a directory.
}
\usage{
selectShpFiles(dir, ogr = TRUE, polygons.only = TRUE)
}

\arguments{
	  \item{dir}{character string specifying the directory containing .shp files.}	
	  \item{ogr}{A logical value. 
	  If \code{TRUE} a  \code{\link{data.frame}} with two columns (\code{dsn} and \code{layer}) is returned. 
	  If \code{FALSE} a character vector containing the names of the files in the specified directories  will be returned.
	   }
	\item{polygons.only}{A logical value.
	  If \code{TRUE} only polygon file paths are returned, else the location to all shape files regardless their type is returned.
	}}

\value{
Either a \code{\link{data.frame}} or a character vector is returned.
}	

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\seealso{
	\code{\link{rangeMapper}} for a complete example.\cr
	\code{\link{rangeFiles-class}} 
}

\references{
Valcu, M., Dale, J. and Kempenaers, B. (in prep) rangeMapper: A platform for the study of macroecology of life history traits.
}
	
	
\examples{
f= system.file(package="rangeMapper", "extdata", "wrens", "vector")
selectShpFiles(f, ogr = TRUE, polygons.only = TRUE)
}

\keyword{misc}











