\name{processRanges}
\alias{processRanges}
\alias{rangeMapProcess-class}
\alias{rangeMapProcess,rangeMapProcess-method}

\title{Process ranges}
\description{
Each polygon range is overlayed on the \code{\link[rangeMapper]{canvas}} and the results are saved to the active project file.
}

\details{The overlay is performed using \code{\link{overlay}}. If the overlay returns no results (i.e. the polygon is smaller than a grid cell) then the centroid of the range will snap to the nearest point and only one grid cell will be returned for that range.
}

\arguments{
	  \item{con}{An sqlite connection pointing to a valid \code{rangeMapper} project.}
	  \item{Dir}{ranges file directory}
	  \item{metadata}{A logical value. If \sQuote{TRUE} ranges metadata: range area, median range centroid and range extents (min, max) are saved to \sQuote{metadata_ranges} table}
	}

\usage{
processRanges(dir,con,metadata)
}


 \section{Warning}{
If thousands of range map polygons are processed, their geometries are complex and/or the canvas resolution is relatively high this step can be time consuming. For example processing all amphibians of the world (5816 ranges, total 252 MB) using a grid of 0.25 deg (\emph{unnecessary high}) resolution takes 544 minutes on a 64-bit machine with 4 GB of RAM running Windows Vista and R 2.10.0. However, once this step has been performed and the results saved to the database, the other operations are very fast.
 }


\examples{
	dbcon = rangeMap.start(file = "temp.sqlite", overwrite = TRUE, dir = tempdir() )
	f= system.file(package="rangeMapper", "extdata", "wrens", "vector")
	global.bbox.save(f, dbcon) 
	gridSize.save(dbcon)  
	canvas.save(dbcon) 
	# process ranges
	processRanges(dir = f, con =  dbcon, metadata = TRUE)
} 
 
 
\seealso{
	\code{\link{rangeMapper}} for a complete example.
	}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

 \references{
Hurlbert, AH; Jetz, W (2007) Species richness, hotspots, and the scale dependence of range maps in ecology and conservation, PNAS, 13384-13389
}
 
\keyword{spatial}






















