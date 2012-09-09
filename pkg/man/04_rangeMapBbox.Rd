
\name{global.bbox}
\alias{global.bbox}
\alias{global.bbox.save}
\alias{global.bbox.fetch}
\alias{rangeMapBbox-methods}
\alias{rangeMapBboxSave-methods}
\alias{rangeMapBboxSave}
\alias{rangeMapBboxSave,rangeMap,character,CRS-method}
\alias{rangeMapBboxSave,rangeMap,character,missing-method}
\alias{rangeMapBboxSave,rangeMap,missing,CRS-method}
\alias{rangeMapBboxSave,rangeMap,missing,missing-method}
\alias{rangeMapBboxSave,rangeMap,Spatial,missing-method}
\alias{rangeMapBboxFetch}
\alias{rangeMapBboxFetch-methods}
\alias{rangeMapBboxFetch,rangeMap-method}


\title{Global bounding box}


\description{
Computes, sets or retrieves the global spatial bounding box.
}

\usage{
	global.bbox.save(con, \dots)
	global.bbox.fetch(con)
	}

\arguments{
	  \item{con}{An \code{SQLiteConnection} object pointing to a \code{rangeMapper} project}
	  \item{\dots}{Arguments to pass to the corresponding methods: \cr
				\emph{bbox} can be a \code{character} vector; the path to the range files directory \cr 
				\emph{bbox} can also be an object inheriting from  \code{\linkS4class{Spatial}} \cr 
				\emph{p4s}  an object of class  \code{\linkS4class{CRS}} \cr 
		}
}
	
\details{
	\code{global.bbox.save} saves the \emph{global bounding box} and the \emph{proj4} string to the sqlite database.\cr
	\code{global.bbox.fetch} retrieves the \emph{global bounding box} as a \code{\link{SpatialPolygonsDataFrame}}.
}

	
\note{
If \emph{bbox} is a \code{character} vector then the corresponding method calls \code{rangeMapBbox} 
 with \code{checkProj = TRUE} which requires all ranges to have the same \emph{proj4} argument.  \cr
If \emph{p4s} is set then the \emph{bbox} will be set with that \emph{p4s} string else
   the \emph{p4s} will be identical with the \emph{proj4} string of the range files. \cr
If \emph{bbox} and \emph{p4s} are missing then an unprojected global bounding box is set.   

}	
	
\author{Mihai Valcu \email{valcu@orn.mpg.de}}
	

\references{
Valcu, M., Dale, J. and Kempenaers, B. (2012)  rangeMapper: A platform for the study of macroecology of life history traits. 21(9). (DOI: 10.1111/j.1466-8238.2011.00739.x)
}
	
\seealso{
	\code{\link{rangeMapper}}\cr
	\code{\link{proj4string}}\cr
	\code{\link{bbox}} 
	}

\examples{
require(rangeMapper)
wd = tempdir()

f= system.file(package = "rangeMapper", "extdata", "wrens", "vector")

# Using default values for both bbox and p4s
dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = wd )
global.bbox.save(con = dbcon)
bbox0 = global.bbox.fetch(dbcon)

# Using multiple *.shp range files
dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = wd )
global.bbox.save(con = dbcon, bbox = f)
bbox1 = global.bbox.fetch(dbcon)

# Using multiple *.shp range files and reprojecting
dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = wd )
global.bbox.save(con = dbcon, bbox = f, 
	p4s = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs "))
bbox2 = global.bbox.fetch(dbcon)

# Using a SpatialPolygons object
dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = wd )

troaed = selectShpFiles(f, ogr = TRUE, polygons.only = TRUE)[71, ] # path to Troglodytes_aedon
r = readOGR(troaed$dsn, troaed$layer)

global.bbox.save(con = dbcon, bbox = r)
bbox3 = global.bbox.fetch(dbcon)

par(mfrow = c(2,2))
plot(bbox0, axes = TRUE)
plot(bbox1, axes = TRUE)
plot(bbox2, axes = TRUE)
plot(bbox3, axes = TRUE)



}	
	
	
	
	
\keyword{spatial}




















