\name{rangeMap-class}
\docType{class}
\alias{rangeMap-class}
\alias{canvasFetch,rangeMap-method}
\alias{canvasSave,rangeMap-method}
\alias{gridSizeFetch,rangeMap-method}
\alias{rangeMapBboxFetch,rangeMap-method}

\title{Class "rangeMap"}
\description{
A class to formally define a rangeMapper project.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rangeMap", ...)}.
}

\section{Slots}{
  \describe{
	\item{\code{CON}:}{
		Object of class \code{"SQLiteConnection"} pointing to a \code{rangeMapper} project}
	\item{\code{ID}:}{
		Object of class \code{"character"}; the ID column corresponding to the names of the imported range files. }
    \item{\code{BIOID}:}{
		Object of class \code{"character"} the ID column corresponding to the names of imported 	\sQuote{BIO} tables. }
    \item{\code{METADATA}:}{
		Object of class \code{"character"} The name of a predifined table containing the global bouding box and the proj4 string }
    \item{\code{METADATA_RANGES}:}{
		Object of class \code{"character"} The name of a predifined table optionally constructed at the range import stage}
    \item{\code{CANVAS}:}{
		Object of class \code{"character"} The name of the table hosting the project's regular grid (the canvas) }
    \item{\code{RANGES}:}{Object of class \code{"character"} The name of the table hosting the ranges}
    \item{\code{BIO}:}{Object of class \code{"character"} The prefix for \sQuote{BIO} tables }
    \item{\code{MAP}:}{Object of class \code{"character"} The prefix for \sQuote{MAP} tables  }
  }
}



\section{Methods}{
  \describe{
    \item{canvasFetch}{\code{signature(object = "rangeMap")}: ... }
    \item{canvasSave}{\code{signature(object = "rangeMap")}: ... }
    \item{gridSizeFetch}{\code{signature(object = "rangeMap")}: ... }
    \item{rangeMapBboxFetch}{\code{signature(object = "rangeMap")}: ... }
	 }
}



\author{ Mihai Valcu, \email{valcu@orn.mpg.de}
}


\seealso{
  \code{\link{rangeMapStart-class}},
  \code{\link{rangeMapBbox-class}},
  \code{\link{gridSize-class}},
  \code{\link{rangeMapProcess-class}},
  \code{\link{rangeMapSave-class}},
  \code{\link{rangeMapFetch-class}},
  \code{\link{rangeMapRemove-class}},
  \code{\link{MapImport-class}}
}


\keyword{classes}




