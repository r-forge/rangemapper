\name{rangeMapBbox-class}
\Rdversion{1.1}
\docType{class}
\alias{rangeMapBbox-class}
\alias{rangeMapBbox,rangeMapBbox-method}
\alias{rangeMapBboxSave,rangeMapBbox-method}

\title{Class "rangeMapBbox"}
\description{
A class used to manage the global bounding box of multiple polygons
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rangeMapBbox", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{checkProj}:}{Object of class \code{"logical"}  }
    \item{\code{dir}:}{Object of class \code{"character"}  }
    \item{\code{ogr}:}{Object of class \code{"logical"}  }
    \item{\code{polygons.only}:}{Object of class \code{"logical"}  }
    \item{\code{CON}:}{Object of class \code{"SQLiteConnection"}  }
    \item{\code{ID}:}{Object of class \code{"character"}  }
    \item{\code{BIOID}:}{Object of class \code{"character"}  }
    \item{\code{METADATA}:}{Object of class \code{"character"}  }
    \item{\code{METADATA_RANGES}:}{Object of class \code{"character"}  }
    \item{\code{CANVAS}:}{Object of class \code{"character"}  }
    \item{\code{RANGES}:}{Object of class \code{"character"}  }
    \item{\code{BIO}:}{Object of class \code{"character"}  }
    \item{\code{MAP}:}{Object of class \code{"character"}  }
  }
}
\section{Extends}{
Class \code{"\linkS4class{rangeFiles}"}, directly.
Class \code{"\linkS4class{rangeMap}"}, directly.
}
\section{Methods}{
  \describe{
    \item{rangeMapBbox}{\code{signature(object = "rangeMapBbox")}: ... }
    \item{rangeMapBboxSave}{\code{signature(object = "rangeMapBbox")}: ... }
	 }
}

\author{
Mihai Valcu \email{valcu@orn.mpg.de}
}


\seealso{
	\code{\link{global.bbox}}
}

\keyword{classes}














