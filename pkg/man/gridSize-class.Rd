\name{gridSize-class}
\Rdversion{1.1}
\docType{class}
\alias{gridSize-class}
\alias{gridSizeSave,gridSize-method}

\title{Class "gridSize"}
\description{
Objects of class gridSize are used to set the pixel size and thus the canvas size of a \code{rangeMapper} project. If gridSize is not given a sensible default is set.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gridSize", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{gridSize}:}{Object of class \code{"numeric"}  }
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
Class \code{"\linkS4class{rangeMap}"}, directly.
}
\section{Methods}{
  \describe{
    \item{gridSizeSave}{\code{signature(object = "gridSize")}: ... }
	 }
}

\author{
Mihai Valcu \email{valcu@orn.mpg.de}
}

\seealso{
	\code{\link[rangeMapper]{global.bbox}}
}

\keyword{classes}












