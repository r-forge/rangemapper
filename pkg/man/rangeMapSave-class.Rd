\name{rangeMapSave-class}
\Rdversion{1.1}
\docType{class}
\alias{rangeMapSave-class}
\alias{rangeMapSave,rangeMapSave,character,missing-method}
\alias{rangeMapSave,rangeMapSave,function,formula-method}
\alias{rangeMapSave,rangeMapSave,function,missing-method}
\alias{rangeMapSave,rangeMapSave,missing,missing-method}
\alias{MapImport-class}
\alias{rangeMapSave,MapImport-method}

\title{Class "rangeMapSave"}
\description{
This class' methods  are used to save \sQuote{MAP} tables to an existing rangeMapper project.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rangeMapSave", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{biotab}:}{Object of class \code{"character"}  }
    \item{\code{biotrait}:}{Object of class \code{"character"}  }
    \item{\code{tableName}:}{Object of class \code{"character"}  }
    \item{\code{subset}:}{Object of class \code{"list"}  }
    \item{\code{CON}:}{Object of class \code{"SQLiteConnection"}  }
    \item{\code{VERSION}:}{Object of class \code{"character"}  }
    \item{\code{ID}:}{Object of class \code{"character"}  }
    \item{\code{BIOID}:}{Object of class \code{"character"}  }
    \item{\code{PROJ4STRING}:}{Object of class \code{"character"}  }
    \item{\code{GRIDSIZE}:}{Object of class \code{"character"}  }
    \item{\code{BBOX}:}{Object of class \code{"character"}  }
    \item{\code{METADATA_RANGES}:}{Object of class \code{"character"}  }
    \item{\code{CANVAS}:}{Object of class \code{"character"}  }
    \item{\code{RANGES}:}{Object of class \code{"character"}  }
    \item{\code{BIO}:}{Object of class \code{"character"}  }
    \item{\code{MAP}:}{Object of class \code{"character"}  }
	
	\item{\code{path}:}{Object of class \code{"character"}  }
  }
}
\section{Extends}{
Class \code{"\linkS4class{rangeMap}"}, directly.
Class \code{"\linkS4class{rangeMapSave}"}, directly.
Class \code{"\linkS4class{rangeMap}"}, by class "rangeMapSave", distance 2.
}

\section{Methods}{
  \describe{
    \item{rangeMapSave}{\code{signature(object = "rangeMapSave", FUN = "character", formula = "missing")} }
    \item{rangeMapSave}{\code{signature(object = "rangeMapSave", FUN = "function", formula = "formula")} }
    \item{rangeMapSave}{\code{signature(object = "rangeMapSave", FUN = "function", formula = "missing")} }
    \item{rangeMapSave}{\code{signature(object = "rangeMapSave", FUN = "missing", formula = "missing")} }
	
	\item{rangeMapSave}{\code{signature(object = "MapImport")}: ... }
	 }
}

\author{
Mihai Valcu \email{valcu@orn.mpg.de}
}

\seealso{
\code{\link{rangeMapSave}}
}

\keyword{classes}
