\name{bioSaveFile-class}
\Rdversion{1.1}
\docType{class}
\alias{bioSaveFile-class}
\alias{bioSave,bioSaveFile-method}

\title{Class "bioSaveFile"}
\description{
The methods of this class allows importing external tables to a  \code{rangeMapper} project.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("bioSaveFile", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{loc}:}{Object of class \code{"character"}  }
    \item{\code{sep}:}{Object of class \code{"character"}  }
    \item{\code{biotab}:}{Object of class \code{"character"}  }
    \item{\code{biotrait}:}{Object of class \code{"character"}  }
    \item{\code{tableName}:}{Object of class \code{"character"}  }
    \item{\code{subset}:}{Object of class \code{"list"}  }
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
Class \code{"\linkS4class{rangeMapSave}"}, directly.
Class \code{"\linkS4class{rangeMap}"}, by class "rangeMapSave", distance 2.
}
\section{Methods}{
  \describe{
    \item{bioSave}{\code{signature(object = "bioSaveFile")}: ... }
	 }
}

\author{
Mihai Valcu \email{valcu@orn.mpg.de}
}


\seealso{
\code{\link{bio.save}}
}


\keyword{classes}
