\name{rangeMapProcess-class}
\Rdversion{1.1}
\docType{class}
\alias{rangeMapProcess-class}
\alias{rangeMapProcess,rangeMapProcess-method}

\title{Class "rangeMapProcess"}
\description{
A class 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rangeMapProcess", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{metadata}:}{Object of class \code{"logical"}  }
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
    \item{rangeMapProcess}{\code{signature(object = "rangeMapProcess")}: ... }
	 }
}

\author{
Mihai Valcu \email{valcu@orn.mpg.de}
}


\seealso{
\code{\link[rangeMapper]{rangeMapper}} for a complete example. 
\code{\link[rangeMapper]{rangeMapProcess}}

}

\examples{
showClass("rangeMapProcess")
}
\keyword{classes}
