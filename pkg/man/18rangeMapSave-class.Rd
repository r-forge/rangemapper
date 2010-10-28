\name{rangeMapSave-class}
\Rdversion{1.1}
\docType{class}
\alias{rangeMapSave-class}
\alias{rangeMap.save}
\alias{rangeMapSave,rangeMapSave-method}

\title{Class "rangeMapSave"}
\description{
This class' methods  are used to save \sQuote{MAP} tables to an existing rangeMapper project.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rangeMapSave", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{biotab}:}{Object of class \code{"character"} }
    \item{\code{biotrait}:}{Object of class \code{"character"} }
    \item{\code{tableName}:}{Object of class \code{"character"} }
    \item{\code{subset}:}{Object of class \code{"list"} }
    \item{\code{CON}:}{Object of class \code{"SQLiteConnection"} }
    \item{\code{ID}:}{Object of class \code{"character"} }
    \item{\code{BIOID}:}{Object of class \code{"character"} }
    \item{\code{METADATA}:}{Object of class \code{"character"} }
    \item{\code{METADATA_RANGES}:}{Object of class \code{"character"} }
    \item{\code{CANVAS}:}{Object of class \code{"character"} }
    \item{\code{RANGES}:}{Object of class \code{"character"} }
    \item{\code{BIO}:}{Object of class \code{"character"} }
    \item{\code{MAP}:}{Object of class \code{"character"} }
  }
}
\section{Extends}{
Class \code{"\linkS4class{rangeMap}"}, directly.
}
\section{Methods}{
  \describe{
    \item{rangeMapSave}{\code{signature(object = "rangeMapSave")}: ... }
	 }
}

\author{
Mihai Valcu \email{valcu@orn.mpg.de}
}


\seealso{
\code{\link{rangeMap.save}} 
}

\examples{
showClass("rangeMapSave")
}
\keyword{classes}














