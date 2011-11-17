
\name{rm.rangeMapper}
\alias{rm.rangeMapper}
\alias{rangeMapRemove-methods}
\alias{rangeMapRemove,rangeMapRemove-method}

\title{Remove tables from a give project}
\description{Remove tables given prefix attribute or by name}

\arguments{
	  \item{con}{A valid sqlite connection.}
	  \item{\dots}{arguments passed to methods of \code{"\linkS4class{rangeMapRemove}"}, specifically \sQuote{tablePrefix} or \sQuote{tableName} }
}

\usage{
rm.rangeMapper(con, \dots) 
}

\note{
The default \sQuote{rm.rangeMapper(con)} will remove all \sQuote{MAP} and \sQuote{BIO} tables.
}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

