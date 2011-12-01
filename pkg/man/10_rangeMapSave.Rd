
\name{rangeMap.save}
\alias{rangeMap.save}
\alias{rangeMapSave}
\alias{rangeMapSave-methods}
\alias{rangeMapSave,rangeMapSave,character,missing-method}
\alias{rangeMapSave,rangeMapSave,function,formula-method}
\alias{rangeMapSave,rangeMapSave,function,missing-method}
\alias{rangeMapSave,rangeMapSave,missing,missing-method}
\alias{rangeMapFetch}
\alias{rangeMapFetch-methods}
\alias{rangeMapFetch,rangeMapFetch-method}
\alias{rangeMap.fetch}



\title{Save, retrieve and export maps.}
\description{
Apply a chosen \code{SQL} or \R function at each grid cell, allowing for complex subsetting at both ID (e.g. species) and pixel (e..g assemblage) levels.}

\arguments{
	  \item{CON}{An sqlite connection pointing to a valid \code{rangeMapper} project. }
	  \item{tableName}{Name of the table (quoted) to be added to the sqlite database. The prefix \sQuote{MAP} will be appended to \code{tableName} prior to saving.}
	  \item{FUN}{the function to be applied to each pixel. If \code{FUN} is missing then species richness (species count) is computed.}
	  \item{biotab}{character string identifying the \sQuote{BIO} table to use.}
	  \item{biotrait}{character string identifying the ID of the \sQuote{BIO} table. see \code{\link{bio.save}} }
	 \item{subset}{A named \code{\link{list}}. See details} 
	  \item{path}{Path to the raster file(quoted) to be imported to the existing project. \code{raster package} is required at this step.}			
	  \item{overwrite}{If \code{TRUE} then the table is removed}			
		
	\item{\dots}{When \code{FUN} is an \R function, \dots denotes any extra arguments to be passed to it.}
}


\usage{
rangeMap.save(CON, tableName, FUN, biotab, biotrait, subset, path , overwrite, \dots)
}
	

\value{
\code{TRUE} when the MAP was created successfully. 
\code{rangeMap.fetch} returns a \code{\link{SpatialPixelsRangeMap-class}}. 

}	

\details{
The subset argument accepts a named list. Names refers to \sQuote{BIO}, \sQuote{MAP} and \sQuote{metadata_rages} table names 
while the strings in the list are character strings containing the SQL  \code{WHERE} clause.
The subset can point to either one table type 
(e.g. \code{list(MAP_species_richness = "species_richness > 500")} )
or can point to several table types 
(e.g. \code{list(BIO_lifeHistory = "clutch_size > 4", MAP_meanAltitude = "meanAltitude < 1000",  metadata_ranges = "Area < 1000")} )

Any valid SQL expression can be used to build up a subset. See \url{http://www.sqlite.org/lang_expr.html}
}


\note{
\code{SQL} aggregate functions are more eficient then their \R counterparts. For simple aggregate functions like mean, median, sd, count it is advisable to use \code{SQL} functions rather then R functions. 

}



\seealso{
\code{link{[RSQLite.extfuns-package] {RSQLite.extfuns}}} for a list of additional SQL aggregate functions.
}


\references{
Valcu, M., Dale, J. and Kempenaers, B. (in press)  rangeMapper: A platform for the study of macroecology of life history traits.
}

\examples{
require(rangeMapper)
dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = tempdir() )

# Breeding range vector files location
f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")

# Save the global bounding box, 
global.bbox.save(con = dbcon, bbox = f, 
	p4s = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") ) 

# upload grid size using the proposed default
gridSize.save(dbcon, gridSize = 100000)  # cell size ~ 100km

#  save canvas 
canvas.save(dbcon) 
summary(canvas.fetch(dbcon) )

# Upload BIO tables
data(wrens)
bio.save(con = dbcon, loc = wrens,  ID = "sci_name")

# Process species ranges
r = readOGR(f, "wrens", verbose = FALSE)

processRanges(spdf = r, con =  dbcon, metadata = rangeTraits(), ID = "sci_name" )

###################################################################

# Save MAPs
rangeMap.save(dbcon) #species richness is the default

#Using sqlite aggregate functions
rangeMap.save(dbcon, FUN = "median" , biotab = "wrens", 
			biotrait = "body_size", tableName = "body_size")

#Using an  R function which does not require a formula argument
rangeMap.save(dbcon, FUN = mean , biotab = "wrens", 
	biotrait = "clutch_size", tableName = "clutch_size", 
		na.rm = TRUE, overwrite = TRUE)

###################################################################		
		
#Using an user defined R function which has a formula argument
 # and subsetting
# Map of log(body_mass) ~ clutch_size only for species richness > 10

lmSlope = function (formula, data) {
    fm = try(lm(formula, data = data), silent = TRUE)
	if (inherits(fm, "try-error")) 
		res = NA else 
			res = coef(fm)[2]
    as.numeric(res)
}

rangeMap.save(dbcon, FUN = lmSlope, biotab = "wrens", 
	biotrait = "body_mass", 
	tableName = "slope_body_mass_clutchSize", 
	formula = log(body_mass) ~ clutch_size , overwrite = TRUE, 
	list(MAP_species_richness = "species_richness > 5")
	)

###################################################################
	
# rangeMap.save can be also used to  import raster maps the current project
r = system.file(package = "rangeMapper", 
	"extdata", "etopo1", "etopo1_Americas.tif")

# etopo1_Americas.tif data source:
strwrap(readChar(system.file(package = "rangeMapper", "extdata", "etopo1", "data_source.txt"), 500))
	
rangeMap.save(dbcon, path = r, tableName = 'meanAltitude', 
	FUN = mean , overwrite = TRUE)

###################################################################	
	
# rangeMap.save takes a subset argument on both 
# metadata_ranges, MAP and BIO tables:

# clutch size of the highland species
rangeMap.save(dbcon, biotab = "wrens", biotrait = "clutch_size", 
	tableName = "highlandClutch", overwrite = TRUE, 
	subset = list(MAP_meanAltitude = "meanAltitude > 2000") )

# clutch size of restricted range lowland species 
rangeMap.save(dbcon, biotab = "wrens", biotrait = "clutch_size", 
	tableName = "endemicsLowlandClutch", overwrite = TRUE, 
		subset = list(MAP_meanAltitude = "meanAltitude < 1000",
					 metadata_ranges = "Area < 100000000000") )

###################################################################					 

# Fetch maps

summary(rangeMap.fetch(dbcon) ) # all maps
	
summary(rangeMap.fetch(dbcon, 'species_richness') )	# one map	




}




\author{Mihai Valcu \email{valcu@orn.mpg.de}}
\keyword{spatial}


















