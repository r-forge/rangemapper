
\name{rangeMapper}
\alias{rangeMapper}
\docType{package}

\title{rangeMapper: A platform for the study of macroecology of life history traits.}
\description{ 
	\pkg{rangeMapper} is a front end platform for the study of macroecology of life history traits at both inter-specific and assemblage levels. 
	}
\details{
	The package uses species range (extent-of-occurrence) vector polygons and life history traits datasets to build up maps (e.g. species richness, mean body mass, \dots). \cr
	\pkg{rangeMapper} comes with an user-friendly platform-independent tcltk graphical user interface. 
}
\keyword{package}
\keyword{spatial}

\author{
	Mihai Valcu \email{valcu@orn.mpg.de}, \url{http://orn.mpg.de/mitarbeiter/valcu.html} \cr
	James Dale \email{J.Dale@massey.ac.nz}, \url{http://quelea.net}}


\section{Getting Started}{
	For a quick start open the graphical user interface (\bold{gui}) by typing \code{rangeMapper()}. Mouse over the buttons to see further notes (tool-tips) regarding each step. \cr
	A tutorial can be accessed from Help/\sQuote{Get started} under the \bold{gui} or by browsing the \sQuote{doc} package directory.\cr
	See also the example below in the \emph{examples} section on how to use \pkg{rangeMapper} from the command line.
	}
 
 
\section{The rangeMapper pipeline}{
	 \itemize{
		\item Initiate a new project (an empty sqlite database) using a pre-defined template. 
		\item Extract/define the global bounding box of all selected range maps.
		\item Generate a regular grid (the canvas) using the previously constructed global bounding box.  
		\item Perform polygon-grid overlay of all selected range maps.
		\item Optionally compute pre-defined or user-defined range structure indexes.
		\item Optionally import non-spatial data to be mapped at each grid cell (data are saved as a 'BIO' table to database).		
		\item Optionally import georeferenced raster files.
		\item Compute a chosen statistical model at each grid cell optionally using complex subsets.
		\item  Save, plot, export the MAP-s.
  }
}  

 

\references{
Valcu, M., Dale, J. and Kempenaers, B. (in prep) rangeMapper: A platform for the study of macroecology of life history traits.
}

 
\examples{
#########################################

require(rangeMapper)

# Start or open a project
wd = tempdir()

dbcon = rangeMap.start(file = "wrens.sqlite", 
	overwrite = TRUE, dir = tempdir() )

# To connect latter to the project created with rangeMap.start use
dbcon = rangeMap.open(path = 
	paste(tempdir(), "wrens.sqlite" , sep = .Platform$file.sep) )

# Project summary
summary(new("rangeMap", CON = dbcon) )

# Breeding range vector files location
f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")

# Save the global bounding box, 
global.bbox.save(con = dbcon, bbox = f, 
	p4s = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") ) 

summary( global.bbox.fetch(dbcon) )
	
# upload grid size using the proposed default
gridSize.save(dbcon)  

#  save canvas 
canvas.save(dbcon) 
summary(canvas.fetch(dbcon) )

# Upload BIO tables
data(wrens)
bio.save(con = dbcon, loc = wrens,  ID = "sci_name")

# Process species ranges
r = readOGR(f, "wrens", verbose = FALSE)

processRanges(spdf = r, con =  dbcon, metadata = rangeTraits(), ID = "sci_name" )

# Save MAPs
rangeMap.save(dbcon) #species richness is the default

#Using sqlite aggregate functions
.sqlAggregate()
rangeMap.save(dbcon, FUN = "median" , biotab = "wrens", 
	biotrait = "body_size", tableName = "body_size")

#Using an  R function which does not require a formula argument
rangeMap.save(dbcon, FUN = mean , biotab = "wrens", 
	biotrait = "clutch_size", tableName = "clutch_size", 
		na.rm = TRUE, overwrite = TRUE)

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

	
# rangeMap.save can be also used to  import raster maps the current project
r = system.file(package = "rangeMapper", 
	"extdata", "etopo1", "etopo1_Americas.tif")

# etopo1_Americas.tif data source:
strwrap(readChar(system.file(package = "rangeMapper", "extdata", "etopo1", "data_source.txt"), 500))
	
rangeMap.save(dbcon, path = r, tableName = 'meanAltitude', 
	FUN = mean , overwrite = TRUE)

# rangeMap.save takes a subset argument on both 
# metadata_ranges, MAP and BIO tables

# clutch size of the highland species
rangeMap.save(dbcon, biotab = "wrens", biotrait = "clutch_size", 
	tableName = "highlandClutch", overwrite = TRUE, 
	subset = list(MAP_meanAltitude = "meanAltitude > 2000") )

# clutch size of restricted range lowland species 
rangeMap.save(dbcon, biotab = "wrens", biotrait = "clutch_size", 
	tableName = "endemicsLowlandClutch", overwrite = TRUE, 
		subset = list(MAP_meanAltitude = "meanAltitude < 1000",
					 metadata_ranges = "Area < 100000000000") )


# Fetch and plot Maps
	
plot(rangeMap.fetch(dbcon, 'species_richness') ,n = 10, style = "fisher")		

plot(rangeMap.fetch(dbcon, 'meanAltitude'),  style = "quantile")

clutch = rangeMap.fetch(dbcon,
 c('clutch_size', 'slope_body_mass_clutchSize', 'body_size') )
plot(clutch, style = "fisher")

plot(rangeMap.fetch(dbcon, 'highlandClutch'), n = 4)	


image(rangeMap.fetch(dbcon, 'meanAltitude') )	
image(rangeMap.fetch(dbcon, 'endemicsLowlandClutch') , 
	add = TRUE, col = "blue")	

setwd(wd)	
	
}


\keyword{macroecology}
\keyword{spatial}
\keyword{gui}
\keyword{tcltk}






