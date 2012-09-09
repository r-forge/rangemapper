#+, results = 'hide', echo = FALSE
require(raster)

#' Species richness of the high altitude wrens
#' assemblage level  Subsetting

#+ , results = 'hide', tidy = FALSE
#Start a new project
require(rangeMapper)
dbcon = rangeMap.start(file = "wrens.sqlite",dir = tempdir() , overwrite = TRUE)
f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
global.bbox.save(con = dbcon, bbox = f, 
 p4s = 
  CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
gridSize.save(dbcon) 
canvas.save(dbcon) 
r = readOGR(f, "wrens", verbose = FALSE)
processRanges(spdf = r, con =  dbcon, ID = "sci_name")

#' Import a raster using rangeMap.save
#+ , results = 'hide'
r = system.file(package = "rangeMapper", "extdata", 
	"etopo1", "etopo1_Americas.tif")

rangeMap.save(dbcon, path = r, tableName = 'meanAltitude', 
	FUN = median, overwrite = TRUE)

#' Mapping
#+ , results = 'hide'
# rangeMap.save() takes a named list as a 'subset' argument. Names refers to 'BIO' tables (i.e. species level) and/or
# 'MAP' and 'metadata_rages' tables (i.e. assambladge level) while the corresponding character strings 
# contain the SQL 'WHERE' elements. 

#+ , results = 'hide', tidy = FALSE
rangeMap.save(dbcon, biotab = "wrens", 
	subset = list(MAP_meanAltitude = "meanAltitude > 999"),
	tableName = "highAltitude_wrens_species_richness")

#+ , results = 'hide',  warning=FALSE
# Fetch and plot maps
m = rangeMap.fetch(dbcon)
plot(m, scales = FALSE)



















