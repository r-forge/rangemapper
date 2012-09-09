

#' Altidude range and the area perimeter ratio of the New World Wrens
#' _metadata_ranges_ table is created by processRanges()

#+ , results = 'hide', tidy = FALSE
#Start a new project
require(rangeMapper)
dbcon = rangeMap.start(file = "wrens.sqlite",dir = tempdir() , overwrite = TRUE)
f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
projString =  
 CRS(" +proj=cea +lon_0=0 +lat_ts=30 +x_0=0+y_0=0 +ellps=WGS84 +units=m +no_defs")
global.bbox.save(con = dbcon, bbox = f, p4s = projString)
gridSize.save(dbcon) 
canvas.save(dbcon) 


#+ , results = 'hide', tidy = FALSE
#'Area/Perimeter ratio
APratio = function(x){
	# area
	A = sapply(slot(x, "polygons"), 
		function(x) slot(x, "area") )
	# perimeter
	a = lapply(slot(x, "polygons"),
		function(x) slot(x, "Polygons") )
	b = do.call(c, a)
	a1 = lapply(b, function(x) 
		Line(coordinates(x)))
	L = sapply(a1, LineLength)
	# A-P ratio
	2*(sqrt(pi*sum(A)))/sum(L)
}


#+ , results = 'hide', tidy = FALSE
# Altitude range
etopoSrc =system.file(package = "rangeMapper", "extdata", 
	"etopo1", "etopo1_Americas.tif")
dem = readGDAL(etopoSrc,  silent =  TRUE)

#+
# etopo1_Americas.tif data source:
readChar(system.file(package = "rangeMapper", "extdata", 
	"etopo1", "data_source.txt"), 500)


AltitRange = function(x) {
	sel = which(!is.na(overlay(dem, x)))
	if(length(sel) > 0 ) {
	dem = dem[sel, ]
	res = diff(range(dem@data$band1, na.rm = TRUE))
	} else res = NA
	res
}


#+ , results = 'hide', tidy = FALSE
# Process ranges
r = readOGR(f, "wrens", verbose = FALSE)
processRanges(spdf = r, con =  dbcon, ID = "sci_name", 
	metadata  = rangeTraits(APratio=APratio, AltitRange=AltitRange) )


#' Mapping metadata_ranges table
#+ , results = 'hide', tidy = FALSE
# Convert metadata_ranges to a BIO table
metadata2bio(dbcon)

rangeMap.save(dbcon, biotab = "metadata_ranges", FUN = "median", 
	biotrait = "APratio", 
	tableName = "Area_Perimeter_ratio")

rangeMap.save(dbcon, biotab = "metadata_ranges", FUN = "median", 
	biotrait = "AltitRange", 
	tableName = "Altitude_range")

	
#+ , results = 'hide', fig = TRUE, warning = FALSE
# Fetch and plot maps
m = rangeMap.fetch(dbcon)
plot(m)





















