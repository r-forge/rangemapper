
#'  Unstudied and extinction-prone wren species 
#'  _Subsetting at both species and assambladge level _


#+ , results = 'hide', tidy = FALSE
#Start a new project
require(rangeMapper)
dbcon = rangeMap.start(file = "wrens.sqlite",dir = tempdir() , overwrite = TRUE)
f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
projString = 
CRS(" +proj=cea +lon_0=0 +lat_ts=30 +x_0=0+y_0=0 +ellps=WGS84 +units=km +no_defs")
global.bbox.save(con = dbcon, bbox = f, p4s = projString)
gridSize.save(dbcon)
canvas.save(dbcon)
r = readOGR(f, "wrens", verbose = FALSE)

#+
# rangeMap.save() takes a named list as a 'subset' argument. Names refers to 'BIO' tables (i.e. species level) and/or 'MAP' 
# and 'metadata_rages' tables (i.e. assambladge level) while the corresponding character strings 
# contain the SQL 'WHERE' elements. 

#+ , results = 'hide', tidy = FALSE
processRanges(spdf = r, con =  dbcon, ID = "sci_name", 
	metadata  = rangeTraits() )

data(wrens)
bio.save(con = dbcon, loc = wrens,  ID = "sci_name")

# Clutch size is not known for a bunch of wren species. 
wrens[is.na(wrens$clutch_size), c("sci_name", "genus", "clutch_size")]

#' Restricted range species ( species with a high extinction risk)

#+ 
meta = RMQuery(dbcon, "SELECT * FROM metadata_ranges")
plot(density(log(meta$Area)))
abline(v = quantile(log(meta$Area), 0.50), col = 2, lty = 2)
exp(quantile(log(meta$Area), 0.50))


#' Mapping: Unstudied and extinction-prone wren species
#+ , results = 'hide', tidy = FALSE
rangeMap.save(dbcon)
 rangeMap.save(dbcon, biotab = "wrens", 
	subset = list(BIO_wrens = "clutch_size is NULL", metadata_ranges = "Area < 182751"),
		tableName = "mistery_wrens_species_richness", overwrite = TRUE)


	
#+ , results = 'hide', fig = TRUE, warning = FALSE
# Fetch and plot maps
m = rangeMap.fetch(dbcon)
plot(m, scales = FALSE)

















