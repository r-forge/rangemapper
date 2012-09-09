#+ setup,  include=FALSE
require(knitr); require(rangeMapper)

#'   Body size distribution of the New World Wrens

#+,  results = 'hide'
require(rangeMapper)
dbcon = rangeMap.start(file = "wrens.sqlite",dir = tempdir() , overwrite = TRUE)

# Breeding range vector files
f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")

global.bbox.save(con = dbcon, bbox = f) 
gridSize.save(dbcon) 

#  save canvas 
canvas.save(dbcon) 

# Import taxa and life history data
data(wrens)
bio.save(con = dbcon, loc = wrens ,  ID = "sci_name")

#+,  results = 'hide'
#  Process ranges
 r = readOGR(f, "wrens", verbose = FALSE)
 processRanges(spdf = r, con =  dbcon, ID = "sci_name")

	
#' Mapping using R functions

#+,  results = 'hide', tidy = FALSE
custom_function = function(x, ...) (sd(x, ...))

rangeMap.save(dbcon, biotab = "wrens", 
	FUN = custom_function, biotrait = "body_size", 
	tableName = "sd_body_size", overwrite = TRUE)


#+ , results = 'hide',  fig = TRUE
# Fetch and plot map
sr = rangeMap.fetch(dbcon)
plot(sr)


















