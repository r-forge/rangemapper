#+ setup, include=FALSE
require(knitr); require(rangeMapper)

#'   Body size distribution of the New World Wrens

#+ , results = 'hide',  tidy = FALSE
require(rangeMapper)
dbcon = rangeMap.start(file = "wrens.sqlite",dir = tempdir() , overwrite = TRUE)

# Breeding range vector files
f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
global.bbox.save(con = dbcon, bbox = f) 
gridSize.save(dbcon) 

#  save canvas 
canvas.save(dbcon) 

#' Import taxa and life history data

#+,  results='markup'
data(wrens)
head(wrens[, 3:6])


#+ ,  results = 'hide'
bio.save(con = dbcon, loc = wrens ,  ID = "sci_name")

# Process ranges
 r = readOGR(f, "wrens", verbose = FALSE)
 processRanges(spdf = r, con =  dbcon, ID = "sci_name")


#' Mapping using  an SQL aggregate function  is significantly faster than using an R function

#' Available SQL _aggregate _ functions: 
#' * avg           
#' * stdev         
#' * variance   
#' * mode         
#' * median     
#' * lower_quartile
#' * upper_quartile
#' * sum / total         
#' * max           
#' * min         
#' * count       

#+ , results = 'hide', tidy = FALSE
rangeMap.save(dbcon, biotab = "wrens",FUN = "median", biotrait = "body_size", 
	tableName = "median_body_size")


#+ , results = 'hide',  fig = TRUE
# Fetch and plot map
sr = rangeMap.fetch(dbcon)
plot(sr)


















