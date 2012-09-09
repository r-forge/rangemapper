#+ setup, include=FALSE
require(knitr); require(rangeMapper)

#'  SPECIES RICHNESS OF THE NEW WORLD WRENS: Using default arguments for most of the functions

#+ , results = 'hide'
require(rangeMapper)
dbcon = rangeMap.start(file = "wrens.sqlite",dir = tempdir() , overwrite = TRUE)

#'    Breeding range vector files 
loc = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")

#' Mapping using default parameters
#+ , results= 'markup'
global.bbox.save(con = dbcon, bbox = loc) 
gridSize.save(dbcon) 
canvas.save(dbcon) 

#' Process breeding ranges and create "species_richness" map

#+ , results='hide'
r = readOGR(loc, "wrens", verbose = FALSE)
processRanges(spdf = r, con =  dbcon, ID = "sci_name")
rangeMap.save(dbcon) 

#' Fetch and plot species richness map
#+  , warning=FALSE
sr = rangeMap.fetch(dbcon) 
plot(sr) 



















