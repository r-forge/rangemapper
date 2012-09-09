
#'  Mapping   [NatureServe](  http://www.natureserve.org)   ranges

#' Data provided by NatureServe in collaboration with  Robert Ridgely, James Zook, 
#' The Nature Conservancy - Migratory Bird Program, Conservation International - CABS, 
#' World Wildlife Fund - US, and Environment Canada - WILDSPACE.
#' Ridgely, R. S., T. F. Allnutt, T. Brooks, D. K. McNicol, D. W. Mehlman, 
#' B. E. Young, and J. R. Zook. 2007. 
#' Digital Distribution Maps of the Birds of the Western Hemisphere, 
#' version 3.0. NatureServe, Arlington, Virginia, USA._ 



#+, results = 'hide', tidy=FALSE
require(rangeMapper)

#'  Download  range vector files
#+, results = 'hide', tidy=FALSE
fs = "http://www.natureserve.org/getData/dataSets/mammalMapData/Canidae.zip"
if(!file.exists('Canidae.zip')) download.file(fs, "Canidae.zip")
unzip("Canidae.zip")

#' Process one shp file at a time (slow, only reccomended for very large shp files)

#+ , results = 'hide', tidy=FALSE, fig = TRUE, warning = FALSE
dbcon = rangeMap.start(file = "dogs.sqlite", dir = tempdir() , overwrite = TRUE)
global.bbox.save(con = dbcon, bbox = getwd() )
gridSize.save(dbcon) 
canvas.save(dbcon) 
processRanges(dir = getwd(), con =  dbcon)

#'  Mapping species richness 
rangeMap.save(dbcon) 
sr = rangeMap.fetch(dbcon) 
plot(sr) 


summary(rangeMap("dogs.sqlite"))

































