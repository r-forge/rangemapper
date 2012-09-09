
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
dbcon = rangeMap.start(file = "falcons.sqlite", dir = tempdir() , overwrite = TRUE)


#'  Download  range vector files
#+, results = 'hide', tidy=FALSE
fs = "http://www.natureserve.org/getData/dataSets/birdMapData/Falconidae.zip"
if(!file.exists('Falconidae.zip')) download.file(fs, "Falconidae.zip")
unzip("Falconidae.zip")

#' Combine all ranges in one "SpatialPolygonsDataFrame" (fast , recommended method)

#+, results = 'hide', tidy=FALSE
R = list() # a container for all ranges

lst = selectShpFiles("Falconidae")

for(i in 1:nrow(lst)) {
	  ri = readOGR(lst$dsn[i], lst$layer[i], verbose = FALSE)
	  ri = spChFIDs(ri,  paste(i, 1:length(ri), sep = "." ) )
	  print(i)
	 R[[i]] = ri
}	
	
R = do.call(rbind, R)

# see http://www.natureserve.org/getData/Metadata_Birds_ver_3.0_Oct_07.pdf
proj4string(R) = CRS("+proj=longlat +datum=NAD83") 


#+ , results = 'hide', tidy=FALSE, fig = TRUE
plot(R, border = "grey", axes = TRUE)

#+
# Check out the field names
strwrap(names(R), width = 20)

#'  Mapping species richness 

#+ , results = 'hide', tidy=FALSE, fig = TRUE, warning = FALSE
global.bbox.save(con = dbcon, bbox = R) 
gridSize.save(dbcon) 
canvas.save(dbcon) 
processRanges(spdf = R, con =  dbcon, ID = "SCI_NAME")
rangeMap.save(dbcon) 
sr = rangeMap.fetch(dbcon) 
plot(sr) 




































