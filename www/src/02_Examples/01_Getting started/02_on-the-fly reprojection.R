#+ setup, include=FALSE
require(knitr); require(rangeMapper)

#'  ON-THE-FLY REPROJECTION

#+ , results = 'hide'
require(rangeMapper)
dbcon = rangeMap.start(file = "wrens.sqlite",dir = tempdir() , overwrite = TRUE)
# Breeding range vector files
f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")

#' Creating a global bbox using the  breeding range vector files; re-project to equal area

projString =  
CRS(" +proj=cea +lon_0=0 +lat_ts=30 +x_0=0+y_0=0 +ellps=WGS84 +units=km +no_defs")

global.bbox.save(con = dbcon, bbox = f, p4s = projString)

# upload grid size to 100 sqkm
gridSize.save(dbcon, gridSize = 100)
canvas.save(dbcon)

smr = summary(rangeMap(dbGetInfo(dbcon)$dbname))

#+ , results = 'hide', echo = FALSE
smr$Project_location = paste("...", basename(smr$Project_location), sep = "/")

#+ 
smr




 












