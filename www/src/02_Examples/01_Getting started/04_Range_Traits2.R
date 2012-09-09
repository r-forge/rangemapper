
#'  Wrens breeding in extreme (most humid / arid) enviroments  
#' _metadata_ranges_ table is updated *after* processRanges()

#+ , results = 'hide'
# The project
wd = setwd(tempdir() )
require(rangeMapper)

dbcon = rangeMap.start(file = "wrens.sqlite", overwrite = TRUE, dir = tempdir() )
rmap = new("rangeMap", CON = dbcon)	
spdf = readOGR(system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined"), "wrens", verbose = FALSE)
spdf = spTransform(spdf, CRS('+proj=longlat +ellps=WGS84 +no_defs') )

global.bbox.save(con = dbcon, bbox = spdf )
gridSize.save(dbcon)  
canvas.save(dbcon) 
processRanges(spdf = spdf, con =  dbcon, ID = "sci_name" )


#' Download  raster file
#+ , results = 'hide'
# Downloaded from http://spatial-analyst.net
# See http://spatial-analyst.net/worldmaps/PRECm.rdc for source reference and file description
download.file("http://spatial-analyst.net/worldmaps/PRECm.zip", "PRECm.zip")
unzip("PRECm.zip")
PRECm = readGDAL("PRECm.tif", output.dim = c(300, 300))


#' Update  _metadata_ranges_ table 
#+ , results = 'hide'
metadata.update(rmap, FUN = median, name = 'Median_GSMaP_Precipitation', map = PRECm, na.rm = TRUE, overwrite = TRUE)

#+ , results = 'hide', fig = TRUE
# Query metadata_ranges table
mr = RMQuery(dbcon, 'select * from metadata_ranges')
wetWren = mr[mr$Median_GSMaP_Precipitation== max(mr$Median_GSMaP_Precipitation), 'bioid']
dryWren = mr[mr$Median_GSMaP_Precipitation== min(mr$Median_GSMaP_Precipitation), 'bioid']

wetWrenRange = rangeFetch(rmap,wetWren)
dryWrenRange = rangeFetch(rmap,dryWren)

#+ 
image(PRECm, xlim = bbox(spdf)[1, ], ylim = bbox(spdf)[2, ] , col = brewer.pal(10, 'YlGnBu') ) 
plot(wetWrenRange, border = 2, add = T)
text(bbox(wetWrenRange)[1, 1], bbox(wetWrenRange)[2,1], label = wetWren, pos = 4, col =2)
plot(dryWrenRange, border = 2, add = T)
text(bbox(dryWrenRange)[1, 1], bbox(dryWrenRange)[2,1], label = dryWren, pos = 1, col = 2)























