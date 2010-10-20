

#### BBOX ###
setMethod("rangeMapBbox",  
	signature  = "rangeMapBbox", 
		definition = function(object) {
		shpFiles = rangeFiles(object)
		
		Msg(paste("computing global bouding box for",length(shpFiles ), "ranges...") )
		
		nfo = lapply(shpFiles, getinfo.shape)
			
		bb = do.call(rbind, lapply(nfo, function(x) c(x$minbounds[1:2], x$maxbounds[1:2]) ) )
		bb = c( xmin = min(bb[,1]), xmax = max(bb[,3]), ymin = min(bb[,2]), ymax = max(bb[,4]) )
		
		
		ogrShpFiles = data.frame(dsn = dirname(shpFiles), layer = gsub(".shp", "", basename(shpFiles)), stringsAsFactors = FALSE)
		
		if(object@checkProj) {
			p4s = .extract.p4s(ogrShpFiles) 
			p4s = p4s[!duplicated(p4s)]
			if(length(p4s) > 1) warning(Msg(paste("More than one projection found:\n", paste("  *",p4s, collapse = "\n")) ) )
			}	else 
				p4s = .extract.p4s(ogrShpFiles[1, ])
			
		attributes(bb)$p4s = p4s
		Msg("Done!")
		
		bb
		
		}
	)

setMethod("rangeMapBboxSave",  
	signature  = "rangeMapBbox",
		definition = function(object) {
		if(! .is.empty(object@CON, object@METADATA) ) stop(Msg("Bounding box was allready saved for this project."))
		bb = rangeMapBbox(object)
		metadata = c(bb, p4s= as.character(attributes(bb)$p4s), gridSize = NA)
		res = dbWriteTable(object@CON, object@METADATA, data.frame(t(metadata)), append = TRUE, row.names = FALSE)
		if(! .is.empty(object@CON, object@METADATA)) Msg("Bounding box uploaded.")
	 
	 }
	)

#user level
global.bbox.save <- function(Dir, con) { 
	x = new("rangeMapBbox", CON = con, dir = Dir, ogr = FALSE)
	rangeMapBboxSave(x)
}


setMethod("rangeMapBboxFetch",  
	signature  = "rangeMapBbox",
		definition = function(object) {
		if(.is.empty(object@CON, object@METADATA) ) stop(Msg("Bounding box not yet constructed for this project!"))
		md = dbReadTable(object@CON, object@METADATA)
		bb = cbind(c(md$xmin, md$xmax, md$xmax, md$xmin, md$xmin), c(md$ymin, md$ymin, md$ymax, md$ymax, md$ymin) )
		bb = SpatialPolygons(Srl = list(Polygons(list(Polygon(bb)), "bb")) )
		proj4string(bb) = md$p4s
		return(bb)
		 
	 }
	)

#user level
global.bbox.fetch  <- function(con) {
 
 x = new("rangeMapBbox", CON = con)
 rangeMapBboxFetch(x)


}

#### GRID SIZE ####
setMethod("gridSizeSave",  
	signature  = "gridSize",
		definition = function(object) {
		
			if(!is.na(.sqlQuery(object@CON, "SELECT gridSize from metadata")$gridSize)) stop(Msg("The grid size was allready set!"))
			if(is.na(.sqlQuery(object@CON, "SELECT xmin from metadata")$xmin))          stop(Msg("There is no bouding box!"))
					
			.sqlQuery(object@CON, paste("UPDATE metadata SET gridSize = ", object@gridSize, "where rowid = 1") )

			if(!is.na(.sqlQuery(object@CON, "SELECT gridSize from metadata")$gridSize)) Msg( paste("Grid size set to", object@gridSize) )
	 }
	)

setMethod("gridSizeFetch",  
	signature  = "rangeMap",
		definition = function(object) {
		
		if(is.na(.sqlQuery(object@CON, "SELECT gridSize from metadata")$gridSize)) stop(Msg("The grid size is not yet defined for this project!"))
		
		res = .sqlQuery(object@CON, "select gridSize from metadata")$gridSize
		return(res)
	 }
	)	
	
#user level
gridSize.save <- function(con,...) {
	
	x = new("gridSize", CON = con,...)
		
	gridSizeSave(x)	
		
}

gridSize.fetch <- function(con) { 
	x = new("rangeMap", CON = con)
	gridSizeFetch(x)	

}


#### CANVAS ####
setMethod("canvasSave",  
	signature  = "rangeMap", 
		definition = function(object) {
		
		if( nrow(.sqlQuery(object@CON, 'SELECT * FROM canvas limit 1' )) == 1  ) stop(Msg("The canvas was allready constructed!"))
		if(is.na(.sqlQuery(object@CON, "SELECT gridSize from metadata")$gridSize))  stop(Msg("The grid size is missing!"))

		bbox     = global.bbox.fetch(object@CON)
		cellsize = gridSize.fetch(object@CON)
		
		cnv = spsample(bbox, cellsize = cellsize, type = "regular", offset = c(0.5,0.5))
		
		cnv = data.frame(coordinates(cnv), id = 1:nrow(coordinates(cnv)))

		names(cnv) = c("x", "y", "id")

		res = dbWriteTable(object@CON, object@CANVAS, cnv, append = TRUE, row.names = FALSE) 

		if(res) Msg("Canvas uploaded.")
		
		}
	)	

setMethod("canvasFetch",  
	signature  = "rangeMap", 
		definition = function(object) {
		
		cnv = .sqlQuery(object@CON, 'SELECT * FROM canvas' )

		if(nrow(cnv) == 0) stop(Msg("The canvas is empty!"))

		coordinates(cnv) = ~ x + y

		p4s = .sqlQuery(object@CON, paste("SELECT p4s FROM", object@METADATA) )[,1]
		
		proj4string(cnv) = CRS(p4s)
		
		gridded(cnv) = TRUE
		
		cnv
		
		}
	)	

# user level	
canvas.fetch <- function(con) { 
	x = new("rangeMap", CON = con)
	canvasFetch(x)	

} 


canvas.save  <- function(con) {

	x = new("rangeMap", CON = con)
	canvasSave(x)	


}















































