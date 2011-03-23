

rect2spp <- function(xmin, xmax, ymin, ymax) {
		bb = cbind(c(xmin, xmax, xmax, xmin, xmin), c(ymin, ymin, ymax, ymax, ymin) )
		SpatialPolygons(Srl = list(Polygons(list(Polygon(bb)), "bb")) )
}


#### BBOX ###
setMethod("rangeMapBbox",  
	signature  = c(object = "rangeFiles"),
		definition = function(object, checkProj = TRUE,...) {
		shpFiles = rangeFiles(object)
		
		Msg(paste("computing global bouding box for",length(shpFiles ), "ranges...") )
		
		nfo = lapply(shpFiles, getinfo.shape)
			
		bb = do.call(rbind, lapply(nfo, function(x) c(x$minbounds[1:2], x$maxbounds[1:2]) ) )
		bb = c( xmin = min(bb[,1]), xmax = max(bb[,3]), ymin = min(bb[,2]), ymax = max(bb[,4]) )
		
		ogrShpFiles = data.frame(dsn = dirname(shpFiles), layer = gsub(".shp", "", basename(shpFiles)), stringsAsFactors = FALSE)
		
		if(checkProj) {
		Msg("Checking for proj4 string differences...")
			p4s = .extract.p4s(ogrShpFiles) 
			p4s = p4s[!duplicated(p4s)]
			if(length(p4s) > 1) warning(Msg(paste("More than one projection found:\n", paste("  *",p4s, collapse = "\n")) ) )
			}	else 
				p4s = .extract.p4s(ogrShpFiles[1, ])
		
		attributes(bb)$p4s = as.character(p4s)
		Msg("Done!")
		
		bb
		
		}
	)

	setMethod("rangeMapBboxSave",  
		signature  = c(object = "rangeMap", bbox = "missing", p4s = "missing"),
		definition = function(object,bbox, p4s, ...) {
	if(! .is.empty(object@CON, object@BBOX) ) stop(Msg("Bounding box was allready saved for this project."))
	
	bb = structure(c(-180, 180, -90,90), 
		.Names = c("xmin", "xmax", "ymin", "ymax"), 
		p4s = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
	
	Msg(paste("Using unprojected global bouging box [", paste(bb, collapse = ","), "]..." ) )
	
	res1 = dbWriteTable(object@CON, object@BBOX, data.frame(t(bb)), append = TRUE, row.names = FALSE)
	res2 = dbWriteTable(object@CON, object@PROJ4STRING, data.frame(p4s = attributes(bb)$p4s), append = TRUE, row.names = FALSE)
	
	res = all(res1, res2)
	
	if(res) 
		Msg(c("Bounding box uploaded.", "PROJ4STRING set to ", attributes(bb)$p4s) ) else 
		Msg("Bounding box upload failed.")
 
	 }
)

setMethod("rangeMapBboxSave",  
	signature  = c(object = "rangeMap", bbox = "missing", p4s = "CRS"),
		definition = function(object, bbox, p4s, ...) {
		if(! .is.empty(object@CON, object@BBOX) ) stop(Msg("Bounding box was allready saved for this project."))
		
	bb = structure(c(-180, 180, -90,90), 
		.Names = c("xmin", "xmax", "ymin", "ymax"), 
		p4s = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
	
	Msg(paste("Using unprojected global bouging box [", paste(bb, collapse = ","), "]..." ) )
	
		Msg(paste("Converting to", p4s@projargs) )
			bbnew = rect2spp(bb[1], bb[2], bb[3], bb[4])
			bbnew =  spsample(bbnew, n = 50, type = "regular" )
			proj4string(bbnew) = attributes(bb)$p4s
			bbnew = spTransform(bbnew , p4s )
			bb = c(sp::bbox(bbnew )[1, ], sp::bbox(bbnew )[2, ] )
			attributes(bb)$p4s = p4s@projargs
		
		res1 = dbWriteTable(object@CON, object@BBOX, data.frame(t(bb)), append = TRUE, row.names = FALSE)
		res2 = dbWriteTable(object@CON, object@PROJ4STRING, data.frame(p4s = attributes(bb)$p4s), append = TRUE, row.names = FALSE)
		
		res = all(res1, res2)
		
		if(res) 
			Msg(c("Bounding box uploaded.", "PROJ4STRING set to ", attributes(bb)$p4s) ) else 
			Msg("Bounding box upload failed.")
	 
	 }
)


	setMethod("rangeMapBboxSave",  
		signature  = c(object = "rangeMap", bbox = "character", p4s = "missing"),
		definition = function(object,bbox, p4s, ...) {
		if(! .is.empty(object@CON, object@BBOX) ) stop(Msg("Bounding box was allready saved for this project."))
		
		# bbox  the path to the range file(s) directory, pass to new("rangeFiles" ....
		
		bb = rangeMapBbox( new("rangeFiles", dir = bbox, ogr = FALSE) )
		
		res1 = dbWriteTable(object@CON, object@BBOX, data.frame(t(bb)), append = TRUE, row.names = FALSE)
		res2 = dbWriteTable(object@CON, object@PROJ4STRING, data.frame(p4s = attributes(bb)$p4s), append = TRUE, row.names = FALSE)
		
		res = all(res1, res2)
		
		if(res) 
			Msg(c("Bounding box uploaded.", "PROJ4STRING set to ", attributes(bb)$p4s) ) else 
			Msg("Bounding box upload failed.")
	 
	 }
)

setMethod("rangeMapBboxSave",  
		signature  = c(object = "rangeMap", bbox = "character", p4s = "CRS"),
		definition = function(object, bbox, p4s, ...) {
		if(! .is.empty(object@CON, object@BBOX) ) stop(Msg("Bounding box was allready saved for this project."))
		
		bb = rangeMapBbox( new("rangeFiles", dir = bbox, ogr = FALSE) )

		Msg(paste("Converting to", p4s@projargs) )
			bbnew = rect2spp(bb[1], bb[2], bb[3], bb[4])
			bbnew =  spsample(bbnew, n = 50, type = "regular" )
			proj4string(bbnew) = attributes(bb)$p4s
			bbnew = spTransform(bbnew , p4s )
			bb = c(bbox(bbnew )[1, ], bbox(bbnew )[2, ] )
			attributes(bb)$p4s = p4s@projargs
		
		res1 = dbWriteTable(object@CON, object@BBOX, data.frame(t(bb)), append = TRUE, row.names = FALSE)
		res2 = dbWriteTable(object@CON, object@PROJ4STRING, data.frame(p4s = attributes(bb)$p4s), append = TRUE, row.names = FALSE)
		
		res = all(res1, res2)
		
		if(res) 
			Msg(c("Bounding box uploaded.", "PROJ4STRING set to ", attributes(bb)$p4s) ) else 
			Msg("Bounding box upload failed.")
	 
	 }
)


setMethod("rangeMapBboxSave",  
		signature  = c(object = "rangeMap", bbox = "Spatial", p4s = "missing"),
		definition = function(object, bbox, p4s, ...) {
		if(! .is.empty(object@CON, object@BBOX) ) stop(Msg("Bounding box was allready saved for this project."))
		
		bb = c( bbox(bbox)[1, ], bbox(bbox)[2, ])
		p4s = proj4string(bbox)
		
		res1 = dbWriteTable(object@CON, object@BBOX, data.frame(t(bb)), append = TRUE, row.names = FALSE)
		res2 = dbWriteTable(object@CON, object@PROJ4STRING, data.frame(p4s), append = TRUE, row.names = FALSE)
		
		res = all(res1, res2)
		
		if(res) 
			Msg(c("Bounding box uploaded.", "PROJ4STRING set to ", p4s) ) else 
			Msg("Bounding box upload failed.")
		

	 
	 }
)

#user level
global.bbox.save <- function(con, ...) { 
	x = new("rangeMap", CON = con)
	rangeMapBboxSave(x, ... )

}

setMethod("rangeMapBboxFetch",  
	signature  = "rangeMap",
		definition = function(object) {
		if(.is.empty(object@CON, object@BBOX ) ) stop(Msg("Bounding box not yet constructed for this project!"))
		md = dbReadTable(object@CON, object@BBOX)
		p4s = dbReadTable(object@CON, object@PROJ4STRING)
		
		bb = rect2spp(md$xmin, md$xmax, md$ymin, md$ymax)
		proj4string(bb) = p4s$p4s
		return(bb)
		 
	 }
	)

#user level
global.bbox.fetch  <- function(con) {
 
 x = new("rangeMap", CON = con)
 rangeMapBboxFetch(x)


}

#### GRID SIZE ####
setMethod("gridSizeSave",  
	signature  = "gridSize",
		definition = function(object) {
		
			if(!.is.empty(object@CON, object@GRIDSIZE)) stop(Msg("The grid size was allready set!"))
			if(.is.empty(object@CON, object@BBOX)) stop(Msg("There is no bouding box!") )
			
			if( length(object@gridSize)!=1  ) {
				bb  = global.bbox.fetch(object@CON)
				minSpan = min(diff(bbox(bb)[1, ]), diff(bbox(bb)[2, ]))
				object@gridSize = minSpan/100
				Msg(paste("Default grid size used!"))
				
			}
			grd = data.frame(object@gridSize)
			names(grd) = object@GRIDSIZE
			res = dbWriteTable(object@CON, object@GRIDSIZE, grd, append = TRUE, row.names = FALSE)
			
			if(res) Msg( paste("Grid size set to", object@gridSize, "map units.") )
			
	 }
	)

setMethod("gridSizeFetch",  
	signature  = "rangeMap",
		definition = function(object) {
		
		if(.is.empty(object@CON, object@GRIDSIZE)) stop(Msg("The grid size is not yet defined for this project!"))
		
		res = dbReadTable(object@CON, object@GRIDSIZE)[1,1]
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
		
		if(!.is.empty(object@CON, object@CANVAS) ) stop(Msg("The canvas was allready constructed!"))
		if(.is.empty(object@CON, object@GRIDSIZE) )  stop(Msg("The grid size is missing!"))

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
		
		cnv = RMQuery(object@CON, 'SELECT * FROM canvas' )

		if(nrow(cnv) == 0) stop(Msg("The canvas is empty!"))

		coordinates(cnv) = ~ x + y

		p4s = dbReadTable(object@CON, object@PROJ4STRING)[1,1]
		
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




















