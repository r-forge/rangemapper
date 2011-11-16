

rangeTraits <- function(..., use.default = TRUE) {

	Area = function(spdf) sum(sapply(slot(spdf, "polygons"), function(x) slot(x, "area") ))
	Median_x = function(spdf) median(coordinates(spdf)[, 1])
	Median_y = function(spdf) median(coordinates(spdf)[, 2])
	Min_x = function(spdf) min(coordinates(spdf)[, 1])
	Min_y = function(spdf) min(coordinates(spdf)[, 2])
	Max_x = function(spdf) max(coordinates(spdf)[, 1])
	Max_y = function(spdf) max(coordinates(spdf)[, 2])

	
	res = list(Area = Area, Median_x = Median_x, Median_y = Median_y, Min_x = Min_x, Min_y = Min_y, Max_x = Max_x, Max_y = Max_y)
	
	x = list(...)
	if(length(x) > 0) {
		 if(length(names(x)) != length(x)) stop (dQuote("..."), " elements should be named, e.g. myFun = abc")
		 if( !all(sapply(x, is.function))) stop (dQuote("..."), " elements should be functions.")
		 if(use.default) res = c(res, x)
	}
	
	res

}


.rangeOverlay <- function(spdf, canvas, name) {
	#SpatialPolygonsDataFrame
	# canvas 	SpatialPointsDataFrame
	# name character, length 2
	
	overlayRes = which(!is.na(overlay(spdf, canvas)[, 1]))
	
	if(length(overlayRes) > 0) { # do grid interpolation
		sp = canvas[overlayRes, ]
		o = data.frame(id = sp$id, bioid = rep(name, nrow(sp)) ) 
		}
		
	if(length(overlayRes) == 0) { # the polygon is smaller than a grid cell: snap to the nearest point
			ctr = apply(coordinates(spdf),2, mean)
			nn = spDistsN1(canvas, ctr)
			sp = canvas[which(nn == min(nn) ), ]
			o = data.frame(id = sp$id, bioid = rep(name, nrow(sp@coords)) )
		} 
	
	return(o)

}

		
setGeneric("rangeMapProcess", function(object,spdf, dir, ID,metadata, parallel)  standardGeneric("rangeMapProcess") )


 # Method 1.1 :  Each range file is a separate shp file. No metadata
setMethod("rangeMapProcess",  
		signature = c(object = "rangeMapProcess",spdf = "missing", dir = "character", ID = "missing", metadata = "missing", parallel = "missing"), 
		definition = function(object, dir){
	
	Startprocess = Sys.time()
	Files = rangeFiles(new("rangeFiles", dir = dir))
	cnv = as(canvasFetch(object), "SpatialPointsDataFrame")
	
	
	.processRange = function(i) {
		
	name = Files$layer[i]
	r = readOGR(Files$dsn[i], Files$layer[i], verbose = FALSE)
	#  reproject
	p4s =  dbReadTable(object@CON, object@PROJ4STRING)[1,1]
		if(!identical(gsub(" ", "", proj4string(r)), gsub(" ", "", p4s) ) ) r = spTransform( r , CRS(p4s) )
	
	# progress report	
		.X.Msg( paste("Processsing ranges, please wait!...", 
				   paste("Range:", Files$layer[i]),	
					 paste(round(i/length(Files$layer)*100,2), "% done"), 
					   paste("Elapsed time:",round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), sep = "\n"), 
					 keep = FALSE)
		

		o = .rangeOverlay(r,  cnv, name) 
		
		names(o) = c(object@ID, object@BIOID)
		
		# save  to db
		dbWriteTable(object@CON, object@RANGES, o, append = TRUE, row.names = FALSE) 

	}		
	
	lapply (1:length(Files$layer), FUN = .processRange) 
	 

	# last Msg
	.X.Msg(paste(nrow(Files), "ranges updated to database; Elapsed time:", 
							round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), keep = TRUE )

			} 
	)
 
 #  Method 1.2 :  Each range file is a separate shp file. Metadata are computed
setMethod("rangeMapProcess",  
		signature = c(object = "rangeMapProcess",spdf = "missing", dir = "character", ID = "missing", metadata = "list", parallel = "missing"), 
		definition = function(object, dir, metadata){
	
	# . . . pass to rangeTraits	

	Startprocess = Sys.time()

	Files = rangeFiles(new("rangeFiles", dir = dir))
	
	cnv = as(canvasFetch(object), "SpatialPointsDataFrame")
	
	
	.processRange = function(i) {
		
	name = Files$layer[i]
	r = readOGR(Files$dsn[i], Files$layer[i], verbose = FALSE)
	
	#  reproject
	p4s =  dbReadTable(object@CON, object@PROJ4STRING)[1,1]
		if(!identical(gsub(" ", "", proj4string(r)), gsub(" ", "", p4s) ) ) r = spTransform( r , CRS(p4s) )
	
	
	# progress report	
		.X.Msg( paste("Processsing ranges, please wait!...", 
				   paste("Range:", Files$layer[i]),	
					 paste(round(i/length(Files$layer)*100,2), "% done"), 
					   paste("Elapsed time:",round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), sep = "\n"), 
					 keep = FALSE)
		

		o = .rangeOverlay(r,  cnv, name) 
		
		names(o) = c(object@ID, object@BIOID)
		
		# save  to @RANGES
		dbWriteTable(object@CON, object@RANGES, o, append = TRUE, row.names = FALSE) 
		
		# save  to @METADATA_RANGES
		rtr = sapply(rangeTraits(), function(x) x(r) )
		
		rtr = data.frame(t(rtr))
		id = data.frame(name); names(id) = object@BIOID
		metadata = cbind(id, rtr)
		
		 if(i == 1) { 
		  lapply( 
			paste("ALTER TABLE metadata_ranges ADD COLUMN", names(rtr[, 1:ncol(rtr), drop = FALSE]), "FLOAT"), 
				function(x)  RMQuery(object@CON, x)) }

		dbWriteTable(object@CON, object@METADATA_RANGES, metadata, append = TRUE, row.names = FALSE) 

			

	}		

	
	lapply (1:length(Files$layer), FUN = .processRange) 
	 

	# last Msg
	.X.Msg(paste(nrow(Files), "ranges updated to database; Elapsed time:", 
							round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), keep = TRUE )
	
 } 
	)

#   Method 2.1:  One shp file containing all ranges, ID is required. No metadata
setMethod("rangeMapProcess",  
		signature = c(object = "rangeMapProcess",spdf = "SpatialPolygonsDataFrame", dir = "missing", ID = "character", metadata = "missing", parallel = "missing"), 
		definition = function(object, spdf, ID,  metadata){
		
		Startprocess = Sys.time()
	
		.X.Msg("Processsing ranges, please wait!...", keep = FALSE)
			
		cnv = as(canvasFetch(object), "SpatialPointsDataFrame")
		
		#  reproject
		p4s =  dbReadTable(object@CON, object@PROJ4STRING)[1,1]
		if(!identical(gsub(" ", "", proj4string(spdf)), gsub(" ", "", p4s) ) ) { 
			.X.Msg( paste("Reprojecting to", dQuote(p4s)), keep = FALSE)	
			spdf = spTransform( spdf , CRS(p4s) )
			}
	
		# split by range	
		.X.Msg( "Identifing ranges...", keep = FALSE)	
		spdf = split(spdf, spdf@data[, ID])

		rnames = names(spdf)
		pb = txtProgressBar(min = 0, max = length(rnames), char = ".", style = 3)
	
		.X.Msg( "Processing ranges...", keep = FALSE)
		.processRange = function(x) {
				name = x@data[1, ID]
				pos = which(rnames%in%name)
				setTxtProgressBar(pb, pos)
				.rangeOverlay(x,  cnv, name) 
			}
		
		overlayRes = lapply(spdf, .processRange)
			
		close(pb)

		overlayRes = do.call(rbind, overlayRes)	
		
		names(overlayRes) = c(object@ID, object@BIOID) 

		.X.Msg("Writing to project.", keep = FALSE)		
		res = dbWriteTable(object@CON, "ranges", overlayRes, append = TRUE, row.names = FALSE) 

		
		# last Msg
		if(res) .X.Msg(paste(length(rnames) , "ranges updated to database; Elapsed time:", 
							round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), keep = TRUE )
	

}
)	
	
#   Method 2.2:  One shp file containing all ranges, ID is required.  Metadata are computed
setMethod("rangeMapProcess",  
		signature = c(object = "rangeMapProcess",spdf = "SpatialPolygonsDataFrame", dir = "missing", ID = "character", metadata = "list", parallel = "missing"), 
		definition = function(object, spdf, ID,  metadata){
		
		Startprocess = Sys.time()
	
		.X.Msg("Processsing ranges, please wait!...", keep = FALSE)
			
		cnv = as(canvasFetch(object), "SpatialPointsDataFrame")
		
		#  reproject
		p4s =  dbReadTable(object@CON, object@PROJ4STRING)[1,1]
		if(!identical(gsub(" ", "", proj4string(spdf)), gsub(" ", "", p4s) ) ) { 
			.X.Msg( paste("Reprojecting to", dQuote(p4s)), keep = FALSE)	
			spdf = spTransform( spdf , CRS(p4s) )
			}
	
		# split by range	
		.X.Msg( "Identifing ranges...", keep = FALSE)	
		spdf = split(spdf, spdf@data[, ID])
		.X.Msg( paste(length(spdf), " ranges found."), keep = FALSE)
		
		rnames = names(spdf)
		pb = txtProgressBar(min = 0, max = length(rnames), char = ".", style = 3)
	
		.X.Msg( "Processing ranges...", keep = FALSE)
		.processRange = function(x) {
				name = x@data[1, ID]
				pos = which(rnames%in%name)
				setTxtProgressBar(pb, pos)
				.rangeOverlay(x,  cnv, name) 
			}
		
		overlayRes = lapply(spdf, .processRange)
			
		close(pb)

		overlayRes = do.call(rbind, overlayRes)	
		
		names(overlayRes) = c(object@ID, object@BIOID) 

		# save  to @RANGES
		.X.Msg("Writing to project...", keep = FALSE)		
		res = dbWriteTable(object@CON, "ranges", overlayRes, append = TRUE, row.names = FALSE) 
			.X.Msg(res, keep = FALSE)	
		
		# last Msg
		if(res) .X.Msg(paste(length(rnames) , "ranges updated to database; Elapsed time:", 
							round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), keep = TRUE )
	
		
		# save  to @METADATA_RANGES
		.X.Msg("Extracting metadata..", keep = FALSE)
		rtr = lapply( spdf, function(R) sapply(rangeTraits(), function(x) x(R) ) )
		rtr = data.frame(do.call(rbind, rtr))

		  lapply( 
			paste("ALTER TABLE metadata_ranges ADD COLUMN", names(rtr[, 1:ncol(rtr), drop = FALSE]), "FLOAT"), 
				function(x)  RMQuery(object@CON, x))
		
		rtr = cbind(rownames(rtr), rtr)
		names(rtr)[1] = object@BIOID
		
		res = dbWriteTable(object@CON, object@METADATA_RANGES, rtr, append = TRUE, row.names = FALSE) 
			.X.Msg(res, keep = FALSE)
	
	
	
}
)	
	
# user level function
processRanges <- function(con, ...) {

	x = new("rangeMapProcess", CON = con)
	rangeMapProcess(x, ... )	
	
}


















