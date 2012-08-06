
# TODO

setGeneric("metadataUpdate", function(object,spdf, dir, ID,metadata, parallel)  standardGeneric("metadataUpdate") )

 # Method 1
setMethod("metadataUpdate",  
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
		x.Msg( paste("Processsing ranges, please wait!...", 
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
	x.Msg(paste(nrow(Files), "ranges updated to database; Elapsed time:", 
							round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), keep = TRUE )

			} 
	)
 