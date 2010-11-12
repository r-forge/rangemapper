
setMethod("rangeMapProcess",  
		signature = "rangeMapProcess", 
		definition = function(object){
		
	Startprocess = Sys.time()
	
	Files = rangeFiles(object)
	
	cnv = as(canvasFetch(object), "SpatialPointsDataFrame")

	processRangei = function(i) {
		
	r = try(readOGR(Files$dsn[i], Files$layer[i], verbose = FALSE), silent = TRUE)
	
	
	#  reproject
	p4s =  dbReadTable(object@CON, object@PROJ4STRING)[1,1]
	if(!identical(gsub(" ", "", proj4string(r)), gsub(" ", "", p4s) ) ) r = spTransform( r , CRS(p4s) )
	
	
	if(!inherits(r, "try-error") ) {
			 
		# progress report	
		Msg( paste("Processsing ranges, please wait!...", 
				   paste("Range:", Files$layer[i]),	
					 paste(round(i/length(Files$layer)*100,2), "% done"), 
					   paste("Elapsed time:",round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), sep = "\n") )
		
		

		
			overlayRes = overlay(r, cnv) 
			overlayRes = which(!is.na(overlayRes[, 1]))
			
			if(length(overlayRes) > 0) { # do grid interpolation
				sp = cnv[overlayRes, ]
				
				o = data.frame(id = sp$id, bioid = rep(Files$layer[i], nrow(sp@coords)) ) 
				
				} else { # the polygon is smaller than a grid cell: snap to the nearest point
					ctr = apply(coordinates(r),2, mean)
					nn = spDistsN1(cnv, ctr)
					sp = cnv[which(nn == min(nn) ), ]
					o = data.frame(id = sp$id, bioid = rep(Files$layer[i], nrow(sp@coords)) )
				} 
			names(o) = c("id", "bioid")

		# save  to db
		dbWriteTable(object@CON, "ranges", o, append = TRUE, row.names = FALSE) 
		
		if(object@metadata) {
			md = data.frame(bioid =Files$layer[i], .sp.metadata(r) )
			dbWriteTable(object@CON, "metadata_ranges", md, append = TRUE, row.names = FALSE) 
			}
			
			
		} else Msg(r)
	}		
	
      	#if(object@parallel)
	  #mclapply (1:length(Files$layer) ,FUN = processRangei) else  lapply (1:length(Files$layer) ,FUN = processRangei)

	  
	  lapply (1:length(Files$layer), FUN = processRangei) 


	# last msg
	Msg(paste(nrow(Files), "ranges updated to database; Elapsed time:", 
							round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), keep = TRUE )
	
		
		
			} 
	
	)

# user level function
processRanges <- function(dir, con, ...) {

	x = new("rangeMapProcess", CON = con, dir = dir, ...)

	rangeMapProcess(x)	
		
}

 
 


























