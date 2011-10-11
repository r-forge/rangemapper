

 .rangeTraits <- function(spdf, ...) {
# ... must be functions
# if functions in ... return a named numeric vector then the name is used else names T1, T2, Tn are used.

	dots = list(...)

	if( ! all(sapply(dots, is.function)) ) stop (.X.Msg("All extra arguments must be functions"))


	Area = sum(sapply(slot(spdf, "polygons"), function(x) slot(x, "area") ))

	midExt = apply(coordinates(spdf), 2, FUN = function(x) data.frame(Median = median(x), Min = min(x), Max = max(x) ) )

	names(midExt[[1]]) = paste(names(midExt[[1]]), "x", sep = "_")
	names(midExt[[2]]) = paste(names(midExt[[2]]), "y", sep = "_")

	default = cbind(Area, midExt[[1]], midExt[[2]])

	if(length(dots) > 0) {
		
		userdef = sapply(dots, function(x) x(spdf))	

		if( !is.numeric(userdef) ) stop(.X.Msg("User defined functions should return numeric vectors"))
		
		if(is.null(names(userdef))) names(userdef) = paste("V", 1:length(userdef), sep = "")
		
		if( any(nchar(names(userdef)) == 0 )) {
			nonam = which(nchar(names(userdef)) == 0)
			names(userdef)[nonam]  = paste("V", 1:length(nonam), sep = "")
			}
		names(userdef) =   make.db.names.default(names(userdef))	

		res = cbind(default,  data.frame( t(userdef )) )
		} else 
		res = default
	
	res
}

setGeneric("rangeMapProcess", function(object,file, dir, ID, ...)  standardGeneric("rangeMapProcess") )

 # Method 1:  Each range file is a separate shp file. 
setMethod("rangeMapProcess",  
		signature = c(object = "rangeMap",file = "missing", dir = "character", ID = "missing"), 
		definition = function(object, dir , ...){
	# . . . pass to rangeTraits	

	Startprocess = Sys.time()

	Files = rangeFiles(new("rangeFiles", dir = dir))
	
	cnv = as(canvasFetch(object), "SpatialPointsDataFrame")

	processRangei = function(i) {
		
	r = try(readOGR(Files$dsn[i], Files$layer[i], verbose = FALSE), silent = TRUE)
	
	#  reproject
	p4s =  dbReadTable(object@CON, object@PROJ4STRING)[1,1]
	if(!identical(gsub(" ", "", proj4string(r)), gsub(" ", "", p4s) ) ) r = spTransform( r , CRS(p4s) )
	
	
	if(!inherits(r, "try-error") ) {
			 
		# progress report	
		.X.Msg( paste("Processsing ranges, please wait!...", 
				   paste("Range:", Files$layer[i]),	
					 paste(round(i/length(Files$layer)*100,2), "% done"), 
					   paste("Elapsed time:",round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), sep = "\n"), 
					 keep = FALSE)
		

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
			rtr = .rangeTraits(r, ...)
			if( i == 1 && ncol(rtr) > 7 )# reshape metadata_ranges table
			 lapply( paste("ALTER TABLE metadata_ranges ADD COLUMN", names(rtr[, 8:ncol(rtr), drop = FALSE]), "FLOAT"), function(x)  RMQuery(object@CON, x))
	
			md = data.frame(bioid =Files$layer[i],  rtr)
			dbWriteTable(object@CON, "metadata_ranges", md, append = TRUE, row.names = FALSE) 
			}
			
			
		} else .X.Msg(r)
	}		
	
     if(object@parallel) {
	 
	lapply (1:length(Files$layer), FUN = processRangei) 
		 
	 } else

	 lapply (1:length(Files$layer), FUN = processRangei) 
	 

	# last Msg
	.X.Msg(paste(nrow(Files), "ranges updated to database; Elapsed time:", 
							round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), keep = TRUE )
	
		
		
			} 
	
	)
	
#   Method 2:  One big shp file
setMethod("rangeMapProcess",  
		signature = c(object = "rangeMap",file = "character", dir = "missing", ID = "character"), 
		definition = function(object, file, ID,  ...){
		Startprocess = Sys.time()
	
		.X.Msg("Processsing ranges, please wait!...", keep = FALSE)
			
		cnv = as(canvasFetch(object), "SpatialPointsDataFrame")
					
		r = readOGR(dirname(file), gsub(".shp", "", basename(file)), verbose = FALSE)
		
		#  reproject
		p4s =  dbReadTable(object@CON, object@PROJ4STRING)[1,1]
		if(!identical(gsub(" ", "", proj4string(r)), gsub(" ", "", p4s) ) ) r = spTransform( r , CRS(p4s) )
	
		# split by range		
		r = split(r, r@data[, ID])

		overlayRes = lapply(r, function(x) {
		
				.X.Msg(paste("Processsing", x@data[1, ID]), keep = FALSE)
				o = overlay(x[, ID], cnv)
				
				# TODO
				#snap to grid if o returns nothing. 
				# add snap = TRUE for both methods
				
				m = cbind(cnv@data, o)
				m[!is.na(m[, ID]), ]
			})
			

		overlayRes = do.call(rbind, overlayRes )	
		names(overlayRes) = c("id", object@BIOID) 

		.X.Msg("Writing to project.", keep = FALSE)		
		dbWriteTable(object@CON, "ranges", overlayRes, append = TRUE, row.names = FALSE) 

		# last Msg
		nranges = RMQuery(object@CON, paste("SELECT count(*) FROM (SELECT distinct", object@BIOID, " from", object@RANGES, ")"))[1,1]
		
		.X.Msg(paste(nranges , "ranges updated to database; Elapsed time:", 
							round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), keep = TRUE )
	

}
)	
	
	
# user level function
processRanges <- function(con, metadata = TRUE, ...) {

	x = new("rangeMapProcess", CON = con, metadata = metadata)

	rangeMapProcess(x, ...)	
	
}




























