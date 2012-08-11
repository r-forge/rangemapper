

setGeneric("metadataUpdate", function(object, FUN,name, raster,vector, parallel, ...)  standardGeneric("metadataUpdate") )

 # Method 1: using  a RasterLayer object
setMethod("metadataUpdate",  
		signature = c(object = 'rangeMap', FUN = 'function', name = 'character', 
					 raster = 'SpatialGridDataFrame', vector = 'missing' , parallel = 'missing'), 
		definition = function(object, FUN, name, raster, ...){ # ... goes to FUN
			Startprocess = Sys.time()
			
			# only use 1st band of the raster
			if(length(names(raster)) > 1) {
				warning( paste("The raster has more than one band, only", dQuote(names(raster)[1]), "will be used!") )
			raster = raster[names(raster)[1]]
			}
			
			# check if metadata_ranges is populated
			empty = RMQuery(object@CON, "SELECT count(bioid) from metadata_ranges") == 0
			if(empty)
				RMQuery(object@CON, "INSERT INTO metadata_ranges SELECT distinct bioid from ranges") 
			
			# add new column
			if( is.element(name, c(names(RMQuery(object@CON, 'SELECT * FROM metadata_ranges limit 1')), SQLKeywords(object@CON) ) ) )
				stop(paste(dQuote(name), "allready exists or is not a valid SQL name!") )
			name = make.db.names(object@CON, name)
			RMQuery(object@CON, paste("ALTER TABLE metadata_ranges ADD COLUMN", name ,"NUMERIC;") )
			
			# get ranges listing
			mr = RMQuery(object@CON, "SELECT bioid from metadata_ranges")$bioid
						
			# loop through ranges,  apply FUN and update metadata_ranges
			
			x.Msg("Updating metadata_ranges ...", keep = FALSE)
			pb = txtProgressBar(min = 0, max = length(mr), char = ".", style = 3)

			for( i in 1:length(mr) ) {
				idi = mr[i]
				ri = range.fetch(object, idi)
				sraster = raster[!is.na(over(raster, ri)), ] #  SpatialGridDataFrame subset
				x = as.numeric(sraster@data[,1])
				res = FUN(x, ...) #apply FUN
				
				if( any(length(res) > 1 | res%in%c(-Inf, Inf)) ) {
					stop( paste("FUN returned ", dQuote(res), ". It should only return a non-infinite numeric vector of length 1.", sep = '') )
				
				
				
				}
				
				if(!is.na(res)) 
					RMQuery(object@CON, paste('UPDATE metadata_ranges SET' ,name, '=' ,res, 'WHERE bioid =' ,shQuote(idi) ))
				setTxtProgressBar(pb, i)
			}
			
	# last Msg
	x.Msg( paste("Done in ", round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), keep = TRUE )
	} 
)
	

# Method 2: using  a Spatial object, sp::over, and rgeos methods

	
	
	
	
	
	
	
	
	
 