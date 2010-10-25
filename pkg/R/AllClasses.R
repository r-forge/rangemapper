	
# Class definitions
setClass("rangeFiles", 
		representation(
		dir = "character",
		ogr = "logical", 
		polygons.only = "logical") , 
		prototype(
		dir = "",
		ogr = TRUE, 
		polygons.only = TRUE), 
		validity = function(object) {
		if(!file.exists(object@dir)) stop(Msg("'dir' should be set and point to a valid location."))
		
		}
	)

setClass("rangeMapStart", 
		representation(
			dir = "character", 
			file = "character", 
			scheleton = "list",
			overwrite = "logical"
			), 
		prototype(
			dir = "",
			file = paste("rangeMapperProj",format(Sys.time(), "%Y-%m-%d_%H-%M-%S.sqlite"), sep = "_"),
			scheleton = 	list(
		create = 
		c("CREATE TABLE metadata (xmin FLOAT,xmax FLOAT,ymin FLOAT,ymax FLOAT,p4s CHAR,gridSize FLOAT)", 
			"CREATE TABLE canvas (x FLOAT,y FLOAT,id INT)", 
			"CREATE TABLE ranges (id INT,bioid CHAR)",
			"CREATE TABLE metadata_ranges (bioid CHAR, Area FLOAT, Median_x FLOAT,Min_x FLOAT, Max_x FLOAT,Median_y FLOAT, Min_y FLOAT, Max_y FLOAT)")
		,
		index = 
		c(  "CREATE INDEX IF NOT EXISTS   id_canvas ON canvas (id)", 
			"CREATE INDEX IF NOT EXISTS   id_ranges ON ranges (id)", 
			"CREATE INDEX IF NOT EXISTS   bioid_ranges ON ranges (bioid)",
			"CREATE INDEX IF NOT EXISTS   bioid_metadata_ranges ON metadata_ranges (bioid)")
		), 
		overwrite = FALSE
		), 
		
		validity = function(object) {
		if(!file.exists(object@dir)) stop(Msg("'dir' should be set and point to a valid location."))
		}
	)
	

setClass("rangeMap", 
		representation(
			CON = "SQLiteConnection", 
			ID = "character",         			# the common id column
			BIOID = "character",         		# the common id column in BIo tables
			METADATA = "character",   			#pre-defined table
			METADATA_RANGES = "character",   	#optional pre-defined table
			CANVAS = "character",   			#pre-defined table
			RANGES = "character", 				# pre-defined table containing id and bioid
			BIO = "character", 					# prefix for BIO tables,
			MAP = "character" 					# prefix for MAP tables
			),
		prototype(
			ID = "id",         
			BIOID = "bioid",         
			METADATA = "metadata", 
			METADATA_RANGES = "metadata_ranges",  
			CANVAS = "canvas",   
			RANGES = "ranges", 
			BIO = "BIO_", 
			MAP = "MAP_"
			), 
		
		validity = function(object) {
		if ( ! init_extensions(object@CON)) warning(Msg("Warning: RSQLite.extfuns not available!"))
		if ( ! all( .dbtable.exists(object@CON, object@METADATA),
				  .dbtable.exists(object@CON, object@METADATA_RANGES),
				  .dbtable.exists(object@CON, object@CANVAS),
				  .dbtable.exists(object@CON, object@RANGES) ) ) stop (Msg("Corrupt rangeMapper project!"))
		
	
		}	
	)

setClass("rangeMapBbox", 
		representation(
		checkProj = "logical"	
			), 
		
		contains = c("rangeFiles", "rangeMap"), 
		
		validity = function(object)	{
					return(TRUE)
			
		},
		prototype(
			checkProj = TRUE)
	)	

setClass("gridSize", 
		representation(
		gridSize = "numeric"	
			), 
		
		contains = "rangeMap", 
		
		validity = function(object)	{
			invisible(TRUE)
		},
	)
	
setClass("rangeMapProcess", 
		representation(
		metadata = "logical"	
			), 
		
		contains = c("rangeFiles", "rangeMap"), 
		
		validity = function(object)	{
					return(TRUE)
			
		},
		prototype(
			metadata = TRUE)
	)	
	
setClass("rangeMapSave", 
		representation(
			biotab    = "character", 
			biotrait  = "character",
			tableName = "character",
			subset    = "list"), 
			
		contains = "rangeMap", 
		
		prototype(
			tableName = "species_richness"), 
			
		validity = function(object)	{
		# the new table should not exist
			if(.dbtable.exists(object@CON, paste(object@MAP, object@tableName, sep = "") ) ) 
				stop(Msg( paste(sQuote(object@tableName), " already exists."))	)
			
			if(.dbtable.exists(object@CON, paste(object@BIO, object@tableName, sep = "") ) ) 
				stop(Msg( paste(sQuote(object@tableName), " already exists."))	)
			
		}
	)

	
setClass("bioSaveFile", representation(loc = "character", sep = "character"), 
							contains = "rangeMapSave", 
							prototype( sep = ";", 
									  tableName = "unknown"
							
							), 
							validity = function(object) {
							if(!file.exists(object@loc)) stop(Msg(paste(sQuote(object@loc), "is not a valid file")))


							}
		)	
	
	
setClass("bioSaveDataFrame", representation(loc = "data.frame"), 
							contains = "rangeMapSave", 
							validity = function(object) {

							}
		)	
		
	
	
setClass("rangeMapSaveSQL", representation (FUN = "character"), 
							contains = "rangeMapSave", 
							validity = function(object) {
							
							biotab = paste(object@BIO, object@biotab, sep = "")
							
							if(!.dbtable.exists(object@CON,biotab) ) 
								stop(Msg( paste(sQuote(object@biotab), "is not a table of", sQuote(dbGetInfo(object@CON)$dbname))))
							
							# object@biotrait should exist as a field in biotab
							if(!.dbfield.exists(object@CON,biotab, object@biotrait) ) 
								stop(Msg(paste(sQuote(object@biotrait), "is not a field of", sQuote(object@biotab))))
										
							# fun should  be known by sqlite	
							.sqlAggregate(object@FUN)

								}
		)

setClass("rangeMapSaveR", representation (FUN = "function", formula = "formula"), 
							contains = "rangeMapSave", 
 
							validity = function(object) {
							
							biotab = paste(object@BIO, object@biotab, sep = "")
							
							# biotab should exist 
							if(!.dbtable.exists(object@CON, biotab) ) 
								stop(Msg(paste(sQuote(biotab), "is not a table of", sQuote(dbGetInfo(object@CON)$dbname))))
							
							# object@biotrait should exist as a field in biotab
							if(!.dbfield.exists(object@CON, biotab, object@biotrait) ) 
								stop(Msg(paste(sQuote(object@biotrait), "is not a field of", sQuote(biotab))))
							
							# FUN should be of form biotab ~ ...
							if(update(object@formula, . ~ 1 ) != as.formula( paste(object@biotrait,  " ~ 1"))) 
								stop(Msg("Formula and biotrait does not match"))
							
							return(TRUE)
							}
		)						
	
setClass("MapImport", representation (path = "character", FUN = "function"), 
							contains = "rangeMapSave", 
 
							validity = function(object) {
							
							if(!file.exists(object@path)) stop(sQuote(object@path), "is not a valid path.")	
							if(!require("raster")) stop(Msg("package raster is not available"))

							}, 
							prototype(
							FUN = mean 
							) 
		)		

setClass("rangeMapFetch", representation(
					tableName    = "character"), 
				    contains = "rangeMap", 
	
					validity = function(object)	{
						mapNam =paste(object@MAP, object@tableName, sep = "") 
						
						invalidNam = sapply(mapNam, FUN = function(x) !.dbtable.exists(object@CON, x) )
						
						if( any(invalidNam) )
						  stop(Msg(paste(sQuote(names(invalidNam[invalidNam] )), "is not a valid MAP!\n")))
						
						# check if empty map
						mapvar = sapply(mapNam, function(x)
									setdiff(.sqlQuery(object@CON, paste("pragma table_info(", x, ")"))$name, object@ID ) )
						
						sql = paste("select count (", mapvar, ") FROM", mapNam)
						isempty = sapply(sql, function(x) .sqlQuery(object@CON,  x)[, 1] ) < 1
						
						if(any(isempty))
						 stop(Msg(paste(sQuote(mapNam[isempty]), "is an empty MAP!\n")))
			}
	)
	

setClass("rangeMapRemove", representation(
						tableName = "character" 
						), 
						 contains = "rangeMap",
						 
						validity = function(object)	{
									if(object@METADATA%in%object@tableName |object@CANVAS%in%object@tableName|object@CANVAS%in%object@tableName)
									stop(Msg( paste(object@METADATA,",", object@CANVAS, "or", object@RANGES, "table(s) cannot be removed!")) )

			}
	)
		
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

