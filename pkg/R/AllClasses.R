	
# Class definitions

setClass("rangeMap", 
		representation(
			CON = "SQLiteConnection", 
			ID = "character",     # the common id column
			METADATA = "character",   #pre-defined table
			METADATA_RANGES = "character",   #optional pre-defined table
			CANVAS = "character",   #pre-defined table
			RANGES = "character", # pre-defined table containing id and bioid
			BIO = "character", # prefix for BIO tables,
			MAP = "character" # prefix for MAP tables
			),
		prototype(
			ID = "id",         
			METADATA = "metadata", 
			METADATA_RANGES = "metadata_ranges",  
			CANVAS = "canvas",   
			RANGES = "ranges", 
			BIO = "BIO_", 
			MAP = "MAP_"
			), 
		validity = function(object) {
		init_extensions(object@CON)
		
		}	
			
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
			if(.dbtable.exists(object@CON, paste(object@MAP, object@tableName, sep = "") ) ) stop(sQuote(object@tableName), " already exists.")	
			
		}
	)

setClass("rangeMapSaveSQL", representation (FUN = "character"), 
							contains = "rangeMapSave", 
							validity = function(object) {
							
							biotab = paste(object@BIO, object@biotab, sep = "")
							
							if(!.dbtable.exists(object@CON,biotab) ) 
								stop(sQuote(object@biotab), "is not a table of", sQuote(dbGetInfo(object@CON)$dbname))
							
							# object@biotrait should exist as a field in biotab
							if(!.dbfield.exists(object@CON,biotab, object@biotrait) ) 
								stop(sQuote(object@biotrait), "is not a field of", sQuote(object@biotab))
										
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
								stop(sQuote(biotab), "is not a table of", sQuote(dbGetInfo(object@CON)$dbname))
							
							# object@biotrait should exist as a field in biotab
							if(!.dbfield.exists(object@CON, biotab, object@biotrait) ) 
								stop(sQuote(object@biotrait), "is not a field of", sQuote(biotab))
							
							
							# FUN should be of form biotab ~ ...
							stopifnot(update(object@formula, . ~ 1 ) == as.formula( paste(object@biotrait,  " ~ 1")))
							
							return(TRUE)
							}
		)						
	
setClass("MapImport", representation (path = "character", FUN = "function"), 
							contains = "rangeMapSave", 
 
							validity = function(object) {
							
							if(!file.exists(object@path)) stop(sQuote(object@path), "is not a valid path.")	
							stopifnot(require("raster"))

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
					  stop(paste(sQuote(names(invalidNam[invalidNam] )), "is not a valid MAP(s)!"))
					
					# check if empty map
					mapvar = sapply(mapNam, function(x)
								setdiff(.sqlQuery(object@CON, paste("pragma table_info(", x, ")"))$name, object@ID ) )
					
					sql = paste("select count (", mapvar, ") FROM", mapNam)
					isempty = sapply(sql, function(x) .sqlQuery(object@CON,  x)[, 1] ) < 1
					
					if(any(isempty))
					 stop(paste(sQuote(mapNam[isempty]), "is an empty MAP(s)!"))
			}
	)
	

	

		

		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		