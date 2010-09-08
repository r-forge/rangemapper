
	
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

# TODO; set validity, ini database etc for rangeMap

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
			stopifnot(!.dbtable.exists(object@CON, object@tableName) )

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
	
		

setClass("rangeMapFetch", representation(
				tableName    = "character"	), 
				contains = "rangeMap", 
		
				validity = function(object)	{
					mapNam =paste(object@MAP, object@tableName, sep = "") 
					
					if(!.dbtable.exists(object@CON, mapNam) )
					 stop(paste(sQuote(object@tableName), "is not a valid MAP!"))
					
					# check if empty map
					mapvar = setdiff(.sqlQuery(object@CON, paste("pragma table_info(", mapNam, ")"))$name, object@ID )
					isempty = .sqlQuery(object@CON, paste("select count (", mapvar, ") FROM", mapNam) )[, 1]
					
					if(isempty == 0)
					 stop(paste(sQuote(object@tableName), "is an empty MAP!"))
			}
	)
		
		

		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		