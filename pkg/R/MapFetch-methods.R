



setMethod("rangeMapFetch",  
	signature  = "rangeMapFetch", 
		definition = function(object) {
		
    	#build tableName
		tableName = paste(object@MAP, object@tableName, sep = "")

		# map variable
		mapvar = setdiff(.sqlQuery(object@CON, paste("pragma table_info(", tableName, ")"))$name, object@ID )
		
		# fetch map
		map = .sqlQuery(object@CON, paste("SELECT c.x, c.y, r.", mapvar, "from canvas as c LEFT JOIN", tableName ,"r on c.id = r.id") )
	
		coordinates(map) = ~ x + y

		p4s = .sqlQuery(object@CON, paste("SELECT p4s FROM", object@METADATA) )[,1]
		
		proj4string(map) = CRS(p4s)
		
		gridded(map) = TRUE
		
		map
		
		}
	)	



# user level functions 
rangeMap.fetch <- function(dbcon, map) { 
	x = new("rangeMapFetch", CON = dbcon, tableName = map)
	rangeMapFetch(x)	

} 


