
.sqlMapIntersect <-function(x) {

		maps = paste(x@MAP, x@MAPS, sep = "")
		
		S1 = expand.grid(a = maps[1], b= maps[-1])
		S1 = apply(S1, 1, function(j) paste(paste(j, x@ID, sep = "."), collapse = "=" ))
		
		S2 = paste(paste("JOIN", maps[-1], "ON" , S1), collapse = " ")
		S3 = sapply(maps, function(j) setdiff(.sqlQuery(x@CON, paste("pragma table_info(", j, ")"))$name, x@ID ) )	
		S3 = paste("SELECT", paste(maps[1], x@ID, sep = "."), "," , paste(S3, collapse = ","), "FROM" )
		
		sql = paste(S3, maps[1], S2 )
}


#  Map intersect
setMethod("rangeMapIntersect",  
	signature  = c(object = "rangeMapIntersect", FUN = "missing"),
		definition = function(object, FUN,...) {
		
		if(length(object@tableName) == 0) tableName = paste(object@MAPS, collapse = "_INTERSECT_")

		fieldNam = paste(object@MAPS, collapse = "_x_")
		
		sql = .sqlMapIntersect(object)

		sql = paste("SELECT" , object@ID, ",1 as ", fieldNam, " from (", sql, ")")		
		
			
		# build table and index
		.sqlQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",fieldNam, "NUMERIC)"))
		.sqlQuery(object@CON, paste("CREATE INDEX", paste(tableName, "id", sep = "_") , "ON", tableName, "(id)") )
		.sqlQuery(object@CON, paste("INSERT INTO" ,tableName, sql) )
		
		# out msg
		return(.dbtable.exists(object@CON, tableName))
			
		

		}
	)

	
setMethod("rangeMapIntersect",  
	signature  = c(object = "rangeMapIntersect", FUN ="function"),
		definition = function(object, FUN,...) {
		
		if(length(object@tableName) == 0) tableName = paste(object@MAPS, collapse = "_INTERSECT_")

		fieldNam = paste(object@MAPS, collapse = "_x_")
		
		sql = .sqlMapIntersect(object)

		X = .sqlQuery(object@CON, sql)
		
		vars = setdiff(names(X), object@ID)
		X$x = apply(X[,  vars], 1, FUN)
		X = X[ ,setdiff(names(X), vars) ]
		
		names(X) = c(object@ID, fieldNam)
		
			
		# build table and index
		.sqlQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",fieldNam, "NUMERIC)"))
		.sqlQuery(object@CON, paste("CREATE INDEX", paste(tableName, "id", sep = "_") , "ON", tableName, "(id)") )
		dbWriteTable(object@CON, tableName, X, row.names = FALSE, append = TRUE)
		
		# out msg
		return(.dbtable.exists(object@CON, tableName))
			
		

		}
	)
	
	
	
# user level function calling rangeMapIntersect
rangeMap.intersect  <- function(CON, MAPS, overwrite = FALSE,...) {
	
	if(overwrite) 
	try(.sqlQuery(CON, paste("DROP TABLE", paste("MAP", tableName, sep = "_"))), silent = TRUE)
					
	object = new("MapIntersect", MAPS = MAPS , CON = CON, ... )

	rangeMapIntersect(object)

}				



















