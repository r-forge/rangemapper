
# accessor functions for methods
subsetSQLstring   <- function(dbcon, subset = list() ) {

	if(length(subset) == 0) sql = NULL else {
	if(is.null(names(subset))) stop(sQuote('subset'), " must be a named list!")

	m = subset[grep("^MAP_", names(subset))]
	b = subset[grep("^BIO_", names(subset))]
	r = subset[which(names(subset)=="metadata_ranges")]

	msql = if(length(m) > 0) paste(paste("r.id in (SELECT id FROM", names(m), "WHERE", m, ")"), collapse = " AND ") else NULL
	bsql = if(length(b) > 0) paste(paste("r.bioid in (SELECT", 
			sapply(names(b), function(x) .extract.indexed(dbcon, x)) ,
				"FROM", names(b), "WHERE", b, ")"), collapse = " AND ") else NULL
	rsql = if(length(r) > 0) paste(paste("r.bioid in (SELECT bioid FROM", names(r), "WHERE", r, ")"), collapse = " AND ") else NULL

	sql = paste( c(msql, bsql, rsql), collapse = " AND ")
	}
	sql	
}
	

# method for species richness, no biotab, biotrait is given
setMethod("rangeMapSave",  
		signature = "rangeMapSave", 
		definition = function(object){
			#build tableName
			tableName = paste(object@MAP, object@tableName, sep = "")
			
			# build sql subset
			sset = subsetSQLstring(object@CON, object@subset)
			
			# build sql string
			richnessSQL = paste("SELECT id, count(r.id) as", object@tableName ,"from ranges as r", 
							if(!is.null(sset)) paste("WHERE", sset), "group by r.id")
			
			# build table and index	
			.sqlQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",object@tableName, "NUMERIC)"))
			.sqlQuery(object@CON,paste("CREATE  INDEX", paste(object@tableName, object@ID, sep = "_") ,"ON", tableName, "(id)") )
			.sqlQuery(object@CON, paste("INSERT INTO" ,tableName, richnessSQL) )

		 return(.dbtable.exists(object@CON, tableName))
			} 
	
	)
		
# agggregate method using sqlite 	 	
setMethod("rangeMapSave",  
	signature  = "rangeMapSaveSQL", 
		definition = function(object) {
		#build tableName
		tableName = paste(object@MAP, object@tableName, sep = "")
	
		# build sql subset
		sset = subsetSQLstring(object@CON, object@subset)
		# build sql string
		sql = paste("SELECT r.id, b.",object@biotrait,"FROM ranges r left join ", 
				object@biotab, " b WHERE r.bioid = b.", .extract.indexed(object@CON, object@biotab), 
				  if(!is.null(sset)) paste("AND", sset) )
		sql = paste("SELECT id,", object@FUN ,"(", object@biotrait, ") as", object@biotrait, "from (",sql,") group by id")	

		# build table and index
		.sqlQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",object@biotrait, "NUMERIC)"))
		.sqlQuery(object@CON, paste("CREATE INDEX", paste(tableName, "id", sep = "_") , "ON", tableName, "(id)") )
		.sqlQuery(object@CON, paste("INSERT INTO" ,tableName, sql) )
		
		# out msg
		return(.dbtable.exists(object@CON, tableName))
			
		
		#cat(strwrap(sql, width = 100))
		}
	)						

# agggregate method using R functions 
setMethod("rangeMapSave",  
	signature  = "rangeMapSaveR", 
		definition = function(object, ...) {
		

		#build tableName
		tableName = paste(object@MAP, object@tableName, sep = "")

		# build sql subset
		sset = subsetSQLstring(object@CON, object@subset)
		# build sql string
		sql = paste("SELECT r.id, b.* FROM ranges r left join ", 
				object@biotab, " b WHERE r.bioid = b.", .extract.indexed(object@CON, object@biotab), 
				  if(!is.null(sset)) paste("AND", sset) )

		# fetch table
		d = .sqlQuery(object@CON, sql)
		
		# apply R function (FUN has  a formula method)
		dl = split(d, d[, object@ID])
		X = sapply(dl, FUN = function(x) object@FUN(formula = object@formula, data = x, ...) )
				
		X = data.frame(id = names(X), X)
		names(X) = c(object@ID, object@biotrait)
		row.names(X) = NULL
				
		# build table and index
		.sqlQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",object@biotrait, "NUMERIC)"))
		.sqlQuery(object@CON, paste("CREATE INDEX", paste(tableName, "id", sep = "_") , "ON", tableName, "(id)") )
		dbWriteTable(object@CON, tableName, X, row.names = FALSE, append = TRUE)
		
		#out msg
		return(.dbtable.exists(object@CON, tableName))
			
		#cat(strwrap(sql, width = 100))
		}
	)						

# user level function calling rangeMapSave
rangeMap.save  <- function(CON, FUN = NULL, biotab = NULL, biotrait = NULL, formula = NULL, tableName = NULL, subset = list(), ...) {
	
	# species richness
	if(is.null(FUN))  {
		rmap = new("rangeMapSave", CON = CON, subset = subset) } else
	# sqlite aggregate
	if(is.character(FUN)) {
		rmap = new("rangeMapSaveSQL", CON = CON, 
					biotab = biotab, biotrait = biotrait, FUN = FUN,tableName = tableName, subset = subset) } else
	# R aggregate		
	rmap = new("rangeMapSaveR", CON = CON,
			  biotab = biotab, biotrait = biotrait, FUN = FUN, formula = formula, tableName = tableName, subset = subset)

	rangeMapSave(rmap, ...)

}				



















