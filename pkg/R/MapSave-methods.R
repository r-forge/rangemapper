
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
	
# method for species richness,
setMethod("rangeMapSave",  
		signature = c(object = "rangeMapSave", FUN = "missing", formula = "missing"), 
		definition = function(object, FUN, formula){
			
			if(length(object@tableName) == 0) object@tableName = "species_richness"
			
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
	signature  = c(object = "rangeMapSave", FUN = "character", formula = "missing"),
		definition = function(object, FUN, formula) {
		
		# CHECKS
		biotab = paste(object@BIO, object@biotab, sep = "")
			if(!.dbtable.exists(object@CON,biotab) ) 
			stop(Msg( paste(sQuote(object@biotab), "is not a table of", sQuote(dbGetInfo(object@CON)$dbname))))
		# object@biotrait should exist as a field in biotab
		if(!.dbfield.exists(object@CON,biotab, object@biotrait) ) 
			stop(Msg(paste(sQuote(object@biotrait), "is not a field of", sQuote(object@biotab))))
		# fun should  be known by sqlite	
		.sqlAggregate(FUN)
		
		
		# BIO_tab name
		biotab = paste(object@BIO, object@biotab, sep = "")
		
		#build MAP_ tableName
		tableName = paste(object@MAP, object@tableName, sep = "")
	
		# build sql subset
		sset = subsetSQLstring(object@CON, object@subset)
		# build sql string
		sql = paste("SELECT r.id, b.",object@biotrait,"FROM ranges r left join ", 
				biotab, " b WHERE r.bioid = b.", .extract.indexed(object@CON, biotab), 
				  if(!is.null(sset)) paste("AND", sset) )
		sql = paste("SELECT id,", FUN ,"(", object@biotrait, ") as", object@biotrait, "from (",sql,") group by id")	

		# build table and index
		.sqlQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",object@biotrait, "NUMERIC)"))
		.sqlQuery(object@CON, paste("CREATE INDEX", paste(tableName, "id", sep = "_") , "ON", tableName, "(id)") )
		.sqlQuery(object@CON, paste("INSERT INTO" ,tableName, sql) )
		
		# out msg
		return(.dbtable.exists(object@CON, tableName))
			
		
		#cat(strwrap(sql, width = 100))
		}
	)						

.rangeMapSaveData <-function(object) {
		# CHECKS
		biotab = paste(object@BIO, object@biotab, sep = "")
			if(!.dbtable.exists(object@CON,biotab) ) 
			stop(Msg( paste(sQuote(object@biotab), "is not a table of", sQuote(dbGetInfo(object@CON)$dbname))))
		# object@biotrait should exist as a field in biotab
		if(!.dbfield.exists(object@CON,biotab, object@biotrait) ) 
			stop(Msg(paste(sQuote(object@biotrait), "is not a field of", sQuote(object@biotab))))
		
		# BIO_tab name
		biotab = paste(object@BIO, object@biotab, sep = "")
		
		#build tableName
		tableName = paste(object@MAP, object@tableName, sep = "")

		# build sql subset
		sset = subsetSQLstring(object@CON, object@subset)
		# build sql string
		sql = paste("SELECT r.id, b.* FROM ranges r left join ", 
				biotab, " b WHERE r.bioid = b.", .extract.indexed(object@CON, biotab), 
				  if(!is.null(sset)) paste("AND", sset) )

		# fetch table
		d = .sqlQuery(object@CON, sql)
		
		# return list
		split(d, d[, object@ID])
}
	
	
# agggregate method using R functions called directly on the data 
setMethod("rangeMapSave",  
	signature  = c(object = "rangeMapSave", FUN = "function", formula = "missing"), 
		definition = function(object, FUN, formula, ...) {
		
		# get data afer checking
		dl = .rangeMapSaveData (object)
		
		# apply R function
		X = sapply(dl, FUN = function(x) FUN(x[, object@biotrait], ...) )
				
		X = data.frame(id = names(X), X)
		names(X) = c(object@ID, object@biotrait)
		row.names(X) = NULL
				
		# build table and index
		.sqlQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",object@biotrait, "NUMERIC)"))
		.sqlQuery(object@CON, paste("CREATE INDEX", paste(tableName, "id", sep = "_") , "ON", tableName, "(id)") )
		dbWriteTable(object@CON, tableName, X, row.names = FALSE, append = TRUE)
		
		#out msg
		return(.dbtable.exists(object@CON, tableName))
			
		}
	)

# agggregate method using R functions called directly using formula, data interface
setMethod("rangeMapSave",  
	signature  = c(object = "rangeMapSave", FUN = "function", formula = "formula"), 
		definition = function(object, FUN, formula, ...) {
		
		# get data afer checking
		dl = .rangeMapSaveData (object)
		
		# apply R function
		X = sapply(dl, FUN = function(x) FUN(formula = formula, data = x, ...) )
				
		X = data.frame(id = names(X), X)
		names(X) = c(object@ID, object@biotrait)
		row.names(X) = NULL
				
		# build table and index
		.sqlQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",object@biotrait, "NUMERIC)"))
		.sqlQuery(object@CON, paste("CREATE INDEX", paste(tableName, "id", sep = "_") , "ON", tableName, "(id)") )
		dbWriteTable(object@CON, tableName, X, row.names = FALSE, append = TRUE)
		
		#out msg
		return(.dbtable.exists(object@CON, tableName))
			
		}
	)







	

# method for  importing external files
setMethod("rangeMapSave",  
	signature  = c(object = "MapImport", FUN = "function"),
		definition = function(object,FUN, ...) {

	filenam = basename(object@path)
	
	if(length(object@tableName)== 0) tableName = make.db.names.default(filenam)
	
	tableName = paste(object@MAP, object@tableName, sep = "")		
	
	
	cnv = canvas.fetch(object@CON)
	Msg("Converting canvas to polygons...")
	cnv = rasterToPolygons(raster(cnv))
	
	Msg("Loading external MAP data")
	if(raster::nlayers(stack(object@path)) > 1) stop(sQuote(filenam), " contains more than one layer")
	
	rst = raster::raster(object@path)
		

	# is there any other way to compare CRS-s ?	
	if(!CRSargs(CRS(proj4string(cnv))) == CRSargs(projection(rst, FALSE))) 
		warning(sQuote(filenam), " may have a different PROJ4 string;\n", "canvas:", CRSargs(CRS(proj4string(cnv))), "\n", filenam, ":", CRSargs(projection(rst, FALSE)) )

	
	rstp = as(as(rst, "SpatialGridDataFrame"), "SpatialPointsDataFrame") 
	rstp = rstp[which(!is.na(rstp@data[,1])), ]
	
	rstp@data$ptid = as.numeric(rownames(rstp@data)) # add point id
	
	Msg(paste("Performing overlay: canvas polygons over", filenam) )	
	o = overlay(cnv, rstp)
	o$ptid = as.numeric(rownames(o))

	o = merge(o, rstp@data, by = "ptid")
	o$ptid = NULL
	
	Msg("Agregating data")
	o = aggregate(o[, 2], list(o[,1]), FUN = FUN, na.rm = TRUE, ...)
	
	names(o) = c(object@ID, object@tableName) 

	# build table and index
	.sqlQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",object@tableName, "NUMERIC)"))
	.sqlQuery(object@CON, paste("CREATE INDEX", paste(tableName, "id", sep = "_") , "ON", tableName, "(id)") )
	dbWriteTable(object@CON, tableName, o, row.names = FALSE, append = TRUE)
	
	
	res = .dbtable.exists(object@CON, tableName)
	
	if(res) Msg(paste(sQuote(basename(object@path)), "imported"))
	
	return(res)	
							
								
		}
	)


	# TODO .........................
	
# user level function calling rangeMapSave
rangeMap.save  <- function(CON, tableName, subset = list(), path , overwrite = FALSE,...) {
	
	if(overwrite) 
	try(.sqlQuery(CON, paste("DROP TABLE", paste("MAP", tableName, sep = "_"))), silent = TRUE)
		
	#  external map
	if(!missing(path)) {
			rmap = new("MapImport", CON = CON, path = path, tableName = tableName) 
			} else
	
		rmap = new("rangeMapSave", CON = CON, tableName  = tableName, subset = subset)

		  
	rangeMapSave(rmap, ....)

}				



















