
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

# method for  importing external files
setMethod("rangeMapSave",  
	signature  = "MapImport", 
		definition = function(object, ...) {

	#build tableName
	tableName = paste(object@MAP, object@tableName, sep = "")		
	
	cnv = canvas.fetch(object@CON)
	gui.msg("Converting canvas to polygons.")
	cnv = rasterToPolygons(raster(cnv))
	
	if(nlayers(stack(object@path)) > 1) stop(sQuote(basename(object@path)), " contains more than one layer")
	
	gui.msg("Loading external MAP data")
	rst = raster(object@path)

	# is there any other way to compare CRS-s ?	
	if(!CRSargs(CRS(proj4string(cnv))) == CRSargs(projection(rst, FALSE))) 
		warning(sQuote(basename(object@path)), " may have a different PROJ4 string;\n", 
								"canvas:", CRSargs(CRS(proj4string(cnv))), "\n", 
								basename(object@path), ":", CRSargs(projection(rst, FALSE)) )

	
	rstp = as(as(rst, "SpatialGridDataFrame"), "SpatialPointsDataFrame") 
	rstp = rstp[which(!is.na(rstp@data[,1])), ]
	
	rstp@data$ptid = as.numeric(rownames(rstp@data)) # add point id
	
	gui.msg("Performing overlay: canvas polygons over external MAP")	
	o = overlay(cnv, rstp)
	o$ptid = as.numeric(rownames(o))

	o = merge(o, rstp@data, by = "ptid")
	o$ptid = NULL
	
	gui.msg("Agregating data")
	o = aggregate(o[, 2], list(o[,1]), FUN = mean, na.rm = TRUE)
	
	names(o) = c(object@ID, object@tableName) 

	# build table and index
	.sqlQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",object@tableName, "NUMERIC)"))
	.sqlQuery(object@CON, paste("CREATE INDEX", paste(tableName, "id", sep = "_") , "ON", tableName, "(id)") )
	dbWriteTable(object@CON, tableName, o, row.names = FALSE, append = TRUE)
	
	
	res = .dbtable.exists(object@CON, tableName)
	
	if(res) gui.msg(paste(sQuote(basename(object@path)), "imported"))
	
	return(res)	
							
								
		}
	)

	
# user level function calling rangeMapSave
rangeMap.save  <- function(CON, FUN = NULL, biotab = NULL, biotrait = NULL, formula = NULL, tableName = NULL, subset = list(), path = NULL, ...) {
	
	#  external map
	if(!is.null(path))  {
		if(is.null(tableName)) tn = make.db.names.default(basename(path))
		rmap = new("MapImport", CON = CON, path = path, tableName =  tn) } else
	
	# species richness
	if(is.null(FUN))  {
		rmap = new("rangeMapSave", CON = CON, subset = subset) } else
	
	# sqlite aggregate
	if(is.character(FUN)) {
		rmap = new("rangeMapSaveSQL", CON = CON, 
					biotab = biotab, biotrait = biotrait, FUN = FUN, tableName = tableName, subset = subset) } else
	# R aggregate		
	rmap = new("rangeMapSaveR", CON = CON,
			  biotab = biotab, biotrait = biotrait, FUN = FUN, formula = formula, tableName = tableName, subset = subset)

		  
	rangeMapSave(rmap, ...)

}				



















