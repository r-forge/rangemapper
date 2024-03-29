
if(getRversion() >= '2.15.1') utils::globalVariables(c('dbcon')) 


setGeneric("bioSave", function(object, ...)  	standardGeneric("bioSave") )

setMethod("bioSave",  
	signature  = "bioSaveFile", 
		definition = function(object) {

		tableName = paste(object@BIO, object@tableName, sep = "")
		
		d = read.table(object@loc, sep = object@sep, header = TRUE, stringsAsFactors = FALSE)
		
		nam = d[, object@ID]
		
		ranges.nam = RMQuery(object@CON, "select distinct bioid from ranges")$bioid

		d$has_range= is.element(nam, ranges.nam)
		
		res = dbWriteTable(object@CON ,tableName , d, row.names = FALSE)

		if(res) {
			RMQuery(object@CON,(paste("CREATE  INDEX", paste(tableName, object@ID, sep = "_") , "ON", tableName ,  "(", object@ID ,")")) )
			message(paste("Table", object@tableName, "saved as a ", object@BIO, "table") )
			}
		}
	)

setMethod("bioSave",  
	signature  = "bioSaveDataFrame", 
		definition = function(object) {

		tableName = paste(object@BIO, object@tableName, sep = "")
		
		d = object@loc
		
		nam = d[, object@ID]
		
		ranges.nam = RMQuery(object@CON, "select distinct bioid from ranges")$bioid

		d$has_range = is.element(nam, ranges.nam)
		
		res = dbWriteTable(object@CON ,tableName , d, row.names = FALSE)

		if(res) {
			RMQuery(object@CON,(paste("CREATE  INDEX", paste(tableName, object@ID, sep = "_") , "ON", tableName ,  "(", object@ID ,")")) )
			message(paste("Table", object@tableName, "saved as a ", object@BIO, "table") )
			} else 
				message( paste("Error in saving", object@tableName) )
		}
	)


# user level functions
bio.save   <- function(con, loc, tableName, ...) {
	
	
	if(is.character(loc)) {
		if(missing(tableName)) tableName = gsub("\\.", "_", basename(loc))
		dat = new("bioSaveFile", CON = con, loc = loc, tableName = tableName, ...)
	}
	
	if(is.data.frame(loc)) {			
		if(missing(tableName)) tableName = deparse(substitute(loc))
		dat = new("bioSaveDataFrame", CON = con, loc = loc, tableName = tableName, ...)
	}
	
	bioSave(dat)

}	

bio.merge <-  function(con, tableName, ...) {
# merge 2 or more BIO tables, default is merge all

	r = new("rangeMap", CON = con)
	tableName = paste(r@BIO, tableName, sep = "")
	dots = list(...)

	if(length(dots) > 0) 
	 btabs = paste(r@BIO, dots, sep = "") else
	 btabs = RMQuery(con, paste("select name from sqlite_master where type = 'table' and tbl_name like '", r@BIO,"%'", sep = ""))$name

	ok = sapply(btabs, function(x) .dbtable.exists(con, x) )

	if(!all(ok)) 
		stop(paste( dQuote(names(ok[!ok])), "is not a table of this rangeMapper project"))

	ids = sapply(btabs, function(x) .extract.indexed(con, x) )

	head = paste("(", paste(paste("SELECT DISTINCT",ids,"as", r@BIOID , "FROM",  names(ids)), collapse = " UNION "), ") as x")

	colnames = unlist(lapply(btabs, 
		function(x) { 
			a = RMQuery(con, paste("pragma table_info(" , shQuote(x),")" ))$name
			alias = paste(a, gsub(r@BIO, "", x) , sep = "_" )
			nm = paste(x, a,sep = "." )
			paste(nm, alias, sep = " as ")		
			} ))

	colnames = paste(setdiff(colnames, ids), collapse = ",")
	colnames = paste("SELECT DISTINCT", paste(paste("x", r@BIOID, sep = "."), "as", r@BIOID ),  ",", colnames)


	joins = character()
		for(i in 1:length(ids)) joins[i] = paste("LEFT JOIN", names(ids[i]), "ON", paste("x", r@BIOID, sep ="."), "=", paste(names(ids[i]), ids[i], sep =".") ) 

	sqls = paste("create table", tableName,  "as", colnames, "FROM", head,  paste(joins, collapse = " " ))
	# make table
	res = RMQuery(con, sqls)
	
	# add index
	RMQuery(r@CON,( paste("CREATE  INDEX", paste(tableName, r@BIOID, sep = "_") , "ON", tableName ,  "(", r@BIOID ,")") ) )


}

metadata2bio <-function(con, ...) {

	r = new("rangeMap", CON = con)

	dat = RMQuery(r@CON, paste("select * from",  r@METADATA_RANGES) )

	if(nrow(dat) == 0) stop( paste("Empty", r@METADATA_RANGES, "table"))
	

	b = new("bioSaveDataFrame", CON = con, loc = dat, tableName = r@METADATA_RANGES, ID = r@BIOID, ...)
	
	bioSave(b)


}























