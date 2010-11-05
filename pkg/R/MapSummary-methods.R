
if (!isGeneric("summary"))
	setGeneric("summary", function(object, ...) standardGeneric("summary"))

summary.rangeMap = function(object, ...) {
    out = list()
	out[["class"]] = class(object)

	dbinfo = dbGetInfo(object@CON)
	out[["Project_location"]] = dbinfo$dbname
	out[["SQLite_version"]] = dbinfo$serverVersion

	if( nrow(.sqlQuery(object@CON, paste("select", object@ID, "from", object@CANVAS, "limit 1") )) == 0)
	out[["empty_project"]] = "Empty rangeMapper project." else {
	
		mtd = .sqlQuery(object@CON, paste("select * from", object@METADATA))
		out[["Proj4"]] = mtd$p4s
		out[["CellSize"]] = mtd$gridSize
	
		tbs = .sqlQuery(object@CON, "select name from sqlite_master where type = 'table' ")$name
		
		out[["BIO_tables"]] = paste(  gsub(object@BIO, "", tbs[grep(object@BIO, tbs)]), collapse = "\n" )
		out[["MAP_tables"]] = paste(  gsub(object@MAP, "", tbs[grep(object@MAP, tbs)]), collapse = "\n" )
	}
	
	class(out) = "summary.rangeMap"
	out
}

setMethod("summary", "rangeMap", summary.rangeMap)

print.summary.rangeMap <- function(x, ...) {


	Msg(paste(paste(names(x), ":", x), collapse = "\n"), ...)
	

}


