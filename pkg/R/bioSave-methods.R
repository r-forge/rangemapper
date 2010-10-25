


setMethod("bioSave",  
	signature  = "bioSaveFile", 
		definition = function(object) {

		tableName = paste(object@BIO, object@tableName, sep = "")
		
		d = read.table(object@loc, sep = object@sep, header = TRUE, stringsAsFactors = FALSE)
		
		nam = d[, object@ID]
		
		ranges.nam = .sqlQuery(object@CON, "select distinct bioid from ranges")$bioid

		d$has_range= is.element(nam, ranges.nam)
		
		res = dbWriteTable(object@CON ,tableName , d, row.names = FALSE)

		if(res) {
			.sqlQuery(object@CON,(paste("CREATE  INDEX", paste(tableName, object@ID, sep = "_") , "ON", tableName ,  "(", object@ID ,")")) )
			Msg(paste("Table", object@tableName, "saved as a ", object@BIO, "table") )
			}
		}
	)

setMethod("bioSave",  
	signature  = "bioSaveDataFrame", 
		definition = function(object) {

		tableName = paste(object@BIO, object@tableName, sep = "")
		
		d = object@loc
		
		nam = d[, object@ID]
		
		ranges.nam = .sqlQuery(object@CON, "select distinct bioid from ranges")$bioid

		d$has_range= is.element(nam, ranges.nam)
		
		res = dbWriteTable(object@CON ,tableName , d, row.names = FALSE)

		if(res) {
			.sqlQuery(object@CON,(paste("CREATE  INDEX", paste(tableName, object@ID, sep = "_") , "ON", tableName ,  "(", object@ID ,")")) )
			Msg(paste("Table", object@tableName, "saved as a ", object@BIO, "table") )
			}
		}
	)


# user level function calling rangeMapSave
bio.save   <- function(CON, loc, overwrite = FALSE, ...) {
	

	if(is.character(loc))
		dat = new("bioSaveFile", CON = CON, loc = loc, ...)
	
	if(is.data.frame(loc))				
	dat = new("bioSaveDataFrame", CON = CON, loc = loc, ...)

	bioSave(dat)

}	


metadata2bio <-function(con, ...) {

	r = new("rangeMap", CON = con)

	dat = .sqlQuery(r@CON, paste("select * from",  r@METADATA_RANGES) )

	if(nrow(dat) == 0) stop(Msg( paste("Empty", r@METADATA_RANGES, "table")) )
	

	b = new("bioSaveDataFrame", CON = con, loc = dat, tableName = r@METADATA_RANGES, ID = r@BIOID, ...)
	
	bioSave(b)


}























