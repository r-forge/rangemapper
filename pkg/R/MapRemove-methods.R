
setMethod("rangeMapRemove",  
		signature = "rangeMapRemove", 
		definition = function(object){
		
				sql = paste("DROP TABLE ", object@tableName)
							for (i in 1:length(sql)) .sqlQuery(object@CON , sql[i]) 
		
		}
)


# user level
rm.rangeMapper <- function(con, tableName,  tablePrefix) {

	if(missing(tableName) ) 
		tableName = .sqlQuery(con, 'select name from sqlite_master where type = "table" and (tbl_name like "MAP_%" OR tbl_name like "BIO_%")')$name
	
	if( ! missing(tablePrefix) )
		tableName = tableName[grep(tablePrefix, tableName)]
	
	 x =  new("rangeMapRemove", CON = con, tableName = tableName)
	 rangeMapRemove(x)
	 
	 Msg( paste("The following tables were removed:\n", paste(tableName, collapse = "\n")) )
	 
 }


 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 


	



