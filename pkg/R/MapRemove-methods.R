
setMethod("rangeMapRemove",  
		signature = "rangeMap", 
		definition = function(object, table.nam, table.type){
		if(missing(table.nam)) 
		table.nam = .sqlQuery(object@CON, 
			paste("select name from sqlite_master where type = 'table' and name LIKE '",slot(x, table.type) ,"%'", sep = ""))$name else
		table.nam = paste(slot(x, table.type) , table.nam, sep = "")
		if(length(table.nam) >0) {
				sql = paste("DROP TABLE if exists", table.nam)
					for (i in 1:length(sql)) .sqlQuery(object@CON , sql[i]) } else
		Msg(paste("This project does not have any",table.type, "tables"))
		
		}
	)

	
	

	
# user defined	
rm.rangeMapper <- function(con, table.nam, table.type) {
 x =  new("rangeMap", CON = con, table.type =table.type,table.nam =table.nam)
 rangeMapRemove(x)
 }




	



