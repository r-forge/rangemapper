

setMethod("summary",  
		signature = "rangeMap", 
		definition = function(object, ...){
			
		out = list()	
		
		 dbinfo = dbGetInfo(object@CON)
		 out = c(out, paste("Project location:", dbinfo$dbname) )
		 out = c(out, paste("SQLite version:", dbinfo$serverVersion) )
		if( nrow(.sqlQuery(object@CON, paste("select", object@ID, "from", object@CANVAS, "limit 1") )) == 0)
			out = c(out,"Empty rangeMapper project.") 
			else {
				mtd = .sqlQuery(object@CON, paste("select * from", object@METADATA))
				
				out = c(out, paste("Proj4 string:", mtd$p4s) )
				out = c(out, paste("Cell size:", paste(mtd$gridSize) ) )
				
				tbs = .sqlQuery(object@CON, "select name from sqlite_master where type = 'table' ")$name
				
				out = c(out, paste(object@BIO, "tables:\n",  paste(  gsub(object@BIO, "", tbs[grep(object@BIO, tbs)]), collapse = "\n" )  ))
				out = c(out, paste(object@MAP, "tables:\n",  paste(  gsub(object@MAP, "", tbs[grep(object@MAP, tbs)]), collapse = "\n" )  ))
				}
		invisible(out)
		invisible(lapply(out, Msg, ...))
	}
	)
	





	

	
	
	
	
	
	
	
	
	
	
	
