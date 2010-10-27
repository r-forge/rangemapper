

setMethod("rangeMapStart",  
		signature = "rangeMapStart", 
		definition = function(object){
			f = paste(object@dir, object@file, sep = .Platform$file.sep)
			file.exists = if(file.exists(f) )  TRUE else FALSE
			CON = dbConnect(dbDriver("SQLite"), dbname= f)
			if(!file.exists) { 				
				Queries = object@scheleton
				db = unlist(Queries)
					for (i in 1:length(db)) .sqlQuery(CON , db[i])
			}
			
			if(object@overwrite && file.exists) {
				dropAll = .sqlQuery(CON, "select 'drop table if exists ' || name from sqlite_master where type = 'table';")
				if(nrow(dropAll) == 0) dropAll = NULL else dropAll = dropAll[,1 ]
				Queries = c(dropAll, "vacuum", object@scheleton )
				db = unlist(Queries)
					for (i in 1:length(db)) .sqlQuery(CON , db[i])
				}
		
			if(!object@overwrite && file.exists) stop(Msg(paste("File", object@file, "allready exsits!")))
		
		   
			} 
	)

# user level functions 
rangeMap.start <- function(overwrite = FALSE,...) {

	obj = new("rangeMapStart", ... )

	obj@overwrite = overwrite
	
	rangeMapStart(obj)
	
	Msg(paste("PROJECT:", obj@file, "\nDIRECTORY:",obj@dir), keep = TRUE, getTime = TRUE)
	
	f = paste(obj@dir, obj@file, sep = .Platform$file.sep)
	
	invisible(dbConnect(dbDriver("SQLite"), dbname= f))
	
	
}
	

rangeMap.open <- function(path, verbose = FALSE) {
# ... to pass to summary
	dbcon = dbConnect(dbDriver("SQLite"), dbname= path)
	o = new("rangeMap", CON = dbcon)
	if(verbose) summary(o)
	invisible(dbcon)
}
	

