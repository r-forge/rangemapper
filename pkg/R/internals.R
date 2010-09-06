
# DB
.sqlQuery <- function (con, statement) {
	
	dat = sqliteQuickSQL(con, statement)
	dat

}

.extract.indexed <-function(con,table.name) {
	# extract name of indexed colum
	indx = .sqlQuery(con, 
		paste("select * from sqlite_master where type = 'index' and tbl_name = '", 
				table.name, "'", sep = ""))$name
				
	.sqlQuery(con, paste("PRAGMA index_info(",indx, ")" ))$name
}

.dbtable.exists <- function(con, table.name) {
	# returns TRUE if the table exists on channel 
	x = .sqlQuery(con,paste('select name from sqlite_master where type = "table" and tbl_name like', shQuote(table.name) ) )
	if(nrow(x)>0) TRUE else FALSE
	
	}

.dbfield.exists <-function(con, table.name, col.name) {
	# returns TRUE if the column is part of table
	stopifnot(.dbtable.exists(con, table.name))
	
	ans = length(intersect(.sqlQuery(con, paste("pragma table_info(", table.name, ")") )$name, col.name)) > 0
	ans
}	
	
.is.empty <- function(con, table.name) {
# returns TRUE if table is  empty FALSE otherwise
# performs a SELECT * from table limit 1;

res = .sqlQuery(con, paste("SELECT * from", table.name, "limit 1") )
if(nrow(res) == 0) TRUE else 
	FALSE

} 

.sqlAggregate <- function(fun){
 # list of sql aggregate functions
 # If fun is given checks for its existence else return the list of sqlite aggregate functions

funs = list(avg      = "avg", 
stdev         = "stdev",
variance      = "variance",
mode          = "mode",
median        = "median",
lower_quartile= "lower_quartile",
upper_quartile= "upper_quartile",
sum           = "total",
max           = "max",
min           = "min",
count         = "total")
 
class(funs) = "simple.list"



if(missing(fun) )
 return(funs) else if
	(fun%in%funs) return(TRUE) else
			stop(sQuote(fun), "is not a known sqlite aggregate function!" )
	}


# SP
.extract.p4s <- function(ShpFiles) {
#  extract proj4 string
# EXAMPLE
#  Dir  = choose.dir(paste(system.file(package="rangeMapper"), "extdata", "wrens", "vector", sep = .Platform$file.sep))
#  ShpFiles = selectShpFiles(Dir)

fl = split(ShpFiles, ShpFiles$layer)

unlist(lapply(fl, FUN = function(x) .Call("ogrP4S", x[,1], x[,2], PACKAGE = "rgdal") ))

}

.sp.metadata <- function(spdf) {

	Area = sum(sapply(slot(spdf, "polygons"), function(x) slot(x, "area") ))

	metad = apply(coordinates(spdf), 2, FUN = function(x) data.frame(Median = median(x), Min = min(x), Max = max(x) ) )

	names(metad[[1]]) = paste(names(metad[[1]]), "x", sep = "_")
	names(metad[[2]]) = paste(names(metad[[2]]), "y", sep = "_")

	cbind(Area, metad[[1]], metad[[2]])
	
	}











 