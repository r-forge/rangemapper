
canvas.make   <- function(con) {
	
	if(is.na(.sqlQuery(con, "SELECT gridSize from metadata")$gridSize)) 
		stop(gui.msg("The grid size is missing!"))
	
		bbox     = global.bbox.fetch(con)
		cellsize = gridSize.fetch(con)
		
		return(spsample(bbox, cellsize = cellsize, type = "regular", offset = c(0.5,0.5)))
}

canvas.save   <- function(con) {

	if(!.is.empty(con, "canvas")) stop (gui.msg("Canvas was allready uploaded!") )

	cnv = canvas.make(con)
	cnv = data.frame(coordinates(cnv), id = 1:nrow(coordinates(cnv)))

	names(cnv) = c("x", "y", "id")

	res = dbWriteTable(con, "canvas", cnv, append = TRUE, row.names = FALSE) 

	if(res) gui.msg("Canvas uploaded.")


}


setMethod("canvasFetch",  
	signature  = "rangeMap", 
		definition = function(object) {
		
		# fetch map
		map = .sqlQuery(object@CON, 'SELECT * FROM canvas' )
	
		coordinates(map) = ~ x + y

		p4s = .sqlQuery(object@CON, paste("SELECT p4s FROM", object@METADATA) )[,1]
		
		proj4string(map) = CRS(p4s)
		
		gridded(map) = TRUE
		
		map
		
		}
	)	

canvas.fetch <- function(dbcon) { 
	x = new("rangeMap", CON = dbcon)
	canvasFetch(x)	

} 




