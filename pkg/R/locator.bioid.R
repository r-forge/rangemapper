### 2010-Aug-10 09:44:22 ###
### UNDER CONSTRUCTION ###

locator.bioid <- function(con,  rangeMap) {

	rangeMap = SpatialPointsDataFrame(rangeMap, rangeMap@data)
	rangeMapVar = names(rangeMap@data)[2]
	
	
	trellis.focus("panel", 1, 1)
	xy = spplot.locator(unit = "native", type = "l")

	selectedPoly = SpatialPolygons(Srl = list(Polygons(list(Polygon(rbind(xy, xy [1,]))), 1)))	

	selectedPts = rangeMap[which(overlay(selectedPoly, rangeMap) ==1),]

	# table name given Map name: rangeMapVar
	tabs = .sqlQuery(con, "select name from sqlite_master where type = 'table' and name like 'BIO_%';")$name
	fields = lapply(split(tabs, tabs), function(x) .sqlQuery(con, paste("PRAGMA table_info(", x, ");"))$name)
	
	BIO_ = unlist(lapply(fields, function(x) rangeMapVar%in%x))
	BIO_ = names(BIO_[which(BIO_ == TRUE)])
	
	# bioid entries
	bioid = .sqlQuery(con, paste("SELECT", .extract.indexed(con, BIO_) ," as bioid FROM", BIO_) )
	
	
	sqlString = paste("SELECT DISTINCT bioid from ranges where id in (",paste(selectedPts@data$id, collapse = ","), ") and ",
														   "bioid in (", paste("'",paste(bioid$bioid, collapse = "','"), "')", sep = "") )
	
	.sqlQuery(con, sqlString)
	

	
}

gui.locator.bioid <- function() {

	tclRequire("Tktable")

	dbcon = gui.get.from.env("con")
	if(is.null(dbcon)) stop(gui.msg("There is no active project!"))

	map = gui.get.from.env("rangeMap")
	if(is.null(map)) stop(gui.msg("There is no active map!"))
	
		
	bioid = locator.bioid(dbcon, map)
	bioid = rbind(bioid = "bioID", bioid)


	tclarray = tclArray()
	for (i in (0:nrow(bioid))) tclarray[[i,0]] <- bioid[i+1,]

	top = tktoplevel()
	title =names(rangeMap@data)[2]
	tkwm.title(top, paste(title, "map selection") )
		
	bioidTab  =  tkwidget(top,"table",rows= nrow(bioid) ,cols=1,titlerows=1,titlecols=0, colwidth= max(nchar(bioid$bioid)) + 2,
					 xscrollcommand=function(...) tkset(xscr,...),
					 yscrollcommand=function(...) tkset(yscr,...))

	xscr  = tkscrollbar(top,orient="horizontal", command=function(...)tkxview(bioidTab,...))
	yscr  =  tkscrollbar(top,command=function(...)tkyview(bioidTab,...))


	tkgrid(bioidTab,yscr)
	tkgrid.configure(yscr,sticky="nsw")
	tkgrid(xscr,sticky="new")

	tkconfigure(bioidTab,variable=tclarray,background="white",selectmode="extended")
	tkconfigure(bioidTab,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")

  

}


