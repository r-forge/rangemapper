

if (!isGeneric("summary")) { 
	setGeneric("summary", function(object, ...) standardGeneric("summary")) 
	}

summary.rangeMap <- function(object, ...) {
    out = list()
	out[["class"]] = class(object)

	dbinfo = dbGetInfo(object@CON)
	out[["Project_location"]] = dbinfo$dbname
	out[["SQLite_version"]] = dbinfo$serverVersion

	if( nrow(RMQuery(object@CON, paste("select", object@ID, "from", object@CANVAS, "limit 1") )) == 0)
	out[["empty_project"]] = "Empty rangeMapper project." else {
	
		out[["Proj4"]]    = dbReadTable(object@CON, object@PROJ4STRING)[1,1]
		out[["CellSize"]] = dbReadTable(object@CON, object@GRIDSIZE)[1,1]
		out[["Extent"]]   = dbReadTable(object@CON, object@BBOX)
		
		tbs = RMQuery(object@CON, "select name from sqlite_master where type = 'table' ")$name
		
		out[["BIO_tables"]] = paste(  gsub(object@BIO, "", tbs[grep(object@BIO, tbs)]), collapse = ";" )
		out[["MAP_tables"]] = paste(  gsub(object@MAP, "", tbs[grep(object@MAP, tbs)]), collapse = ";" )
	 
		mtd =.is.empty(object@CON, object@METADATA_RANGES)
		out[[object@METADATA_RANGES]]= paste(object@METADATA_RANGES, "is empty:", mtd, collapse = ";" )

	}
	
	class(out) = "summary.rangeMap"
	out
}

print.summary.rangeMap <- function(x, ...) {


	x.Msg(paste(paste(names(x), ":", x), collapse = ";"), ...)
	

}


rangeMap.diagram <- function(con, ..) {

p = new("rangeMap", CON = dbcon)

# Functions boxGrob() and boxCurve () are taken from the gridDiagram package
# http://www.stat.auckland.ac.nz/~paul/

# Package:      gridDiagram
# Version:      0.2-1
# Depends:      R (>= 2.9.0), grid
# Title:        Drawing Diagrams with Grid
# Author:       Paul Murrell
# Maintainer:   Paul Murrell <p.murrell@auckland.ac.nz>
# Packaged: 2010-04-26 00:54:58 UTC; paul
require(grid)
boxGrob <- function(tname, # table name
                    cnames, # column names
                    x=0.5, y=0.5,
                    default.units="npc",
                    just="centre",
                    gap=1.5*stringWidth(" "), 
                    primary=NULL, # which columns are part of the primary key
                    foreign=NULL, # which columns are foreign keys
                    gp=NULL, vp=NULL, name=tname) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    grob(x=x, y=y, tname=tname, cnames=cnames,
         just=just, gap=gap,
         primary=primary, foreign=foreign,
         gp=gp, vp=vp, name=name, cl="box")
}

boxCurve <- function(box1, box2,
                     from, to,
                     direction=c("right", "up"),
                     arrows=arrow(angle=15,
                                 length=unit(2, "mm"),
                                 type="closed"),
                     dotSize=unit(1, "mm"),
                     dotFill="white",
                     arrowFill="black") {
    if (!is.grob(box1))
        box1 <- grid.get(box1)
    if (!is.grob(box2))
        box2 <- grid.get(box2)
    fromLine <- length(box1$cnames) - match(from, box1$cnames)
    toLine <- length(box2$cnames) - match(to, box2$cnames)
    if (is.na(fromLine) || is.na(toLine))
        stop("'from' and 'to' must name existing columns")
    if (direction[1] == "right") {
        theta1 <- 0
        theta2 <- 180
        hadjust <- unit(-2, "mm")
        if (direction[2] == "up") {
            curvature <- 1
        } else {
            curvature <- -1
        }
    } else {
        theta1 <- 180
        theta2 <- 0
        hadjust <- unit(2, "mm")
        if (direction[2] == "up") {
            curvature <- -1
        } else {
            curvature <- 1
        }
    }
    grid.curve(grobX(box1, theta1) + hadjust,
               grobY(box1, 270) + unit(fromLine + 0.5, "lines"),
               grobX(box2, theta2),
               grobY(box2, 270) + unit(toLine + 0.5, "lines"),
               curvature=curvature,
               inflect=TRUE,
               arrow=arrows, gp=gpar(fill=arrowFill))
    grid.circle(grobX(box1, theta1) + hadjust,
                grobY(box1, 270) + unit(fromLine + 0.5, "lines"),
                r=0.5*dotSize, 
                gp=gpar(fill=dotFill))
}



{ # swimmers example
	
grid.newpage()
grid.box("result_table",
         paste("  ",
               c("swimmer", "distance", "stroke", "gender", "stage",
                 "time", "place"),
               sep=""),
         x=.5,
         y=.5,
         primary=paste("  ",
           c("swimmer", "distance", "stroke", "gender", "stage"),
           sep=""),
         foreign=paste("  ",
           c("swimmer", "distance", "stroke", "gender", "stage"),
           sep=""))

grid.box("swimmer_table",
         c("ID", "first", "last"),
         x=.8,
         y=.8,
         primary=c("ID"))

grid.box("distance_table",
         c("length"),
         x=.2,
         y=.7,
         primary="length")

grid.box("stroke_table",
         c("ID", "stroke"),
         x=.8,
         y=.5,
         primary="ID")

grid.box("gender_table",
         c("ID", "gender"),
         x=.2,
         y=.3,
         primary="ID")

grid.box("stage_table",
         c("stage"),
         x=.8,
         y=.25,
         primary="stage")

boxCurve("result_table", "swimmer_table",
         "  swimmer", "ID")
boxCurve("result_table", "stroke_table",
         "  stroke", "ID",
         direction=c("right", "down"))
boxCurve("result_table", "stage_table",
         "  stage", "stage",
         direction=c("right", "down"))
boxCurve("result_table", "distance_table",
         "  distance", "length",
         direction=c("left", "up"))
boxCurve("result_table", "gender_table",
         "  gender", "ID",
         direction=c("left", "down"))

	
	
	
	}







}