
if (!isGeneric("plot"))    { setGeneric("plot", function(x,y,...) standardGeneric("plot")) }	
if (!isGeneric("summary")) { setGeneric("summary", function(object, ...) standardGeneric("summary")) }


setGeneric("rangeMapIntersect", function(object, FUN, ...) 			standardGeneric("rangeMapIntersect") )
setGeneric("rangeMapSave", function(object,FUN,formula, ...)  	    standardGeneric("rangeMapSave") )
setGeneric("rangeMapFetch", function(object, ...) 					standardGeneric("rangeMapFetch") )
setGeneric("canvasFetch", function(object, ...)   					standardGeneric("canvasFetch") )
setGeneric("canvasSave", function(object, ...)   					standardGeneric("canvasSave") )
setGeneric("rangeFiles", function(object, ...)   					standardGeneric("rangeFiles") )
setGeneric("rangeMapProcess", function(object, ...)  				standardGeneric("rangeMapProcess") )
setGeneric("rangeMapRemove", function(object, ...)   		     	standardGeneric("rangeMapRemove") )
setGeneric("rangeMapStart", function(object, ...)  			      	standardGeneric("rangeMapStart") )
setGeneric("rangeMapBbox", function(object, ...)   	             	standardGeneric("rangeMapBbox") )
setGeneric("rangeMapBboxSave", function(object,bbox, p4s, ...)		standardGeneric("rangeMapBboxSave") )
setGeneric("rangeMapBboxFetch", function(object, ...)   			standardGeneric("rangeMapBboxFetch") )
setGeneric("gridSizeSave", function(object, ...)   					standardGeneric("gridSizeSave") )
setGeneric("gridSizeFetch", function(object, ...)  					standardGeneric("gridSizeFetch") )
setGeneric("bioSave", function(object, ...)  		    			standardGeneric("bioSave") )
	

