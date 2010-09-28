
# TODO: add suitable methods and classes
# ...arguments to pass to classIntervals (eg. n, style) and spplot

rangeMap.plot  <- function(map, colorpalette= brewer.pal.get('Spectral'), scales = FALSE, ...) {

	trellis.par.set("regions", list(col= colorRampPalette(colorpalette, space = "Lab")(ncols) ) , warn = FALSE)
	
	mapVars = names(map)[!names(map)=="id"]
	
	 nr <- nc <- ceiling(sqrt(length(mapVars )))

	layout = cbind(x = rep(1:nr[1], each = nc), y = rep(1:nr, nc), nr, nc)	
	
	if(length(mapVars ) == 2)   layout[, 'nr'] = 1
	if(length(mapVars ) == 3)  layout = cbind(rep(1, 3), 1:3, 1, 3)

	for(i in seq(along = mapVars)) {
	
	Int = classIntervals(as.numeric(na.omit(map@data[,mapVars[i]])), ...)
	printMore = if(i<length(mapVars)) TRUE else FALSE
	
	print(spplot(map, mapVars[i] ,scales = list(draw = scales), cuts = ncols, checkEmptyRC = FALSE, 
		 at = Int$brks, main = if(length(mapVars) > 1) mapVars[i] else "", ...), 
			split=layout[i, ], more=printMore)
		

	}
	
}









