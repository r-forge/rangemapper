
# TODO: add suitable methods and classes


rangeMap.plot  <- function(map, colorpalette= brewer.pal.get('Spectral') , ncols = 20,
						style = "fisher", scales = FALSE) {
	
	mapVars = names(map)[!names(map)=="id"]
	
	if(length(mapVars) > 1) { 
		warning("Only the 1st variable will be used for plotting")
		mapVars = mapVars[1]
		}	
	
	y = as.numeric(na.omit(map@data[,mapVars]))
	
	Int = classIntervals(y, ncols, style = style)
	
	trellis.par.set("regions", list(col= colorRampPalette(colorpalette, space = "Lab")(ncols) ) , warn = FALSE)
	
	print(spplot(map , mapVars ,scales = list(draw = scales), cuts = ncols, checkEmptyRC = FALSE, at = Int$brks))

}







