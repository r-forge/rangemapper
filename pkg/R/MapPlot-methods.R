



rangeMap.plot  <- function(map, colorpalette, ncols, style, scales = FALSE) {
	
	mapVar = names(map)[!names(map)=="id"]
	Int = classIntervals(map@data[, mapVar], ncols, style = style)
	
	trellis.par.set("regions", list(col= colorRampPalette(colorpalette, space = "Lab")(ncols) ) )
	
	print(spplot(map , mapVar ,scales = list(draw = scales), cuts = ncols, checkEmptyRC = FALSE, at = Int$brks))

}




