
.onLoad <- function(lib, pkg) {
	wd = 75
	markline = paste("\n",paste(rep("-", wd),collapse = ""),"\n", collapse = "")
	
	 dsc <- packageDescription(pkg)
	
	chunk1 = paste("This is", pkg, dsc$Version,
	   "\n   Type", sQuote("rangeMapper()"), "to start the graphical user interface.",
	   "\n   Type", sQuote("?rangeMapper"), "to access the help files.")
	
	Citation = 	paste(strwrap(attributes(unclass(citation(package = pkg))[[1]])$textVersion, wd), collapse = "\n")
	
	chunk2 = paste("The latest version of", dQuote("AppendixS2_S5.R"), "accompanying\n", dQuote(Citation), 
		"can be found at\n", strsplit(dsc$URL, ",")[[1]][[2]] )

	chunk3 = paste("Changes are documented in\n", system.file(package = pkg, "NEWS") )
		
	packageStartupMessage(markline,chunk1 ,markline,  chunk2, markline, chunk3, markline)
	
	
	
}