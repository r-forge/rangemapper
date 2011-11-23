
{ # tools; settings
setwd("M:\\SOFTWARE\\R\\PACKAGES\\rangeMapper\\R-forge\\www")

bldfun = function(nam, sweave = TRUE) {
	require(ascii)
	opt = options()
	
	x = paste(nam, "Rnw", sep = ".")
	z = paste(nam, "txt", sep = ".")

	if(sweave) 
		Sweave(x, driver = RweaveAsciidoc, syntax = "SweaveSyntaxNoweb")

	shell(paste("asciidoc.py --backend=xhtml11 -a icons -a iconsdir=media -a linkcss -a stylesdir=media -a stylesheet=style.css -a disable-javascript -a badges -a icons  -a max-width=1024px -a linkcss --conf-file=layout.conf", z) )
	
	options(opt)
}


}

# ASCIIDOC
bldfun("index", FALSE)
bldfun("faq", FALSE)
bldfun("gui", FALSE)
bldfun("gallery", FALSE)
bldfun("bibliography", FALSE)

#SWEAVE
bldfun("01_canned")
bldfun("02_sql_and_R")


