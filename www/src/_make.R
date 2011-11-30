


layoutUpdate = function(layout.conf) {

	l = readLines(layout.conf)

	p1 = l[1:grep('EXAMPLE_start', l)]
	p2 = l[grep('EXAMPLE_stop', l):length(l)]
	
	f = list.files(pattern = "^[0-9]{2}_.*.Rnw$")
	x = data.frame(fn = gsub(".Rnw", ".html", f ), nam = gsub("^[0-9]{2}_|.Rnw$", "", f) )

	markup = paste("<div><a href=", shQuote(x$fn), ">", x$nam, "</a></div>", sep  = "")

	write.table(p1, file = "layout.conf", append = FALSE,  row.names = FALSE, col.names =  FALSE, quote = FALSE)
	write.table(markup, file = "layout.conf", append = TRUE,  row.names = FALSE, col.names =  FALSE, quote = FALSE)
	write.table(p2, file = "layout.conf", append = TRUE,  row.names = FALSE, col.names =  FALSE, quote = FALSE)

}

do = function(nam, layout = "layout.conf", 
				src = "M:\\SOFTWARE\\R\\PACKAGES\\rangeMapper\\R-forge\\www\\src", 
				target = dirname(src) ) {
	
	setwd(src)
	require(ascii)
	
	layoutUpdate(layout)
	
	x = paste(nam, "Rnw", sep = ".")
	z = paste(nam, "txt", sep = ".")
	h = paste(nam, "html", sep = ".")
	
	Sweave(x, driver = RweaveAsciidoc, syntax = "SweaveSyntaxNoweb")
	
	run = paste("asciidoc.py --backend=xhtml11 --doctype=article -a icons -a iconsdir=style -a linkcss -a stylesdir=style -a stylesheet=style.css -a disable-javascript -a badges -a icons  -a max-width=1024px -a linkcss --conf-file=",layout, " ", z, sep = "")
	print(run)
	shell(run)

	file.copy(from = paste(src, h, sep = .Platform$file.sep), to = paste(target, h, sep = .Platform$file.sep), overwrite = TRUE)
	file.remove(h)
	file.remove(z)
	
	figs = list.files(paste(src, "figs", sep = .Platform$file.sep), full.names = TRUE)
	file.copy(figs , paste(dirname(src), "figs", sep = .Platform$file.sep) )
	file.remove( figs)
	
}



lapply( c("index", "faq", "gui","gallery", "bibliography"), do)

ex = gsub(".Rnw", "", list.files("M:\\SOFTWARE\\R\\PACKAGES\\rangeMapper\\R-forge\\www\\src", pattern = "^[0-9]{2}_.*.Rnw$") )

lapply( ex , do)

do("99_appendix_S2")
do("98_appendix_S3")
do("97_appendix_S4")
do("96_appendix_S5")
