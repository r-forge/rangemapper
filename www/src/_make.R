

layoutUpdate = function(layout.conf) {
 #XX__level1__level2 (CSS should work for more than 2 levels!!)
 # for now works only for menus with 2 levels
 
	l = readLines(layout.conf)

	p1 = l[1:grep('EXAMPLE_start', l)]
	p2 = l[grep('EXAMPLE_stop', l):length(l)]
	
	f = list.files(pattern = "^[0-9]{2}_.*.Rnw$")
	f = gsub(".Rnw$", "", f)
	
	u= sapply(strsplit(f, "__"), function(x) x[1])
	u = split(f, u)
	
	# build up the menu
	menu = vector()
	for(i in 1:length(u)) {
		ui = strsplit(u[[i]], "__")
		nlev = unique(sapply(ui, length))-1
		lev1 = ui[[1]][2]

		href = sapply(ui, function(x) paste(paste(x, collapse = "__"), ".html", sep = "") )
		
		if(nlev == 1) {
			menu[[i]] = paste('<li class="menui0"><a class="menui0" href=', shQuote(href),'>', unlist(ui)[2] ,'</a></li>')
			}
		
		if(nlev == 2) {
			menui = paste('<li class="menui"><a class="menui" href=', shQuote(href),'>', sapply(ui, function(x) x[3]) ,'</a></li>')
			menui = paste( c('<ul class="menum">', menui, '</ul>'), collapse = "\n")
			menu0 = paste('<li class="menui0"> <a class="menui0" href="#"><span>',  ui[[1]][2], '</span></a>')
			menu[[i]] = paste(c(menu0, menui, "</li>"), collapse = "\n")
			}
	}
	menu = paste( c('<ul class="menu menum">', menu, '</ul>'), collapse = "\n")
	

	write.table(p1, file = "layout.conf", append = FALSE,  row.names = FALSE, col.names =  FALSE, quote = FALSE)
	write.table(menu, file = "layout.conf", append = TRUE,  row.names = FALSE, col.names =  FALSE, quote = FALSE)
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

lapply(gsub(".Rnw", "", list.files("M:\\SOFTWARE\\R\\PACKAGES\\rangeMapper\\R-forge\\www\\src", pattern = "^[0-9]{2}_.*.Rnw$") ), do)


do("index")




















