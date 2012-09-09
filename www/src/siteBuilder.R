root = "m:/SOFTWARE/R/PACKAGES/rangeMapper"
root = "c:/Users/valcu/rangeMapper"

source( paste0(root, "/www_new/SRC/siteBricks.R") )
  
  
 

# 1) buildup site structure
X = siteStr(src = paste0(root, "/www_new/SRC"), html = paste0(root, "/www_new"))
naviBar = navBar(X)

# knit Rmd files
Rmd = X[X$type == 'Rmd', ]
for(J in 1:nrow(Rmd) ) {
	doLego(Head,  
		  leftBar = naviBar, 
		  Title, 
		  scriptPrefix = NULL, 
		  Content = markdownToHTML( knit(Rmd$file[J]) , fragment.only = TRUE), 
		  scriptSuffix, 
		  footer, 
		  file = Rmd$html[J])
		  
	}



# spin R files
Rsrc = X[X$type == 'R', ]; rownames(Rsrc) = NULL

for(J in  14) {
	x = spin(Rsrc$file[J]) # make Rmd
	content = markdownToHTML( basename(gsub('.Rmd', '.md', x) ), fragment.only = TRUE)
	### R  object 
	doLego(Head,
		  leftBar = naviBar, 
		  Title, 
		  scriptPrefix, 
		  Content = content, 
		  scriptSuffix, 
		  footer, 
		  file = Rsrc$html[J])

	file.remove(x) 
	}

























