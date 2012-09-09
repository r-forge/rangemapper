
# TOOLS & settings
require(knitr)
require(markdown)
setwd(tempdir())

# ELEMENTS
{ Head =
	'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
	<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
	<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
	<meta name="generator" content="AsciiDoc 8.6.6" />
	<title>rangeMapper</title> 
	<meta name="description" content="rangeMapper: A platform for the study of macroecology of life history traits">
	<meta name="author" content="Mihai Valcu">
	<meta name="keywords" content="macroecology, GIS, species richness, life history traits, rangeMapper, Aves,R, spatial analysis" />
	<link rel="stylesheet" href="style/style.css" type="text/css" />
	
	<!-- google-analytics -->
	<script type="text/javascript">

			  var _gaq = _gaq || [];
			  _gaq.push(["_setAccount", "UA-25987540-1"]);
			  _gaq.push(["_trackPageview"]);

			  (function() {
					var ga = document.createElement("script"); ga.type = "text/javascript"; ga.async = true;
					ga.src = ("https:" == document.location.protocol ? "https://ssl" : "http://www") + ".google-analytics.com/ga.js";
					var s = document.getElementsByTagName("script")[0]; s.parentNode.insertBefore(ga, s);
			  })();
	</script>
	
	<script src="style/syntaxHighlighter.js"> </script>
	
	</head>
	<body style="max-width:1024px">	
	'
}

{ Title = 
	'<div id="layout-content-box"> <div id="layout-banner"> <div id="layout-title">rangeMapper</div><div id="layout-description">A platform for the study of macroecology of life history traits</div></div>'	
	}
	
{ Content = # markdownToHTML output
	'<blockquote> content content </blockquote>'	
	}

{ footer = # the </> closings and the validator
	'</div></div> <div id="footer"> <div id="footer-badge"> <a href="http://validator.w3.org/check?uri=referer"> <img style="border:0;width:88px;height:31px" src="http://www.w3.org/Icons/valid-xhtml11-blue" alt="Valid XHTML 1.1" height="31" width="88" /></a></div></div></body></html>' 
	} 


{ scriptPrefix = markdownToHTML(
				text = knit(textConnection(
				"<h6> `r paste('This script runs with ', version$version.string, '&', 'rangeMapper', packageDescription('rangeMapper')$Version)`) </h6>") )
				,fragment.only = TRUE)
}

{ scriptSuffix = markdownToHTML(
				text = knit(textConnection(
				"<footer>  Website built with `r paste(version$version.string,  ',', packageDescription('knitr')$Package,packageDescription('knitr')$Version ,'&',packageDescription('markdown')$Package, packageDescription('markdown')$Version)` </footer> 
				<footer> Last update `r format(Sys.time(), '%m/%d/%Y %H:%M') ` , <valcu@orn.mpg.de> </footer> 
				") )
,fragment.only = TRUE) }


# FUNCTIONS
siteStr = function(src, html) {
	FL = list.files(src,  include.dirs=TRUE, pattern= '^[0-9]{2}_', full.names = TRUE, recursive = TRUE)
	FL = FL[!file.info(FL)$isdir]

	X = data.frame(file = FL,  stringsAsFactors = FALSE)
	X$Head = gsub('/', '', gsub(src, '', dirname(dirname(FL)) ))
	headOrder = as.numeric(substr(X$Head, 1, 2))
	X$Head = reorder( gsub("^[0-9]{2}_", "", X$Head), as.numeric(substr(X$Head, 1, 2))  )

	X$Title = basename(dirname(FL)) 
	X = merge(X, data.frame(xtabs(~Title, X)), sort = FALSE )
	titlOrder = as.numeric( paste0(headOrder, substr(X$Title, 1, 2)))
	
	X$subTitle = gsub('\\.[^.]*$', '', basename(X$file) )
	subtitlOrder = as.numeric(substr(X$subTitle, 1, 2))
	
	X$subTitle <- htmlNam <- gsub('^[0-9]{2}_', '', basename(X$subTitle))
	
	X[X$Freq == 1, 'Title'] = X[X$Freq == 1, 'subTitle']

	X[X$Freq == 1, 'subTitle'] = NA

	X$subTitle = reorder( gsub("^[0-9]{2}_", "", X$subTitle), subtitlOrder )

	X$type = substr(basename(X$file), start = regexpr('[^.]*$', basename(X$file)), stop = nchar(basename(X$file)) )
	
	X$html = paste(html, paste(htmlNam, 'html', sep = '.'), sep = '/')
	
	# index name is home
	X[X$Title == 'index', 'Title'] = 'home'
	X$Title = reorder( gsub("^[0-9]{2}_", "", X$Title),   titlOrder)
	
	
	X
}

navBar = function(X) { # X is a data.frame as returned by siteStr()
# this fuction constructs a a left bar using this template:	
		# <div id="layout-leftBar"> 
			# <ul id="layout-leftBar" class="level1">
				# <h4>""</h4>	
				# <li><a href="/xxxx.html">Home</a></li>
				# <h4>Examples</h4>	
				# <li><a href="/xxxx.html">Title 1</a></li>
				# <li class="submenu"><a href="//xxxx.html">Title 2</a>
					# <ul class="level2">
						# <li><a href="/xxxx.html">Ex 1</a></li>
						# <li><a href="/xxxx.html">Ex 2</a></li>
						# <li><a href="/xxxx.html">Ex 3</a></li>
					# </ul>
				# </li>
			# </ul>
		# </div>
	
	xl = lapply(split(X, X$Head), function(x) {x = split(x, x$Title) 
											x[!sapply(x, function(x) nrow(x) == 0)]
											} )

	
	out = list()

	out[[1]] = '<div id="layout-leftBar"> <ul id="layout-leftBar" class="level1">\n'
	
	for(j in 1:length(xl) ) { # Title level
	xj = xl[[j]]
	
	Title_j = 	paste('<h4>', xj[[1]]$Head[1] , '</h4>\n') 
	out = c(out, list(Title_j) )
		
	for(i in 1:length(xj)) { # Menu level
		xi = xj[[i]]
					
		if(nrow(xi) == 1) # simple menu
			MenuElem = paste('<li><a href=', shQuote(basename(xi$html)), '>',  xi$Title , '</a> </li>\n')
			
		if(nrow(xi) > 1) { # nested menu
			MenuElem =paste(	
					paste('<li class="submenu"><a>', xi$Title[1] ,'</a>', collapse = ''),
						paste('<ul class="level2">', 
							paste('<li><a href=',shQuote(basename(xi$html)), '>', xi$subTitle, '</a></li>', collapse = '\n'),'</ul>', collapse = ''), '</li>\n')
		}	
			
		

		out = c(out, list(MenuElem) )
		
		}
	
	}

out = c(out, list('</ul></div>\n') ) # close ups

out

}

doLego = function(Head, leftBar, Title, scriptPrefix, Content, scriptSuffix, footer, file = 'temp.html') {
				p = list(Head, leftBar, Title, scriptPrefix, Content, scriptSuffix, footer) 
				cat('', file = file)
				lapply(p, 
					function(x) write.table(x, file = file, append = TRUE,  row.names = FALSE, col.names =  FALSE, quote = FALSE) ) 
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	



