
# force a function, taking a vector argument, to take a data.frame argument using formula, data.

force.formula <- function(f) {
	funtxt = paste(c("function(formula, data, fun){" ,
				"v = as.character(formula[[2L]])", 
				"x = na.omit(data[, v])", 
				'f<-', 
				paste(deparse(f), collapse = '\n'),
				"f(x) }"), collapse = "\n")
				
	return(eval(parse(text = funtxt)))
}

# Example
# F =  force.formula(function(x) {mean(x)/sd(x)} ) 
# F(x ~ 1, data = data.frame(x = rnorm(100)), function(x) mean(x)/sd(x) )


Msg <- function(msg, tkMainWindow = "win", tkElement = "msg", eol = "\n", keep = FALSE, clearup = FALSE, getTime = FALSE, envir = ".RangeMapper") {

  if(getTime) msg = paste( "<", Sys.time(), ">\n", msg, sep = "")
  
  if(exists(envir)) env = eval(parse(text = envir)) else env = NULL
  
  # if  tcltk envir is set
  if(is.environment(env) && exists(tkMainWindow, envir = env) && exists(tkElement, envir = env) ) {

	  msgWindow = get(tkElement, envir = env)
	  mainWindow = get(tkMainWindow, envir = env)
	  
	# prepare  message container 
	 if(! exists("session.msg", envir = env) ) assign("session.msg", list(), envir = env)
	 
	# clearup any existing messages from env	
		if(clearup) {
			tkdelete(msgWindow, "0.0" , "1000.0" )
			assign("session.msg", list(), envir = env)
			}
	# if msg is to be kept then append it to session.msg 
		if(keep) assign("session.msg", c( get("session.msg", envir = env), msg ) , envir = env )
			
	#  print to GUI element
		tkdelete(msgWindow, "1.0" , "100.0" ) 
		msgList = get("session.msg", envir = env)
		
		lapply(msgList , function(x) tkinsert(msgWindow, "end" , paste(x,eol) ) )
		
		if(!keep) tkinsert(msgWindow, "end" , paste(msg,eol) ) 
		
		tkyview.moveto(get(tkElement, envir = env), 1)
		tkfocus(get(tkMainWindow, envir = env ))
		
		tcl("update", "idletasks") 		 

}	else   cat(msg, eol)
	
    invisible(flush.console() )
	
} 












