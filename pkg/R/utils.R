
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


