

lmer_mean <- function(formula, data, lm.on.fail = FALSE) {

		if(!require(lme4)) stop ("Package lme4 is not available")

		fm = try(lmer(formula, data = data), silent = TRUE)
		
		if( inherits(fm, "try-error") ) res = NA else res = fixef(fm)[1]
		
		if( inherits(fm, "try-error") & lm.on.fail) {
			fm = lm(eval(parse(text = paste(formula[[2L]], "~1") ) ) , data = data)
			res = coef(fm)[1]
			} 
		as.numeric(res)
}	



















