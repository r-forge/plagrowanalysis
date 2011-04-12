covar <-
function(x, y, ...) {
	cov(x, y, ...) * ((length(x)-1)/length(x))
}

