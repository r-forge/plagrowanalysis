area_conv <-
function(from, to) {
  #constantes
 units <- c(1e-6, 1e-4,1e-2,1, 1e2, 1e4, 1e6)
 names(units) <- c("mm2", "cm2", "dm2", "m2", "a", "ha", "km2") 
  u1 <- units[pmatch(from, names(units))]
  u2 <- units[pmatch(to, names(units))]
  if(is.na(u1)) stop(paste("Unit called \"", from, "\" doesn't match, please check your units", sep = ""))
  if(is.na(u2)) stop(paste("Unit called \"", to, "\" doesn't match, please check your units", sep = ""))
  mult <- u1/u2
  names(mult) <- paste(names(u1), names(u2), sep=" -> ")
  return(mult)
}

