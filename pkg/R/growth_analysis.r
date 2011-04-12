#Wrapper functions

growth_analysis <- function(time, root, leaf, non.leaf, leaf.area, total, data = data,
  units = NULL) {
  #Creo un data.frame para que se m?s comodo la asignaci?n de cosas
  if (is.null(data$time))  stop("Times not supplied")
  if (is.null(data$total)) data$total <- data$root + data$non.leaf + data$leaf
  
  rgr <- RGR_analysis(time, total, data = data, units = units)
  ulr <- ULR_analysis(time, total, leaf.area, data = data, units = units)
  lar <- LAR_analysis(time, total, leaf.area, data = data, units = units)
  lwr <- LWR_analysis(time, total, leaf, data = data, units = units)
  lsa <- LSA_analysis(time, leaf, leaf.area, data = data, units = units)
  rsa <- RSA_analysis(time, root, leaf, non.leaf, data = data, units = units)
  
  ans <- list(rgr = rgr, ulr = ulr, lar = lar, lwr = lwr, lwr = lsa, rsa = rsa)

  ans
}


