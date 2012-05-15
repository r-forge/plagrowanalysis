#Wrapper functions

growth_analysis <- function(time, root, leaf, non.leaf, leaf.area, total, data = data,
  units = NULL, first_date = FALSE) {
  #Creo un data.frame para que se m?s comodo la asignaci?n de cosas
  if (is.null(data$time))  stop("Times not supplied")
  if (is.null(data$total)) data$total <- data$root + data$non.leaf + data$leaf
  
  rgr <- RGR_analysis(time, total, data = data, units = units, first_date = first_date)
  ulr <- ULR_analysis(time, total, leaf.area, data = data, units = units, first_date = first_date)
  lar <- LAR_analysis(time, total, leaf.area, data = data, units = units, first_date = first_date)
  lwr <- LWR_analysis(time, total, leaf, data = data, units = units, first_date = first_date)
  lsa <- LSA_analysis(time, leaf, leaf.area, data = data, units = units, first_date = first_date)
  rsa <- RSA_analysis(time, root, leaf, non.leaf, data = data, units = units, first_date = first_date)
  
  ans <- list(rgr = rgr, ulr = ulr, lar = lar, lwr = lwr, lwr = lsa, rsa = rsa)

  ans
}


