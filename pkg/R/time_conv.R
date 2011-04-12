time_conv <-
function(from, to) {
  units <- c(1/86400, 1/1440, 1/24, 1, 7, 30.4375, 365.25)
  names(units) <- c("second", "minute", "hour", "day", "week", "month", "year")
  u1 <- units[pmatch(from, names(units))]
  u2 <- units[pmatch(to, names(units))]
  if(is.na(u1)) stop(paste("Unit called \"", from, "\" doesn't match, please check your units", sep = ""))
  if(is.na(u2)) stop(paste("Unit called \"", to, "\" doesn't match, please check your units", sep = ""))
  mult <- u1/u2
  names(mult) <- paste(names(u1), names(u2), sep=" -> ")
  return(mult)
}