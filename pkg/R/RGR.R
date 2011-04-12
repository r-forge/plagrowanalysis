RGR <-
function(W1, W2, T1, T2) {
  RGR_bar <- (mean(log(W2))-mean(log(W1)))/(T2-T1)
  RGR_var <- (var(log(W2))+var(log(W1)))/((T2-T1)^2)
  RGR_se <- sqrt(RGR_var)
  RGR_cl <- RGR_se * qt(0.975, df = length(c(W1, W2)) -2)
  ans <- c(RGR_bar, RGR_se, RGR_cl)
  names(ans) <- c("RGR", "Std_err", "95% CL")
  ans
}

RGR_analysis <- function(time, total, data = data, units = NULL) {

  if (is.null(data$time))  stop("Times not supplied")
   #Creo un data.frame para que se m?s comodo la asignaci?n de cosas  
  gdata <- data.frame(time = data$time, total = data$total)

  #Cuantos tiempos hay
  times <- unique(gdata$time)
  #Listas para guardar resultados
  periods <- make_periods(times)
  
  ans <- vector("list", length(times) - 1)
  names(ans) <- periods
  #Loop desde tiempo uno a tmax-1
  #donde subseteo los datos segun el tiempo y aplico las funciones
  for(i in seq_along(ans)) {
    d1 <- subset(gdata, gdata$time==times[i])
    d2 <- subset(gdata, gdata$time==times[i+1])
    ans[[i]] <- RGR(d1$total, d2$total, times[i], times[i+1])
  }

  #Uno las listas en vectores, m?s comodo si hay que transformar y da una salida m?s compacta
  ans <- do.call(rbind, ans)
  result <- list(ans = ans)
  #Si units no es nulo convierto los datos a las unidades pedidas
  if(!is.null(units)) {
    time.mult <- time_conv(units$time[1], units$time[2])
    result$ans <- result$ans/time.mult
    result$units <- paste(units$weight[2],"/",units$weight[2], "/",
    units$time[2], sep = "")
  } else  result$units <- "mass/mass/time"
  result$lname <- "Mean Relative Growth Rate "
  class(result) <- c("gral", "list")
  result
}