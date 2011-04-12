LSA <-
function(L1, L2, LW1, LW2){
  #Precomputo
  lnQ1 <- mean(log(L1)) - mean(log(LW1))
  lnQ2 <- mean(log(L2)) - mean(log(LW2))
  V_lnQ1 <- var(log(L1)) + var(log(LW1)) - 2 * covar(log(L1),log(LW1))
  V_lnQ2 <- var(log(L2)) + var(log(LW2)) - 2 * covar(log(L2),log(LW2))
  
  Q1 <- exp(lnQ1 + V_lnQ1/2)
  Q2 <- exp(lnQ2 + V_lnQ2/2)
  V_Q1 <- exp(2*lnQ1 + V_lnQ1) * (exp(V_lnQ1) - 1)
  V_Q2 <- exp(2*lnQ2 + V_lnQ2) * (exp(V_lnQ2) - 1)
  Q_bar <- (Q1 + Q2)/2
  Variance <- (V_Q1 + V_Q2)/2
  Q_se <- sqrt(Variance)
  Q_cl <- Q_se * qt(0.975, df = length(c(L1, L2)) - 2)
    
  ans <- c(Q_bar, Q_se, Q_cl)
  names(ans) <- c("Q bar", "Std err", "95% CL")
  ans
}

LSA_analysis <- function(time, leaf, leaf.area, data = data, units = NULL) {
  if (is.null(data$time))  stop("Times not supplied")
   #Creo un data.frame para que se m?s comodo la asignaci?n de cosas  
  gdata <- data.frame(time = data$time, leaf = data$leaf, leaf.area = data$leaf.area)

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
    ans[[i]] <- LSA(d1$leaf.area, d2$leaf.area, d1$leaf, d2$leaf)
  }

  #Uno las listas en vectores, m?s comodo si hay que transformar y da una salida m?s compacta
  ans <- do.call(rbind, ans)
  result <- list(ans = ans)
  #Si units no es nulo convierto los datos a las unidades pedidas
  if(!is.null(units)) {
    weight.mult <- weight_conv(units$weight[1], units$weight[2])
    area.mult <- area_conv(units$area[1], units$area[2])
    result$ans <- result$ans*weight.mult/area.mult
    result$units <- paste( units$area[2], "/", units$weight[2], sep = "")
  } else attr(ans, "units") <- "mass/area"
  result$lname <- "Mean Specific Leaf Area"
  class(result) <- c("gral", "list")
  result
}
