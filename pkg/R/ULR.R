ULR <-
function (W1, W2, L1, L2, T1, T2) {
#Precomputos
  dT <- T2 - T1
  mW1 <- mean(W1)
  mW2 <- mean(W2)
  lW1 <- mean(log(W1))
  lW2 <- mean(log(W2))
  V_W1 <- var(W1)
  V_W2 <- var(W2)
  mL1 <- mean(L1)
  mL2 <- mean(L2)
  lL1 <- mean(log(L1))
  lL2 <- mean(log(L2))
  V_L1 <- var(L1)
  V_L2 <- var(L2)
#E_bar   
  E <- (mW2 - mW1 ) * (lL2 - lL1) / ((mL2 - mL1) * dT)
  Left1 <- (1/mL1^2) * (mL2 - mL1)^2
  MidLeft1 <- (2/mL1) * (mL2 - mL1)
  MidRight <-  2 * (lL2 - lL1)
  Right <-  mW2 - mW1
  Top <- (Left1 - MidLeft1 + MidRight) * Right
  Bottom <- (mL2 - mL1)^3 * dT
  d2ELd12 <- Top/Bottom
    
  Left2 <- (-1/mL2^2) * (mL2 - mL1)^2
  MidLeft2 <- (2/mL2) * (mL2 - mL1)
  Top2 <- (Left2 - MidLeft2 + MidRight) * Right
  d2EdL22 <- Top2/Bottom
  Line2 <- d2ELd12/2 *  V_L1 + d2EdL22/2 * V_L2
    
  Left3 <- (1/mL1) * (mL2 - mL1)
  Right3 <- lL2 - lL1
  Top3 <- Left3 - Right3
  Bottom3 <-  (mL2 - mL1)^2 * dT
  d2EdW1dL1 <- Top3/Bottom3
  Left4 <- (1/mL2) * (mL2 - mL1)
  Top4 <- Left4 - Right3
  d2EdW2dL2 <- Top4/Bottom3
  Cov1 <- abs(covar(W1, L1))
  Cov2 <- abs(covar(W2, L2))
  Line3 <- d2EdW1dL1 * Cov1 + d2EdW2dL2 * Cov2
    
  Ebar <- E + Line2 + Line3
    
#Varianza de E
  Top5 <- lL2 - lL1
  Bottom5 <- (mL2 - mL1)*dT
  dEdW2 <- Top5/Bottom5
  dEdW1 <- -dEdW2
  VLine1 <- dEdW1^2 * V_W1 + dEdW2^2 * V_W2
  
  Left4 <-  -1/mL1 * (mL2 - mL1) + (lL2 - lL1)
  Right4 <-  mW2 - mW1
  Top6 <- Left4 * Right4
  Bottom6 <- (mL2 - mL1)^2 * dT
  dEdL1 <- Top6/Bottom6
  Left5 <-  1/mL2 * (mL2 - mL1) - (lL2 - lL1)
  Top7 <- Left5 * Right4
  dEdL2 <- Top7/Bottom6
  VLine2 <-  dEdL1^2 * V_L1 + dEdL2^2 * V_L2
  VLine3 <-  2 * dEdW2 * dEdL2 * Cov2
  VLine4 <-  2 * dEdW1 * dEdL1 * Cov1
  E_var <- sum(VLine1, VLine2, VLine3, VLine4)
  E_se <- sqrt(E_var)
  E_cl <- E_se * qt(0.975, df = length(c(W1, W2)) - 2)
    
  ans <- c(Ebar, E_se, E_cl)
  names(ans) <- c("E bar", "Std err", "95% CL")
  ans
}

ULR_analysis <- function(time, total, leaf.area, data, units = NULL) {
   if (is.null(data$time))  stop("Times not supplied")
   #Creo un data.frame para que se m?s comodo la asignaci?n de cosas  
  gdata <- data.frame(time = data$time, total = data$total,
    leaf.area = data$leaf.area)

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
    ans[[i]] <-  ULR(d1$total, d2$total, 
      d1$leaf.area, d2$leaf.area, times[i], times[i+1])
  }

  #Uno las listas en vectores, m?s comodo si hay que transformar y da una salida m?s compacta
  ans <- do.call(rbind, ans)
  result <- list(ans = ans)
  #Si units no es nulo convierto los datos a las unidades pedidas
  if(!is.null(units)) {
    time.mult <- time_conv(units$time[1], units$time[2])
    weight.mult <- weight_conv(units$weight[1], units$weight[2])
    area.mult <- area_conv(units$area[1], units$area[2])
    result$ans <- result$ans*weight.mult/area.mult/time.mult
    result$units <- paste(units$weight[2], "/", units$area[2], "/", 
      units$time[2], sep = "")
  } else result$units <- "mass/area/time"
  result$lname <- "Unit Leaf Leaf Rate "
  class(result) <- c("gral", "list")
  result
}

