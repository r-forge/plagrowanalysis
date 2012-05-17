RSA <-
function(R1, R2, LW1, LW2, S1, S2){ 
  #Method 2
  mS1 <- mean(log(LW1 + S1))
  mS2 <- mean(log(LW2 + S2))
  mR1 <- mean(log(R1))
  mR2 <- mean(log(R2))
  V_S1 <- var(log(LW1 + S1))
  V_S2 <- var(log(LW2 + S2))
  V_R1 <- var(log(R1))
  V_R2 <- var(log(R2))

  slope <- (mR2 - mR1)/(mS2 - mS1)
  lambda <- (V_R1 / V_S1 + V_R2 / V_S2)/2
  top <- (lambda*(V_S1 + V_S2))/2
  grand_X_bar <- sum(c(mS1,mS2))/2
  bottom <- (mS1 - grand_X_bar)^2 + (mS2 - grand_X_bar)^2
  var_slope <- top/bottom

  #Metodo 3
  sigma_x2 <- sum(log(c(LW1 + S1, LW2 + S2))^2)
  sigma_x <- sum(log(c(LW1 + S1, LW2 + S2)))
  sigma_y2 <- sum(log(c(R1, R2))^2)
  sigma_y <- sum(log(c(R1, R2)))
  sigma_xy <- sum(log(c(LW1 + S1, LW2 +S2)) * log(c(R1, R2)))
  N <- length(c(R1,R2))

  Sxx <- sigma_x2 - sigma_x^2 / N
  Syy <- sigma_y2 - sigma_y^2 / N
  Sxy <- sigma_xy - sigma_y * sigma_x / N

  slope_2 <- ((Syy - lambda * Sxx) + sqrt((Syy - lambda * Sxx)^2 + 
    4 * lambda * Sxy^2)) / (2 * Sxy)
    
  Ybar <- sigma_y/N
  Xbar <- sigma_x/N
  alpha <- Ybar - slope_2 * Xbar
  Xigh1 <- (lambda * mS1 + slope_2 * mR1 - slope_2 * alpha) / 
    (lambda + slope_2^2)
  Xigh2 <- (lambda * mS2 + slope_2 * mR2 - slope_2 * alpha) / 
    (lambda + slope_2^2)
  Xigh_bar <- sum(c(Xigh1, Xigh2)) / 2
  var_slope2 <- (lambda * (V_S1 + V_S2) /2) / 
    sum(c(Xigh1 - Xigh_bar, Xigh2 - Xigh_bar)^2)
  se_slope2 <- sqrt(var_slope2)
  slope2_cl <- se_slope2 * qt(0.975, df = N - 4)

  ans <- c(slope_2, se_slope2, slope2_cl)
  names(ans) <- c("Coeff", "Std Err", "95% CL")
  ans
}

RSA_analysis <- function(time, root, leaf, non.leaf, data, units = NULL, first_date = FALSE) {

  if (is.null(data[,time]) | (class(data[,time]) != "Date" & !is.numeric(data[,time]))){
    stop("Times not supplied")
  }
   #Creo un data.frame para que se m?s comodo la asignaci?n de cosas  
  gdata <- data.frame(time = data[,time], root = data[,root],
    leaf = data[,leaf], non.leaf = data[, non.leaf])
  gdata <- gdata[order(gdata$time),]
  #Cuantos tiempos hay
  times <- unique(gdata$time)
  
  #Si esta en formato Date lo cambio a numerico para después poder sacar las
  #diferencias de tiempo
  if(class(gdata$time) == "Date"){
    labs <- c(0, cumsum(as.numeric(diff(unique(gdata$time)))))
    gdata$time <- as.numeric(as.character(factor(gdata$time, labels = labs)))
  }
  
  #Divido el dataframe en tiempos para no volver a tener que subsetear en cada paso
  ldata <- split(gdata, gdata$time)
  
  #Listas para guardar resultados
  periods <- make_periods(times, first_date = first_date)
  
  ans <- vector("list", length(times) - 1)
  names(ans) <- periods
  
  #Loop desde tiempo uno a tmax-1
  #donde subseteo los datos segun el tiempo y aplico las funciones
  #Elegir con la primer fecha versus todas
  #o el comporatmiento normal
  if(first_date){
    for(i in seq_len(length(ldata)-1)) {
      ans[[i]] <- RSA(ldata[[1]]$root, ldata[[i+1]]$root, 
                      ldata[[1]]$leaf, ldata[[i+1]]$leaf, 
                      ldata[[1]]$non.leaf, ldata[[i+1]]$non.leaf)
      }
  } else {
    for(i in seq_len(length(ldata)-1)) {
      ans[[i]] <- RSA(ldata[[i]]$root, ldata[[i+1]]$root, 
                      ldata[[i]]$leaf, ldata[[i+1]]$leaf,
                      ldata[[i]]$non.leaf, ldata[[i+1]]$non.leaf)
      }
  }

  #Uno las listas en vectores, m?s comodo si hay que transformar y da una salida m?s compacta
  ans <- do.call(rbind, ans)
  result <- list(ans = ans)
  result$units <- paste("mass/mass (dimesionless)")
  result$lname <- "Shoot - Root Allometry"
  class(result) <- c("gral", "list")
  result
}

