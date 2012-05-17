LWR <-
function(W1, W2, LW1, LW2){
  #Precomputo
  lnP1 <- mean(log(LW1)) - mean(log(W1))
  lnP2 <- mean(log(LW2)) - mean(log(W2))
  V_lnP1 <- var(log(W1)) + var(log(LW1)) - 2 * covar(log(W1),log(LW1))
  V_lnP2 <- var(log(W2)) + var(log(LW2)) - 2 * covar(log(W2),log(LW2))
    
  P1 <- exp(lnP1 + V_lnP1/2)
  P2 <- exp(lnP2 + V_lnP2/2)
  V_P1 <- exp(2*lnP1 + V_lnP1) * (exp(V_lnP1) - 1)
  V_P2 <- exp(2*lnP2 + V_lnP2) * (exp(V_lnP2) - 1)
  P_bar <- (P1 + P2)/2
  Variance <- (V_P1 + V_P2)/2
  P_se <- sqrt(Variance)
  P_cl <- P_se * qt(0.975, df = length(c(W1, W2)) -2)
    
  ans <- c(P_bar, P_se, P_cl)
  names(ans) <- c("P bar", "Std err", "95% CL")
  ans
}

LWR_analysis <- function(time, total, leaf, data, units = NULL, first_date = FALSE) {
  if (is.null(data[,time]) | (class(data[,time]) != "Date" & !is.numeric(data[,time]))){
    stop("Times not supplied")
  }
   #Creo un data.frame para que se m?s comodo la asignaci?n de cosas  
  gdata <- data.frame(time = data[, time], total = data[, total],
    leaf = data[, leaf])
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
       ans[[i]] <- LWR(ldata[[1]]$total, ldata[[i+1]]$total, ldata[[1]]$leaf, ldata[[i+1]]$leaf)
     }
   } else {
     for(i in seq_len(length(ldata)-1)) {
       ans[[i]] <- LWR(ldata[[i]]$total, ldata[[i+1]]$total, ldata[[i]]$leaf, ldata[[i+1]]$leaf)    }
   }


  #Uno las listas en vectores, m?s comodo si hay que transformar y da una salida m?s compacta
  ans <- do.call(rbind, ans)
  result <- list(ans = ans)
  result$units <- "mass/mass (dimesionless)"
  result$lname <- "Mean Leaf Weight Fraction"
  class(result) <- c("gral", "list")
  result
}
