LAR <-
function(W1, W2, L1, L2){
  #Precomputo
  lnF1 <- mean(log(L1)) - mean(log(W1))
  lnF2 <- mean(log(L2)) - mean(log(W2))
  V_lnF1 <- var(log(W1)) + var(log(L1)) - 2 * covar(log(W1),log(L1))
  V_lnF2 <- var(log(W2)) + var(log(L2)) - 2 * covar(log(W2),log(L2))
    
  F1 <- exp(lnF1 + V_lnF1/2)
  F2 <- exp(lnF2 + V_lnF2/2)
  V_F1 <- exp(2*lnF1 + V_lnF1) * (exp(V_lnF1) - 1)
  V_F2 <- exp(2*lnF2 + V_lnF2) * (exp(V_lnF2) - 1)
  F_bar <- (F1 + F2)/2
  Variance <- (V_F1 + V_F2)/2
  F_se <- sqrt(Variance)
  F_cl <- F_se* qt(0.975, df = length(c(W1, W2)) - 2)
  
  ans <- c(F_bar, F_se, F_cl)
  names(ans) <- c("F bar","Std err", "95% CL")
  ans
}

LAR_analysis <- function(time, total, leaf.area, data, units = NULL, first_date = FALSE) {
    
  if (is.null(data$time))  stop("Times not supplied")
   #Creo un data.frame para que se m?s comodo la asignaci?n de cosas  
  gdata <- data.frame(time = data$time, total = data$total,
    leaf.area = data$leaf.area)

  #Cuantos tiempos hay
  times <- unique(gdata$time)
  
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
      ans[[i]] <- LAR(ldata[[1]]$total, ldata[[i+1]]$total, 
                      ldata[[1]]$leaf.area, ldata[[i+1]]$leaf.area)
      }
  } else {
    for(i in seq_len(length(ldata)-1)) {
      ans[[i]] <- LAR(ldata[[i]]$total, ldata[[i+1]]$total, 
                      ldata[[i]]$leaf.area, ldata[[i+1]]$leaf.area)
      }
  }
  #Uno las listas en vectores, m?s comodo si hay que transformar y da una salida m?s compacta
  ans <- do.call(rbind, ans)
  result <- list(ans = ans)
  #Si units no es nulo convierto los datos a las unidades pedidas
  if(!is.null(units)) {
    weight.mult <- weight_conv(units$weight[1], units$weight[2])
    area.mult <- area_conv(units$area[1], units$area[2])
    result$ans <- result$ans*area.mult/weight.mult
    result$units <- paste(units$area[2], "/", units$weight[2],sep = "")
  } else result$units <- "area/mass"
  result$lname <- "Leaf Area Ratio"
  class(result) <- c("gral", "list")
  result
}

