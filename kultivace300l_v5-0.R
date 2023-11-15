##### OPTIMALIZACE DOBY FEEDOVANI #####

#funkce pro optimalizaci doby kultivace (feedu) 


mu_poc <- 0.21   # maxim?ln? r?stov? rychlost (na za??tku kultivace)
mu_kon <- 0.035   # minim?ln? r?stov? rychlost (na konci kultivace)
par_k <- 0.23    # prohnut? k?ivky pr?b?hu r?stov? rychlosti

OD <- 5.40       # OD inokula

par_vRPM <- 1/8.565   # prepocet: otacky pumpy [%] = feed [ml/min] * par_vRPM

{
  library(zoo)
  
  opt_fct <- function(delka_kultivace){
    # cas po 10 s
    data <- data.frame(cas = c(0:(delka_kultivace*360)))
    # cas v hod
    data$cas_h <- data$cas / 360
    
    # odhadovane mnozstvi susiny na zacatku feedu
    # vypocte se jako OD 1,8136 * objem inokula + odhadovan? zisk z batch
    DWT_0 <- OD /1.8136 * 22.5 + 200
    
    mu_min <- mu_poc - 2* (mu_poc - mu_kon) # rozdil beru 2*, protoze chci jen pulku sigmoidy
    mu_max <- mu_poc
    par_k <- par_k
    data$mu <- mu_min + (mu_max - mu_min) / (1 + exp(par_k * (data$cas_h - delka_kultivace)))
    
    data$DWT <- DWT_0  # pocatecni mnozstvi biomasy
    
    for(i in c(2: nrow(data))){
      data$DWT[i] <- data$DWT[i-1] * ((1 + data$mu[i]) ^ (1/360))
    }
    
    # mnozstvi feedu od zacatku (vypocteno z DWT)
    data$feed_vol_total <- (data$DWT - data$DWT[1]) / 0.5 / 0.22
    
    
    # vratit rozdil mezi feed_vol_total a cilovym objemem 100 l
    return(abs(100000 - data$feed_vol_total[nrow(data)]))
  }
  
  
  # optimalizace: vysledek je doba feedovani pro nadavkovani 100 l
  opt <- optim(24, opt_fct, 
               control = list(maxit = 100), 
               method = "Brent", lower = 0, upper = 100)
  
  
  ##### SIMULACE KULTIVACE #####
  delka_kultivace <- opt$par
  
  data <- list()
  data$cas_h <- (c(0:(delka_kultivace*3600))) / 3600  #cas kultivace v hod
  data <- data.frame(data)
  DWT_0 <- OD / 1.8136 * 22.5 + 200  #odhadovane mnozstvi susiny na zacatku feedu; OD * 0.5 * objem inokula + odhadovan? zisk z batch
  
  mu_min <- mu_poc - 2* (mu_poc - mu_kon) # rozdil beru 2*, protoze chci jen pulku sigmoidy
  mu_max <- mu_poc
  par_k <- par_k
  data$mu <- mu_min + (mu_max - mu_min) / (1 + exp(par_k * (data$cas_h - delka_kultivace)))
  data$DWT <- DWT_0  # pocatecni mnozstvi biomasy
  for(i in c(2: nrow(data))){ #samotna iterace rustu biomasy
    data$DWT[i] <- data$DWT[i-1] * ((1 + data$mu[i]) ^ (1/3600))
  }
  data$feed_vol_total <- (data$DWT - data$DWT[1]) / 0.5 / 0.22  #mnozstvi feedu od zacatku (vypocteno z DWT)
  data$feed_speed <- c(0, diff(data$feed_vol_total)) * 60 #rychlost feedu [ml/min]
  data$feed_speed[1] <- data$feed_speed[2]
  data$pump_value <- data$feed_speed * par_vRPM  #rychlost pumpy [%]
  
  # graf rustova rychlost
  plot(mu~cas_h, data, type = "l",
       xlab = "cas [h]", ylab = "rustova rychlost [1/h]")
  
  ##### aproximace feedu pro 300l reaktor #####
  data_appr <- approx(x = data$cas_h, y = data$pump_value,
                      n = 10)
  data_appr <- data.frame(data_appr)
  names(data_appr)[1] <- "cas_h"
  
  # vypis delku feedovani:
  cat("doba davkovani feedu: ", opt$par, " hod\n")
  
  # VYSLEDNA TABULKA 10 BODU:
  print(data_appr)
  
  # A co s tim udela system rizeni reaktoru?
  data_appr <- merge(data[1], data_appr, all = T)
  data_appr$y <- na.approx(data_appr$y)
  
  data_appr$feed_spd <- 1/par_vRPM * data_appr$y
  data_appr$feed_vsc <- data_appr$feed_spd / 60
  data_appr$feed_vol_total <- cumsum(data_appr$feed_vsc)
  
  # jak to bude vypadat po zaokrouhleni na cela % (tak to dela rizeni reaktoru)
  plot(pump_value~cas_h, data, type = "l", lwd = 1,
       xlab = "cas [h]", ylab = "rychlost pumpy feedu [%]")+
    lines(round(y) ~ cas_h, data_appr, col = "red")+
    points(approx(x = data$cas_h, y = data$pump_value, n = 10), 
           pch = 20, col = "blue")
  
}
