# 2l reaktor:
# 100 ml inokula + 900 ml batch media (10 g glc/l) + 500 ml feedu
##### OPTIMALIZACE DOBY FEEDOVANI #####

#funkce pro optimalizaci doby kultivace (feedu) 


mu_poc <- 0.2   # maxim?ln? r?stov? rychlost (na za??tku kultivace)
mu_kon <- 0.048   # minim?ln? r?stov? rychlost (na konci kultivace)
par_k <- 0.2    # prohnut? k?ivky pr?b?hu r?stov? rychlosti

OD <- 8.65      # OD inokula

par_vRPM <- 19.56   # prepocet "nacerpany objem feedu [ml]" >> "celkove otacky pumpy": par_vRPM
                    # prepocet: data$feed_rot <- data$feed_vol_total * par_vRPM

{
  #funkce pro optimalizaci doby kultivace (feedu) !!!
  opt_fct <- function(delka_kultivace){
    # delka kultivace v hodinach
    
    # cas po 10 s
    data <- data.frame(cas = c(0:(delka_kultivace*360)))
    
    # cas v hod
    data$cas_h <- data$cas / 360
    
    # odhadovane mnozstvi susiny na zacatku feedu
    # zadej OD; vypocte se DWT jako OD / 1.8136 * objem inokula + odhadovany zisk z batch
    # (pocitame s vyteznostnim koeficientem batch = 0.1)
    DWT_0 <- OD / 1.8136 * 0.1 + 0.9
    
    
    mu_min <- mu_poc - 2* (mu_poc - mu_kon) # rozdil beru 2*, protoze chci jen pulku sigmoidy
    mu_max <- mu_poc
    par_k <- par_k
    data$mu <- mu_min + (mu_max - mu_min) / (1 + exp(par_k * (data$cas_h - delka_kultivace)))
    
    
    
    data$DWT <- DWT_0  # pocatecni mnozstvi biomasy z inokula + batch
    
    # iterativni vypocet DWT v prubehu kultivace
    for(i in c(2: nrow(data))){
      data$DWT[i] <- data$DWT[i-1] * ((1 + data$mu[i]) ^ (1/360))
    }
    
    # mnozstvi feedu od zacatku (vypocteno z DWT)
    # narust DWT od zacatku / vyteznostni koeficient / koncentrace glc ve feedu
    data$feed_vol_total <- (data$DWT - data$DWT[1]) / 0.55 / 0.22   
    
    
    
    # vratit rozdil mezi feed_vol_total a cilovym objemem 0.5 l
    return(abs(500 - data$feed_vol_total[nrow(data)]))
  }
  
  # optimalizace: vysledek je doba feedovani pro nadavkovani 500 ml
  opt <- optim(24, opt_fct, 
               control = list(maxit = 100), 
               method = "Brent", lower = 0, upper = 100)
  
  
  ##### SIMULACE KULTIVACE #####
  delka_kultivace <- opt$par
  
  
  {
    data <- list()
    data$cas_h <- (c(0:(delka_kultivace*3600))) / 3600  #cas kultivace v hod
    data <- data.frame(data)
    DWT_0 <- OD / 1.8136 * 0.1 + 0.9  #odhadovane mnozstvi susiny na zacatku feedu
    
    mu_min <- mu_poc - 2* (mu_poc - mu_kon) # rozdil beru 2*, protoze chci jen pulku sigmoidy
    mu_max <- mu_poc
    par_k <- par_k
    data$mu <- mu_min + (mu_max - mu_min) / (1 + exp(par_k * (data$cas_h - delka_kultivace)))
    
    data$DWT <- DWT_0  # pocatecni mnozstvi biomasy
    for(i in c(2: nrow(data))){ #samotna iterace rustu biomasy
      data$DWT[i] <- data$DWT[i-1] * ((1 + data$mu[i]) ^ (1/3600))
    }
    data$feed_vol_total <- (data$DWT - data$DWT[1]) / 0.55 / 0.22  #mnozstvi feedu od zacatku (vypocteno z DWT)
    data$feed_rot <- data$feed_vol_total * par_vRPM # a prepocet na otacky pumpy
    data$feed_speed <- c(0, diff(data$feed_vol_total)) * 60 #rychlost feedu [ml/min]
    #data$pump_value <- data$feed_speed / xxxxx  #rychlost pumpy [%]
  }
  
  # graf rustova rychlost
  plot(mu~cas_h, data, type = "l",
       xlab = "cas [h]", ylab = "rustova rychlost [1/h]")
  
  # graf rychlost feedu
  plot(feed_speed~cas_h, data, type = "l",
       xlab = "cas [h]", ylab = "rychlost feedu [ml/min]")
  
  # fitting objemu
  model <- nls(feed_rot ~ a*cas_h^3 + b*cas_h^2 + c*cas_h + d, data, #chybi linearni clen!!! (naschval)
               start = list(a=0, b=0, c=0, d=0))
  
  # graf fitted model vs. feed_vol_total
  plot(feed_rot~cas_h, data, type = "l",
       xlab = "cas [h]", ylab = "celkovy objem nacerpaneho feedu [otacky]")+
    lines(data$cas_h, predict(model), lty = "dashed", col = "red")
  
  cat("doba davkovani feedu: ", opt$par, " hod\n")
  print(coef(model))
}

