# Konwerter temperatur

# W tym konwerterze zostały użyte następujące skróty:
# Stopnie Celsjusza - C
# Stopnie Fahrenheita - F
# Kelwiny - K
# Stopnie Newtona - N
# Stopnie Rømer'a' - R

# Aby użyć konwertera, należy wybrać jednostkę z której chcemy przeliczyć temperaturę, a następnie wybrać,
# na którą jednostkę ma być przeliczona. Na przykład: 
# C_F <- konwersja ze stopni Celsjusza na stopnie Fahrenheita. 

# Możeśż użyć konwertera poniżej. Przed użyciem zaznacz wszystko (ctrl+a) i użyj skrótu ctrl + enter,
# aby wczytać wszystkie funkcje.








# Zbiór funkcji do przeliczania stopni Celsjusza na inne jednostki temperatury. 
C_F <- function(stopnie_C){
  if(!(is.numeric(stopnie_C))){
    stop("Podaj wartość numeryczną!")
  }
else paste(stopnie_C,"stopni Celsjusza to", stopnie_C*1.8 + 32,
           "stopni Fahrenheita.")
}
C_K <- function(stopnie_C){
  if(!(is.numeric(stopnie_C))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(stopnie_C,"stopni Celsjusza to", stopnie_C + 273.15,
             "Kelwinów.")
}
C_N <- function(stopnie_C){
  if(!(is.numeric(stopnie_C))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(stopnie_C,"stopni Celsjusza to", stopnie_C*33/100,
             "Stopni Newtona.")
  }
C_R <- function(stopnie_C){
  if(!(is.numeric(stopnie_C))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(stopnie_C,"stopni Celsjusza to", stopnie_C*21/40+7.5,
             "Stopni Rømer'a.")
}

# Zbiór funkcji służących do przeliczania stopni Fahrenheita na inne jednostki temperatury. 

F_C <- function(stopnie_F){
  if(!(is.numeric(stopnie_F))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(stopnie_F,"stopni Fahrenheita to", (stopnie_F-32)/1.8,
             "Stopni Celsjusza.")
  }
F_K <- function(stopnie_F){
  if(!(is.numeric(stopnie_F))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(stopnie_F,"stopni Fahrenheita to", (stopnie_F + 459.67) * 5/9,
             "Kelwinów.")
}
F_N <- function(stopnie_F){
  if(!(is.numeric(stopnie_F))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(stopnie_F,"stopni Fahrenheita to", (stopnie_F - 32) * 11/60 ,
             "stopni Newtona.")
}
F_R <- function(stopnie_F){
  if(!(is.numeric(stopnie_F))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(stopnie_F,"stopni Fahrenheita to", (stopnie_F - 32) * 7/24 + 7.5,
             "stopni Rømera.")
}

#Zbiór funkcji służących do przeliczania Kelwinów na inne jednostki temperatur.

K_C <- function(K){
  if(!(is.numeric(K))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(K,"Kelwinów to",K-273.15,
             "stopni Celsjusza.")
}

K_F <- function(K){
  if(!(is.numeric(K))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(K,"Kelwinów to",(K * 1.8) - 459.67,
             "stopni Fahrenheita.")
}
K_N <- function(K){  if(!(is.numeric(K))){
  stop("Podaj wartość numeryczną!")
}
  else paste(K,"Kelwinów to",(K - 273.15) * 33/100,
             "stopni Newtona.")
}
K_R <- function(K){
   if(!(is.numeric(K))){
    stop("Podaj wartość numeryczną!")
  }
    else paste(K,"Kelwinów to",(K - 273.15) *21/40+7.5 ,
               "stopni Rømera.")
}

#Zbiór funkcji służących do przeliczania stopni Newtona na inne jednostki temperatury.

N_C <- function(stopnie_N){
  if(!(is.numeric(stopnie_N))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(stopnie_N,"stopni Newtona to",stopnie_N * 100/33,
             "stopni Celsjusza.")
  }
N_F <- function(stopnie_N){
  if(!(is.numeric(stopnie_N))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(stopnie_N,"stopni Newtona to",stopnie_N * 60/11 + 32,
             "stopni Fahrenheita.")
}
N_K <- function(stopnie_N){
  if(!(is.numeric(stopnie_N))){
  stop("Podaj wartość numeryczną!")
}
  else paste(stopnie_N,"stopni Newtona to",stopnie_N * 100/33 + 273.15	,
             "Kelwinów.")
  }
N_R <- function(stopnie_N){
  if(!(is.numeric(stopnie_N))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(stopnie_N,"stopni Newtona to",stopnie_N*100/33*21/40+7.5 	,
             "stopni Rømera")
  }

# Zbiór funkcji służących do przeliczania stopni  Rømera na inne jednostki temperatur. 

R_C <- function(stopnie_R){
  if(!(is.numeric(stopnie_R))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(stopnie_R,"stopni Rømera to",(stopnie_R-7.5)*40/21,
             "stopni Celsjusza")
}
R_F <- function(stopnie_R){
  if(!(is.numeric(stopnie_R))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(stopnie_R,"stopni Rømera to",(stopnie_R-7.5)*24/7+32,
             "stopni Fahrenheita")
}
R_K <- function(stopnie_R){
  if(!(is.numeric(stopnie_R))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(stopnie_R,"stopni Rømera to",(stopnie_R-7.5)*24/7 + 273.15,
             "stopni Fahrenheita")
}
R_N <- function(stopnie_R){
  if(!(is.numeric(stopnie_R))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(stopnie_R,"stopni Rømera to",((stopnie_R-7.5)*40/21)*33/100,
             "stopni Newtona")
}
