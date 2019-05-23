#  W tym konwerterze użyte zostały następujące skróty:
#
#
# - kilometry na godzinę - kmh
# - mile na godzinę - mph
# - kilometry na sekundę - kms
# - mile na sekundę - mps
# - metr na sekundę - ms
# - węzeł(mila morska na godzinę) - kn
#
#
# Aby przeliczyć jednostki, wybierz skrót jednostki, którą chcesz przeliczyć na inną i wybierz również 
# jej skrót. Skróty oddziel podkreślnikiem. 
# Przykład: kmh_mph("tu wpisz wartość którą chcesz przeliczyć")
#
# Możesz przliczać jednostki poniżej.







#zbiór funkcji do przeliczania kilometrów na godzinę na inne jednostki

kmh_mph <- function(prędkość_kmh){
  if(prędkość_kmh<0){
  stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_kmh))){
    stop("Podaj wartość numeryczną!")
  }
 else paste(prędkość_kmh, "kilometrów na godzinę to",
                                prędkość_kmh/1.609344,"mil na godzinę.")
 
}

kmh_kms <- function(prędkość_kmh){
  if(prędkość_kmh<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_kmh))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_kmh, "kilometrów na godzinę to",
             prędkość_kmh/3600,"kilometrów na sekundę.")
  
}

kmh_ms <- function(prędkość_kmh){
  if(prędkość_kmh<0){
  stop("Prędkość musi być wartością dodatnią!")
}
  else if(!(is.numeric(prędkość_kmh))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_kmh, "kilometrów na godzinę to",
             prędkość_kmh*0.2777784,"metrów na sekundę.")
  
}

kmh_mps <- function(prędkość_kmh){
  if(prędkość_kmh<0){
  stop("Prędkość musi być wartością dodatnią!")
}
  else if(!(is.numeric(prędkość_kmh))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_kmh, "kilometrów na godzinę to",
             prędkość_kmh/1.609344/3600,"mil na sekundę.")

}

kmh_kn <- function(prędkość_kmh){
  if(prędkość_kmh<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_kmh))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_kmh, "kilometrów na godzinę to",
             prędkość_kmh*0.53995726994149,"węzłów.")
  
}

#zbiór funkcji do przeliczania mili na godzinę na inne jednostki

mph_kmh <- function(prędkość_mph){
  if(prędkość_mph<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_mph))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_mph, "mil na godzinę to",
             prędkość_mph*1.609344,"kilometrów na godzinę")
}

mph_mps <- function(prędkość_mph){
  if(prędkość_mph<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_mph))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_mph, "mil na godzinę to",
             prędkość_mph/3600,"mil na sekundę")
 
}

mph_kms <- function(prędkość_mph){
  if(prędkość_mph<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_mph))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_mph, "mil na godzinę to",
             prędkość_mph*1.609344/3600,"kilometrów na sekundę")
  
}

mph_ms <- function(prędkość_mph){
  if(prędkość_mph<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_mph))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_mph, "mil na godzinę to",
             prędkość_mph*1.609344*0.2777778,"metrów na sekundę")
  
}

mph_kn <- function(prędkość_mph){
  if(prędkość_mph<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_mph))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_mph, "mil na godzinę to",
             prędkość_mph*1.609344*0.53995726994149,"węzłów")
  

}

#zbiór funkcji służących do przeliczania kilometrów na sekundę na inne jednostki

kms_kmh <- function(prędkość_kms){
  if(prędkość_kms<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_kms))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_kms, "kilometrów na sekundę to",
             prędkość_kms*3600,"kilometrów na godzinę")

}

kms_mph <- function(prędkość_kms){
  if(prędkość_kms<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_kms))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_kms, "kilometrów na sekundę to",
             prędkość_kms*3600/1.609344,"mil na godzinę")
  
}

kms_ms <- function(prędkość_kms){
  if(prędkość_kms<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_kms))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_kms, "kilometrów na sekundę to",
             prędkość_kms*3600*0.277778,"metrów na sekundę")
  

}

kms_kn <- function(prędkość_kms){
  if(prędkość_kms<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_kms))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_kms, "kilometrów na sekundę to",
             prędkość_kms*3600*0.53995726994149,"węzłów")
  
  
}

kms_mps <- function(prędkość_kms){
  if(prędkość_kms<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_kms))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_kms, "kilometrów na sekundę to",
             prędkość_kms*3600/1.609344/3600,"mil na sekundę")
}

#zbiór funkcji służących do przeliczania mil na sekundę na inne jednostki 

mps_mph <- function(prędkość_mps){
  if(prędkość_mps<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_mps))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_mps, "mil na sekundę to",
             prędkość_mps*3600,"mil na godzinę")

}

mps_kmh <- function(prędkość_mps){
  
  if(prędkość_mps<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_mps))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_mps, "mil na sekundę to",
             prędkość_mps*3600*1.609344,"kilometrów na godzinę")
  
}

mps_kms <- function(prędkość_mps){
  if(prędkość_mps<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_mps))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_mps, "mil na sekundę to",
             prędkość_mps*1.609344,"kilometrów na sekundę")
  
}

mps_ms <- function(prędkość_mps){
  if(prędkość_mps<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_mps))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_mps, "mil na sekundę to",
             prędkość_mps*3600*1.609344*0.2777784,"metrów na sekundę")

}

mps_kn <- function(prędkość_mps){
  if(prędkość_mps<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_mps))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_mps, "mil na sekundę to",
             prędkość_mps*3600*1.609344*0.53995726994149,"węzłów")
  

}

#zbiór funkcji służących do przeliczania metrów na sekundę na inne jednostki 

ms_kmh <- function(prędkość_ms){
  if(prędkość_ms<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_ms))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_ms, "metrów na sekundę to",
             prędkość_ms*3.6,"kilometrów na godzinę")

}

ms_mph <- function(prędkość_ms){
  if(prędkość_ms<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_ms))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_ms, "metrów na sekundę to",
             prędkość_ms*3.6/1.609344,"mil na godzinę")
  
}

ms_kms <- function(prędkość_ms){
  if(prędkość_ms<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_ms))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_ms, "metrów na sekundę to",
             prędkość_ms/1000,"kilometrów na sekundę")
  
}

ms_mps <- function(prędkość_ms){
  if(prędkość_ms<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_ms))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_ms, "metrów na sekundę to",
             prędkość_ms/1000/1.609344,"mil na sekundę")
  
}

ms_kn <- function(prędkość_ms){
  if(prędkość_ms<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_ms))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_ms, "metrów na sekundę to",
             prędkość_ms*3.6*0.53995726994149,"węzłów")
  
}

#zbiór funkcji służących do przeliczania węzłów na inne jednostki

kn_kmh <- function(prędkość_kn){
  if(prędkość_kn<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_kn))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_kn, "węzłów to",
             prędkość_kn*1.8519984,"kilometrów na godzinę")
  
 
}

kn_mph <- function(prędkość_kn){
  if(prędkość_kn<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_kn))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_kn, "węzłów to",
             prędkość_kn*1.8519984/1.609344,"mil na godzinę")
}

kn_kms <- function(prędkość_kn){  if(prędkość_kn<0){
  stop("Prędkość musi być wartością dodatnią!")
}
  else if(!(is.numeric(prędkość_kn))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_kn, "węzłów to",
             prędkość_kn*1.8519984/3600,"kilometrów na sekundę")
}

kn_mps <- function(prędkość_kn){  if(prędkość_kn<0){
  stop("Prędkość musi być wartością dodatnią!")
}
  else if(!(is.numeric(prędkość_kn))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_kn, "węzłów to",
             prędkość_kn*1.8519984/1.609344/3600,"mil na sekundę")
}

kn_ms <- function(prędkość_kn){
  if(prędkość_kn<0){
    stop("Prędkość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(prędkość_kn))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(prędkość_kn, "węzłów to",
             prędkość_kn*1.8519984*0.2777784,"metrów na sekundę")

}
