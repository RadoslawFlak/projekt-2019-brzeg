#  w tym konwerterze użyte zostały następujące skróty:
#
#
# - kilometry na godzinę - kmh
# - mile na godzinę - mph
# - kilometry na sekundę -kms
# - mile na sekundę - mps
# - metr na sekundę - ms
# - węzeł(mila morska na godzinę) - kn
#
#
# Aby przeliczyć jednostki, wybierz skrót jednostki, którą chcesz przeliczyć na inną i wybierz również 
# jej skrót. Skróty oddziel podkreślnikiem. 
# Przykład: kmh_mph("tu wpisz wartość którą chcesz przeliczyć")


#zbiór funkcji do przeliczania kilometrów na godzinę na inne jednostki
kmh_mph <- function(prędkość_kmh){
  prędkość_kmh/1.609344
}
kmh_kms <- function(prędkość_kmh){
  prędkość_kmh/3600
}
kmh_ms <- function(prędkość_kmh){
  prędkość_kmh*0.277778
}
kmh_mps <- function(prędkość_kmh){
  prędkość_kmh/1.609344/3600
}
kmh_kn <- function(prędkość_kmh){
  prędkość_kmh*0.53995726994149
}

#zbiór funkcji do przeliczania mili na godzinę na inne jednostki
mph_kmh <- function(prędkość_mph){
 cat(paste(prędkość_mph, "mil na godzinę to", prędkość_mph*1.609344, "kilometrów na godzinę."))
}
mph_mps <- function(prędkość_mph){
  prędkość_mph/3600
}
mph_kms <- function(prędkość_mph){
  prędkość_mph*1.609344/3600
}
mph_ms <- function(prędkość_mph){
  prędkość_mph*1.609344*0.2777778
}
mph_kn <- function(prędkość_mph){
  prędkość_mph*1.609344*0.53995726994149
}
#zbiór funkcji służących do przeliczania kilometrów na sekundę na inne jednostki

kms_kmh <- function(prędkość_kms){
  prędkość_kms*3600
}
kms_mph <- function(prędkość_kms){
  prędkość_kms*3600/1.609344
}
kms_ms <- function(prędkość_kms){
  prędkość_kms*3600*0.277778
}
kms_kn <- function(prędkość_kms){
  prędkość_kms*3600*0.53995726994149
}
kms_mps <- function(prędkość_kms){
  prędkość_kms*3600/1.609344/3600
}

#zbiór funkcji służących do przeliczania mil na sekundę na inne jednostki 

mps_mph <- function(prędkość_mps){
  prędkość_mps*3600
}
mps_mph(10) 
  
mps_kmh
  