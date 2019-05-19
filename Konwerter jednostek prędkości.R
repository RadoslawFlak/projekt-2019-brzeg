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

kmh_mph <- function(prędkość_kmh){
  prędkość_kmh/1.609344
}
kmh_kms <- function(prędkość_kmh){
  prędkość_kmh/3600
}
kmh_mps <- function(prędkość_kmh){
  prędkość_kmh
}
