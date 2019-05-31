# Kalkulator walutowy
# Działający na API - https://exchangeratesapi.io
# 
# Aby program zadziałał, należy wczytać dane z wszystkich sekcji a następnie
# przejść do podsekcji Program aby zacząć przeliczać według interesując nas waluty.
#
# Założenia ---------------------------------------------------------------
  install.packages('httr')
  library(httr)
  options(stringsAsFactors = FALSE)
# Wczytywanie Danych -------------------------------------------------------------
#PLN
  PLN = {
    Pobierz_Kursy_PLN = GET(url="https://api.exchangeratesapi.io/latest?base=PLN")
    Pokaz_Kursy_PLN = content(Pobierz_Kursy_PLN)
    Kursy_PLN = as.data.frame(Pokaz_Kursy_PLN)
  }
#AUD
  AUD = {
    Pobierz_Kursy_AUD = GET(url="https://api.exchangeratesapi.io/latest?base=AUD")
    Pokaz_Kursy_AUD = content(Pobierz_Kursy_AUD)
    Kursy_AUD = as.data.frame(Pokaz_Kursy_AUD)
  }
#BGN
  BGN = {
    Pobierz_Kursy_BGN = GET(url="https://api.exchangeratesapi.io/latest?base=BGN")
    Pokaz_Kursy_BGN = content(Pobierz_Kursy_BGN)
    Kursy_BGN = as.data.frame(Pokaz_Kursy_BGN)
  }
#HRK
  HRK = {
    Pobierz_Kursy_HRK = GET(url="https://api.exchangeratesapi.io/latest?base=HRK")
    Pokaz_Kursy_HRK = content(Pobierz_Kursy_HRK)
    Kursy_HRK = as.data.frame(Pokaz_Kursy_HRK)
  }
#CZK
  CZK = {
    Pobierz_Kursy_CZK = GET(url="https://api.exchangeratesapi.io/latest?base=CZK")
    Pokaz_Kursy_CZK = content(Pobierz_Kursy_CZK)
    Kursy_CZK = as.data.frame(Pokaz_Kursy_CZK)
  }
#DKK
  DKK = {
    Pobierz_Kursy_DKK = GET(url="https://api.exchangeratesapi.io/latest?base=DKK")
    Pokaz_Kursy_DKK = content(Pobierz_Kursy_DKK)
    Kursy_DKK = as.data.frame(Pokaz_Kursy_DKK)
  }
#CAD
  CAD = {
    Pobierz_Kursy_CAD = GET(url="https://api.exchangeratesapi.io/latest?base=CAD")
    Pokaz_Kursy_CAD = content(Pobierz_Kursy_CAD)
    Kursy_CAD = as.data.frame(Pokaz_Kursy_CAD)
  }
#NOK
  NOK = {
    Pobierz_Kursy_NOK = GET(url="https://api.exchangeratesapi.io/latest?base=NOK")
    Pokaz_Kursy_NOK = content(Pobierz_Kursy_NOK)
    Kursy_NOK = as.data.frame(Pokaz_Kursy_NOK)
  }
#RON
  RON = {
    Pobierz_Kursy_RON = GET(url="https://api.exchangeratesapi.io/latest?base=RON")
    Pokaz_Kursy_RON = content(Pobierz_Kursy_RON)
    Kursy_RON = as.data.frame(Pokaz_Kursy_RON)
  }
#CHF
  CHF = {
    Pobierz_Kursy_CHF = GET(url="https://api.exchangeratesapi.io/latest?base=CHF")
    Pokaz_Kursy_CHF = content(Pobierz_Kursy_CHF)
    Kursy_CHF = as.data.frame(Pokaz_Kursy_CHF)
  }
#SEK
  SEK = {
    Pobierz_Kursy_SEK = GET(url="https://api.exchangeratesapi.io/latest?base=SEK")
    Pokaz_Kursy_SEK = content(Pobierz_Kursy_SEK)
    Kursy_SEK = as.data.frame(Pokaz_Kursy_SEK)
  }
#EUR
  EUR = {
    Pobierz_Kursy_EUR = GET(url="https://api.exchangeratesapi.io/latest?base=EUR")
    Pokaz_Kursy_EUR = content(Pobierz_Kursy_EUR)
    Kursy_EUR = as.data.frame(Pokaz_Kursy_EUR)
  }
#USD
  USD = {
    Pobierz_Kursy_USD = GET(url="https://api.exchangeratesapi.io/latest?base=USD")
    Pokaz_Kursy_USD = content(Pobierz_Kursy_USD)
    Kursy_USD = as.data.frame(Pokaz_Kursy_USD)
  }
#HUF
  HUF = {
    Pobierz_Kursy_HUF = GET(url="https://api.exchangeratesapi.io/latest?base=HUF")
    Pokaz_Kursy_HUF = content(Pobierz_Kursy_HUF)
    Kursy_HUF = as.data.frame(Pokaz_Kursy_HUF)
  }
#GBP
  GBP = {
    Pobierz_Kursy_GBK = GET(url="https://api.exchangeratesapi.io/latest?base=GBP")
    Pokaz_Kursy_GBK = content(Pobierz_Kursy_GBK)
    Kursy_GBP = as.data.frame(Pokaz_Kursy_GBK)
  }
# Funkcje -----------------------------------------------------------------
# AUD - dolar australijski  - Australia
Sprzedaz_AUD = function(AUD){
  if(AUD<=0){
  stop("Waluta musi być wartością większą od zera!")
   }
  else if(!(is.numeric(AUD))){
    stop("Podaj wartość numeryczną!")
  }
 else cat(paste("Przy wpłacie", AUD, "AUD, otrzymasz", round(AUD*Kursy_AUD$rates.PLN, digits = 2), "PLN." ))  
}
Kupno_AUD = function(PLN){
   if(PLN<=0){
  stop("Waluta musi być wartością dodatnią!")
   }
  else if(!(is.numeric(PLN))){
    stop("Podaj wartość numeryczną!")
  }
 else cat(paste("Przy wpłacie", PLN, "PLN, otrzymasz", round(PLN*Kursy_PLN$rates.AUD, digits = 2), "AUD." ))  
}
  
# BGN - lew bułgarski       - Bułgaria
Sprzedaz_BGN = function(BGN){
   if(BGN<=0){
  stop("Waluta musi być wartością większą od zera!")
}
  else if(!(is.numeric(BGN))){
    stop("Podaj wartość numeryczną!")
  }
  cat(paste("Przy wpłacie", BGN, "BGN, otrzymasz", round(BGN*Kursy_BGN$rates.PLN, digits = 2), "PLN." ))  
}
Kupno_BGN = function(PLN){
   if(PLN<=0){
  stop("Waluta musi być wartością większą od zera!")
}
  else if(!(is.numeric(PLN))){
    stop("Podaj wartość numeryczną!")
  }
  cat(paste("Przy wpłacie", PLN, "PLN, otrzymasz", round(PLN*Kursy_PLN$rates.BGN, digits = 2), "BGN." )) 
}
  
# HRK - kuna chorwacka      - Chorwacja
Sprzedaz_HRK = function(HRK){ 
  if(HRK<=0){
  stop("Waluta musi być wartością większą od zera!")
}
  else if(!(is.numeric(HRK))){
    stop("Podaj wartość numeryczną!")
  }
  cat(paste("Przy wpłacie", HRK, "HRK, otrzymasz", round(HRK*Kursy_HRK$rates.PLN, digits = 2), "PLN." ))
}
Kupno_HRK = function(PLN){
   if(PLN<=0){
  stop("Waluta musi być wartością większą od zera!")
}
  else if(!(is.numeric(PLN))){
    stop("Podaj wartość numeryczną!")
  }
  cat(paste("Przy wpłacie", PLN, "PLN, otrzymasz", round(PLN*Kursy_PLN$rates.HRK, digits = 2), "HRK." )) 
}
  
# CZK - korona czeska       - Czechy
Sprzedaz_CZK = function(CZK){
   if(CZK<=0){
  stop("Waluta musi być wartością większą od zera!")
}
  else if(!(is.numeric(CZK))){
    stop("Podaj wartość numeryczną!")
  }
 else cat(paste("Przy wpłacie", CZK, "CZK, otrzymasz", round(CZK*Kursy_CZK$rates.PLN, digits = 2), "PLN." ))
}
Kupno_CZK = function(PLN){
  if(PLN<=0){
  stop("Waluta musi być wartością większą od zera!")
}
  else if(!(is.numeric(PLN))){
    stop("Podaj wartość numeryczną!")
  }
 else cat(paste("Przy wpłacie", PLN, "PLN, otrzymasz", round(PLN*Kursy_PLN$rates.CZK, digits = 2), "CZK." ))
}
  
# DKK - korona duńska       - Dania
Sprzedaz_DKK = function(DKK){
  if(DKK<=0){
  stop("Waluta musi być wartością większą od zera!")
}
  else if(!(is.numeric(DKK))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", DKK, "DKK, otrzymasz", round(DKK*Kursy_DKK$rates.PLN, digits = 2), "PLN." ))  
}
Kupno_DKK = function(PLN){
  if(PLN<=0){
  stop("Waluta musi być wartością większą od zera!")
}
  else if(!(is.numeric(PLN))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", PLN, "PLN, otrzymasz", round(PLN*Kursy_PLN$rates.DKK, digits = 2), "DKK." ))  
}
# CAD - dolar kanadyjski    - Kanada
Sprzedaz_CAD = function(CAD){ 
  if(CAD<=0){
  stop("Waluta musi być wartością większą od zera!")
}
  else if(!(is.numeric(CAD))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", CAD, "CAD, otrzymasz", round(CAD*Kursy_CAD$rates.PLN, digits = 2), "PLN." ))  
}
Kupno_CAD = function(PLN){
  if(PLN<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(PLN))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", PLN, "PLN, otrzymasz", round(PLN*Kursy_PLN$rates.CAD, digits = 2), "CAD." ))  
}
# NOK - korona norweska     - Norwegia
Sprzedaz_NOK = function(NOK){
  if(NOK<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(NOK))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", NOK, "NOK, otrzymasz", round(NOK*Kursy_NOK$rates.PLN, digits = 2), "PLN." ))  
}
Kupno_NOK = function(PLN){
  if(PLN<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(PLN))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", PLN, "PLN, otrzymasz", round(PLN*Kursy_PLN$rates.NOK, digits = 2), "NOK." ))  
}
# RON - lej rumuński        - Rumunia
Sprzedaz_RON = function(RON){
  if(RON<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(RON))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", RON, "RON, otrzymasz", round(RON*Kursy_RON$rates.PLN, digits = 2), "PLN." ))  
}
Kupno_RON = function(PLN){
  if(PLN<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(PLN))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", PLN, "PLN, otrzymasz", round(PLN*Kursy_PLN$rates.RON, digits = 2), "RON." ))  
}
# CHF - frank szwajcarski   - Szwajcaria
Sprzedaz_CHF = function(CHF){
  if(CHF<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(CHF))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", CHF, "CHF, otrzymasz", round(CHF*Kursy_CHF$rates.PLN, digits = 2), "PLN." ))  
}
Kupno_CHF = function(PLN){
  if(PLN<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(PLN))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", PLN, "PLN, otrzymasz", round(PLN*Kursy_PLN$rates.CHF, digits = 2), "CHF." ))  
}
# SEK - korona szwedzka     - Szwecja
Sprzedaz_SEK = function(SEK){
  if(SEK<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(SEK))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", SEK, "SEK, otrzymasz", round(SEK*Kursy_SEK$rates.PLN, digits = 2), "PLN." ))  
}
Kupno_SEK = function(PLN){
  if(PLN<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(PLN))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", PLN, "PLN, otrzymasz", round(PLN*Kursy_PLN$rates.SEK, digits = 2), "SEK." ))  
}
# EUR - euro                - Unia Europejska (strefa euro)
Sprzedaz_EUR = function(EUR){
  if(EUR<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(EUR))){
    stop("Podaj wartość numeryczną!")
  }
  else  cat(paste("Przy wpłacie", EUR, "EUR, otrzymasz", round(EUR*Kursy_EUR$rates.PLN, digits = 2), "PLN." ))  
}
Kupno_EUR = function(PLN){
  if(PLN<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(PLN))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", PLN, "PLN, otrzymasz", round(PLN*Kursy_PLN$rates.EUR, digits = 2), "EUR." ))  
}
# USD - dolar amerykański   - USA
Sprzedaz_USD = function(USD){
  if(USD<=0){
  stop("Waluta musi być wartością większą od zera!")
}
  else if(!(is.numeric(USD))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", USD, "USD, otrzymasz", round(USD*Kursy_USD$rates.PLN, digits = 2), "PLN." ))  
}
Kupno_USD = function(PLN){
  if(PLN<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(PLN))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", PLN, "PLN, otrzymasz", round(PLN*Kursy_PLN$rates.USD, digits = 2), "USD." ))  
}
# HUF - forint węgierski    - Węgry
Sprzedaz_HUF = function(HUF){ 
  if(HUF<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(HUF))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", HUF, "HUF, otrzymasz", round(HUF*Kursy_HUF$rates.PLN, digits = 2), "PLN." ))  
}
Kupno_HUF = function(PLN){
  if(PLN<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(PLN))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", PLN, "PLN, otrzymasz", round(PLN*Kursy_PLN$rates.HUF, digits = 2), "HUF." ))  
}
# GBP - funt brytyjski      - Wielka Brytania
Sprzedaz_GBP = function(GBP){
  if(GBP<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(GBP))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", GBP, "GBP, otrzymasz", round(GBP*Kursy_GBP$rates.PLN, digits = 2), "PLN." ))  
}
Kupno_GBP = function(PLN){
  if(PLN<=0){
    stop("Waluta musi być wartością większą od zera!")
  }
  else if(!(is.numeric(PLN))){
    stop("Podaj wartość numeryczną!")
  }
  else cat(paste("Przy wpłacie", PLN, "PLN, otrzymasz", round(PLN*Kursy_PLN$rates.GBP, digits = 2), "GBP." ))  
}
# Program -----------------------------------------------------------------
# 1. Przelicznik AUD (dolary australijskie)
Kupno_AUD()
Sprzedaz_AUD()
# 2. Przelicznik BGN (lew bułgarski)
Kupno_BGN()
Sprzedaz_BGN()
# 3. Przelicznik CAD (dolary kanadyjskie)
Kupno_CAD()
Sprzedaz_CAD()
# 4. Przelicznik CHF (franki szwajcarskie)
Kupno_CHF()
Sprzedaz_CHF()
# 5. Przelicznik CZK (korony czeskie)
Kupno_CZK()
Sprzedaz_CZK()
# 6. Przelicznik DKK (korony duńskie)
Kupno_DKK()
Sprzedaz_DKK()
# 7. Przelicznik EUR (euro)
Kupno_EUR()
Sprzedaz_EUR()
# 8. Przelicznik GBP (funty brytyjskie)
Kupno_GBP()
Sprzedaz_GBP()
# 9. Przelicznik HRK (kuny chorwackie)
Kupno_HRK()
Sprzedaz_HRK()
# 10. Przelicznik HUF (forinty węgierskie)
Kupno_HUF()
Sprzedaz_HUF()
# 11. Przelicznik NOK (korony norweskie)
Kupno_NOK()
Sprzedaz_NOK()
# 12. Przelicznik RON (leje rumuńskie)
Kupno_RON()
Sprzedaz_NOK()
# 13. Przelicznik SEK (korony szwedzkie)
Kupno_SEK()
Sprzedaz_SEK()
# 14. Przelicznik USD (dolary amerykańskie)
Kupno_USD()
Sprzedaz_USD()

# Dziękujemy za skorzystanie z naszego kalkulatora.
