#konwerter jednostek masy

#konwerter zawiera następujące jednostki:

#a)podstawowe jednostki systemu metrycznego:
gram[g]
dekagram[dkg]
kilogram[kg]
tona[Tn]

#b)jednostki anglosaskie:
uncja[oz]
funt[lb]

#c) inne jednostki
karat[ct]
kwintal[q]


##1.GRAM

#konwersja na podstawowe jednostki systemu metrycznego:

g_g <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "gramów to",
             x*1,"dgramów")
}
g_dkg  <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "gramów to",
             x*0.1,"dekagramów")
}

g_kg <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "gramów to",
             x*0.001,"kilogramów")
}

g_Tn <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "gramów to",
             x*1.0000e-6,"ton.")
}

#konwersja na jednostki anglosaskie

g_oz <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "gramów to",
             x*0.0353,"uncji.")
}

g_lb <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "gramów to",
             x*0.0022,"funtów.")
}

#konwersja na inne jednostki masy

g_ct <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "gramów to",
             x*5,"karatów.")
}

g_q <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "gramów to",
             x*1.0000e-5,"kwintali.")
}

##2.DEKAGRAM

#konwersja na podstawowe jednostki systemu metrycznego

dkg_g <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "dekagramów to",
             x*10,"gramów.")
}

dkg_dkg <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "dekagramów to",
             x*1,"dekagramów.")
}

dkg_kg <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "dekagramów to",
             x*0.01,"kilogramów.")
}

dkg_Tn  <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "dekagramów to",
             x*1.0000e-5,"ton.")
}

#konwersja na jednostki anglosaskie

dkg_oz <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "dekagramów to",
             x*0.3527,"uncji.")
}

dkg_lb <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "dekagramów to",
             x*0.022,"funtów.")
}
 
#konwersja na inne jednostki masy

dkg_ct  <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "dekagramów to",
             x*50,"karatów.")
}

dkg_q  <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "dekagramów to",
             x*0.0001,"kwintali.")
}

##3.KILOGRAM

#konwersja na podstawowe jednostki systemu metrycznego

kg_g  <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilogramów to",
             x*1000,"gramów.")
}

kg_dkg <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilogramów to",
             x*100,"dekagramów.")
}

kg_kg <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilogramów to",
             x*1,"kilogramów.")
}

kg_Tn <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilogramów to",
             x*0.001,"ton.")
}
 
#konwersja na jednostki anglosaskie

kg_oz <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilogramów to",
             x*35.274,"uncji.")
}

kg_lb <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilogramów to",
             x*2.2046,"funtów.")
}

#konwersja na inne jednostki masy

kg_ct  <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilogramów to",
             x*5000,"karatów.")
}

kg_q <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilogramów to",
             x*0.01,"kwintali")
}

##4.TONA

Tn_g  <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "ton to",
             x*1.0000e+6,"gramów.")
}

Tn_dkg <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "ton to",
             x*100000,"dekagramów.")
}

Tn_kg <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "ton to",
             x*1000,"kilogramów")
}

Tn_Tn <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "ton to",
             x*1,"ton.")
}

#konwersja na jednostki anglosaskie

Tn_oz <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "ton to",
             x*35273.9621,"uncji.")
}

Tn_lb <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "ton to",
             x*2204.6226,"funtów.")
}

#konwersja na inne jednostki masy

Tn_ct <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "ton to",
             x*5.0000e+6,"karatów.")
}

Tn_q <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "ton to",
             x*10,"kwintali.")
}


#KONWERSJA ANGLOSASKICH JEDNOSTEK MASY

##5.UNCJA

oz_g <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "uncji to",
             x*28.3495,"gramów.")
}

oz_dkg <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "uncji to",
             x*2.835,"dekagramów.")
}

oz_kg <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "uncji to",
             x*0.0283,"kilogramów.")
}

oz_Tn <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "uncji to",
             x*2.8350e-5,"ton.")
}


#konwersja na anglosaskie jednostki masy

oz_oz <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "uncji to",
             x*1,"uncji.")
}

oz_lb <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "uncji to",
             x*0.0625,"funtów.")
}

#konwersja na inne jednostki masy

oz_ct <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "uncji to",
             x*141.7476,"karatów.")
}

oz_q  <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "uncji to",
             x*0.0003,"kwintali.")
}

##6.FUNT

#konwersja na podstawowe jednostki masy

lb_g <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "funtów to",
             x*453.5924,"gramów.")
}

lb_dkg  <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "funtów to",
             x*45.3592,"dekagramów.")
}

lb_kg <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "funtów to",
             x*0.4536,"kilogramów.")
}

lb_Tn <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "funtów to",
             x*0.0005,"ton.")
}

#konwersja na anglosaskie jednostki masy

lb_lb <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "funtów to",
             x*1,"funtów.")
}

lb_oz <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "funtów to",
             x*16,"uncji.")
}

#konwersja na inne jednostki masy

lb_ct <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "funtów to",
             x*2267.9619,"karatów.")
}

lb_q <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "funtów to",
             x*0.0045,"kwintali.")
}

##KONWERSJA INNYCH JEDNOSTEK MASY

##1.KARAT

#konwersja na podstawowe jednostki masy

ct_g <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "karatów to",
             x*0.2,"gramów.")
}

ct_dkg <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "karatów to",
             x*0.02,"dekagramów.")
}

ct_kg <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "karatów to",
             x*0.0002,"kilogramów.")
}

ct_Tn <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "karatów to",
             x*2.0000e-7,"ton.")
}

#konwersja na anglosaskie jednostki masy

ct_oz <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "karatów to",
             x*0.0071,"uncji.")
}

ct_lb <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "karatów to",
             x*0.0004,"funtów.")
}

#konwersja na inne jednostki masy

ct_ct <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "karatów to",
             x*1,"karatów.")
}

ct_q <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "karatów to",
             x*2.0000e-6,"kwintali.")
}

##2.KWINTAL

q_g <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kwintali to",
             x*100000,"gramów.")
}

q_dkg <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kwintali to",
             x*10000,"dekagramów.")
}

q_kg <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kwintali to",
             x*100,"kilogramów.")
}

q_Tn <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kwintali to",
             x*0.1,"ton.")
}

#konwersja na anglosaskie jednostki masy

q_oz <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kwintali to",
             x*3527.3962,"uncji.")
}

q_lb <- function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kwintali to",
             x*220.4623,"funtów.")
}

#konwersja na inne jednostki masy

q_ct <-  function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kwintali to",
             x*5.0000e+5,"karatów.")
}

q_q <-  function(x){
  if(x<0){
    stop("Masa musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kwintali to",
             x*1,"kwintali.")
}






