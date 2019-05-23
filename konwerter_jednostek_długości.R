#konwersja jednostek długości

#Konwerter zawiera następujące jednostki:

#a)podstawowe jednostki systemu metrycznego
#milimetr[mm]
#centymetr[cm]
#decymetr[dcm]
#metr[m]
#kilometr[km]

#b)miary anglosaskie
#cal[inc]
#stopa[ft]
#jard[yd]
#mila[mi]
#liga[li]

#c)miary morskie
#kabel[kl]
#mila_morska[NM]
#liga_morska[lm]

#Aby przeliczyć jednostkę wpisz jej skrót, wpisz skrót jednostki, którą chcesz otrzymać a ich nazwy oddziel od siebie podkreślnikiem, 
#pamiętaj, że jednostki ujemne muszą być liczbami, oraz nie mogą przyjmować wartości ujemnych.

###KONWERSJA PODSTAWOWYCH JEDNOSTEK SYSTEMU METRYCZNEGO

##1. METRY

#metry na podstawowe jednostki systemu metrycznego

m_km <- function(x){
  if(x<0){
    stop("Długość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "metrów to",
             x*0.001,"kilometra.")
}


m_mm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "metrów",
             x*1000,"milimetrów.")
}


m_cm  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "metrów to",
            x*100,"centymetrów.")
  
}

m_dcm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "metrów to",
             x*10,"decymetrów.")
}

m_m  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "metrów to",
             x*1,"metrów.")
}


#metry na miary anglosaskie

m_inc <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "metrów to",
             x*39.3701,"cali.")
}


m_ft <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "metrów to",
             x*3.2808, "stóp.")
}


m_yd <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "metrów to",
             x*1.0936, "jardów.")
}


m_mi  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "metrów to",
             x*0.0006, "mili.")
}


m_li <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "metrów to",
             x*0.0002, "ligi.")
}


#metry na miary morskie

m_kl <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "metrów to",
             x*0.0054, "kabla.")
}

m_NM <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "metrów to",
             x*0.0005, "mili morskiej.")
}

m_lm<- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "metrów to",
             x*0.0002, "ligi morskiej.")
}



##2. KILOMETRY

#kilometry na podstawowe jednostki systemu metrycznego

km_m <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilometrów to",
             x*1000, "metrów.")
}

km_dcm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilometrów to",
             x*10000, "decymetrów.")
}

km_cm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilometrów to",
             x*100000, "centymetrów.")
}

km_mm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilometrów to",
             x*1000000, "milimetrów.")
}

km_km <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilometrów to",
             x*1, "kilometrów.")
}


#kilometry na miary anglosaskie

km_inc <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilometrów to",
             x*39370.0787, "cali.")
}

km_ft <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilometrów to",
             x*3280.8399, "stóp.")
}

km_yd <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilometrów to",
             x*1093.6133, "jardów.")
}


m_mi <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilometrów to",
             x*0.6214, "mil.")
}

km_li <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilometrów to",
             x*0.2071, "lig.")
}


#kilometry na miary morskie


km_kl <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilometrów to",
             x*5.3996, "kabli.")
}

km_NM  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilometrów to",
             x*0.54, "mil morskich.")
}

km_lm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kilometrów to",
             x*0.18, "lig morskich.")
}



##3.DECYMETRY

#decymetry na podstawowe jednostki systemu metrycznego

dcm_mm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*100, "milimetrów")
}

dcm_cm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*10, "centymetrów")
}
dcm_dcm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*1, "decymetrów")
}

dcm_m <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*0.1, "metrów")
}

dcm_km <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*0.0001, "kilometrów")
}


#decymert na miary anglosaskie

dcm_inc <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*3.937, "cali")
}
 
dcm_ft <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*0.3281, "stóp")
}

dcm_yd <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*0.1094, "jardów.")
}

dcm_mi <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*6.2137e-5, "mili")
}

dcm_li <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*2.0712e-5, "ligi")
}

#decymetr na miary morskie

dcm_kl <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*0.0005, "kabla")
}

dcm_NM  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*5.3996e-5, "mili morskiej")
}

dcm_lm  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*1.7999e-5, "ligi morskiej")
}



##4.CENTYMETRY

#centymetry na podstawowe jednostki systemu metrycznego

cm_mm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "centymetrów to",
             x*10,"milimetrów")
}

cm_cm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "centymetrów to" ,
             x*1, "centymetrów")
}

cm_dcm <-function(x){ 
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "centymetrów to",
             x*0.1, "decymetrów")
}

cm_m <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*0.01,"metrów")
}

cm_km <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*1.0000e-5,"kilometra")
}


#centymetr na miary anglosaskie

cm_inc <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*0.3937,"cali.")
}

cm_ft <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*0.0328,"stóp")
}

cm_yd  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*0.0109,"jardów")
}

cm_mi <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*6.2137e-6,"mili")
}

m_li <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*2.0712e-6,"ligi.")
}


#centymetr na miary morskie

cm_kl <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*5.3996e-5,"kabla.")
}

cm_NM  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*5.3996e-6,"mili morskiej.")
}


cm_lm  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "decymetrów to",
             x*1.7999e-6,"ligi morskiej.")
}


##5. MILIMETRY

#milimetry na podstawowe jednostki systemu metrycznego

mm_mm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "milimetrów to",
             x*1,"milimetrów.")
}

mm_cm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "milimetrów to",
             x*0.1,"centymetrów.")
}

mm_dcm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "milimetrów to",
             x*0.01,"decymetrów.")
}


mm_m <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "milimetrów to",
             x*0.001,"metrów.")
}

mm_km <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "milimetrów to",
             x*1.0000e-6,"kilometra.")
}

#milimetry na miary anglosaskie

mm_inc <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "milimetrów to",
             x*0.0394,"cali.")
}

mm_ft <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "milimetrów to",
             x*0.0033,"stóp.")
}

mm_yd <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "milimetrów to",
             x*0.0011,"jardów.")
}

mm_mi <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "milimetrów to",
             x*6.2137e-7,"mili.")
}

mm_li <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "milimetrów to",
             x*2.0712e-7,"ligi")
}

#milimetry na miary morskie

mm_kl <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "milimetrów to",
             x*5.3996e-6,"kabla.")
}


mm_NM <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "milimetrów to",
             x*5.3996e-7,"mili morskiej")
}

mm_lm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "milimetrów to",
             x*1.7999e-7,"ligi morskiej.")
}



###KONWERSJA MIAR ANGLOSASKICH

##1.CALE

#cale na podstawowe jednostki systemu metrycznego

inc_mm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "cali to",
             x*25.4,"milimetrów.")
}


inc_cm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "cali",
             x*2.54,"centymetrów.")
}


inc_dcm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "cali",
             x*0.254,"decymetrów.")
}

inc_m <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "cali",
             x*0.0254,"metrów.")
}

inc_km <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "cali",
             x*2.5400e-5,"kilometra.")
}

#cale na miary anglosaskie

inc_inc <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "cali",
             x*1,"cali.")
}

inc_ft <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "cali",
             x*0.0833,"stóp.")
}

inc_yd <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "cali",
             x*0.0278,"jardów.")
}

inc_mi <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "cali",
             x*1.5783e-5,"mili.")
}

inc_li <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "cali",
             x*5.2609e-6,"ligi.")
}

#cale na miary morskie

inc_kl <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "cali",
             x*0.0001,"kabla.")
}

inc_NM <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "cali",
             x*1.3715e-5,"mili morskich.")
}

inc_lm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "cali",
             x*4.5716e-6,"lig morskich.")
}

##2.STOPA

#stopa na podstawowe jednostki systemu metrycznego

ft_mm  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "stóp",
             x*304.8,"milimetrów.")
}

ft_cm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "stóp",
             x*30.48,"centymetrów.")
}

ft_dcm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "stóp",
             x*3.048,"decymetrów.")
}

ft_m <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "stóp",
             x*0.3048,"metrów.")
}

ft_km  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "stóp",
             x*0.0003,"kilometrów.")
}

#stopa na miary anglosaskie

ft_inc <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "stóp",
             x*12,"cali.")
}

ft_ft <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "stóp",
             x*1,"stóp.")
}

ft_yd <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "stóp",
             x*0.3333,"jardów.")
}

ft_mi <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "stóp",
             x*0.0002,"mil.")
}

ft_li <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "stóp",
             x*6.3131e-5,"ligi.")
}

#stopa na miary morskie

ft_kl <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "stóp",
             x*0.0016,"kabla.")
}

ft_NM <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "stóp",
             x*0.0002,"mil morskich.")
}

ft_lm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "stóp",
             x*5.4860e-5,"ligi morskiej.")
}

##3.JARD

#jard na podstawowe jednostki systemu metrycznego

yd_mm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "jardów",
             x*914.4,"milimetrów.")
}

yd_cm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "jardów",
             x*91.44,"centymetrów.")
}


yd_dcm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "jardów",
             x*9.144,"decymetrów.")
}


yd_m <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "jardów",
             x*0.9144,"metrów.")
}


yd_km <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "jardów",
             x*0.0009,"kilometrów.")
}


#jard na miary anglosaskie

yd_inc <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "jardów",
             x*36,"cali.")
}


yd_ft <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "jardów",
             x*3,"stóp.")
}


yd_yd <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "jardów",
             x*1,"jardów.")
}


yd_mi <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "jardów",
             x*0.0006,"mil.")
}


yd_li <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "jardów",
             x*0.0002,"ligi.")
}


#jard na miary morskie

yd_kl <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "jardów",
             x*0.0049,"kabla.")
}


yd_NM <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "jardów",
             x*0.0005,"mil morskich.")
}


yd_lm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "jardów",
             x*0.0002,"lig morskich.")
}



##4.MILA

#mila na podstawowe jednostki systemu metrycznego

mi_mm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil",
             x*1.6093e+6,"milimetrów.")
}


mi_cm  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil",
             x*1.6093e+5,"centymetrów")
}

mi_dcm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil",
             x*16093.44,"decymetrów")
}

mi_m <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil",
             x*1609.344,"metrów")
}

mi_km <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil",
             x*1.6093,"kilometrów.")
}

#mila na miary anglosaskie

mi_inc <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil",
             x*63360,"cali.")
}

mi_ft <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil",
             x*5280,"stóp.")
}

mi_yd <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil",
             x*1760,"jardów.")
}

mi_mi <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil",
             x*1,"mil")
}

mi_li <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil",
             x*0.3333,"lig.")
}

# mila na miary morskie

mi_kl <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil",
             x*8.6898,"kabli.")
}

mi_NM <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil",
             x*0.869,"mil morskich.")
}

mi_lm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil",
             x*0.2897,"lig morskich.")
}

#5.LIGA

#liga na podstawowe jednostki systemu metrycznego

li_mm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig",
             x*4.8280e+6,"milimetrów")
}

li_cm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig",
             x*4.8280e+5,"centymetrów")
}


li_dcm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig",
             x*48280.32,"decymetrów")
}


li_m <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig",
             x*4828.032,"metrów")
}


li_km <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig",
             x*4.828,"kilometrów")
}


#liga na miary anglosaskie

li_inc <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig",
             x*1.9008e+5,"cali.")
}


li_ft <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig",
             x*15840,"stóp.")
}

li_yd <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig",
             x*5280, "jardów.")
}

li_mi <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig",
             x*3,"mili.")
}

li_li <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig",
             x*1,"lig.")
}

#liga na miary morskie

li_kl <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig",
             x*26.0693,"kabli")
}

li_NM <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig",
             x*2.6069,"mil morskich.")
}


li_lm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig",
             x*0.869,"lig morskich")
}



###KONWERSJA MIAR MORSKICH

##1.KABEL

#konwersja na podstawowe jednostki systemu metrycznego

kl_mm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kabli",
             x*1.8520e+5,"milimetrów")
}


kl_cm  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kabli",
             x*18520,"centymetrów")
}

kl_dcm  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kabli",
             x*1852,"decymetrów")
}

kl_m  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kabli",
             x*185.2,"metrów")
}

kl_km <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kabli",
             x*0.1852,"kilometrów")
}

#konwersja na miary anglosaskie

kl_inc  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kabli",
             x*7291.3386,"cali")
}

kl_ft  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kabli",
             x*607.6115,"stóp.")
}


kl_yd <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kabli",
             x*202.5372,"jardów.")
}


kl_mi  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kabli",
             x*0.1151,"mil.")
}


kl_li  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kabli",
             x*0.0384,"lig")
}


#konwersja na miary morskie

kl_kl <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kabli",
             x*1,"kabli.")
}


kl_NM <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kabli",
             x*0.1,"mil morskich.")
}


kl_lm  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "kabli",
             x*0.0333,"lig morskich.")
}



##2.MILA MORSKA

#konwersja mil morskich na podstawowe jednostki systemu metrycznego

NM_mm  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil morskich",
             x*1.8520e+6,"milimetrów")
}


NM_cm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil morskich",
             x*1.8520e+5,"centymetrów")
}

NM_dcm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil morskich",
             x*18520,"decymetrów")
}

NM_m <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil morskich",
             x*1852,"metrów")
}

NM_km <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil morskich",
             x*1.852,"kilometrów")
}

#konwersja mil morskich na miary anglosaskie

NM_inc <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil morskich",
             x*72913.3858,"cali.")
}

NM_ft  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil morskich",
             x*6076.1155,"stóp.")
}

NM_yd  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil morskich",
             x*2025.3718,"jardów.")
}

NM_mi  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil morskich",
             x*1.1508,"mil.")
}

NM_li  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil morskich",
             x*0.3836,"lig.")
}

#konwersja mil morskich na miary morskie

NM_kl <- function(x){
  x*10
} <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil morskich",
             x*10,"kabli.")
}

NM_NM <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil morskich",
             x*1,"mil morskich.")
}

NM_lm <- function(x){
  x*0.3333
} <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "mil morskich",
             x*6076.1155,"lig morskich.")
}

##3.LIGA MORSKA

#konwersja ligi morskiej na podstawowe jednostki systemu metrycznego

lm_mm  <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig morskich",
             x*5.5560e+6,"milimetrów")
}

lm_cm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig morskich",
             x*5.5560e+5,"centymetrów")
}

lm_dcm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig morskich",
             x*55560,"decymetrów")
}

lm_m <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig morskich",
             x*5556,"metrów")
}

lm_km <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig morskich",
             x*5.556,"kilometrów")
}

#konwersja ligi morskiej na miary anglosaskie

lm_inc <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig morskich",
             x*2.1874e+5,"cali.")
}

lm_ft <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig morskich",
             x*18228.3465,"cali.")
}

lm_yd <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig morskich",
             x*6076.1155,"jardów.")
}

lm_mi <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig morskich",
             x*3.4523,"mil.")
}

lm_li <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig morskich",
             x*1.1508,"lig.")
}

#konwersja ligi morskiej na miary morskie

lm_kl <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig morskich",
             x*30,"kabli")
}

lm_NM <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig morskich",
             x*3,"mil morskich.")
}

lm_lm <- function(x){
  if(x<0){
    stop("Dlugość musi być wartością dodatnią!")
  }
  else if(!(is.numeric(x))){
    stop("Podaj wartość numeryczną!")
  }
  else paste(x, "lig morskich",
             x*1,"lig morskich.")
}








