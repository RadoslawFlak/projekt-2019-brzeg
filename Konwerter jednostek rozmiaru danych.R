# Program do przeliczania rozmiaru danych bazujący na 6 powszechnych jednostkach.
#
# Legenda:
# B   - Bajt
# KB  - Kilobajt
# MB  - Megabajt
# GB  - Gigabajt
# TB  - Terabajt
# PB  - Petabajt
#
# Funkcje:

# B = B
# KB = B^3
# MB = B^6
# GB = B^9
# TB = B^12
# PB = B^15
If (Typ = "B"){
  
  cat(paste("Dla", Rozmiar, Typ, "otrzymamy:",
            1000^(1/3), "KB",
            1000^(1/6), "MB",
            1000^(1/9), "GB",
            1000^(1/12), "TB",
            1000^(1/15), "PB"))
} else if(Typ = "KB"){
  cat(paste("Dla", Rozmiar, Typ, "otrzymamy:",
            1000^3, "KB",
            1000^(1/3), "MB",
            1000^(1/6), "GB",
            1000^(1/9), "TB",
            1000^(1/12), "PB"))
}


# Program:
# 1. Podaj jednostkę, którą chcesz przeliczyć
Typ = "B"
# 2. Podaj rozmiar:
Rozmiar = 1000

