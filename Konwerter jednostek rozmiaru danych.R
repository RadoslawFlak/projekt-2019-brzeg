# Program do przeliczania rozmiaru danych bazujący na 6 powszechnych jednostkach.
#
# Legenda:
#  B  - Bajt
# KB  - Kilobajt  (Kibibajt -KiB)
# MB  - Megabajt  (Mebibajt -MiB)
# GB  - Gigabajt  (Gibibajt -GiB)
# TB  - Terabajt  (Tebibajt -TiB)
# PB  - Petabajt  (Pebibajt -PiB)
#
# WAŻNE: Wartości są obliczane według binarnego systemu, stosowanego w komputerach. W teorii obliczamy inne przedrostki (widniejące w nawiasach wyżej), które chociaż istnieją, nie przyjęły się w codziennym życiu.
# Odnośnie powyższego komunikatu - Nowe nazwy zostały opracowane przez IEC w 1997 lecz ze względu na małe przebicie JEDEC zmieniło ponownie znaczenia słów (ale jedynie w standardach pamięci)
# Inna rzecz warta odnotowania, to fakt, że to właśnie z powodu tego 'konfliktu' nazw kupując dysk SSD np. 250Gb w rzeczywistości mamy mniej niż oczekiwane 250GB.
#
# Aby wszystko zadziałało, należy przejść do podsekcji Program (127 wers) i zastosować się do polecenia.
#
# Funkcje:
{
options(scipen=999)
Typ = function(TypF){
  if(grepl("KB", TypF, fixed=TRUE)) {
    print("Wybrałeś jednostkę: kilobajty")
    list = unlist(strsplit(TypF, ""))
    K = match('K', list)
    B = match('B', list)
    rozmiar = list[-c(K, B)]
    string_rozmiar = paste(rozmiar, collapse='')
    numeric_rozmiar = as.numeric(string_rozmiar)
    cat(paste("Przeliczasz: ", numeric_rozmiar, "KB","\n",
              "\n",
              "Otrzymane wartości:", "\n",
              "    Bajtów: ", numeric_rozmiar*(2^10),"B", "\n",
              "Megabajtów: ", numeric_rozmiar/(2^10),"MB", "\n",
              "Gigabajtów: ", numeric_rozmiar/(2^20),"GB", "\n",
              "Terabajtów: ", numeric_rozmiar/(2^30),"TB", "\n",
              "Petabajtów: ", numeric_rozmiar/(2^40),"PB", sep=""))
  }
  else if(grepl("MB", TypF, fixed=TRUE)) {
    print("Wybrałeś jednostkę: megabajty")
    list = unlist(strsplit(TypF, ""))
    a = match('M', list)
    b = match('B', list)
    rozmiar = list[-c(a, b)]
    string_rozmiar = paste(rozmiar, collapse='')
    numeric_rozmiar = as.numeric(string_rozmiar)
    cat(paste("Przeliczasz: ", numeric_rozmiar, "MB","\n",
              "\n",
              "Otrzymane wartości:", "\n",
              "    Bajtów: ", numeric_rozmiar*(2^20),"B", "\n",
              "Kilobajtów: ", numeric_rozmiar*(2^10),"KB", "\n",
              "Gigabajtów: ", numeric_rozmiar/(2^10),"GB", "\n",
              "Terabajtów: ", numeric_rozmiar/(2^20),"TB", "\n",
              "Petabajtów: ", numeric_rozmiar/(2^30),"PB", sep=""))
  }
  else if(grepl("GB", TypF, fixed=TRUE)) {
    print("Wybrałeś jednostkę: gigabajty")
    list = unlist(strsplit(TypF, ""))
    a = match('G', list)
    b = match('B', list)
    rozmiar = list[-c(a, b)]
    string_rozmiar = paste(rozmiar, collapse='')
    numeric_rozmiar = as.numeric(string_rozmiar)
    cat(paste("Przeliczasz: ", numeric_rozmiar, "GB","\n",
              "\n",
              "Otrzymane wartości:", "\n",
              "    Bajtów: ", numeric_rozmiar*(2^30),"B", "\n",
              "Kilobajtów: ", numeric_rozmiar*(2^20),"KB", "\n",
              "Megabajtów: ", numeric_rozmiar*(2^10),"MB", "\n",
              "Terabajtów: ", numeric_rozmiar/(2^10),"TB", "\n",
              "Petabajtów: ", numeric_rozmiar/(2^20),"PB", sep=""))
  }
  else if(grepl("TB", TypF, fixed=TRUE)) {
    print("Wybrałeś jednostkę: terabajty")
    list = unlist(strsplit(TypF, ""))
    a = match('T', list)
    b = match('B', list)
    rozmiar = list[-c(a, b)]
    string_rozmiar = paste(rozmiar, collapse='')
    numeric_rozmiar = as.numeric(string_rozmiar)
    cat(paste("Przeliczasz: ", numeric_rozmiar, "TB","\n",
              "\n",
              "Otrzymane wartości:", "\n",
              "    Bajtów: ", numeric_rozmiar*(2^40),"B", "\n",
              "Kilobajtów: ", numeric_rozmiar*(2^30),"KB", "\n",
              "Megabajtów: ", numeric_rozmiar*(2^20),"MB", "\n",
              "Gigabajtów: ", numeric_rozmiar*(2^10),"GB", "\n",
              "Petabajtów: ", numeric_rozmiar/(2^10),"PB", sep=""))
  }
  else if(grepl("PB", TypF, fixed=TRUE)) {
    print("Wybrałeś jednostkę: petabajty")
    list = unlist(strsplit(TypF, ""))
    a = match('P', list)
    b = match('B', list)
    rozmiar = list[-c(a, b)]
    string_rozmiar = paste(rozmiar, collapse='')
    numeric_rozmiar = as.numeric(string_rozmiar)
    cat(paste("Przeliczasz: ", numeric_rozmiar, "PB","\n",
              "\n",
              "Otrzymane wartości:", "\n",
              "    Bajtów: ", numeric_rozmiar*(2^50),"B", "\n",
              "Kilobajtów: ", numeric_rozmiar*(2^40),"KB", "\n",
              "Megabajtów: ", numeric_rozmiar*(2^30),"MB", "\n",
              "Gigabajtów: ", numeric_rozmiar*(2^20),"GB", "\n",
              "Terabajtów: ", numeric_rozmiar*(2^10),"TB", sep=""))
  }
  else if(grepl("B", TypF, fixed=TRUE)) {
    print("Wybrałeś jednostkę: bajty")
    list = unlist(strsplit(TypF, ""))
    a = match('B', list)
    rozmiar = list[-c(a)]
    string_rozmiar = paste(rozmiar, collapse='')
    numeric_rozmiar = as.numeric(string_rozmiar)
    cat(paste("Przeliczasz: ", numeric_rozmiar, "B","\n",
              "\n",
              "Otrzymane wartości:", "\n",
              "Kilobajtów: ", numeric_rozmiar/(2^10),"KB", "\n",
              "Megabajtów: ", numeric_rozmiar/(2^20),"MB", "\n",
              "Gigabajtów: ", numeric_rozmiar/(2^30),"GB", "\n",
              "Terabajtów: ", numeric_rozmiar/(2^40),"TB", "\n",
              "Petabajtów: ", numeric_rozmiar/(2^50),"PB", sep=""))
  }
  else {
    print("Hola hola, coś poszło nie tak, czy aby na pewno podałeś odpowiedni rozmiar oraz rozwinięcie(jednostkę)?")
  }
}
}
# Program:
{
  
# Podaj rozmiar wraz z jednostką. (Example: "1024KB")
Typ("")
  
}
# Dziękujemy za korzystanie z naszych usług. 