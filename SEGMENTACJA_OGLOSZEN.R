library(XML)
library(RSQLite)
library(stringi)

# funkcja pomocnicza 
convert_and_prepare <- function(x){
  
  x <- as(x,"character") # konwersja 
  x <- stri_replace_all_regex(x,"&#13;|&amp;"," ") #usuwamy smieci
  x <- stri_replace_all_regex(x,"(?<=\\w)/"," ") #usuwamy znak "/" (gdy wystepuje w teksie, a nie wewnatrz < >), bo przez ten znak funkcje sie wywalaja
  x
}

# funkcja wlasciwa

f <- function(opis, min_length = 150) {

# funkcja zwraca wektor segmentow wydzielonych z ogloszenia
#
# opis - tresc ogloszenia (ta ktora znajduje sie w bazie)
# min_length - wyciagamy segmenty, ktore maja przynajmniej taka dlugosc
  
  opis <- stri_replace_all_regex(opis, "<li[^>]*?>", "<li> ") #trik po to zeby nie sklejaly sie potem slowo
  
  if(stri_count_fixed(opis,"<ul>") > stri_count_fixed(opis,"<li>")/2){ 
    #jesli liczba list jest duza w stosunku do liczby punktow w listach,
    #co moze oznaczac dwie sytuacje:
    # - albo jedna lista odpowiada jednemu segmentowi i to jest ok, tylko ktos np zrobil listy jednopunktowe i w tym jednym punkcie wpisal tekst ciagly
    # - albo ktos bez sensu z kazdego punktu robil liste i zamiast w jednej liscie, to sekcja (np. "wymagania") jest w kilku listach 
    # to wowczas sklejamy listy, ktore sa tuz obok siebie (nie dalej niz 8 znakow)
    # bo przyjmujemy ze jezeli sa bardzo blisko to to bedzie ten drugi przypadek:
    opis <- stri_replace_all_regex(opis,"</li>[^<]{0,10}</ul>[^<]{0,8}<ul>[^<]{0,10}<li>","</li><li>",multiline=TRUE)
  }
  
  doc <- htmlTreeParse(opis, useInternalNodes = TRUE, encoding = 'UTF-8')
  root <- xmlRoot(doc)
  
  # pierwszy tag zawierający tekst, który poprzedza jakąś niezagnieżdżoną listę
  xpath <- "//ul[not(ancestor::ul)]/preceding::*[text()][1]"
  
  nodes <- getNodeSet(root, xpath)
  nazwy <- sapply(nodes, xmlValue)
  #removeNodes(nodes)
  
  nodes <- getNodeSet(root, "//ul[not(ancestor::ul)]") #nie wycigamy list zagniezdzonych
  listy <- sapply(nodes, xmlValue)
  removeNodes(nodes)
  
  listy <- structure(listy, names = c(nazwy))
  
  
  div <- unlist(lapply(getNodeSet(doc, "//div"), convert_and_prepare ))
  div <- div[stri_count_fixed(div,"<div")==1] # bierzemy tylko "liscie" - czyli divy ktore nie zawieraja w sobie divow
  # w ten sposob oczywisce utracimy czesc tekstow, ale jakos to dziala
  
  end <- which(stri_detect_regex(div,"<div id=\"clause\"|<div id=\"contact\"|class=\"clause\"|1997")) 
  # po divach, w ktorych znajduje sie cos takiego juz nigdy nic wartosciowego nie ma,
  # wiec zostawiamy tylko te divy ktore poprzedzaja divy zawierajace cos z powyzszego
  if(length(end)>0){
    div <- div[1:min(end)-1] #bierzemy wszystkie znaczniki poprzedzajace bez tych w ktorych juz nic nie ma 
  }
  
  #czyszczenie:
  div <- stri_replace_all_regex(div,"<(.|\\s)+?>","")
  div <- stri_replace_all_regex(div,"\\W+"," ")
  # zostawiamy tylko dlugie teksty zeby nie zbierac smieci:
  div <- div[stri_length(div)>min_length]
  # wyrzucamy teksty ktore sa tekstem na temat rekrutacji
  div <- div[!stri_detect_regex(div,"CV|aplikowania|aplikacji|aplikację|motywacyjnego|Aplikuj|Aplikujpraca|Ustawą")]
  
  
  c(div, listy)
  
  
}


