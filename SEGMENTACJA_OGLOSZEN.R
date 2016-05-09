library(XML)
library(RSQLite)
library(stringi)

convert_and_prepare <- function(x){
  
  x <- as(x,"character") # konwersja 
  x <- stri_replace_all_regex(x,"&#13;|&amp;"," ") #usuwamy smieci
  x <- stri_replace_all_regex(x,"(?<=\\w)/"," ") #usuwamy znak "/" (gdy wystepuje w teksie, a nie wewnatrz < >), bo przez ten znak funkcje sie wywalaja
  
  if(stri_count_fixed(x,"<ul>") == stri_count_fixed(x,"<li>")/2){ 
    #jesli liczba list jest duza w stosunku do liczby punktow
    #co moze oznaczac dwie sytuacje:
    # - albo jedna lista odpowiada jednemu segmentowi i to jest ok, tylko ktos np zrobil listy jednopunktowe i w tym jednym punkcie wpisal tekst ciagly
    # - albo ktos bez sensu z kazdego punktu robil liste i zamiast w jendej liscie to sekscja (np. "wymagania") jest w kilku listach 
    # to sklejamy listy, ktore sa tuz obok siebie - nie dalej niz 8 znakow
    # bo zakladamy ze jezeli sa bardzo blisko to to bedzie ten drugi przypadek:
    x <- stri_replace_all_regex(x,"</li>[^<]{0,10}</ul>[^<]{0,8}<ul>[^<]{0,10}<li>","</li><li>",multiline=TRUE)
  }
  
  # rozwiazujemy problem zagniezdzonych list -
  # punkty z zagniezdzonej listy sa zamieniane na dodatkowe punkty list zewnetrzej
  x <- stri_replace_all_regex(x,"<li><ul>|</ul></li>","") # <li><ul>|</ul></li> trzeba poprawic tak zeby obslugiwalo:
  #<li><span style=\"font-size:10pt;\">Minimum 3 years of experience in financial services in one of following areas: </span><ul><li>
  x
}


f <- function(opis, min_length = 150) {
  
  opis <- stri_replace_all_regex(opis, "<li[^>]*?>", "<li> ")
  doc <- htmlTreeParse(opis, useInternalNodes = TRUE, encoding = 'UTF-8')
  root <- xmlRoot(doc)
  
  # pierwszy tag zawieraj¹cy tekst, który poprzedza jak¹œ niezagnie¿d¿on¹ listê
  xpath <- "//ul[not(ancestor::ul)]/preceding::*[text()][1]"
  
  nodes <- getNodeSet(root, xpath)
  nazwy <- sapply(nodes, xmlValue)
  #removeNodes(nodes)
  
  nodes <- getNodeSet(root, "//ul[not(ancestor::ul)]")
  listy <- sapply(nodes, xmlValue)
  removeNodes(nodes)
  
  listy <- structure(listy, names = c(nazwy))
  
  
  div <- unlist(lapply(getNodeSet(doc, "//div"), convert_and_prepare ))
  div <- div[stri_count_fixed(div,"<div")==1] # bierzemy tylko "liscie" 
  
  end <- which(stri_detect_regex(div,"<div id=\"clause\"|<div id=\"contact\"|class=\"clause\"|1997")) 
  # po divach, w ktorych znajduje sie cos takiego juz nigdy nic wartosciowego nie ma 
  if(length(end)>0){
    div <- div[1:min(end)-1] #bierzemy wszystkie znaczniki poprzedzajace bez tych w ktorych juz nic nie ma 
  }
  
  div <- stri_replace_all_regex(div,"<(.|\\s)+?>","")
  div <- stri_replace_all_regex(div,"\\W+"," ")
  div <- div[stri_length(div)>min_length]
  div <- div[!stri_detect_regex(div,"CV|aplikowania|aplikacji|aplikacjê|motywacyjnego|Aplikuj|Aplikujpraca|Ustaw¹")]
  
  
  c(div, listy)
  
  
}


