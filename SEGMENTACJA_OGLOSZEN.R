library(XML)
library(RSQLite)
library(stringi)

con = dbConnect(SQLite(),"F:/Norbert/studia/KoloNaukoweDS/Projekty/Analiza_rynku_pracy/oferty.db")
dbListFields(con,"oferty")
x <- dbGetQuery(con,"select opis from oferty limit 5000")[[1]]

convert_and_prepare <- function(x){
  
  x <- as(x,"character")
  x <- stri_replace_all_regex(x,"(?<=\\w)/"," ")
  x <- stri_replace_all_regex(x,"</li>[^<]{0,10}</ul>[^<]{0,10}<ul>[^<]{0,10}<li>","</li><li>",multiline=TRUE)
  x <- stri_replace_all_regex(x,"&#13;|&amp;"," ")
  x
}

for(i in 1:100){
 doc = htmlTreeParse(x[i], useInternalNodes = T,encoding = 'UTF8')
 
 div <- unlist(lapply(getNodeSet(doc, "//div"), convert_and_prepare ))
 
 lists <- unlist(stri_extract_all_regex(div[1],"(?<=(<ul>))[\\w\\W]+?(?=(</ul>))",multiline=TRUE))
 
 #wyciaganie naglowkow list - nie dziala
 #lists_headres <- stri_trim_both(unlist(
 #   stri_extract_all_regex(
 #    div[1]
 #     ,"(?<=(>))\\s*[^>]+?;?:?\\s*(?=((<[^>]+?>\\s*)*?<ul>))",multiline=TRUE)
 # ))
 # lists_headres <- lists_headres[!is.na(lists_headres)]
 # lists_headres <- lists_headres[stri_length(lists_headres)>3] #dla bezpieczenstwa zeby jakis smiec nie wpadl
 
 
 div <- div[stri_count_fixed(div,"<div")==1]
 
 #print(i)
 #print(lists_headres)
 #stopifnot(length(lists_headres)==length(lists))
 
 end <- which(stri_detect_regex(div,"<ul>|<div id=\"clause\"|<div id=\"contact\"|class=\"clause\"|1997")) 
 # po divach, w ktorych znajduje sie cos takiego juz nigdy nic wartosciowego nie ma 
 if(length(end)>0){
   div <- div[1:min(end)-1] #bierzemy wszystkie znaczniki poprzedzajace bez tych w ktorych juz nic nie ma 
 }
 div <- stri_replace_all_regex(div,"<ul>(.|\\s)+?</ul>","",multiline = TRUE)
 div <- stri_replace_all_regex(div,"<(.|\\s)+?>","")
 div <- stri_replace_all_regex(div,"\\W+"," ")
 div <- div[stri_length(div)>150]
 div <- div[!stri_detect_regex(div,"CV|aplikowania|aplikacji|aplikację|motywacyjnego|Aplikuj|Aplikujpraca|Ustawą")]
 
 #print(lists)
}


dbDisconnect(con)


# smieci



#if(is.na(lists)){ lists = character(0)}
#lists <- unlist(lapply(getNodeSet(div[1], "//ul"), convert_and_prepare)) #konwersja do napisu


#czy_sa_zagniezdzone <- stri_detect_fixed(lists,"<ul>")
#if(any(czy_sa_zagniezdzone)){
#  ind <- which(czy_sa_zagniezdzone)
#  for(a in ind){
#     lista_zagniezdzona <- unlist(stri_extract_all_regex(lists[a],"(?<=(<ul>))[\\w\\W]+?(?=(</ul>))",multiline=TRUE))
#    lists[a] <- stri_replace_all_regex(lists[a],"<ul>[\\w\\W]+","",multiline = TRUE)
#     lists[a] <- stri_paste(lists[a],lista_zagniezdzona)
#   }
# }