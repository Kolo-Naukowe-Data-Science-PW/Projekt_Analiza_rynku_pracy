source("SEGMENTACJA_OGLOSZEN.R")
setwd("~/Projekty/Pracuj.pl/github/Projekt_Analiza_rynku_pracy/")
#install.packages("pbapply")
library(XML)
library(RSQLite)
library(stringi)
library(pbapply)  # tylko po to, aby w lapply było widać postęp
library(magrittr) # operator %>%


baza <- dbConnect(SQLite(), "oferty_lang.db")
opisy <- dbGetQuery(baza, "select [index] as id, opis from oferty where lang = 'pl'")$opis
dbDisconnect(baza)

segmenty <- pblapply(opisy, f) 

# save(segmenty, file = "segmenty.rda")
# load("segmenty.rda")

indeksy <- unlist(mapply(function(i, seg) {
   rep(i, length(seg))
}, 1:length(segmenty), segmenty))

segmenty2 <- unlist(segmenty)

nazwy <- unlist(lapply(segmenty, names))
nazwy[is.na(nazwy)] <- ""

nazwy <- stri_replace_all_regex(nazwy, "[^\\w|\\s]", " ")
nazwy <- stri_replace_all_regex(nazwy, "\\s+", " ")
nazwy <- stri_trans_tolower(nazwy)
nazwy <- stri_trim(nazwy)


naglowki_oferta <- c("oferujemy", "oferta", "nasz klient oferuje",
                     "co oferujemy", "zapewniamy", "w zamian oferujemy",
                     "firma oferuje", "kandydatom oferujemy", "klient oferuje",
                     "wybranym kandydatom oferujemy", "lient oferuje",
                     "możemy ci zaoferować", "naszym pracownikom oferujemy", 
                     "oferujemy ci", "korzyści", "wybranym osobom oferujemy",
                     "gwarantujemy", "co możemy ci zaoferować",
                     "w zamian za twoje zaangażowanie oferujemy",
                     "naszym pracownikom zapewniamy", "to co możemy ci zaoferować")


naglowki_wymagania <- c("wymagania", "od kandydatów oczekujemy", "oczekujemy",
                        "oczekiwania", 
                        "nasze oczekiwania", "oczekujemy od ciebie", 
                        "nasze wymagania", "wymagamy", "profil kandydata",
                        "jeżeli", "jeśli", "oczekiwania wobec kandydatów",
                        "od kandydata oczekujemy", "jeżeli jesteś osobą która",
                        "czego oczekujemy", "czego od ciebie oczekujemy", 
                        "poszukujemy osób", "od wybranej osoby oczekujemy",
                        "od kandydatek kandydatów oczekujemy", 
                        "profil idealnego kandydata", "kogo szukamy",
                        "jeśli posiadasz", "jeśli zatem", 
                        "dołącz do nas jeśli", "wymagania wobec kandydatów",
                        "aplikuj jeśli", "poszukujemy", 
                        "oczekiwania wobec kandydata")


naglowki_zadania <- c("opis stanowiska", "zakres obowiązków", "zadania",
                      "obowiązki", "główne zadania", 
                      "osoba zatrudniona na tym stanowisku będzie odpowiedzialna za",
                      "zakres zadań", "główne obowiązki", "zakres odpowiedzialności",
                      "będziesz odpowiedzialny za",
                      "osoba na tym stanowisku będzie odpowiedzialna za",
                      "zadania na stanowisku", "twoje zadania", 
                      "podstawowe obowiązki", 
                      "osoba zatrudniona na tym stanowisku odpowiedzialna będzie za",
                      "opis stanowiska pracy", "charakterystyka pracy",
                      "mamy dla ciebie pracę która polega na", 
                      "wyzwania które na ciebie czekają", 
                      "wybrana osoba będzie odpowiedzialna za",
                      "do twoich zadań należeć będzie",
                      "główne zadania na stanowisku",
                      "do twojego zakresu obowiązków należeć będzie",
                      "zatrudniona osoba będzie odpowiedzialna za",
                      "do twoich zadań należeć będzie w szczególności",
                      "kluczowe obowiązki", 
                      "mamy dla ciebie ofertę pracy która polega na",
                      "czym się będziesz zajmować",
                      "do zakresu obowiązków na w w stanowisku należy",
                      "twoją rolą będzie", "czym będziesz się zajmować",
                      "do twoich obowiązków będzie należało",
                      "pożądany profil")



naglowki_atuty <- c("mile widziane", 
                    "dodatkowym atutem będzie", "dodatkowe atuty", 
                    "dodatkowymi atutami będą", "kwalifikacje będące dodatkowym atutem",
                    "dodatkowym atutem będą", "twoimi dodatkowymi atutami będą",
                    "twoim dodatkowym atutem będzie", "twoim dodatkowym atutem jest",
                    "twoje atuty to", "dodatkowy atut", 
                    "kwalifikacje będące dodatkowym atutem mile widziane", 
                    "dodatkowym atutem będzie znajomość", 
                    "co będzie dodatkowym atutem", "twoim atutem będzie", 
                    "atutami będą", "co będzie twoim dodatkowym atutem")
                      

oferty <- (nazwy %in% naglowki_oferta)
wymagania <- (nazwy %in% naglowki_wymagania)
atuty <- (nazwy %in% naglowki_atuty)
zadania <- (nazwy %in% naglowki_zadania)


baza_in <- dbConnect(SQLite(), "oferty_lang.db")
ramka_in <- dbGetQuery(baza_in, "select * from oferty where lang == 'pl'")
dbDisconnect(baza_in)

baza_out <- dbConnect(SQLite(), "oferty_segmenty.db")

kolumny <- paste(c(colnames(ramka_in), "oferta", "wymagania", "atuty", "zadania"),
                 c("integer", rep("text", 14)), 
                 sep = " ", collapse = ", ")

kolumny <- gsub("index", "id", kolumny)

dbSendQuery(baza_out, paste0("create table oferty (",
                             kolumny,
                             ")"))

for (i in 1:length(segmenty)) {
    ktore <- which(indeksy == i)
    of <- paste(segmenty[[i]][oferty[ktore]], collapse = "\n")
    wym <- paste(segmenty[[i]][wymagania[ktore]], collapse = "\n")
    at <- paste(segmenty[[i]][atuty[ktore]], collapse = "\n")
    zad <- paste(segmenty[[i]][zadania[ktore]], collapse = "\n")

    query <- paste0("insert into oferty values (",
                    paste(rep(c("?"), 15 ), collapse = ", "),
                    ")")
    dbSendPreparedQuery(baza_out, query,
                        cbind(ramka_in[i, ], 
                              oferta = of, 
                              wymagania = wym, 
                              atuty = at, 
                              zadania = zad))
    
    cat("\r", i, " / ", length(segmenty))
}

dbGetQuery(baza_out, "select count(*) from oferty where oferta <> ''")
dbGetQuery(baza_out, "select count(*) from oferty where wymagania <> ''")
dbGetQuery(baza_out, "select count(*) from oferty where atuty <> ''")
dbGetQuery(baza_out, "select count(*) from oferty where zadania <> ''")


dbDisconnect(baza_out)

# baza <- dbConnect(SQLite(), "oferty_lang.db")
# firmy <- dbGetQuery(baza, "select [index] as id, firma from oferty where lang = 'pl'")$firma
# dbDisconnect(baza)
# 
# 
# opisy_firm <- mapply(function(seg, firma) {
#     if (length(seg) == 0) return(NULL)  
#     if (stri_length(firma) == 0) return(rep(FALSE, length(seg)))
#     
#     stri_detect_fixed(seg, firma) & (names(seg) == "")
# }, segmenty, firmy)
# 
# opisy_firm <- unlist(opisy_firm)




# 
# klasa <- mapply(function(o, w, a, z, f) {
#     ktory <- which(c(o,w,a,z,f))[1]
#     if (is.na(ktory)) 0
#     else ktory
# }, oferty, wymagania, atuty, zadania, opisy_firm)
# 
# 
# 
# 
# library(data.table)
# 
# tworz.lematyzacje <- function(unikalneSlowa) {
# 
#     slowa <- fread("spotkanie_28_04/polimorf2.txt",
#                    header = FALSE, encoding = "UTF-8")
# 
#     suppressWarnings(slowa <- slowa[V1 %in% unikalneSlowa])
# 
#     # pierwsza kolumna jako klucz
#     setkey(slowa, "V1")
# 
#     function(tekst) {
# 
#         tekst <- stri_split_regex(tekst, "\\W", omit_empty = TRUE)[[1]]
# 
#         suppressWarnings(lemat <- slowa[tekst, V2])
# 
#         puste <- is.na(lemat)
#         lemat[puste] <- tekst[puste]
#         paste(lemat, collapse = " ")
#     }
# }
# 
# 
# segmenty2 <- stri_trans_tolower(segmenty2)
# 
# unikalneSlowa <- unique(unlist(stri_split_regex(segmenty2, "\\W")))
# 
# lematyzacja <- tworz.lematyzacje(unikalneSlowa)
# segmenty2 <- sapply(segmenty2, lematyzacja)
# segmenty2 <- stri_replace_all_regex(segmenty2, "[0-9]+", " ")
# segmenty2
# segmenty2 <- stri_replace_all_regex(segmenty2, "\\s+", " ")
# 
# 
# # save(segmenty2, klasa, file = "segmenty2.rda")
# 
# # load("segmenty2.rda")
# 
# 
# 
# 
# library("tm")
# 
# 
# 
# korpus <- VCorpus(VectorSource(segmenty2))
#            
# stopWords <- read.table("spotkanie_28_04/stopWords.txt", stringsAsFactors = FALSE)[,1]
# 
# korpus <- tm_map(korpus, removeWords, stopWords)
# 
# 
# macierz <- DocumentTermMatrix(korpus, control = list(weighting = weightBin))
# 
# 
# # save(macierz, segmenty2, klasa, file = "macierz.rda")
# # load("macierz.rda")
# 
# macierz <- removeSparseTerms(macierz, 0.997)
# 
# table(klasa)
# tren <- (klasa != 0)
# 
# klasa[klasa == 3] <- 2
# 
# 
# macierz_tren <- macierz[tren, ]
# macierz_test <- macierz[!tren, ]
# rm(macierz)
# 
# 
# library("e1071")
# 
# 
# 
# 
# 
# bay1 <- naiveBayes(as.matrix(macierz_tren), factor(klasa[tren] == 1),
#                    laplace = 1)
# bay2 <- naiveBayes(as.matrix(macierz_tren), factor(klasa[tren] == 2),
#                    laplace = 1)
# bay4 <- naiveBayes(as.matrix(macierz_tren), factor(klasa[tren] == 4),
#                    laplace = 1)
# 
# 
# ind <- sample(nrow(macierz_test), 10)
# 
# pred1 <- predict(bay1, as.matrix(macierz_test[ind, ]))
# pred2 <- predict(bay2, as.matrix(macierz_test[ind, ]))
# pred4 <- predict(bay4, as.matrix(macierz_test[ind, ]))
# 
# 
# for (k in 1:10) {
#     cat(as.character(pred1[k]),
#         as.character(pred2[k]),
#         as.character(pred4[k]),
#         names(segmenty2[!tren][ind][k]), "\n", sep = "\n")
# }

# oferty, wymagania, atuty, zadania, opisy_firm

