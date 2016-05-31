

source("../SEGMENTACJA_OGLOSZEN.R")
library(XML)
library(RSQLite)
library(stringi)
library(magrittr)
library(data.table)
library(randomForest)




#' segmenty_lista - Lista wektorów
#' firmy - wektor zawierający nazwy firm
wybierz_opisy_firm <- function(segmenty_lista, firmy) {
    
    firmy <- sapply(firmy, function(x) unlist(stri_split_regex(x, "\\s+"))[1])
    
    opisy_firm <- mapply(function(seg, firma) {
        if (length(seg) == 0) return(NULL)
        if (stri_length(firma) == 0) return(rep(FALSE, length(seg)))
        
        stri_detect_fixed(seg, firma) & (names(seg) == "")
        
    }, segmenty_lista, firmy)
    
    opisy_firm <- unlist(opisy_firm)
    
    
    opisy_firm[is.na(opisy_firm)] <- FALSE
    
    opisy_firm
}




#' segmenty - wektor segmentów
#' naglowki_rda - ścieżka do pliku .rda z listą charakterystycznych nagłówków list 
#'      w poszczególnych klasach
#' opisy_firm - wektor logiczny wskazujący, które segmenty są opisami firm
wyznacz_klase <- function(segmenty, opisy_firm,  naglowki_rda = "naglowki.rda") {
    
    load(naglowki_rda)
    
    nazwy <- names(segmenty)
    nazwy[is.na(nazwy)] <- ""
    
    nazwy <- stri_replace_all_regex(nazwy, "[^\\w|\\s]", " ")
    nazwy <- stri_replace_all_regex(nazwy, "\\s+", " ")
    nazwy <- stri_trans_tolower(nazwy)
    nazwy <- stri_trim(nazwy)
    
    
    oferty <- (nazwy %in% naglowki$oferta)
    zadania <- (nazwy %in% naglowki$zadania)
    wymagania <- (nazwy %in% naglowki$wymagania)
    atuty <- (nazwy %in% naglowki$atuty)
    
    
    klasa <- apply(cbind(opisy_firm, oferty, zadania, wymagania, atuty), 1,
                   function(x) {
                       t <- which(x)
                       if (length(t) == 0) return(0)
                       return(t[1])
                   })
    
    c("","opis_firmy", "oferta", "zadania", "wymagania", "atuty")[klasa+1]
}




#' baza_odmian - scieżka do pliku "polimorf.txt"
#' podzbiór - jakie słowa wybrać z bazy_slow

tworz_lematyzacje <- function(baza_odmian, podzbior) {
    
    slowa <- fread(baza_odmian, header = FALSE, encoding = "UTF-8")
    
    suppressWarnings(slowa <- slowa[V1 %in% podzbior])
    
    # pierwsza kolumna jako klucz
    setkey(slowa, "V1")
    
    function(tekst) {
        
        tekst <- stri_split_regex(tekst, "\\W", omit_empty = TRUE)[[1]]
        
        suppressWarnings(lemat <- slowa[tekst, V2])
        
        puste <- is.na(lemat)
        lemat[puste] <- tekst[puste]
        paste(lemat, collapse = " ")
    }
}



najczestsze_w_klasach <- function(segmenty, klasa, ile = 100) {
   
    kategorie <- unique(klasa)
    kategorie <- kategorie[kategorie != ""]
    
    najczestsze <- lapply(kategorie, function(kat) {
        tab <- table(unlist(stri_split_regex(segmenty[klasa == kat], "\\W")))
        names(sort(tab, decreasing = TRUE)[1:100])
    })
    
    najczestsze <- unique(unlist(najczestsze))
    najczestsze[najczestsze != ""]
}




tworz_macierz_wystapien <- function(segmenty, klasa, 
                                    baza_odmian = "polimorf.txt") {
    
    segmenty <- stri_trans_tolower(segmenty)
    
    unikalne_slowa <- unique(unlist(stri_split_regex(segmenty, "\\W")))
    
    lematyzacja <- tworz_lematyzacje(baza_odmian, unikalne_slowa)
    segmenty <- sapply(segmenty, lematyzacja)
    
    segmenty <- stri_replace_all_regex(segmenty, "[0-9]+", " ")
    segmenty <- stri_replace_all_regex(segmenty, "\\s+", " ")
    
    najczestsze <- najczestsze_w_klasach(segmenty, klasa)
    
    macierz <- lapply(najczestsze, 
                      function(slowo) stri_detect_fixed(segmenty, slowo))
    
    macierz <- do.call(cbind, macierz)
}





time <- Sys.time()



baza <- dbConnect(SQLite(), "../oferty_lang.db")
opisy <- dbGetQuery(baza, "select opis from oferty where lang = 'pl'")$opis
firmy <- dbGetQuery(baza, "select firma from oferty where lang = 'pl'")$firma
dbDisconnect(baza)


#segmenty_lista <- lapply(opisy, f)
rm(opisy, baza)

# save(segmenty_lista, file = "segmenty_lista.rda")
load("segmenty_lista.rda")


indeksy <- unlist(mapply(function(i, seg) {
    rep(i, length(seg))
}, 1:length(segmenty_lista), segmenty_lista))


segmenty <- unlist(segmenty_lista)


opisy_firm <- wybierz_opisy_firm(segmenty_lista, firmy)


klasa <- wyznacz_klase(segmenty, opisy_firm)

# złaczenie atutów i wymagań
klasa2 <- klasa
klasa2[klasa2 == "atuty"] <- "wymagania"  


macierz <- tworz_macierz_wystapien(segmenty, klasa2)
macierz_tren <- macierz[klasa2 != "", ]
macierz_test <- macierz[klasa2 == "", ]
rm(macierz)

y <- factor(klasa2[klasa2 != ""])

las <- randomForest(x = macierz_tren, y = y,
                     xtest = macierz_test,
                     ntree = 100,
                     importance = FALSE, 
                     localImp = FALSE,
                     proximity = FALSE,
                     keep.forest = FALSE,
                     keep.inbag = FALSE)


pred <- las$test$predicted
save(pred, file = "pred.rda")

# for (k in sample(20000, 100)) {
#     cat(as.character(pred[k]),
#         segmenty[klasa2 == ""][k],
#         "\n",
#         sep = "\n", file = "out.txt", append = TRUE)
# }

klasa3 <- klasa
klasa3[klasa3 == ""] <- as.character(pred)

opis <- vector("list", length = length(segmenty_lista))

for (i in seq_along(segmenty_lista)) {
    
    ktore <- (indeksy == i)
    
    opis[[i]] <- sapply(c("oferta", "wymagania", "zadania", "atuty", "opis_firmy"),
                        function(x) paste(segmenty[ktore & klasa3 == x], 
                                          collapse = "\n"),
                        USE.NAMES = TRUE, simplify = FALSE)
    opis[[i]] <- as.data.frame(opis[[i]])
}


ramka <- do.call(rbind, lapply(opis, as.data.frame))

# save(ramka, file ="ramka.rda")
# load("ramka.rda")

baza_in <- dbConnect(SQLite(), "../oferty_lang.db")
wszystko <- dbGetQuery(baza_in, "select * from oferty where lang == 'pl'")
dbDisconnect(baza_in)

baza_out <- dbConnect(SQLite(), "../oferty_segmenty.db")
dbWriteTable(baza_out, "oferty", cbind(wszystko, ramka))




dbGetQuery(baza_out, "select wymagania from oferty")[,1] -> wym

cat(sample(wym, 100), sep = "\n ---------- \n")

dbDisconnect(baza_out)
