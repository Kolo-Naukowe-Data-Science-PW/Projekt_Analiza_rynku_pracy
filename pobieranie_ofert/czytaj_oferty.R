#' Skrypt pobierający oferty z portalu pracuj.pl
#'
#' Wywołanie w konsoli polecenia
#'      Rscript czytaj_oferty.R
#' pobiera oferty pracy z portalu Pracuj.pl.
#' Jeśli w bieżącym katalogu istnieje plik oferty.db, to pobierane są jedynie 
#' te oferty, których nie ma w bazie. 
#' W przeciwnym przypadku tworzona jest nowa baza danych o nazwie oferty.db.

library("rvest")
library("RSQLite")
library("stringi")

# Wczytuje liczbe stron z odpowiedniego przycisku na dole strony
# http://www.pracuj.pl/praca/
wczytaj.liczbe.stron <- function() {
    
    response <- read_html("http://www.pracuj.pl/praca/", encoding = "utf-8")
    
    # xpath do przycisków nawigacji między stronami (pod listą ofert)
    xpath <- "//ul/li[@class='desktopPagin_item']"
    przycisk <- html_nodes(response, xpath = xpath)[[4]] # czwarty przycisk
    
    as.numeric(html_text(przycisk)) 
}



# Zwraca wektor linków do wszystkich ofert na pracuj.pl
wczytaj.linki <- function(liczba.stron) {
    
    link <- "http://www.pracuj.pl/praca?pn="
    
    
    lista <- vector("list", liczba.stron)
    
    for (i in 1:liczba.stron) {
        
        response <- read_html(paste0(link, i), encoding = "utf-8")
        
        # xpath do linków do ofert z jedną lokalizacją
        xpath <- "//ul[@class='offer__list']/li/h2/a/@href" 
        linki <- html_text(html_nodes(response, xpath = xpath))
        
        # xpath do linków do ofert z więcej niż jedną lokalizacja
        xpath <- "//ul[@class='offer__list']/li/ul/li/h2/a/@href" 
        linki <- c(linki, html_text(html_nodes(response, xpath = xpath)))
        
        
        lista[[i]] <- paste0("http://www.pracuj.pl", linki)
        
        
        cat(paste0("\r", i,"/", liczba.stron))    
    }
    
    unlist(lista)
}



czytaj.oferte <- function(URL){
    
    pracuj <- read_html(URL, encoding = 'utf-8')
    
    nazwa.xpath   <- "//*[contains(concat(' ',@class,' '),' offerTop__cnt_main_job ')]"
    firma.xpath   <- "//*[contains(concat(' ',@class,' '),' offerTop__cnt_main_emplo-inline ')]//span"
    miejsce.xpath <- "//*[contains(concat(' ',@class,' '),' latlng ')]//span"
    typ.xpath     <- "//*[contains(concat(' ',@class,' '),' ico-briefcase ')]"
    zarobki.xpath <- "//*[contains(concat(' ',@class,' '),' ico-money ')]"
    czas.xpath    <- "//*[contains(concat(' ',@class,' '),' ico-time ')]"
    #opis.xpath    <- "//tbody/tr[1]/td[1]/div[1]"
    opis.xpath    <- "//div[@id='offCont']"
    
    
    nazwa   <- html_nodes(pracuj, xpath = nazwa.xpath)
    firma   <- html_nodes(pracuj, xpath = firma.xpath)
    miejsce <- html_nodes(pracuj, xpath = miejsce.xpath)[2]
    typ     <- html_nodes(pracuj, xpath = typ.xpath)
    zarobki <- html_nodes(pracuj, xpath = zarobki.xpath)[1]
    czas    <- html_nodes(pracuj, xpath = czas.xpath)
    
    opis    <- html_nodes(pracuj, xpath = opis.xpath)[1]
    
    f <- function(html.node){
        wynik <- html_text(html.node)
        wynik <- gsub('[\n]', '', wynik)
        ifelse(length(wynik) > 0, wynik, "")
    }
    
    nazwa   <- f(nazwa)
    firma   <- f(firma)
    miejsce <- f(miejsce)
    typ     <- f(typ)
    zarobki <- f(zarobki)
    czas    <- f(czas)
    if(zarobki == "Sprawdź, ile powinieneś zarabiać")
        zarobki <- ""
    
    
    
    rozp <- stri_extract_all_regex(czas, "[0-9]{4}-[0-9]{2}-[0-9]{2}")[[1]]
    rozp <- as.Date(rozp)
    do.konca <- stri_extract_all_regex(czas, " [0-9]+ ")
    do.konca <- as.numeric(do.konca)
    zakonczenie <- Sys.Date() + do.konca
    rozp <- as.character(rozp)
    zakonczenie <- as.character(zakonczenie)
    opis <- as.character(opis)
    
    
    c(nazwa, firma, miejsce, typ, zarobki, rozp, zakonczenie, opis)
}



# Pobiera oferty, do których prowadzą linki, a następnie zapisuje je do bazy 
# SQLitowej baza.
pobierz.oferty <- function(linki, baza, proby = 3, przerwa = 5) {
    
    db <- dbConnect(SQLite(), dbname = baza)
    
    linki <- rev(linki)
    
    for (i in seq_along(linki)) {
        
        for (j in 1:proby) {
            tryCatch({
                oferta <- czytaj.oferte(linki[i])
                break
            }, error = function(err) {
                cat(paste0("\n", j,". próba pobierania oferty ",
                           i, " nie powiodła się.\n"))
                if (j == proby) {
                    warning("Pobieranie oferty \n", linki[i],
                        "\nnie powiodło się", immediate. = TRUE)
                    
                }
                Sys.sleep(przerwa)
            })
        }
        
        if (length(oferta) != 8) {
            cat("\n")
            warning("Pobieranie oferty \n", linki[i],
                    "\nnie powiodło się", immediate. = TRUE)
        }
        else
            dbSendPreparedQuery(db, 
                                "insert into oferty values (?,?,?,?,?,?,?,?,?)",
                                data.frame(linki[i], t(oferta)))
        
     
        cat(paste0("\r", i, "/", length(linki)))   
    }
    
    dbDisconnect(db)
}




#' Wczytuje linki do ofert, a następnie wybiera te, które nie znajdują się w
#' bazie. Z tych linków pobierane są oferty i zapisywane do bazy.
aktualizuj.oferty <- function(baza) {

    cat("Wczytywanie linkow: \n")
    liczba.stron <- wczytaj.liczbe.stron()
    linki <- wczytaj.linki(liczba.stron)
    
    db <- dbConnect(SQLite(), dbname = baza)
    
    if (!"oferty" %in% dbListTables(db)) {
        dbSendQuery(db, "create table oferty 
                    (link text, nazwa text, firma text, miejsce text, typ text,
                     zarobki text, rozpoczecie text, zakonczenie text, opis text)")
    }
    
    linki.stare <- dbGetQuery(db, "select link from oferty")$link
    
    dbDisconnect(db)
    
    linki.nowe <- setdiff(linki, linki.stare)
    
    cat("\nPobieranie ofert: \n")
    pobierz.oferty(linki.nowe, baza = baza)
}



aktualizuj.oferty("oferty.db")


