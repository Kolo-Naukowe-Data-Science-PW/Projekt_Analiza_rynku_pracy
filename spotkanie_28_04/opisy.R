library(RSQLite)
library(rvest)
library(stringi)
library(XML)

baza <- dbConnect(SQLite(), "../oferty.db")
opisy <- dbGetQuery(baza, "select opis from oferty limit 1000")$opis
dbDisconnect(baza)


h <- function(opis) {
    
    # sprawdzenie, czy da się sparsować
    error <- try({
        opis_xml <- xmlParse(opis)
    })
    if (inherits(error, "try-error")) return("")
    
    root <- xmlRoot(opis_xml)
    
    listy <- XML::xpathSApply(root, "//ul[not(ancestor::ul)]")
    listy <- sapply(listy, function(x) paste(capture.output(x), collapse = " "))
    listy <- stri_replace_all_regex(listy, "<li>", "\n* ")
    listy <- stri_replace_all_regex(listy, "<.*?>", " ")
    listy <- stri_replace_all_fixed(listy, "&#13;", " ")
    
    
    # reszta <- stri_replace_all_fixed(opis, "\n", " ")
    # 
    ## Jeśli będzie lista potrójnie zagnieżdżona, to mogą się pojawić problemy
    # reszta <- unlist(stri_split_regex(reszta, "<ul>.*?(<ul>.*?</ul>)?</ul>"))
    # 
    # reszta <- unlist(stri_split_regex(reszta, "(</div>)|(<br>)|(<br/>)"))
    # 
    # Usuwanie html-a
    # reszta <- stri_replace_all_regex(reszta, "<.*?>", " ")
    # 
    # reszta <- stri_replace_all_fixed(reszta, "&#13;", " ")
    # 
    # reszta <- stri_trim(reszta)
    # 
    # c(listy, reszta[reszta != ""])
    
    listy
}




# for (i in sample(1:length(opisy))) {
#     cat(paste0("\n\n ++++++++++++++ ",i, " +++++++++++++++\n\n"))
#     cat(h(opisy[i]), sep = "\n------------------------------\n")
#     if (readline() != "") break
# }

# cat(opisy[2532])





library(data.table)

tworz.lematyzacje <- function(unikalneSlowa) {

    slowa <- fread("polimorf2.txt", header = FALSE, encoding = "UTF-8")
    
    suppressWarnings(slowa <- slowa[V1 %in% unikalneSlowa])
    
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

    


library("tm")


opisy <- unlist(lapply(opisy, h))
opisy2 <- opisy

opisy <- tolower(opisy)

unikalneSlowa <- unique(unlist(stri_split_regex(opisy, "\\W")))

lematyzacja <- tworz.lematyzacje(unikalneSlowa)


t <- Sys.time()
opisy <- sapply(opisy, lematyzacja)
Sys.time() - t


opisy <- stri_replace_all_regex(opisy, "[0-9]", " ")

opisy <- stri_replace_all_regex(opisy, "\\s+", " ")

dluzsze <-  (stri_length(opisy) >= 100)



korpus <- VCorpus(VectorSource(opisy[dluzsze]))


stopWords <- read.table("stopWords.txt", stringsAsFactors = FALSE)[,1]

korpus <- tm_map(korpus, removeWords, stopWords)


dtm <- DocumentTermMatrix(korpus)
    
dtm2 <- removeSparseTerms(dtm, 0.99)
    


### Analiza skupień - raczej mało sensowne


M <- as.matrix(dtm2)
head(M)

km <- kmeans(M, 8)
km$iter
#save(km, file = "kmeans.rda")
table(km$cluster)
i = 1
while (stri_length(readline()) == 0) {
    cat(opisy2[dluzsze][km$cluster==8][[i]])
    cat("\n---------------------------------------------------")
    i = i+1
}

opisy[10]

# https://pl.wikipedia.org/wiki/TFIDF
wagi <- weightTfIdf(dtm, normalize = TRUE)
wagi2 <- removeSparseTerms(wagi, 0.995)

#install.packages("cluster")
library("cluster")
cla <- clara(wagi2, 6)
table(cla$clustering)

km <- kmeans(wagi2, 8)

table(km$cluster)
i = 1
while (stri_length(readline()) == 0) {
    cat("---------------------------------------------------\n")
    cat(opisy2[dluzsze][km$cluster==2][i])
    
    i = i+1
}


wagi3 <- wagi2[km$cluster == 3, ]

km2 <- kmeans(wagi3, 6)

table(km2$cluster)
i = 1
while (stri_length(readline()) == 0) {
    print(opisy[dluzsze][km$cluster==3][km2$cluster == 4][i])
    i = i+1
}



w2 <- as.matrix(wagi2)

#install.packages('proxy')
library('proxy') 
D <- dist(w2, method="cosine")

clust <- hclust(D)

groups <- cutree(clust, 2)

opisy2[dluzsze][groups==2]





