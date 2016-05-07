library(RSQLite)
library(rvest)
library(stringi)
library(XML)


baza <- dbConnect(SQLite(), "oferty.db")
opisy <- dbGetQuery(baza, "select opis from oferty")$opis
dbDisconnect(baza)


f <- function(opis, min_length) {
    doc <- htmlTreeParse(opis, useInternalNodes = TRUE, encoding = 'UTF-8')
    root <- xmlRoot(doc)
    
    
    listy <- readHTMLList(doc)
    
    # nodes <- getNodeSet(root, "//ul[not(ancestor::ul)]/preceding-sibling::*[1]")
    # nazwy <- sapply(nodes, xmlValue)
    
    # Usunięcie list
    nodes <- getNodeSet(root, "//ul[not(ancestor::ul)]")
    removeNodes(nodes)
    
    
    nodes <- getNodeSet(root, "//div[@id = 'company']")
    company <- sapply(nodes, function(x) 
                    paste(sapply(xmlChildren(x), xmlValue), collapse = " "))
    removeNodes(nodes)
    
    
    xpath <-"//div[@class = 't' or @class = 'desc']"
    #xpath <-"//div"
    nodes <- getNodeSet(root, xpath)
    reszta <- sapply(nodes, function(x) 
                    paste(sapply(xmlChildren(x), xmlValue), collapse = " "))
    
    removeNodes(nodes)
    
    reszta <- sapply(nodes, function(x) stri_trim(xmlValue(x)))
    
    listy <- sapply(listy, function(x) paste0("* ", x, "\n", collapse = ""))
    
    reszta <- reszta[stri_length(reszta) > min_length]
    
    unlist(c(listy, company, reszta))
}


opis <- sample(opisy, 1)
htmlTreeParse(opis, useInternalNodes = TRUE, encoding = 'UTF-8')
cat(f(opis, 50), sep ="\n***************************\n")


