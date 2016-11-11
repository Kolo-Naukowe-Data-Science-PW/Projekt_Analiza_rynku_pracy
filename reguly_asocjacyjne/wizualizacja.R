library(visNetwork)
options(stringsAsFactors = FALSE)
setwd("~/Projekty/Projekt_Analiza_rynku_pracy/reguly_asocjacyjne/")


links <- read.csv("links.csv")
names(links) <- c("from", "to", "value")

links <- links[links$value > 2, ]



nodes <- data.frame(id = unique(c(links$from, links$to)))
nodes$title <- nodes$id

visNetwork(nodes, links, width = "1600px", height = "900px") %>%
    visEdges(arrows = "to") %>%
    visOptions(highlightNearest = TRUE) ->
    graph


visSave(graph, "index.html")



