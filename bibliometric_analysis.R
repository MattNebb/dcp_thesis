###Libraries
library(bibliometrix)
library(ggplot2)
library(tm)
library(pdftools)
library(dplyr)

setwd("C:/Users/matte/Desktop/MATTEO/wdirectory/bibliometric")

#datasets
dfwos1 <- convert2df(file = "WOS1.bib", dbsource = "wos", format = "bibtex")
dfwos1l <- convert2df(file = "WOS1L.bib", dbsource = "wos", format = "bibtex")
dfwos1e <- convert2df(file = "WOS1E.bib", dbsource = "wos", format = "bibtex")
dfwos2 <- convert2df(file = "WOS2.bib", dbsource = "wos", format = "bibtex")

#-----------------------------------------------------
##introduction of full texts
setwd("C:/Users/matte/Desktop/MATTEO/wdirectory/bibliometric/test_pdf")
#vector with all pdf in the folder
files <- list.files(pattern = "pdf$")

#extraction from pdf files
texts <- lapply(files, pdf_text)

#cleaning
for (i in 1:length(texts)) {texts[[i]] <- gsub("(f|ht)tp(s?)://\\S+", " ", texts[[i]], perl=T)}
for (i in 1:length(texts)) {texts[[i]] <- gsub("\r?\n|\r", " ", texts[[i]])}

texts <- lapply(texts, removePunctuation)
texts <- lapply(texts, removeNumbers)
texts <- lapply(texts, tolower)
texts <- lapply(texts, stripWhitespace)
#texts <- lapply(texts, stemDocument)
stop <- stopwords(kind = "en")
texts <- lapply(texts, removeWords, "stop")

#full text column in data frame
for (i in 1:length(texts)) {df$text[i] <- texts[i]}

setwd("C:/Users/matte/Desktop/MATTEO/wdirectory/bibliometric")
#------------------------------------------------------------------
#descriptive analysis
r_wos1 <- biblioAnalysis(dfwos1, sep = ";")
summary(r_wos1)

r_wos1l <- biblioAnalysis(dfwos1l, sep = ";")
summary(r_wos1l)

r_wos1e <- biblioAnalysis(dfwos1e, sep = ";")
summary(r_wos1e)

r_wos2 <- biblioAnalysis(dfwos2, sep = ";")
summary(r_wos2)

r_split <- timeslice(dfwos2, breaks=c(1995, 2005))
summary(r_split[1])

S <- summary(object = results, k = 10, pause = FALSE)

plot(x = results, k = 20, pause = FALSE)

###Authors' Coupling
NetMatrix <- biblioNetwork(dfwos1e, analysis = "coupling", network = "authors", sep = ";")
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 50, Title = "Authors' Coupling", type = "fruchterman",
                size=5,size.cex=T,remove.multiple=TRUE, remove.isolates = T,label.n=50,label.cex=T)



#coupling references
NetMatrix <- biblioNetwork(dfwos1l, analysis = "coupling", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "References Coupling (Law)", type = "fruchterman", size=T,
                label.n=50, remove.multiple=T, labelsize=0.7, remove.isolates = T, size.cex=T)



###Co-citation network

NetMatrix <- biblioNetwork(dfwos1e, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "References Coupling (Economics)", type = "fruchterman", size=T,
                label.n=50, remove.multiple=T, labelsize=0.7, remove.isolates = T, size.cex=T)

NetMatrix <- biblioNetwork(dfwos1l, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "References Coupling (Law)", type = "fruchterman", size=T,
                label.n=50, remove.multiple=T, labelsize=0.7, remove.isolates = T, size.cex=T)

NetMatrix <- biblioNetwork(dfwos1e, analysis = "collaboration", network = "authors", sep = ";")
net=networkPlot(NetMatrix, n = 200, Title = "Co-Citation Network", type = "fruchterman", size.cex=T, edgesize = 2,
                label.n=100, remove.multiple=T, remove.isolates = T, label.cex=T)

NetMatrix <- biblioNetwork(dfwos1l, analysis = "coupling", network = "authors", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "References Coupling", type = "fruchterman", size=T,
                label.n=50, remove.multiple=T, labelsize=0.7, remove.isolates = T, size.cex=T)

#COUPLING

NetMatrix <- biblioNetwork(dfwos1e, analysis = "coupling", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "References Coupling (Economics)", type = "fruchterman", size=T,
                label.n=50, remove.multiple=T, labelsize=0.7, remove.isolates = T, size.cex=T)

NetMatrix <- biblioNetwork(dfwos1l, analysis = "coupling", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "References Coupling (Law)", type = "fruchterman", size=T,
                label.n=50, remove.multiple=T, labelsize=0.7, remove.isolates = T, size.cex=T)

NetMatrix <- biblioNetwork(dfwos1e, analysis = "coupling", network = "authors", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "fruchterman", size.cex=T, edgesize = 2,
                label.n=100, remove.multiple=T, remove.isolates = T, label.cex=T)

NetMatrix <- biblioNetwork(dfwos1l, analysis = "coupling", network = "authors", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "References Coupling", type = "fruchterman", size=T,
                label.n=50, remove.multiple=T, labelsize=0.7, remove.isolates = T, size.cex=T)

#--------------------------------------------------------------------------
### CO-WORD ANALYSIS:

#Keyword co-occurrences network

ac <-c("consumer", "consumers", "competition", "antitrust", "market")

#FREQUENCY WORDS
frequencyl1 <- termExtraction(
  dfwos1l,
  Field = "AB",
  ngrams = 1,
  stemming = FALSE,
  remove.numbers = TRUE,
  remove.terms = c(stopwords(kind = "en"), ac),
)
frequencyl2 <- termExtraction(
  dfwos1l,
  Field = "AB",
  ngrams = 2,
  stemming = FALSE,
  remove.numbers = TRUE,
  remove.terms = c(stopwords(kind = "en"), ac),
)

frequencye1 <- termExtraction(
  dfwos1e,
  Field = "AB",
  ngrams = 1,
  stemming = FALSE,
  remove.numbers = TRUE,
  remove.terms = c(stopwords(kind = "en"), ac)
)
frequencye2 <- termExtraction(
  dfwos1e,
  Field = "AB",
  ngrams = 2,
  stemming = FALSE,
  remove.numbers = TRUE,
  remove.terms = stopwords(kind = "en")
)
extr <- cbind(extr1,extr2,extr3)
rm(extr1)
rm(extr2)
rm(extr3)

NetMatrix <- biblioNetwork(extr, analysis = "co-occurrences", network = "abstracts", sep = ";", short = TRUE)

NetMatrix <- biblioNetwork(dfwos2, analysis = "co-occurrences", network = "keywords", sep = ";", short = TRUE)

#net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", 
#               type = "fruchterman", size.cex = T, size=20, edgesize = 10,labelsize=2, label.cex = TRUE, edges.min = 2)
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 100, Title = "Keyword Co-occurrences", 
                type = "fruchterman", size=2, edgesize = 3,labelsize=1, size.cex=T)#, edges.min = 2)



###Conceptual Structure using keywords

#CS <- conceptualStructure(df,field="ID", method="MCA", minDegree=4, clust="auto", stemming=TRUE, labelsize=10, documents=10)
CS <- conceptualStructure(extr,field="AB", method="CA", minDegree=40, clust="auto", stemming=FALSE, labelsize=8, documents=15) #ID
#graphics:
CS$graph_terms
CS$graph_dendogram
CS$graph_documents_Contrib
CS$graph_documents_TC
CS$docCoord

#historiogram
options(width=130)
histResults <- histNetwork(dfwos1l, min.citations = 1, sep = ";")
net <- histPlot(histResults, n=30, size = 30, labelsize=5)

#-----------------------
#Keyword Taxonomy (TO DELETE)
#combination of 2 words tokens thata appear more than 10 times in abstracts

tax2a <- termExtraction(
  dftax,
  Field = "AB",
  ngrams = 2,
  stemming = FALSE,
  remove.numbers = TRUE,
  remove.terms = stopwords(kind = "en")
)


taxt1 <-tax2a$AB_TM
taxt1 <- gsub(";", " ", taxt1)

vs <- VectorSource(taxt1)
corp <-Corpus(vs)
corp <-tm_map(corp, removePunctuation)
corp <-tm_map(corp, removeWords, stopwords("en"))

dtm <- DocumentTermMatrix(corp)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
freqm <- frequency > 9
res <- frequency[freqm]


#-----------------------
#Summary
an <- biblioAnalysis(tax1a)
results  <- summary(an)

