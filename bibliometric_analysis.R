###Libraries
library(bibliometrix)
library(ggplot2)
library(tm)
library(pdftools)
library(dplyr)

#-----------------------------------------------------
#>>>Bibliometric analysis of ACADEMIC PUBLICATIONS<<<<
#-----------------------------------------------------

#set your working directory with .bib bibliometric sources here!
setwd("C:/yourfolder")

###Datasets
#WoS1 (Law+Economics)
dfwos1 <- convert2df(file = "WOS1.bib", dbsource = "wos", format = "bibtex")
#WoS_L (Law)
dfwos1l <- convert2df(file = "WOS1L.bib", dbsource = "wos", format = "bibtex")
#WoS_E (Economics)
dfwos1e <- convert2df(file = "WOS1E.bib", dbsource = "wos", format = "bibtex")
#WoS2 (All fields)
dfwos2 <- convert2df(file = "WOS2.bib", dbsource = "wos", format = "bibtex")

###Country productivity and most-cited publications 
#[TABLE 5, TABLE 9]
r_wos1l <- biblioAnalysis(dfwos1l, sep = ";")
summary(r_wos1l)

#[TABLE 6, TABLE 8]
r_wos1e <- biblioAnalysis(dfwos1e, sep = ";")
summary(r_wos1e)

#[TABLE 7]
r_wos2 <- biblioAnalysis(dfwos2, sep = ";")
summary(r_wos2)

###Co-citation networks
#[FIGURE 5]
NetMatrix <- biblioNetwork(dfwos1e, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "References Coupling (Economics)", type = "fruchterman", size=T,
                label.n=50, remove.multiple=T, labelsize=0.7, remove.isolates = T, size.cex=T)

#[FIGURE 6]
NetMatrix <- biblioNetwork(dfwos1l, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "References Coupling (Law)", type = "fruchterman", size=T,
                label.n=50, remove.multiple=T, labelsize=0.7, remove.isolates = T, size.cex=T)

###Word frequencies
##KEYWORDS
#[TABLE 12]
frequencye_k <- termExtraction(
  dfwos1e,
  Field = "DE",
  ngrams = 1,
  stemming = FALSE,
  remove.numbers = TRUE,
  remove.terms = c(stopwords(kind = "en"))
)

#[TABLE 13]
frequencyl_k <- termExtraction(
  dfwos1l,
  Field = "DE",
  ngrams = 1,
  stemming = FALSE,
  remove.numbers = TRUE,
  remove.terms = c(stopwords(kind = "en")),
)

##ABSTRACTS
#[TABLE 14]
frequencye1 <- termExtraction(
  dfwos1e,
  Field = "AB",
  ngrams = 1,
  stemming = FALSE,
  remove.numbers = TRUE,
  remove.terms = c(stopwords(kind = "en"))
)
frequencye2 <- termExtraction(
  dfwos1e,
  Field = "AB",
  ngrams = 2,
  stemming = FALSE,
  remove.numbers = TRUE,
  remove.terms = stopwords(kind = "en")
)

#[TABLE 15]
frequencyl1 <- termExtraction(
  dfwos1l,
  Field = "AB",
  ngrams = 1,
  stemming = FALSE,
  remove.numbers = TRUE,
  remove.terms = c(stopwords(kind = "en")),
)
frequencyl2 <- termExtraction(
  dfwos1l,
  Field = "AB",
  ngrams = 2,
  stemming = FALSE,
  remove.numbers = TRUE,
  remove.terms = c(stopwords(kind = "en")),
)



#----------------------------------------------
#>>>Bibliometric analysis of POLICY PAPERS<<<<
#----------------------------------------------

###Extraction of policy reports' texts from pdf
#set your working directory here!
setwd("C:/yourfolder")
files <- list.files(pattern = "pdf$")
texts <- lapply(files, pdf_text)

#Cleaning of texts
for (i in 1:length(texts)) {texts[[i]] <- gsub("(f|ht)tp(s?)://\\S+", " ", texts[[i]], perl=T)}
for (i in 1:length(texts)) {texts[[i]] <- gsub("\r?\n|\r", " ", texts[[i]])}
texts <- lapply(texts, removePunctuation)
texts <- lapply(texts, removeNumbers)
texts <- lapply(texts, tolower)
texts <- lapply(texts, stripWhitespace)
stop <- stopwords(kind = "en")
texts <- lapply(texts, removeWords, stop)

#elimination of additional stepwords
void <- c("can", "may", "also", "use", "will", "see", "however", "number", 
          "particular", "whether", "including", "many", "based", "need", "make", "using", "likely",
          "well", "within", "page", "able", "way", "part", "found", "must", "made", "often", "due",
          "take", "across", "since", "already", "rather", "now")
texts <- lapply(texts, removeWords, void)

corpus <- VCorpus(VectorSource(texts))
funs <- list(stripWhitespace,
             removePunctuation, 
             function(x) removeWords(x, stopwords("english")),
             content_transformer(tolower))
corpus <- tm_map(corpus, FUN = tm_reduce, tmFuns = funs)

###Tokenisation of texts
#Generate X-word ngrams
ngram_token1 <-  function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse=" "), use.names=FALSE)
ngram_token2 <-  function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse=" "), use.names=FALSE)
ngram_token3 <-  function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse=" "), use.names=FALSE)

###Pass into TDM control argument
#1-word tokens
tdm1 <- TermDocumentMatrix(corpus, control = list(tokenize = ngram_token1))
freq1 <- rowSums(as.matrix(tdm1))
freq1 <- sort(freq1, decreasing=TRUE)
tdm_freq1 <- data.frame(term = names(freq1), occurrences = freq1)

#2-word tokens
tdm2 <- TermDocumentMatrix(corpus, control = list(tokenize = ngram_token2))
freq2 <- rowSums(as.matrix(tdm2))
freq2 <- sort(freq2, decreasing=TRUE)
tdm_freq2 <- data.frame(term = names(freq2), occurrences = freq2)

#3-word tokens
tdm3 <- TermDocumentMatrix(corpus, control = list(tokenize = ngram_token3))
freq3 <- rowSums(as.matrix(tdm3))
freq3 <- sort(freq3, decreasing=TRUE)
tdm_freq3 <- data.frame(term = names(freq3), occurrences = freq3)

#Order tokens according to their frequencies 
#1-word tokens
frequ1 <- rowSums(as.matrix(tdm1))
frequ1 <- sort(frequ1, decreasing=TRUE)
tdm_frequ1 <- data.frame(term = names(frequ1), occurrences = frequ1)
dataset1 <- as.data.frame(tdm_frequ1)

#2-word tokens
frequ2 <- rowSums(as.matrix(tdm2))
frequ2 <- sort(frequ2, decreasing=TRUE)
tdm_frequ2 <- data.frame(term = names(frequ2), occurrences = frequ2)
dataset2 <- as.data.frame(tdm_frequ2)

#3-word tokens
frequ3 <- rowSums(as.matrix(tdm3))
frequ3 <- sort(frequ3, decreasing=TRUE)
tdm_frequ3 <- data.frame(term = names(frequ3), occurrences = frequ3)
dataset3 <- as.data.frame(tdm_frequ3)

#Visualization
#[TABLE 16, TABLE 17]
tdm_frequ1
tdm_frequ2
tdm_frequ3