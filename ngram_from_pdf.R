###BIBLIOMETRIC ANALYSIS OF PAPERS
library(tm)
library(pdftools)
library(dplyr)

##introduction of full texts
setwd("C:/Users/matte/Desktop/MATTEO/wdirectory/corpusf")
#test:
#setwd("C:/Users/matte/Desktop/MATTEO/wdirectory/bibliometric/test_pdf")
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
texts <- lapply(texts, removeWords, stop)

void <- c("can", "may", "also", "use", "will", "see", "however", "number", 
          "particular", "whether", "including", "many", "based", "need", "make", "using", "likely",
          "well", "within", "page", "able", "way", "part", "found", "must", "made", "often", "due",
          "take", "across", "since", "already", "rather", "now")
texts <- lapply(texts, removeWords, void)

# VCorpus
corpus <- VCorpus(VectorSource(texts))
funs <- list(stripWhitespace,
             removePunctuation, 
             function(x) removeWords(x, stopwords("english")),
             content_transformer(tolower))
corpus <- tm_map(corpus, FUN = tm_reduce, tmFuns = funs)

### Tokenisation of data

#generate X-word ngrams
#ngram_token1 <-  function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse=" "), use.names=FALSE)
#ngram_token2 <-  function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse=" "), use.names=FALSE)
ngram_token3 <-  function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse=" "), use.names=FALSE)

# Pass into TDM control argument
#tdm1 <- TermDocumentMatrix(corpus, control = list(tokenize = ngram_token1))
#freq1 <- rowSums(as.matrix(tdm1))
#freq1 <- sort(freq1, decreasing=TRUE)
#tdm_freq1 <- data.frame(term = names(freq1), occurrences = freq1)

#tdm2 <- TermDocumentMatrix(corpus, control = list(tokenize = ngram_token2))
#freq2 <- rowSums(as.matrix(tdm2))
#freq2 <- sort(freq2, decreasing=TRUE)
#tdm_freq2 <- data.frame(term = names(freq2), occurrences = freq2)

tdm3 <- TermDocumentMatrix(corpus, control = list(tokenize = ngram_token3))
freq3 <- rowSums(as.matrix(tdm3))
freq3 <- sort(freq3, decreasing=TRUE)
tdm_freq3 <- data.frame(term = names(freq3), occurrences = freq3)

#unify 
#sum <- rbind(tdm1, tdm2, tdm3)
sum <- rbind(tdm3)
frequ <- rowSums(as.matrix(sum))
frequ <- sort(frequ, decreasing=TRUE)
tdm_frequ <- data.frame(term = names(frequ), occurrences = frequ)
dataset <- as.data.frame(tdm_frequ)
#run
tdm_frequ
