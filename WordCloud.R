library(tm)

df <- data.frame(matrix(unlist(DATA)))
df <- na.omit(df)
names(df)[1] = "titles"
df <- data.frame(titles = unique(df$titles))

myCorpus <- Corpus(VectorSource(df$titles))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

myStopwords <- c(stopwords("english"), "available", "via")
myStopwords <- setdiff(myStopwords, c("r", "big"))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, stripWhitespace)

myCorpusCopy <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)

stemCompletion2 <- function(x, dictionary) {
  
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus <- lapply(myCorpus, as.character)
myCorpus <- Corpus(VectorSource(myCorpus))

replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub), pattern=oldword, replacement=newword)
}

tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))

library(RColorBrewer)

termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency >= 10)
freq.df <- data.frame(term=names(termFrequency), freq=termFrequency)
m <- as.matrix(tdm)
word.freq <- sort(rowSums(m), decreasing = T)
pal <- brewer.pal(9, "BuGn")[-(1:4)]

library(wordcloud)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)

library(topicmodels)
library(ggplot2)

dtm <- as.DocumentTermMatrix(tdm)
lda <- LDA(dtm, k = 10)
term <- terms(lda, 7)
term <- apply(term, MARGIN = 2, paste, collapse = ", ")

topics <- topics(lda)
topics <- data.frame(topic=topics)
ggplot(topics, aes(x = topic, fill = term[topic])) + geom_bar()
