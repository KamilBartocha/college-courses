library("tm")
library(wordcloud)
library(SnowballC)
library(e1071)
library(gmodels)

sms_raw <- read.csv("sms_spam.csv")
str(sms_raw)
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

sms_corpus<-VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)
sms_corpus_clean <-tm_map(sms_corpus, content_transformer(tolower))
as.character(sms_corpus_clean[[1]])
sms_corpus_clean<-tm_map(sms_corpus_clean, removeNumbers)
?getTransformations

stopwords()
sms_corpus_clean<-tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean<-tm_map(sms_corpus_clean, removePunctuation)
lapply(sms_corpus_clean[1:2], as.character)
sms_corpus_clean<-tm_map(sms_corpus_clean, stripWhitespace)
lapply(sms_corpus_clean[1:2], as.character)
#redukcja do rdzenia:

wordStem("learning") 
wordStem("learns")
wordStem("learned")
sms_corpus_clean<-tm_map(sms_corpus_clean, stemDocument)
lapply(sms_corpus_clean[1:3], as.character)
lapply(sms_corpus[1:3], as.character)
#tokenizacja - podzial tekstu na słowa, funkcja DocumentTermMatrix() przerabia 
#nasz korpus na tabele ilosci wystąpień danego słowa w danym dokumencie
sms_dtm<-DocumentTermMatrix(sms_corpus_clean)
sms_dtm_train<- sms_dtm[1:4169,]
sms_dtm_test<- sms_dtm[4170:5559,]
sms_train_labels<-sms_raw[1:4169,]$type
sms_test_labels<-sms_raw[4170:5559,]$type
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

freq <- data.frame(sort(colSums(as.matrix(sms_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100)
spam<-subset(sms_raw, type=="spam")
ham<-subset(sms_raw, type=="ham")
wordcloud(spam$text, maxwords=40, scale=c(3,0.5))
wordcloud(ham$text, maxwords=40, scale=c(3,0.5))
# słowa które występują co najmniej 5 razy w uczącym
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

# macierze
sms_dtm_freq_train <- sms_dtm_train[ ,sms_freq_words]
sms_dtm_freq_test  <- sms_dtm_test[ , sms_freq_words]

convert_counts <- function(x){
  x <- ifelse(x > 0, "Yes", "No")
  return(x)
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, MARGIN = 2,  convert_counts)


sms_classifier <- naiveBayes(sms_train, sms_train_labels, laplace = 2)
sms_test_pred <- predict(sms_classifier, sms_test_labels)
CrossTable(sms_test_pred, sms_test_labels, prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

