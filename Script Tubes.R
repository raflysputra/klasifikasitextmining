######################PREPROCESSING#######################
library(xml2)
library(rvest)
library(SnowballC)
library(RColorBrewer)
library(NLP)
library(tm)
library(stringr)
library(caret)
library(dplyr)

dataset<-read.csv("playstoresiap.csv")

corpus<-Corpus(VectorSource(dataset$Judul))

data_casefolding <- tm_map(corpus, content_transformer(tolower))
inspect(data_casefolding[1:10])

removeURL <- function(x)gsub("http[^[:space:]]*","",x)
data_URL <- tm_map(data_casefolding, content_transformer(removeURL))
inspect(data_URL[1:10])


remove.mention <- function(x) 
  gsub('@\\S+', '', x)
data_mention<- tm_map(data_casefolding, remove.mention)
inspect(data_mention[1:10])

remove.hashtag <- function(x) 
  gsub('#\\S+', '', x)

data_hashtag <- tm_map(data_casefolding, remove.hashtag)
inspect(data_hashtag[1:10])

data_punctuation<-tm_map(data_hashtag,content_transformer(removePunctuation))
inspect(data_punctuation[1:10])

data_nonumber<-tm_map(data_punctuation, content_transformer(removeNumbers))
inspect(data_nonumber[1:10])

data_stemming<-tm_map(data_nonumber,stemDocument,language="english")
cStopwordID<-readLines('D:/Kehidupan Kedua/Akademik/Semester 7/MDTT B/englishST.txt')
data_stopword<- tm_map(data_stemming, removeWords, cStopwordID)
inspect(data_stopword[1:6])

data_whitespace<-tm_map(data_stopword,stripWhitespace)

playstorebersih<-data.frame(text=unlist(sapply(data_whitespace,`[`)), stringsAsFactors=F)

View(playstorebersih)


write.csv(playstorebersih,file="D:/playstorebersih.csv")
#############SVM#############


dataset<-read.csv("playstorebersih.csv")
View(dataset)

sample=sample(1:nrow(dataset),0.8*nrow(dataset),replace=TRUE)

training=data.frame(dataset)[sample,]
testing=data.frame(dataset)[-sample,]

model =svm(as.factor(category)~.,data=training,type="C-classification", kernel = "linear")
prediksi=predict(model,newdata=training)

library(caret)
Hasil<-confusionMatrix(table(training$category,prediksi))
Hasil=data.frame(training$category,prediksi)
colnames(Hasil) = c("Aktual", "Prediksi")
head(Hasil, n = 10)

cm=table(training$category,prediksi)
confusionMatrix(cm)

ketepatan1=confusionMatrix(cm)
eval_svm <- data.frame(Accuracy = ketepatan1$overall[1],
                         Recall = ketepatan1$byClass[1],
                         Specificity = ketepatan1$byClass[2],
                         Precision = ketepatan1$byClass[3])
round(eval_svm,2)
######################################
model =svm(as.factor(category)~.,data=testing,type="C-classification", kernel = "linear")
prediksitesting=predict(model,newdata=testing)

library(caret)
Hasil<-confusionMatrix(table(testing$category,prediksitesting))
Hasil=data.frame(testing$category,prediksitesting)
colnames(Hasil) = c("Aktual", "Prediksi")
head(Hasil, n = 10)

cm=table(testing$category,prediksitesting)
confusionMatrix(cm)

ketepatan2=confusionMatrix(cm)

eval_svm <- data.frame(Accuracy = ketepatan2$overall[1],
                       Recall = ketepatan2$byClass[1],
                       Specificity = ketepatan2$byClass[2],
                       Precision = ketepatan2$byClass[3])
round(eval_svm,2)
