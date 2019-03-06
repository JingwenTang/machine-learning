rm(list =ls())
library(readr)
library(e1071)

library(tm)
library(party)
 
dat = read_delim('E:/third-2nd/机器学习概论/exp2/exp2.train1.csv',',')


get.tdm <- function(doc.vec)  
{
  #Sys.setlocale(locale="Chinese (Simplified)_People's Republic of China.936")
  doc.corpus <- Corpus(VectorSource(doc.vec)) 
  control <- list(stopwords = F,  
                  removePunctuation = TRUE,  
                  removeNumbers = TRUE, 
                  minDocFreq = 2,
                  weighting = weightTfIdf) 
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)  
  return(doc.dtm)
}

review = dat[,2]
label = dat[,1]
review = as.matrix(review)
review = as.vector(review)
label = as.matrix(label)
label = as.vector(label)
#label = factor(label)
tdm = get.tdm(review)
#Sys.setlocale(locale="Chinese (Simplified)_People's Republic of China.936")
sms_freq_words = findFreqTerms(tdm,30)
tdm = tdm[sms_freq_words,]
tdm =  t(tdm)
#container <- create_container(tdm,label,trainSize=1:100, testSize=101:120, virgin=FALSE)
#s <- train_model(container,"SVM")
#SVM_CLASSIFY <- classify_model(container, s)
#precision = (sum(as.numeric(SVM_CLASSIFY$SVM_LABEL)-1-label[101:120]==0))/20


tdm = scale(tdm)
testindex = sample(40000:44064,size = 100)
testx = as.matrix(tdm[testindex,])
testy = label[testindex]
N = 3
Predy = matrix(0,nrow = length(testindex),ncol = N)
for (j in 1:N){
trainindex = sample(1:40000,size=3000)
trainx = as.matrix(tdm[trainindex,])
trainy = label[trainindex]
model = svm(trainx,y = trainy)
Predy[,j] = predict(model,testx)
}
#D = matrix(0,length(testindex),3)
predy = rep(0,length(testindex))
#for (i in 1:length(testindex)){
#D[i,1] = sum(Predy[i,]==1)
#D[i,2] = sum(Predy[i,]==0)
#D[i,3] = sum(Predy[i,]==-1)
#if (D[i,1]==max(D[i,])) predy[i] = 1
#if (D[i,2]==max(D[i,])) predy[i] = 0
#if (D[i,3]==max(D[i,])) predy[i] = -1
#}
predy = rowMeans(Predy)
RMSE = sqrt((sum((predy-testy)^2))/length(testindex))


NDT = 4
PredyDT = matrix(0,nrow = length(testindex),ncol = N)
for (j in 1:NDT){
  trainindex = sample(1:40000,size=3000)
  trainx = as.matrix(tdm[trainindex,])
  trainy = label[trainindex]
  DT = ctree(trainy~.,data = as.data.frame(trainx))
  PredyDT[,j] = predict(DT,as.data.frame(testx))
}
#D = matrix(0,length(testindex),3)
predyDT = rep(0,length(testindex))
predyDT = rowMeans(PredyDT)
DRMSE = sqrt((sum((predyDT-testy)^2))/length(testindex))
