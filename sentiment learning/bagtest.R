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
sms_freq_words = findFreqTerms(tdm,20)
TEST = read_delim('E:/third-2nd/机器学习概论/exp2/exp2.validation_review1.csv',',')
Treview = TEST[,2]
id = TEST[,1]
Treview = as.matrix(Treview)
Treview = as.vector(Treview)
id = as.matrix(id)
id = as.vector(id)
#label = factor(label)
Ttdm = get.tdm(Treview)
Tsms_freq_words = findFreqTerms(Ttdm,20)
common = intersect(sms_freq_words,Tsms_freq_words)
Ttdm = Ttdm[common,]
Ttdm =  t(Ttdm)
Ttdm = scale(Ttdm)
tdm = tdm[common,]
tdm =  t(tdm)
tdm = scale(tdm)

N = 10
Predy = matrix(0,nrow = length(id),ncol = N)
for (j in 1:N){
  trainindex = sample(1:40000,size=5000)
  trainx = as.matrix(tdm[trainindex,])
  trainy = label[trainindex]
  model = svm(trainx,y = trainy)
  Predy[,j] = predict(model,Ttdm)
}
predy = rep(0,length(id))
predy = rowMeans(Predy)
d = data.frame(cbind(id,predy))
write.table(d,'E:/third-2nd/机器学习概论/exp2/prediction7.txt',sep = ',',row.names = F,col.names = c("id","label"))


NDT = 4
PredyDT = matrix(0,nrow = length(id),ncol = N)
for (j in 1:NDT){
  trainindex = sample(1:40000,size=3000)
  trainx = as.matrix(tdm[trainindex,])
  trainy = label[trainindex]
  DT = ctree(trainy~.,data = as.data.frame(trainx))
  PredyDT[,j] = predict(DT,as.data.frame(Ttdm))
}
#D = matrix(0,length(testindex),3)
predyDT = rep(0,length(id))
predyDT = rowMeans(PredyDT)
d2 = data.frame(cbind(id,predyDT))
write.table(d2,'E:/third-2nd/机器学习概论/exp2/prediction2.txt',sep = ',',row.names = F,col.names = c("id","label"))
