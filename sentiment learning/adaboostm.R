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
label = factor(label)
tdm = get.tdm(review)
#Sys.setlocale(locale="Chinese (Simplified)_People's Republic of China.936")
sms_freq_words = findFreqTerms(tdm,20)
tdm = tdm[sms_freq_words,]
tdm =  t(tdm)
#container <- create_container(tdm,label,trainSize=1:100, testSize=101:120, virgin=FALSE)
#s <- train_model(container,"SVM")
#SVM_CLASSIFY <- classify_model(container, s)
#precision = (sum(as.numeric(SVM_CLASSIFY$SVM_LABEL)-1-label[101:120]==0))/20
tdm = scale(tdm)
testindex = sample(40000:44064,size = 100)
testx = as.matrix(tdm[testindex,])
testy = as.numeric(label[testindex])
K = 4
trainindex = sample(1:40000,size=3000)
weight = rep(1/length(trainindex),length(trainindex))
Predy = matrix(0,nrow = length(trainindex),ncol = K)
Prednewy = matrix(0,nrow = length(testindex),ncol = K)
er = rep(0,K)
beta = rep(0,K)

  
  for (t in 1: K){
  trainx = as.matrix(tdm[trainindex,])
  Trainy = label[trainindex]
  model = svm(trainx,y = Trainy)
  Predy[,t] = predict(model,trainx)
  Prednewy[,t] = predict(model,testx)
  trainy = as.numeric(Trainy)
  er[t] = 1 - sum(Predy[,t]==trainy)/length(trainindex)
  if (er[t]>0.5) break
  beta[t] = er[t]/(1-er[t])
  weight[Predy[,t]==trainy] = weight[Predy[,t]==trainy]*beta[t]
  weight = weight/sum(weight)
  trainindex = sample(trainindex,length(trainindex),replace = T,prob = weight)
  }
#D = matrix(0,length(testindex),3)
predy = rep(0,length(testindex))
vot = function(a){
D = rep(0,3)
D[1] = sum(a>0)
D[2] = sum(a==0)
D[3] = sum(a<0)
if (D[1]==max(D)) return(1)
else if (D[2]==max(D)) return(0)
else return(-1)
}
Predy = Predy - 2
Prednewy = Prednewy-2
Predy = t(apply(Predy,1,function(a) a*log(1/beta)))
Prednewy = t(apply(Prednewy,1,function(a) a*log(1/beta)))
predy = apply(Predy,1,vot)
prednewy = apply(Prednewy,1,vot)
precision = sum(prednewy == (testy-2))/length(testindex)
RMSE = sqrt((sum((prednewy-(testy-2))^2))/length(testindex))


DK = 4
Dtrainindex = sample(1:40000,size=3000)
Dweight = rep(1/length(Dtrainindex),length(Dtrainindex))
DPredy = matrix(0,nrow = length(Dtrainindex),ncol = DK)
DPrednewy = matrix(0,nrow = length(testindex),ncol = DK)
Der = rep(0,DK)
Dbeta = rep(0,DK)


for (t in 1: DK){
  Dtrainx = as.matrix(tdm[Dtrainindex,])
  DTrainy = label[Dtrainindex]
  DT = ctree(DTrainy~.,data = as.data.frame(Dtrainx))
  DPredy[,t] = predict(DT,as.data.frame(Dtrainx))
  DPrednewy[,t] = predict(DT,as.data.frame(testx))
  Dtrainy = as.numeric(DTrainy)
  Der[t] = 1 - sum(DPredy[,t]==Dtrainy)/length(Dtrainindex)
  if (Der[t]>0.5) break
  Dbeta[t] = Der[t]/(1-Der[t])
  Dweight[DPredy[,t]==Dtrainy] = Dweight[DPredy[,t]==Dtrainy]*Dbeta[t]
  Dweight = Dweight/sum(Dweight)
  Dtrainindex = sample(Dtrainindex,length(Dtrainindex),replace = T,prob = Dweight)
}
#D = matrix(0,length(testindex),3)
Dpredy = rep(0,length(testindex))
vot = function(a){
  D = rep(0,3)
  D[1] = sum(a>0)
  D[2] = sum(a==0)
  D[3] = sum(a<0)
  if (D[1]==max(D)) return(1)
  else if (D[2]==max(D)) return(0)
  else return(-1)
}
DPredy = DPredy - 2
DPrednewy = DPrednewy-2
DPredy = t(apply(DPredy,1,function(a) a*log(1/Dbeta)))
DPrednewy = t(apply(DPrednewy,1,function(a) a*log(1/Dbeta)))
Dpredy = apply(DPredy,1,vot)
Dprednewy = apply(DPrednewy,1,vot)
Dprecision = sum(Dprednewy == (testy-2))/length(testindex)
DRMSE = sqrt((sum((Dprednewy-(testy-2))^2))/length(testindex))

