# 清空变量，加载程序包 
rm(list = ls()) 
setwd("E:/third-2nd/机器学习概论/trec06c-utf8/data_cut");

y = read.table("E:/third-2nd/机器学习概论/trec06c-utf8/label/index");
yspam = y$V2[y$V1 == "spam"]
yham = y$V2[y$V1 == "ham"]
dirspam = paste("E:/third-2nd/机器学习概论/trec06c-utf8/data_cut/",yspam,sep="")   
dirham = paste("E:/third-2nd/机器学习概论/trec06c-utf8/data_cut/",yham,sep="")   
if(require(tm) == FALSE) {    
  install.packages("tm")    
  library(tm)    
}    
if(require(ggplot2) == FALSE) {    
  install.packages("ggplot2")    
  library(ggplot2)  
}  

 

# 首先处理垃圾邮件  
# 定义函数，用于读取邮件正文内容，以第一个空行作为开始标志   
#mixseg <- worker();
get.msg <- function(path)  
{  
  con <- file(path, open = "rt", encoding = "UTF-8")  
  text <- readLines(con)  
 
  #msg = segment(text, mixseg);
  msg <- tryCatch(text[seq(which(text == "")[1] + 1, length(text), 1)], error = function(e) e)  
  close(con)  
  return(paste(msg, collapse = "\n"))  
}  


# 构建一个文本资料库  
get.tdm <- function(doc.vec)  
{  
  
    #mixseg <- worker();
    #doc.vec = segment(doc.vec, mixseg);
    doc.corpus <- Corpus(VectorSource(doc.vec)) 
    control <- list(stopwords = TRUE,  
                  removePunctuation = TRUE,  
                  removeNumbers = TRUE, 
                  minDocFreq = 2) 
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)  
  return(doc.dtm)  
}  

# 保存邮件内容  
#mail.path2 <- dir(mail.path)
#mail.docs<-dir(mail.path2[1])
#spam.docs <- spam.docs[which(spam.docs != "cmds")]

spam <- sapply(dirspam,  
                   function(p) get.msg(p))  
#mixseg <- worker();
#spam = segment(spam, mixseg);
ham <- sapply(dirham,  
              function(p) get.msg(p)) 
spam.tdm <- get.tdm(spam)  
ham.tdm <- get.tdm(ham)

strainvol = 42854*0.5;
trainspam.tdm = spam.tdm[,sample(1:42854,strainvol)];
sms_freq_words = findFreqTerms(trainspam.tdm,1)
str(sms_freq_words)#ham.tdm <- get.tdm(ham)
spamfreq = trainspam.tdm[sms_freq_words,]# 用TDM构建一套垃圾邮件训练数据  
#spam.matrix = apply(spam.tdm,1,as.vector)
spam.counts = rep(0,length(sms_freq_words))
for (i in 1:200){
 spam.matrix <- as.matrix(spamfreq[,(100*(i-1)+1):(100*i)]) 
 spam.counts <- spam.counts + rowSums(spam.matrix);
}
#spam.matrix <- as.matrix(spamfreq) 
#spam.counts <- rowSums(spam.matrix) 
spam.df <- data.frame(cbind(names(spam.counts),  as.numeric(spam.counts)), stringsAsFactors = FALSE)  
names(spam.df) <- c("term", "frequency")  
spam.df$frequency <- as.numeric(spam.df$frequency)  
pspam <- (spam.counts + 1)/(42854 + length(sms_freq_words));
#sapply(1:nrow(spam.matrix),  
                          #function(i)  
                          #{  
                            #length(which(spam.matrix[i, ] > 0)) / ncol(spam.matrix)  
                          #})  
#spam.density <- spam.df$frequency / sum(spam.df$frequency)  

# 统计整个语料库中每个词项的频次  
spam.df <- transform(spam.df,  
                     #density = spam.density,  
                     p = pspam)  

htrainvol = 21766;
trainham.tdm = ham.tdm[,sample(1:21766,htrainvol)];
sms_freq_wordsh = findFreqTerms(trainham.tdm,1)
str(sms_freq_wordsh)#ham.tdm <- get.tdm(ham)
hamfreq = trainham.tdm[sms_freq_wordsh,]# 用TDM构建一套垃圾邮件训练数据  
#spam.matrix = apply(spam.tdm,1,as.vector)
ham.counts = rep(0,length(sms_freq_wordsh))
for (i in 1:200){
  ham.matrix <- as.matrix(hamfreq[,(100*(i-1)+1):(100*i)]) 
  ham.counts <- ham.counts + rowSums(ham.matrix);
}
pham = rep(0,length(sms_freq_wordsh));
ham.df <- data.frame(cbind(names(ham.counts),  as.numeric(ham.counts)), stringsAsFactors = FALSE)  
names(ham.df) <- c("term", "frequency")  
ham.df$frequency <- as.numeric(ham.df$frequency)  
pham <- (ham.counts + 1)/(21766 + length(sms_freq_wordsh));
ham.df <- transform(ham.df,  
                     #density = spam.density,  
                     p = pham)  

ps = 42854/64620;
ph = 21766/64620;
 SPred = rep(0,1000);
 HPred = rep(0,1000);
 cs = 1/(42854 + length(sms_freq_words)) ; 
 ch = 1/(21766 + length(sms_freq_wordsh)) ; 

acc = rep(0,5);
for (u in 1:5){
 for ( i in 1:1000){
tes <- sapply(dirspam[sample(1:42854,1)],  
               function(p) get.msg(p))  
tes.tdm = get.tdm(tes);
 tes.freq <- rowSums(as.matrix(tes.tdm))  
 tes.match <- intersect(names(tes.freq), spam.df$term) ;
 
 if(length(tes.match) < 1)  {
   Ps = log(ps)+ (length(tes.freq)) *log(cs) 
 } else{
   match.probs <- spam.df$p[match(tes.match, spam.df$term)]  
   Ps = log(ps) + sum(log(match.probs))+ (length(tes.freq) - length(tes.match)) *log(cs)
 }  
  
  tes.matchh <- intersect(names(tes.freq), ham.df$term) ;
  if(length(tes.matchh) < 1)  {
    Ph = log(ph)+ (length(tes.freq)) *log(ch) 
  } else{
    matchh.probs <- ham.df$p[match(tes.matchh, ham.df$term)]  
    Ph = log(ph) + sum(log(matchh.probs))+ (length(tes.freq) - length(tes.matchh)) *log(ch)  
  }  
  if(Ph<=Ps){
    SPred[i] = 1;
  }else{
    SPred[i] = 0;
  }
 }
 

 for ( i in 1:1000){
   tes <- sapply(dirham[sample(1:21766,1)],  
                 function(p) get.msg(p))  
   tes.tdm = get.tdm(tes);
   tes.freq <- rowSums(as.matrix(tes.tdm))  
   tes.match <- intersect(names(tes.freq), spam.df$term) ;
   
   if(length(tes.match) < 1)  {
     Ps = log(ps)+ (length(tes.freq)) *log(cs) 
   } else{
     match.probs <- spam.df$p[match(tes.match, spam.df$term)]  
     Ps = log(ps) + sum(log(match.probs))+ (length(tes.freq) - length(tes.match)) *log(cs)
   }  
   
   tes.matchh <- intersect(names(tes.freq), ham.df$term) ;
   if(length(tes.matchh) < 1)  {
     Ph = log(ph)+ (length(tes.freq)) *log(ch) 
   } else{
     matchh.probs <- ham.df$p[match(tes.matchh, ham.df$term)]  
     Ph = log(ph) + sum(log(matchh.probs))+ (length(tes.freq) - length(tes.matchh)) *log(ch)  
   }  
   
   if(Ph<=Ps){
     HPred[i] = 0;
   }else{
     HPred[i] = 1;
   }
 }
 
 acc[u] = (sum(SPred) + sum(HPred))/2000}



