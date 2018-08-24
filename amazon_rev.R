rm(list=ls())

###### importing packages and file

getwd()

setwd("D:/projects/amazon_review")

x=c("stringr","tm","wordcloud","sentimentr","readr","lubridate","scales","syuzhet","dplyr","reshape2")


for(i in x){
  if(!require(i ,character.only = T)){
    
    install.packages(i)
    
    require(i,character.only = T)
  }
}

rm(list=ls())

data=read.delim("amazon_data.txt",header = F)

names(data)="review"

class(data$review)

data$review=as.character(data$review)

data$review=str_trim(data$review)

corpus=Corpus(VectorSource(data$review))

corpus=tm_map(corpus,tolower)

corpus = tm_map(corpus,removePunctuation)

corpus=tm_map(corpus,removeNumbers)

corpus=tm_map(corpus,removeWords,stopwords("english"))

removeurl=function(x)gsub('http[[:alnum:]]*','',x)

corpus=tm_map(corpus,content_transformer(removeurl))

corpus=tm_map(corpus,stripWhitespace)

######### term document matrix

tdm=TermDocumentMatrix(corpus)

tdm_data=data.frame(t(as.matrix(tdm)))

write.csv(tdm_data,"term_doc_mat.csv",row.names =F)

require(slam)

word_fre=rollup(tdm,2,na.rm=T,FUN = sum)

word_fre=data.frame(as.matrix(word_fre))

names(word_fre)="count"

word_fre$Words=row.names(word_fre)

row.names(word_fre)=NULL

word_fre=word_fre[,c(2,1)]

write.csv(word_fre,"word_freq.csv",row.names = F)

w=rowSums(as.matrix(tdm))

w=subset(w,w>=5)

barplot(w,las=2,col=rainbow(50))

# building word cloud

png("word_cloud.png",height = 5,width=10,units="in",res=300)

wordcloud(word_fre$Words,freq = word_fre$count,scale=c(5,0.3),rot.per = 0.5,colors=brewer.pal(8,"Dark2"),ranom.order=F)

dev.off()

#wordcloud 2

require(wordcloud2)

wordcloud2(word_fre,size=0.9,shape="circle",rotateRatio = 0.5,minSize = 1,color = "random-light", backgroundColor = "black")

letterCloud(word_fre,word="s",size=0.6,color="random-light",backgroundColor="black")

letterCloud(word_fre, word = "R", size = 2)


#### sentiment analysis

dim(data)

sen=get_nrc_sentiment(data$review)

head(sen)

data$review[1]

barplot(colSums(sen),col=rainbow(10),las=2,ylab="count",xlab="sentiment",main="amazon reviews")

text=cbind(data,sen)

write.csv(text,"data_sentiment.csv",row.names = F)

install.packages("RSentiment")

require(RSentiment)

df=calculate_sentiment(text$review)

write.csv(df,'sentiment.csv',row.names = F)

require(sentiment)

polarity=classify_polarity(data$review,algorithm = 'bayes',verbose = T)

polarity=data.frame(polarity)

new=cbind(data,polarity)

bestfit=data.frame(table(new$BEST_FIT))

require(plotly)

plot_ly(bestfit,labels=~Var1,values=~Freq,type='pie')%>%layout(title='Sentiment Analysis',xaxis=list(showgrid=F,zeroline=F,showticklabels=F),yaxis=list(showgrid=F,zeroline=F,showticklabels=F))
