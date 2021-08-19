df.train<-read.csv('train.csv')
head(df.train)
str(df.train)
library(Amelia)
missmap(df.train,main = 'Missing Map',col = c('yellow','black'),legend = F)
library(ggplot2)
ggplot(df.train,aes(Survived))+geom_bar()
ggplot(df.train,aes(Pclass))+geom_bar(aes(fill=factor(Pclass)))
ggplot(df.train,aes(Sex))+geom_bar(aes(fill=factor(Sex)))
ggplot(df.train,aes(Age))+geom_histogram(bins = 20,alpha=0.5,fill='blue')
ggplot(df.train,aes(SibSp))+geom_bar()
ggplot(df.train,aes(Fare))+geom_histogram(fill='green',color='black',alpha=0.5)
pl<-ggplot(df.train,aes(Pclass,Age))+geom_boxplot((aes(group=Pclass,fill=factor(Pclass),alpha=0.4)))
pl+scale_y_continuous(breaks = seq(min(0),max(80),by=2))+theme_bw()
impute_age<-function(age,class){
  out<-age
  for(i in 1:length(age)){
    if(is.na(age[i])){
      if(class[i]==1){
        out[i]<-37
      }else if(class[i]==2){
        out[i]<-29
      }else{
        out[i]<-24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}
fixed.ages.train<-impute_age(df.train$Age,df.train$Pclass)
df.train$Age<-fixed.ages.train
missmap(df.train,main = 'Imputation Check',col = c('Yellow', 'Black'),legend = F)
str(df.train)
library(dplyr)
df.train<-select(df.train,-PassengerId,-Name,-Ticket,-Cabin)
head(df.train)
str(df.train)
df.train$Survived<-factor(df.train$Survived)
df.train$Pclass<-factor(df.train$Pclass)
df.train$SibSp<-factor(df.train$SibSp)
df.train$Embarked<-factor(df.train$Embarked)
df.train$Sex<-factor(df.train$Sex)
str(df.train)
log.model<-glm(Survived~.,family = binomial(link = 'logit'),data = df.train)
summary(log.model)
df.test<-read.csv('test.csv')
str(df.test)
missmap(df.test,main = 'Missing Map',col = c('yellow','black'),legend = F)
fixed.ages.test<-impute_age(df.test$Age,df.test$Pclass)
df.test$Age<-fixed.ages.test
missmap(df.test,main = 'Imputation Check', col = c('Yellow', 'Black'),legend = F)
df.test<-select(df.test,-PassengerId,-Name,-Ticket,-Cabin)
head(df.test)
ggplot(df.train,aes(Pclass,Fare))+geom_boxplot((aes(group=Pclass,fill=factor(Pclass),alpha=0.4)))
df.test$Fare<-impute_age(df.test$Fare,df.test$Pclass)
missmap(df.test,main = 'Imputation Check', col = c('Yellow', 'Black'),legend = F)
df.test$Pclass<-factor(df.test$Pclass)
df.test$SibSp<-factor(df.test$SibSp)
df.test$Embarked<-factor(df.test$Embarked)
df.test$Sex<-factor(df.test$Sex)
str(df.test)
fitted.probabilities<-predict(log.model,df.test,type = 'response')
fitted.results<-ifelse(fitted.probabilities>0.5,1,0)
df.test<-read.csv('test.csv')
results<-cbind(df.test$PassengerId,fitted.results)
colnames(results)<-c('PassengerId','Survived')
head(results)
write.csv(results,file = 'Results.csv',row.names = F)