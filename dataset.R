library(readxl)
library(tidyverse)
library(naniar)
library(smotefamily)
library(caret)
library(naivebayes)

vars<-c("id", "mid25", "final25", "att5", "prac5", "crit10", "proposal10", "proj20", "grade")

##train data set
#61-1
rep61_1<-read_excel("/Users/purin/Library/Mobile Documents/com~apple~CloudDocs/Purin's Mac/Machine Learning/midterm exam/report 61-1.xlsx")
str(rep61_1)
head(rep61_1)
rep61_1<-rep61_1[-2,]
rep61_1<-rep61_1%>%rename("grade" = "...9")
names(rep61_1)<-vars
table(rep61_1$grade)

#61-2
rep61_2<-read_excel("/Users/purin/Library/Mobile Documents/com~apple~CloudDocs/Purin's Mac/Machine Learning/midterm exam/report 61-2.xlsx")
str(rep61_2)
rep61_2<-rep61_2[,c(1,5,9,10,11,12,13,14,16)]
names(rep61_2)<-vars
table(rep61_2$grade)

#62-2
rep62_2<-read_excel("/Users/purin/Library/Mobile Documents/com~apple~CloudDocs/Purin's Mac/Machine Learning/midterm exam/report 62-2.xlsx")
str(rep62_2)
rep62_2<-rep62_2[,c(1,2,3,4,5,6,7,8,12)]
rep62_2<-rep62_2[,c(1,2,8,3,4,5,6,7,9)]
names(rep62_2)<-vars
table(rep62_2$grade)

#63-1
rep63_1<-read_excel("/Users/purin/Library/Mobile Documents/com~apple~CloudDocs/Purin's Mac/Machine Learning/midterm exam/63-1.xlsx")
str(rep63_1)
head(rep63_1)
rep63_1<-rep63_1[,c(1,5,9,10,11,12,13,14,17)]
names(rep63_1)<-vars
rep63_1<-rep63_1[-1,]

score_train<-rbind(rep61_1, rep61_2)
score_train<-rbind(score_dat, rep62_2)
score_train<-rbind(score_dat, rep63_1)
str(score_train)
score_train$mid25<-as.numeric(score_train$mid25)
complete.cases(score_train)
score_train<-score_train[-71,]

##test data set
#63-2
rep63_2<-read_excel("/Users/purin/Library/Mobile Documents/com~apple~CloudDocs/Purin's Mac/Machine Learning/midterm exam/63-2.xlsx")
str(rep63_2)
head(rep63_2)
rep63_2<-rep63_2[c(-1,-2),]
rep63_2<-rep63_2[,c(1,5,6,7,8,9,10,11,14)]
names(rep63_2)<-vars
score_test<-rep63_2
str(score_test)
score_test$mid25<-as.numeric(score_test$mid25)
score_test[is.na(score_test)] <- 0
score_test$proposal10<-as.numeric(score_test$proposal10)
score_test$proj20<-as.numeric(score_test$proj20)
table(rep63_2$grade)

##recode grade & remove final score
#1)I,F,D,D+=withdraw(1) 
#2)C,C+,B,B+,A=pass(0)
table(score_train$grade)
score_train$grade<-recode(score_train$grade, "I"=1,"F"=1, "D"=1, "D+"=1, "C"=0, "C+"=0, "B"=0, "B+"=0, "A"=0)
str(score_train)
score_train<-score_train[,-3]
score_train<-score_train[,-1]

table(score_test$grade)
score_test$grade<-recode(score_test$grade, "I"=1,"F"=1, "D"=1, "D+"=1, "C"=0, "C+"=0, "B"=0, "B+"=0, "A"=0)
score_test<-score_test[,-3]
score_test<-score_test[,-1]

##SMOTE
score_smote<-SLS(score_train[,-7], score_train[,7], K=3, C=5)
score_smote<-score_smote$data
glimpse(score_smote)
table(score_smote$class)

train.id<-createDataPartition(score_smote$class, p = 0.7, list=F, times=1)
train<-score_smote[train.id,] #70%
test<-score_smote[-train.id,] #30%

##predicted with random forest
fmla<-as.formula(class~mid25+att5+prac5+crit10+proposal10+proj20)

tgrid <- expand.grid(.mtry = 2:4,.splitrule = "gini",.min.node.size = c(10, 20))
fit.rf<-train(fmla, data=train, method="ranger")
fit.rf

#train predict
train$pred<-predict(fit.rf, train, type="raw")
table(train$pred,train$class)
mean(train$pred==train$class) #acc = 99%

#test predict
test$pred<-predict(fit.rf, test, type="raw")
table(test$pred,test$class)
mean(test$pred==test$class) #acc = 89%

#predict report63_2
score_test$pred<-predict(fit.rf, score_test, type="raw")
table(score_test$pred,score_test$grade)
mean(score_test$pred==score_test$grade) #acc = 93%

id1<-score_test[30,]
id1<-id1[,c(-7,-8)]

p<-as.numeric(as.character(predict(fit.rf, newdata = id1, type = "raw")))
class(p)
p<-as.character(p)


#predict your final score
final_train<-rbind(rep61_1, rep61_2)
final_train<-rbind(final_train, rep62_2)
final_train<-rbind(final_train, rep63_1)
str(final_train)
final_train$mid25<-as.numeric(final_train$mid25)
complete.cases(final_train)
final_train<-final_train[-71,]

final_score<-final_train[,2:8]
final_score<-final_score[,c(1,3,4,5,6,7,2)]

final_train.id<-createDataPartition(final_score$final25, p = 0.7, list=F, times=1)
train.final<-final_score[final_train.id,] #70%
test.final<-final_score[-final_train.id,] #30%

final.fmla<-as.formula(final25~mid25)

fit.lm<-lm(final.fmla, data = train.final)
summary(fit.lm)
plot(fit.lm)

final_score$pred_lm<-predict(fit.lm, final_score)

final_score%>%mutate(residual = final25-pred_lm)%>%
  summarize(rmse = sqrt(mean(residual^2)))

#predict grade
final_score.nb<-final_train[,c(-1,-3)]
table(final_score.nb$grade)

fit.nb<-naive_bayes(grade~mid25, data = final_score.nb, laplace = 1)

final_score.nb$pred<-predict(fit.nb, final_score.nb, type = "class")

table(final_score.nb$pred, final_score.nb$grade)
mean(final_score.nb$pred == final_score.nb$grade) #acc = 63.7%

nb<-predict(fit.nb, id1, type = "prob")

nb.df<-as.data.frame(round(nb, digits = 2))

library(treemapify)
library(ggplot2)
install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)

t.nb.df<-as.data.frame(t(nb.df))

grade <- c("D+", "D", "C+", "C", "B+", "B", "A")
value <- as.vector(n.nb.df)

x<-as.data.frame(grade)
x$value<-t.nb.df[,1]*100

ggplot(x, aes(area = value, fill = grade, label = value)) +
  geom_treemap() + geom_treemap_text()

x%>%mutate(ypos = cumsum(value)-0.5*value) %>%
  ggplot(aes(x="", y=value, fill=grade)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = grade), color = "white", size=4) +
  scale_fill_brewer(palette="Set2")




















