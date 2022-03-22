library(readxl)
library(tidyverse)
library(naniar)
library(smotefamily)
library(caret)

vars<-c("id", "mid25", "final25", "att5", "prac5", "crit10", "proposal10", "proj20", "grade")

##train data set
#61-1
rep61_1<-read_excel("/Users/purin/Library/Mobile Documents/com~apple~CloudDocs/Purin's Mac/Machine Learning/midterm exam/report 61-1.xlsx")
str(rep61_1)
head(rep61_1)
rep61_1<-rep61_1[-2,]
rep61_1<-rep61_1%>%rename("grade" = "...9")
names(rep61_1)<-vars

#61-2
rep61_2<-read_excel("/Users/purin/Library/Mobile Documents/com~apple~CloudDocs/Purin's Mac/Machine Learning/midterm exam/report 61-2.xlsx")
str(rep61_2)
rep61_2<-rep61_2[,c(1,5,9,10,11,12,13,14,16)]
names(rep61_2)<-vars

#62-2
rep62_2<-read_excel("/Users/purin/Library/Mobile Documents/com~apple~CloudDocs/Purin's Mac/Machine Learning/midterm exam/report 62-2.xlsx")
str(rep62_2)
rep62_2<-rep62_2[,c(1,2,3,4,5,6,7,8,12)]
rep62_2<-rep62_2[,c(1,2,8,3,4,5,6,7,9)]
names(rep62_2)<-vars

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

dat$mid25<-data.frame(input$mid)
dat$att5<-input$att
dat$prac5<-input$prac
dat$crit10<-input$crit
dat$proposal10<-input$prop
dat$porj20<-input$proj

vars2<-c("mid25", "att5", "prac5", "crit10", "proposal10", "proj20")

fake<-id1[-1,]

df.test<-transpose(data.frame(
  Value = as.character(c(25,
                         5,
                         5,
                         6,
                         8,
                         20)),
  stringsAsFactors = FALSE))


tran<-transpose(df.test[])

colnames(tran)<-colnames(id1)

vars
vars2

df<-reactive({
  
    df<-data.frame(
      mid25 = as.character(input$mid25),
      att5 = as.character(input$att5),
      prac5 = as.character(input$prac5), 
      crit10 = as.character(input$crit10),
      proposal10 = as.character(input$proposal10),
      proj20 = as.character(input$proj20),
      stringsAsFactors = FALSE)
  
})

if(pred.reac()==0){
  print("THE SHOW MUST GO ON!!!!")
} else {
  print("you should go to the register center")
}



fit.lm<-lm(final.fmla, data = score_train)
summary(fit.lm)

score_test2$pred_lm<-predict(fit.lm, newdata = score_test)

predict(fit.lm, newdata = id1)

final.fmla<-as.formula(final25~mid25+crit10)

str(rep63_2)
score_test2<-rep63_2[,-1]
str(score_test2)
score_test2$mid25<-as.numeric(score_test$mid25)
score_test2[is.na(score_test2)] <- 0
score_test2$proposal10<-as.numeric(score_test$proposal10)
score_test2$proj20<-as.numeric(score_test$proj20)


ggplot(score_test2, aes(x = pred_lm, y = final25)) + geom_point()

score_test2%>%mutate(residual = final25-pred_lm)%>%
  summarize(rmse = sqrt(mean(residual^2)))















