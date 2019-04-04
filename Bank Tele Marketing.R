getwd()

#Importing Dataset:

bd=read.csv("C:/Users/Ranjith P/Desktop/Bank Tele Marketing/bank-additional-full.csv",sep=";")

#Loading the libraries:
library(MASS)
library(DMwR)
library(plyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(corrgram)
library(caret)
library(randomForest)
library(RRF)
library(rockchalk)
library(caTools)
library(car)

#Exploratory Data Analysis:
summary(bd)

str(bd)

#Missing value Analysis and Treatment:
sum(is.na(bd))
#It's found that there is no missing value in any variable in our dataset.

#Data Visualization (EDA):
#Plotting categorical variables:

#Here Target variable is y.

#Target class proportion:
ggplot(bd,aes(bd$y))+geom_bar(aes(fill = bd$y),position = "dodge")+
  labs(title = "Proportion of Target Class")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_fill(vjust=0.5))

#yes=11.3% no=88.7% (eventhough the proportion of yes is low, but ok)


#Transforming Target Variable:
bd$y=factor(bd$y,levels = c("no", "yes"),labels = c(0,1))
summary(bd$y)
colnames(bd)

#Variable 1: Age
hist(bd$age)

ggplot(bd, aes(x = bd$age))+
  geom_histogram(aes(color = bd$y), fill = "white", position = "identity") +
  scale_color_manual(values = c("red", "blue"))+labs(title = "Target vs age")

#The above chart tells us that majority of people with age < 25 years and age > 60 years have accepted the campaign.

age_subset=subset(bd,bd$age<=25 | bd$age>=60)

boxplot(bd$age)
summary(bd$age)
bench1=47+1.5*IQR(bd$age)
bench1
bd$age[bd$age>bench1]=bench1
summary(bd$age)
boxplot(bd$age)


#Variable 2: Job
#Target vs Job:
ggplot(bd,aes(x=bd$job,fill=bd$y))+
  geom_bar(position="dodge")+labs(title = "Target vs Job")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.9),vjust=-0.2)


#Variable 3: Marital
#Target vs Marital:
ggplot(bd,aes(x=bd$marital,fill=bd$y))+
  geom_bar(position="dodge")+labs(title = "Target vs Marital")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.9),vjust=-0.2)

bd$marital=combineLevels(bd$marital, levs = c("married","unknown"), newLabel = "married")


#Variable 4: Education
#Target vs Education:
ggplot(bd,aes(x=bd$education,fill=bd$y))+
  geom_bar(position="dodge")+labs(title = "Target vs Education")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.9),vjust=-0.2)

summary(bd$education)
bd$education=combineLevels(bd$education, levs = c("basic.4y","basic.6y","basic.9y","illiterate","unknown","high.school"), newLabel = "primary")
bd$education=combineLevels(bd$education, levs = c("professional.course"), newLabel = "secondary")
bd$education=combineLevels(bd$education, levs = c("university.degree"), newLabel = "tertiary")


#Variable 5: Default
#Target vs Default:
ggplot(bd,aes(x=bd$default,fill=bd$y))+
  geom_bar(position="dodge")+labs(title = "Target vs Default")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.9),vjust=-0.2)

bd=bd[,-5]#removing default variable.Since, the proportion is inconsistent.


#Variable 6: Housing
summary(bd$housing)
#Target vs Housing:
ggplot(bd,aes(x=bd$housing,fill=bd$y))+
  geom_bar(position="dodge")+labs(title = "Target vs Housing")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.9),vjust=-0.2)


#Variable 7: Loan
summary(bd$loan)
#Target vs Loan:
ggplot(bd,aes(x=bd$loan,fill=bd$y))+
  geom_bar(position="dodge")+labs(title = "Target vs Loan")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.9),vjust=-0.2)


#Variable 8: Contact
summary(bd$contact)
#Target vs Contact:
ggplot(bd,aes(x=bd$contact,fill=bd$y))+
  geom_bar(position="dodge")+labs(title = "Target vs Contact")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.9),vjust=-0.2)


#Variable 8: Month
summary(bd$month)
#Target vs Month:
ggplot(bd,aes(x=bd$month,fill=bd$y))+
  geom_bar(position="dodge")+labs(title = "Target vs Month")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.9),vjust=-0.2)


#Variable 9: Day_of_week
summary(bd$day_of_week)
#Target vs Day_of_week:
ggplot(bd,aes(x=bd$day_of_week,fill=bd$y))+
  geom_bar(position="dodge")+labs(title = "Target vs Day_of_week")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.9),vjust=-0.2)


#Variable 10: duration
summary(bd$duration)
hist(bd$duration)

ggplot(bd, aes(x = bd$duration))+
  geom_histogram(aes(color = bd$y), fill = "white", position = "identity") +
  scale_color_manual(values = c("red", "blue"))+labs(title = "Target vs duration")

#aggregate(bd$duration~bd$y,bd.mean)

boxplot(bd$duration)
summary(bd$duration)
bench2=319+1.5*IQR(bd$duration)
bench2
bd$duration[bd$duration>bench2]=bench2
summary(bd$duration)
boxplot(bd$duration)


#Variable 11: campaign
summary(bd$campaign)
hist(bd$campaign)

ggplot(bd, aes(x = bd$campaign))+
  geom_histogram(aes(color = bd$y), fill = "white", position = "identity") +
  scale_color_manual(values = c("red", "blue"))+labs(title = "Target vs campaign")

boxplot(bd$campaign)
summary(bd$campaign)
bench3=3+1.5*IQR(bd$campaign)
bench3
bd$campaign[bd$campaign>bench3]=bench3
summary(bd$campaign)
boxplot(bd$campaign)

bd$campaign=as.factor(bd$campaign)
unique(bd$campaign)
levels(bd$campaign)[1:3]="lessthan or equal 3"
levels(bd$campaign)[2:4]="more than 3"

ggplot(bd,aes(x=bd$campaign,fill=bd$y))+
  geom_bar(position="dodge")+labs(title = "Target vs campaign")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.9),vjust=-0.2)

#Variable 12: pdays
bd$pdays=as.factor(bd$pdays)
summary(bd$pdays)
unique(bd$pdays)
str(bd$pdays)

levels(bd$pdays)[1:11]="recent call"
levels(bd$pdays)[2:16]="not recent call"
levels(bd$pdays)[3]="first time call"

ggplot(bd,aes(x=bd$pdays,fill=bd$y))+
  geom_bar(position="dodge")+labs(title = "Target vs pdays")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.9),vjust=-0.2)


#Variable 13: previous
summary(bd$previous)
bd$previous=as.factor(bd$previous)
unique(bd$previous)

levels(bd$previous)[1:4]="less than 3 times"
levels(bd$previous)[2:5]="more than 3 times"

#Target vs previous
ggplot(bd,aes(x=bd$previous,fill=bd$y))+
  geom_bar(position="dodge")+labs(title = "Target vs previous")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.9),vjust=-0.2)


#Variable 14: poutcome
summary(bd$poutcome)

#Target vs pOutcome:
ggplot(bd,aes(x=bd$poutcome,fill=bd$y))+
  geom_bar(position="dodge")+labs(title = "Target vs pOutcome")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.9),vjust=-0.2)


#variable 15: emp.var.rate
summary(bd$emp.var.rate)
boxplot(bd$emp.var.rate)


#variable 16: cons.price.idx
summary(bd$cons.price.idx)
boxplot(bd$cons.price.idx)

colnames(bd)
#variable 17: cons.conf.idx
summary(bd$cons.conf.idx)
boxplot(bd$cons.conf.idx)


#variable 18: euribor3m
summary(bd$euribor3m)
boxplot(bd$euribor3m)


#variable 19: nr.employed
summary(bd$nr.employed)
boxplot(bd$nr.employed)


#Feature Selection:
#Correlation Plot:
corrgram(bd[,],order=FALSE,upper.panel=panel.pie,text.panel=panel.txt,main="correlation plot")
#It shows multicollinearity exists, which can be removed during modelling using vif function.


summary(bd)
str(bd)


#Dummy variable creation:
str(bd)
colnames(bd)
cats=sapply(bd, is.factor)
cat=bd[,cats]
cat_names=colnames(cat)
cat_names
library(fastDummies)
bd1=dummy_cols(bd, select_columns = c( "job","marital","education","housing","loan","contact","month","day_of_week","campaign","pdays","previous","poutcome" )
                  ,remove_first_dummy = TRUE)

bd1=bd1[-c(1:9)]
bd1=bd1[-c(2:5)]

str(bd1)


# splitting :into train and test data
set.seed(100)
split_indices=sample.split(bd1$y, SplitRatio = 0.70)
train=bd1[split_indices, ]
test=bd1[!split_indices, ]
nrow(train)/nrow(bd1)
nrow(test)/nrow(bd1)
test1=test[,-7]

#Model Building and Prediction:
#Logistic Regression:

model1=glm(y~.,data=train,family="binomial")
summary(model1)

stepAIC(model1)

model2=glm(y ~ duration + emp.var.rate + cons.price.idx + 
              cons.conf.idx + euribor3m + nr.employed + job_services + 
              `job_blue-collar` + job_retired + job_entrepreneur + marital_single + 
              education_tertiary + housing_unknown + contact_cellular + 
              month_jul + month_aug + month_oct + month_dec + month_mar + 
              month_apr + month_sep + day_of_week_tue + day_of_week_wed + 
              day_of_week_thu + day_of_week_fri + `pdays_recent call` + 
              `pdays_not recent call` + poutcome_failure + poutcome_success, 
               family = "binomial", data = train)
summary(model2)
vif(model2)

#removing nr.employed  
model3=glm(y ~ duration + emp.var.rate + cons.price.idx + 
             cons.conf.idx + euribor3m + job_services + 
             `job_blue-collar` + job_retired + job_entrepreneur + marital_single + 
             education_tertiary + housing_unknown + contact_cellular + 
             month_jul + month_aug + month_oct + month_dec + month_mar + 
             month_apr + month_sep + day_of_week_tue + day_of_week_wed + 
             day_of_week_thu + day_of_week_fri + `pdays_recent call` + 
             `pdays_not recent call` + poutcome_failure + poutcome_success, 
           family = "binomial", data = train)


summary(model3)
vif(model3)

#removing  cons.conf.idx 
model4=glm(y ~ duration + emp.var.rate + cons.price.idx + 
             euribor3m + job_services + 
             `job_blue-collar` + job_retired + job_entrepreneur + marital_single + 
             education_tertiary + housing_unknown + contact_cellular + 
             month_jul + month_aug + month_oct + month_dec + month_mar + 
             month_apr + month_sep + day_of_week_tue + day_of_week_wed + 
             day_of_week_thu + day_of_week_fri + `pdays_recent call` + 
             `pdays_not recent call` + poutcome_failure + poutcome_success, 
           family = "binomial", data = train)

summary(model4)
vif(model4)


#removing pdays_not recent call

model5=glm(y ~ duration + emp.var.rate + cons.price.idx + 
             euribor3m + job_services + 
             `job_blue-collar` + job_retired + job_entrepreneur + marital_single + 
             education_tertiary +  contact_cellular + 
             month_jul + month_aug + month_oct + month_dec + month_mar + 
             month_apr + month_sep + day_of_week_tue + day_of_week_wed + 
             day_of_week_thu + day_of_week_fri + `pdays_recent call` + 
             `pdays_not recent call` + poutcome_failure + poutcome_success, 
           family = "binomial", data = train)

summary(model5)
vif(model5)

#removing job1_entrepreneur
model6=glm(y ~ duration + emp.var.rate + cons.price.idx + 
             euribor3m + job_services + 
             `job_blue-collar` + job_retired +  marital_single + 
             education_tertiary +  contact_cellular + 
             month_jul + month_aug + month_oct + month_dec + month_mar + 
             month_apr + month_sep + day_of_week_tue + day_of_week_wed + 
             day_of_week_thu + day_of_week_fri + `pdays_recent call` + 
             `pdays_not recent call` + poutcome_failure + poutcome_success, 
           family = "binomial", data = train)
summary(model6)
vif(model6)


prediction_glm=round(predict(model6,newdata =test1,type="response"),2)
prediction_glm

compare_glm=table(test$y,round(prediction_glm))
compare_glm
#Confusion matrix for Logistic Regression:
confusionMatrix(compare_glm)


#Variable Importance:
model6$coefficients
important=as.data.frame(varImp(model6, scale = FALSE))
important=data.frame(overall = important$Overall, features = rownames(important))
important=important[order(important$overall,decreasing = TRUE),]

#Accuracy is good but False Negative Rate is high. So, we can go with other algorithms such as Random Forest.
#But due to time constraints I'm submitting it now.
#I will pursue it with other algorithms such as Random Forest, SVM later.


###################################################################################################################



