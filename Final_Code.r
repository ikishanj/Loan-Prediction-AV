#This is a script written by Kishan J for predicting the loan prediction III data set and a learning competation on Analytics Vidhya


#reading and storing the data
test = read.csv(file = "test.csv")
train = read.csv(file = "train.csv")

#creating a loan_status colum in test
test$Loan_Status <- rep("",367)

#understanding the data and data visualization

ggplot(data=train_rf,aes(x=Loan_Status, fill = as.factor(Dependents))) + geom_bar(position = "fill")
ggplot(data=train_rf,aes(x=Loan_Status, fill = as.factor(Income_be_Loan))) + geom_bar(position = "fill")
ggplot(data=train_rf,aes(x=Loan_Status, fill = as.factor(Property_Area))) + geom_bar(position = "fill")
ggplot(data=train_rf,aes(x=Loan_Status, fill = as.factor(Gender))) + geom_bar(position = "dodge")
ggplot(data=train_rf,aes(x=Loan_Status, fill = as.factor(Self_Employed))) + geom_bar(position = "fill")
ggplot(data=train_rf,aes(x=Property_Area, fill = as.factor(Loan_Status))) + geom_bar(position = "fill")
ggplot(data = train_rf, aes(x= Dependents, fill= Property_Area) ) + geom_bar(position = "fill")

ggplot(data=train_rf,aes(x= Property_Area, fill = as.factor(Dependents))) + geom_bar() + facet_wrap(~ Dependents)



ggparallel(names(train_rf)[c(12,18)], order=0,train_rf, weight=NULL) +
  scale_fill_brewer(palette="Paired", guide="none") +
  scale_colour_brewer(palette="Paired", guide="none")

#Dealing with the missing data


#firstly calculating the how many missing values per each variable and adding it as a colomn 
train_NAs<-NULL
test_NAs<-NULL
for(i in 1:nrow(train)) train_NAs[i]<-sum(is.na(train[i, ]))
train$NA_number<-train_NAs
for(i in 1:nrow(test)) test_NAs[i]<-sum(is.na(test[i, ]))
test$NA_number<-test_NAs
#the ratio of missing for each variable
names<-names(train)
missing<-data.frame(variable=names,missing_proportion=sapply(names,function(x) sum(is.na(train[x]))/nrow(train)))


combi <- rbind(train,test)
#gender is differentiated by 112 female and 489 male with 13 missing. SO changing the 13 blanks in to female.
combi$Gender[combi$Gender == ""] <- "Female"
#Filling the Married with yes
combi$Married[combi$Married == ""] <- "Yes"

#Putting out the levels in self-delependents
levels(combi$Dependents)[levels(combi$Dependents) == "3+"] <- "3"
combi$Dependents <- as.integer(as.character(combi$Dependents))
#predicting the missing values in the Self-dependents
DP_fit <- rpart(Dependents ~ Married  + Education + ApplicantIncome+
                  CoapplicantIncome + LoanAmount + Loan_Status + Credit_History,
                data = combi[!is.na(combi$Credit_History),],
                method = "anova")

combi$Dependents[is.na(combi$Dependents)] <- predict(DP_fit,combi[is.na(combi$Dependents),])


#predicting self-employed
combi$Self_Employed[combi$Self_Employed == ""] <- NA
combi$Self_Employed <- as.numeric(combi$Self_Employed)
combi$Self_Employed <- combi$Self_Employed - 2


SE_fit <- rpart(Self_Employed ~ Married  + Education + ApplicantIncome+
                  CoapplicantIncome + LoanAmount + Loan_Status + Credit_History,
                data = combi[!is.na(combi$Self_Employed),],
                method = "anova")

combi$Self_Employed[is.na(combi$Self_Employed)] <- predict(SE_fit,combi[is.na(combi$Self_Employed),])


#predicting the credit history


CH_fit <- rpart(Credit_History ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome+
                  CoapplicantIncome + LoanAmount + Loan_Status,
                data = combi[!is.na(combi$Credit_History),],
                method = "anova")

combi$Credit_History[is.na(combi$Credit_History)] <- predict(CH_fit, combi[is.na(combi$Credit_History),])


#dealing the Loan Amount, replacing Na with the avg amount
combi$LoanAmount[is.na(combi$LoanAmount)] <- 144
#dealing the Loan
combi$Loan_Amount_Term[is.na(combi$Loan_Amount_Term)] <- 360


#Feature Engineering

#Summing up the tootal income of the applicant
combi$Total_income <- combi$ApplicantIncome + combi$CoapplicantIncome

#calculating the EMI of the loan amount
combi$EMI <- (combi$LoanAmount*1000*0.0081*(1+0.0081)^combi$Loan_Amount_Term)/((1+0.0081)^combi$Loan_Amount_Term-1)

#calculating EMI pay back ratio w.r.t applicant income
combi$Income_be_Loan <- combi$EMI/combi$ApplicantIncome

#calculating EMI pay back ratio w.r.t total income
combi$Total_Income_be_Loan <- combi$EMI/combi$Total_income

#calculating the pay back by not adding the interest(just by dividing the loan amount with applicant income)
combi$AI_by_Loan <- combi$ApplicantIncome / combi$LoanAmount

#calculating the pay back by not adding interest(by dividing the loan amount with total income)
combi$AICP_by_Loan <- combi$Total_income / combi$LoanAmount

#Assuming that if the individual is educated and selfemployed, can increase the loan approval chance and so combing both the variables
#combi$Edu_SE <- with(combi,paste0(Education,Self_Employed))

#Rounding off the variables and making them in to factors
combi$Credit_History <- round(combi$Credit_History, digits = 0)
combi$Credit_History <- as.factor(combi$Credit_History)

combi$AI_by_Loan <- round(combi$AI_by_Loan,digits=0)
combi$AICP_by_Loan <- round(combi$AICP_by_Loan, digits = 0)

combi$Dependents <- as.integer(as.character(combi$Dependents))
combi$Dependents <- round(combi$Dependents,digits=0)
combi$Dependents <- as.factor(combi$Dependents)

combi$Self_Employed <- as.factor(combi$Self_Employed)
#modeling

#Splitting the datasets
train <- combi[1:614,]
test <- combi[615:981,]
#droping the levels in train$Loanstatus
train$Loan_Status <- droplevels(train$Loan_Status)

#doing thr fit with rpart

fit<- rpart(Loan_Status ~ Dependents+ Gender +Married + CoapplicantIncome+ ApplicantIncome+ Education + Self_Employed +Property_Area  + Income_be_Loan +EMI + AICP_by_Loan+ NA_number+ app_YN +Credit_History,
            data = train,
            method = "class")



#predicting and creating a submission file
Prediction <- predict(fit,test ,type = "class")
submit <- data.frame(Loan_ID = test$Loan_ID,Loan_Status = Prediction)
write.csv(submit,file = "Finalcode_1.csv",row.names = FALSE)


#Rforest
feature_E_rf <- randomForest(Loan_Status~., data=train[-c (1,8, 9,16,17)], mtry=3, ntree=1000)
feature_E_rf
