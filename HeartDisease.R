library(lattice)
library(ggplot2)
library(readr)
library(caret)
library(tidyr)
library(dplyr)
library(corrplot)

###file's name call is depending on its downloaed location
heart <- read_csv("R_Projects/HeartDiseaseReport/heart.csv")


#structure of the dataset
str(heart)
names(heart)

#does data contain any missing values
sum(is.na(heart))
#removing near zere variance
nzv <- nearZeroVar(heart)
#once removed these columns, these are keeping columns
col_index <- setdiff(1:ncol(heart), nzv)
length(col_index)

summary(heart)
heart_df <- heart

###Displaying graphical correlation matrix, confidence interval.
#along with significant level coded as '***' 0.001 '**' 0.01 '*' 0.05 
correlation <- cor(heart)
col1 <- colorRampPalette(c("#5C9147","#FFFFFF", "#c00000")) #custom color
res1 <- cor.mtest(heart, conf.level = .95) #confidence interval

corrplot(correlation, "ellipse", 
         type = "lower", order = "FPC",
         tl.col = "black", tl.srt = 45, #text color and style
         p.mat = res1$p, insig = "label_sig", 
         sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "white",
         addrect = 2, col = col1(200), 
         diag = FALSE)

###converting type to factor
heart$target<-as.factor(heart$target)
heart$sex<-as.factor(heart$sex)
heart$cp<-as.factor(heart$cp)

heart$fbs<-as.factor(heart$fbs)
heart$exang<-as.factor(heart$exang)
hearteart$restecg<-as.factor(heart$restecg)
heart$slope<-as.factor(heart$slope)
heart$thal<-as.factor(heart$thal)

##converting columns from numeric to categorical
levels(heart$sex)[levels(heart$sex)==0] <- "Female"
levels(heart$sex)[levels(heart$sex)==1] <- "Male"
levels(heart$fbs)[levels(heart$fbs)==0] <- "Fasting Blood Sugar <= 120"
levels(heart$fbs)[levels(heart$fbs)==1] <- "Fasting Blood Sugar > 120"
levels(heart$thal)[levels(heart$thal)==0] <- "No Thalassemia"
levels(heart$thal)[levels(heart$thal)==1] <- "Normal Thalassemia"
levels(heart$thal)[levels(heart$thal)==2] <- "Fixed Defect Thalassemia"
levels(heart$thal)[levels(heart$thal)==3] <- "Reversible Defect Thalassemia"
levels(heart$target)[levels(heart$target)==0] <- "Healthy"
levels(heart$target)[levels(heart$target)==1] <- "Heart Disease"

############VISUALIZATION###########
library("ggplot2")
ggplot(heart, aes(target, fill=target))+
  geom_bar(stat="count",  alpha=0.8 )+
  geom_text(stat = "count", aes(label = ..count.. ), size = 3, 
            position = position_dodge(width = 1),
            vjust = -0.5) +
  guides(fill=F)+
  scale_fill_manual(values=c("#5C9147","#c00000")) +
  labs(x="heart condition") +
  labs(y="number of people") +
  ggtitle("Healthy vs. Heart disease")

###Total Male vs. Female in the dataset
ggplot(heart, aes(sex, fill=sex))+
  geom_bar(stat="count")+
  geom_text(stat = "count", aes(label = ..count.. ), size = 3, 
            position = position_dodge(width = 1),
            vjust = -0.5) +
  guides(fill=F)+
  scale_fill_manual(values=c("#AA4371","#3A5795")) +
  labs(x="sex") +
  labs(y="number of people") +
  ggtitle("Number of Male vs. Female")


###Table of heart condition by sex
heart %>% group_by(sex, target) %>% 
  summarize(n = n()) %>% knitr::kable()

###Heart condition sorted by sex in the dataset
ggplot(heart, aes(x= target, fill=target)) +
  geom_bar(stat="count", position = 'dodge', alpha=0.8 ) +
  geom_text(stat = "count", aes(label = ..count.. ), size = 3, 
            position = position_dodge(width = 1),
            vjust = -0.5) +
  facet_wrap(~sex, ncol=2, scale="fixed") +
  guides(fill=F)+
  scale_fill_manual(values=c("#5C9147","#c00000")) +
  labs(x="heart condition") +
  labs(y="number of people") +
  ggtitle("Heart condition by sex") 


###By age###
heart %>% group_by(target) %>% 
  summarize(n = n(), min = min(age), max = max(age), median = median(age), avg = mean(age)) %>% 
  knitr::kable()

ggplot(heart, aes(age, fill=target))+
  geom_density(alpha=0.6)+
  guides(col=F)+
  scale_fill_manual(values=c("#5C9147","#c00000")) +
  labs(x="Age", fill="Target")+
  ggtitle("Heart condition by age") 

##chest pain
heart %>% group_by(cp, target) %>% 
  summarize(n = n() ) %>% 
  knitr::kable()

ggplot(heart, aes(cp, fill=target))+
  geom_bar(stat="count", alpha=0.8)+
  scale_fill_manual(values=c("#5C9147","#c00000"))+
  labs(x= "Chest Pain Type", fill="Target")

###Blood pressure
ggplot(heart, aes(trestbps, fill=target))+
  geom_density(alpha=0.6)+
  guides(col=F)+
  scale_fill_manual(values=c("#5C9147","#c00000")) +
  labs(x="Resting Blood Pressure", fill="Target")

##Distribution between fasting blood sugar and heart condition 
ggplot(heart, aes(x= target, fill=target)) +
  geom_bar(stat="count", position = 'dodge', alpha=0.7) +
  geom_text(stat = "count", aes(label = ..count.. ), size = 3, 
            position = position_dodge(width = 1),
            vjust = -0.5) +
  facet_wrap(~fbs, ncol=2, scale="fixed") +
  guides(fill=F)+
  scale_fill_manual(values=c("#5C9147","#c00000")) +
  labs(x="heart condition") +
  labs(y="number of people") +
  ggtitle("Heart condition by fasting blood sugar") 

####Distribution between restecg and heart condition 
ggplot(heart, aes(x= target, fill=target)) +
  geom_bar(stat="count", position = 'dodge', alpha=0.7) +
  geom_text(stat = "count", aes(label = ..count.. ), size = 3, 
            position = position_dodge(width = 1),
            vjust = -0.5) +
  facet_wrap(~restecg, ncol=3, scale="fixed") +
  guides(fill=F)+
  scale_fill_manual(values=c("#5C9147","#c00000")) +
  labs(x="heart condition") +
  labs(y="number of people") +
  ggtitle("Heart condition by rest ECG") 

##Distribution between thalac and heart condition 
ggplot(heart, aes(thalach, fill=target))+
  geom_density(alpha=0.6)+
  guides(col=F)+
  scale_fill_manual(values=c("#5C9147","#c00000")) +
  labs(x="thalach", fill="Target")

####Distribution between exang and heart condition 
ggplot(heart, aes(x= target, fill=target)) +
  geom_bar(stat="count", position = 'dodge', alpha=0.7) +
  geom_text(stat = "count", aes(label = ..count.. ), size = 3, 
            position = position_dodge(width = 1),
            vjust = -0.5) +
  facet_wrap(~exang, ncol=2, scale="fixed") +
  guides(fill=F)+
  guides(fill=F)+
  scale_fill_manual(values=c("#5C9147","#c00000")) +
  labs(x="heart condition") +
  labs(y="number of people") +
  ggtitle("Exang")

##Distribution between oldpeak and heart condition 
ggplot(heart, aes(oldpeak, fill=target))+
  geom_density(alpha=0.6)+
  guides(col=F)+
  scale_fill_manual(values=c("#5C9147","#c00000")) +
  ggtitle("No. Major Vessels Histogram") +
  labs(x="Number of old peak", fill="Target")

####Distribution between slope and heart condition 
ggplot(heart, aes(x= target, fill=target)) +
  geom_bar(stat="count", position = 'dodge', alpha=0.7) +
  geom_text(stat = "count", aes(label = ..count.. ), size = 3, 
            position = position_dodge(width = 1),
            vjust = -0.5) +
  facet_wrap(~slope, ncol=3, scale="fixed") +
  guides(fill=F)+
  scale_fill_manual(values=c("#5C9147","#c00000")) +
  labs(x="heart condition") +
  labs(y="number of people") +
  ggtitle("Heart condition by slope") 

##Distribution between number of major vessels colored by flourosopy and heart condition  
ggplot(heart, aes(ca, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(0, 5, by=1), color="grey17", alpha=0.8) +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  scale_fill_manual(values=c("#5C9147","#c00000")) +
  ggtitle("No. Major Vessels Histogram") +
  labs(x="Number of major vessels colored by flourosopy")

####Distribution between thal and heart condition 
ggplot(heart, aes(target, fill=target)) +
  geom_bar(stat="count", position = 'dodge', alpha=0.7) +
  geom_text(stat = "count", aes(label = ..count.. ), size = 3, 
            position = position_dodge(width = 1),
            vjust = -0.5) +
  facet_wrap(~thal, ncol=2,scale="fixed") +
  scale_fill_manual(values=c("#5C9147","#c00000")) +
  ggtitle("Thalassemia Histogram") 

#############Ensemble with ALL predictors############################
#Spliting training set into two parts based on outcome: 75% and 25%
y <- heart$target

set.seed(1)
test_index = createDataPartition(y, times = 1, p=0.75,list=FALSE)
train_set <- heart[test_index,]
valid_set <- heart[-test_index,]

models <- c("glm", "lda",  "naive_bayes",  "svmLinear", "qda",
            "knn", "kknn", "gam", "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "monmlp", "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")


#set.seed(1)
fits <- lapply(models, function(model){ 
  print(model)
  train(target ~ ., method = model, data = train_set)
}) 
#fits
names(fits) <- models

#all the trained models is in a list now. Next, creating a matrix of predictions for the test set
pred <- sapply(fits, function(object) 
  predict(object, newdata = valid_set))
dim(pred)

head(pred)

#Accuracy for each model in the test set 
#and the mean accuracy across all models can be computed using the following code:
acc <- colMeans(pred == valid_set$target)

acc_results <- data_frame(method = models, acc = acc)
acc_results %>% knitr::kable()

#Result of the mean accuracy across all models.
avg <- mean(acc)
avg
#Majority Voting: In majority voting, we’ll assign the prediction for the observation as predicted by 
#the majority of models. Since we have three models for a binary classification task, a tie is not possible.
#Next, build an ensemble prediction by majority vote and compute the accuracy of the ensemble.
#What is the accuracy of the ensemble
votes <- rowMeans(pred == "Heart Disease")
y_hat <- ifelse(votes > 0.5, "Heart Disease", "Healthy")
votes_avg <- mean(y_hat == valid_set$target)
votes_avg

#Which individual methods perform better than the ensemble
ind <- acc > mean(y_hat == valid_set$target)
sum(ind)
models[ind]


#using the accuracy estimates obtained from cross validation with the training data
#finding mean accuracy of the new estimates
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
acc_hat
new_mean <- mean(acc_hat)
new_mean


#Now let's only consider the methods 
#with an estimated accuracy of greater than or equal to the new_mean when constructing the ensemble
ind <- acc_hat >= new_mean
sum(ind)
models[ind]

votes <- rowMeans(pred[,ind] == "Heart Disease")
y_hat <- ifelse(votes >=0.5, "Heart Disease", "Healthy")
new_votes_avg <- mean(y_hat == valid_set$target)
new_votes_avg


####adding new column of accuracies to result table 
acc_results <- bind_cols(acc_results,
                         data_frame(acc_hat = acc_hat ))

#adding new row of Ensemble Averages to result table 
acc_results <- bind_rows(acc_results ,
                         data_frame(method="Ensemble Average",  
                                    acc = avg, acc_hat = new_mean ))
##adding new row of Ensemble Majority Vote to result table 
final_results <- bind_rows(acc_results ,
                         data_frame(method="Ensemble Majority Vote",  
                                    acc = votes_avg, acc_hat = new_votes_avg))
final_results %>% knitr::kable()


#############Ensemble with SELECTED predictors############################
#Spliting training set into two parts based on outcome: 75% and 25%
heart_selected <- heart[,c(2,3,9,10,12,14)]
summary(heart_selected)
y_selected <- heart_selected$target

set.seed(1)
test_index = createDataPartition(y_selected, times = 1, p=0.75,list=FALSE)
train_selected <- heart_selected[test_index,]
test_selected <- heart_selected[-test_index,]

models_selected <- c("glm", "lda",  "naive_bayes",  "svmLinear", "qda",
            "knn", "kknn", "gam", "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "monmlp", "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")


fits_selected <- lapply(models_selected, function(model){ 
  print(model)
  train(target ~ ., method = model, data = train_selected)
}) 

names(fits_selected) <- models_selected

#all the trained models is in a list now. Next, creating a matrix of predictions for the test set
pred_selected <- sapply(fits_selected, function(object) 
  predict(object, newdata = test_selected))
dim(pred_selected)

head(pred_selected)

#Accuracy for each model in the test set 
#and the mean accuracy across all models can be computed using the following code:
acc2 <- colMeans(pred_selected == test_selected$target)

acc2_results <- data_frame(method = models_selected, acc_selected = acc2)
acc2_results %>% knitr::kable()

#Result of the mean accuracy across all models.
avg2 <- mean(acc2)
avg2
#Majority Voting: In majority voting, we’ll assign the prediction for the observation as predicted by 
#the majority of models. Since we have three models for a binary classification task, a tie is not possible.
#Next, build an ensemble prediction by majority vote and compute the accuracy of the ensemble.
#What is the accuracy of the ensemble
votes2 <- rowMeans(pred_selected == "Heart Disease")
y_hat2 <- ifelse(votes2 > 0.5, "Heart Disease", "Healthy")
votes_avg2 <- mean(y_hat2 == test_selected$target)
votes_avg2

#Which individual methods perform better than the ensemble
ind2 <- acc2 > mean(y_hat2 == test_selected$target)
sum(ind2)
models_selected[ind2]


#using the accuracy estimates obtained from cross validation with the training data
#finding mean accuracy of the new estimates
acc2_hat <- sapply(fits_selected, function(fit) min(fit$results$Accuracy))
acc2_hat
new_mean2 <- mean(acc2_hat)
new_mean2


#Now let's only consider the methods 
#with an estimated accuracy of greater than or equal to the new_mean when constructing the ensemble
ind2 <- acc2_hat >= new_mean2
sum(ind2)
models_selected[ind2]

votes2 <- rowMeans(pred_selected[,ind2] == "Heart Disease")
y_hat2 <- ifelse(votes2 >=0.5, "Heart Disease", "Healthy")
new_votes_avg2 <- mean(y_hat2 == test_selected$target)
new_votes_avg2


####adding new column of accuracies to result table 
acc2_results <- bind_cols(acc2_results,
                         data_frame(hat_selected = acc2_hat ))

#adding new row of Ensemble Averages to result table 
acc2_results <- bind_rows(acc2_results ,
                         data_frame(method="Ensemble Average",  
                                    acc_selected = avg2, hat_selected = new_mean2 ))
##adding new row of Ensemble Majority Vote to result table 
final_results2 <- bind_rows(acc2_results ,
                           data_frame(method="Ensemble Majority Vote",  
                                      acc_selected = votes_avg2, hat_selected = new_votes_avg2))
final_results2 %>% knitr::kable()

##########################END#########################
