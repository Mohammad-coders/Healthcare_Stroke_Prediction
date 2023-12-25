library(readxl)
library(dplyr)
library(caret)
library(e1071)

stroke<- read_excel("E:/AIUB/9th semester/Data Science/Final/Dataset/healthcare-dataset-stroke-data.xlsx")
print(stroke)

names(stroke)
summary(stroke)

str(stroke)

stroke$hypertension<-as.factor(stroke$hypertension)
stroke$heart_disease<-as.factor(stroke$heart_disease)
stroke$stroke<-as.factor(stroke$stroke)
stroke$bmi<- as.integer(stroke$bmi)
colSums(is.na(stroke))
stroke$bmi[is.na(stroke$bmi)] <- mean(stroke$bmi, na.rm = TRUE)

colSums(is.na(stroke))
str(stroke)

boxplot(stroke$bmi,main="Outlier BMI")

q1<-quantile(stroke$bmi,0.25)

q3<-quantile(stroke$bmi,0.75)

iqr<-q3-q1
lower<- q1-1.5*iqr
upper<- q3+1.5*iqr

outliers_bmi<- stroke$bmi < lower | stroke$bmi > upper

stroke$bmi <- ifelse(outliers_bmi,NA,stroke$bmi)
colSums(is.na(stroke))

stroke$bmi[is.na(stroke$bmi)]<-mean(stroke$bmi,na.rm = TRUE)
boxplot(stroke$bmi, main='Remove Outlier BMI')

str(stroke)

boxplot(stroke$avg_glucose_level,main="Outlier of Avg Glucose level")

q1 <- quantile(stroke$avg_glucose_level,0.25) 
q3 <- quantile(stroke$avg_glucose_level,0.75)
iqr <- q3 - q1
lower <- q1-1.5*iqr
upper<- q3+1.5*iqr
outliers_avg_glucose_level<- stroke$avg_glucose_level < lower | stroke$avg_glucose_level > upper
stroke$avg_glucose_level <- ifelse(outliers_avg_glucose_level,NA,stroke$avg_glucose_level)
colSums(is.na(stroke))
stroke$avg_glucose_level[is.na(stroke$avg_glucose_level)]<-mean(stroke$avg_glucose_level,na.rm = TRUE)
colSums(is.na(stroke))
boxplot(stroke$avg_glucose_level,main="Remove Outlier Avg Glucose level")


Pearsons_Chi_squared<- function(attribute){
  count_instances<- table(stroke[[attribute]],stroke$stroke)
  p_value<- chisq.test(count_instances)
  return(p_value)
}
pearson_corelation_id <- Pearsons_Chi_squared('id')
pearson_corelation_gender <- Pearsons_Chi_squared('gender')
pearson_corelation_age <- Pearsons_Chi_squared('age')
pearson_corelation_hypertension <- Pearsons_Chi_squared('hypertension')
pearson_corelation_heart_disease <- Pearsons_Chi_squared('heart_disease')
pearson_corelation_ever_married <- Pearsons_Chi_squared('ever_married')
pearson_corelation_work_type <- Pearsons_Chi_squared('work_type')
pearson_corelation_Residence_type <- Pearsons_Chi_squared('Residence_type')
pearson_corelation_smoking_status <- Pearsons_Chi_squared('smoking_status')
pearson_corelation_bmi <- Pearsons_Chi_squared('bmi')
pearson_corelation_avg_glucose_level <- Pearsons_Chi_squared('avg_glucose_level')
pearson_corelation_stroke <- Pearsons_Chi_squared('stroke')

if(pearson_corelation_id$p.value<0.05){
  print("There is a significant relationship between id and stroke.")
}else{
  stroke<- subset(stroke,select = -id)
  print("No significant relationship found.")
}

if(pearson_corelation_gender$p.value<0.05){
  print("There is a significant relationship between gender and stroke.")
}else{
  stroke<- subset(stroke,select = -gender)
  print("No significant relationship found.")
}

if(pearson_corelation_age$p.value<0.05){
  print("There is a significant relationship between age and stroke.")
}else{
  stroke<- subset(stroke,select = -age)
  print("No significant relationship found.")
}

if(pearson_corelation_hypertension$p.value<0.05){
  print("There is a significant relationship between hypertension and stroke.")
}else{
  stroke<- subset(stroke,select = -hypertension)
  print("No significant relationship found.")
}

if(pearson_corelation_heart_disease$p.value<0.05){
  print("There is a significant relationship between heart_disease and stroke.")
}else{
  stroke<- subset(stroke,select = -heart_disease)
  print("No significant relationship found.")
}

if(pearson_corelation_ever_married$p.value<0.05){
  print("There is a significant relationship between ever married and stroke.")
}else{
  stroke<- subset(stroke,select = -ever_married)
  print("No significant relationship found.")
}

if(pearson_corelation_work_type$p.value<0.05){
  print("There is a significant relationship between work type and stroke.")
}else{
  stroke<- subset(stroke,select = -work_type)
  print("No significant relationship found.")
}

if(pearson_corelation_Residence_type$p.value<0.05){
  print("There is a significant relationship between residence type and stroke.")
}else{
  stroke<- subset(stroke,select = -Residence_type)
  print("No significant relationship found.")
}

if(pearson_corelation_smoking_status$p.value<0.05){
  print("There is a significant relationship between smoking status and stroke.")
}else{
  stroke<- subset(stroke,select = -smoking_status)
  print("No significant relationship found.")
}

if(pearson_corelation_bmi$p.value<0.05){
  print("There is a significant relationship between bmi and stroke.")
}else{
  stroke<- subset(stroke,select = -bmi)
  print("No significant relationship found.")
}

if(pearson_corelation_avg_glucose_level$p.value<0.05){
  print("There is a significant relationship between avg glucose level and stroke.")
}else{
  stroke<- subset(stroke,select = -avg_glucose_level)
  print("No significant relationship found.")
}

if(pearson_corelation_stroke$p.value<0.05){
  print("There is a significant relationship between stroke  and stroke.")
}else{
  stroke<- subset(stroke,select = -stroke)
  print("No significant relationship found.")
}



set.seed(123)
split_Index<- createDataPartition(stroke$stroke, p = 0.8, list = FALSE)
train_data<- stroke[split_Index, ]
test_data <- stroke[-split_Index, ]

nb_model <- naiveBayes(stroke ~ ., data = train_data)
predictions <- predict(nb_model, test_data)
table(predictions)

accuracy <- sum(predictions == test_data$stroke) / nrow(test_data)
cat("Naive Bayes Accuracy:", round(accuracy,2), "\n")


confusion_matrix<- table(predictions,test_data$stroke)

recall_matrix<- confusion_matrix[2,2]/sum(confusion_matrix[2,])

precision_matrix<- confusion_matrix[2,2]/sum(confusion_matrix[,2])

f_measure <- 2 * (precision_matrix * recall_matrix) / (precision_matrix + recall_matrix)

cat("Confusion Matrix: ",confusion_matrix,"\n")
cat("Recall: ",recall_matrix_second_class,"\n")
cat("Precision: ",precision_matrix_second_class,"\n")
cat("f-measure: ",f_measure,"\n")

