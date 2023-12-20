library(readxl)
stroke<- read_excel("E:/AIUB/9th semester/Data Science/Final/Dataset/healthcare-dataset-stroke-data.xlsx")
print(stroke)

names(stroke)
summary(stroke)

str(stroke)
stroke$bmi[stroke$bmi=="N/A"]<- NA
colSums(is.na(stroke))
stroke$bmi<- as.integer(stroke$bmi) # convert char to integer

str(stroke)



stroke$bmi[is.na(stroke$bmi)] <- mean(stroke$bmi, na.rm = TRUE)

colSums(is.na(stroke))

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


str(stroke)
stroke$hypertension<-as.factor(stroke$hypertension)
stroke$heart_disease<-as.factor(stroke$heart_disease)
str(stroke)


#function of corelation
corelation<- function(attribute){
  rel<- cor(stroke$stroke,stroke[[attribute]])
  return(rel)
}
corelation ('bmi')
corelation ('avg_glucose_level')
corelation('stroke')
corelation('id')


table(stroke$stroke)
str(stroke)
