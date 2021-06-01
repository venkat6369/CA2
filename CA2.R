#------------------------------------
#CA2 - Predictive Modelling
#Author - Venkata sri sai gowtham
#Module - Data Science
#Mentor - James Conolly
#------------------------------------------------------
#Predictive modelling on Heart attack analysis data
#This modelling an be achieved through the steps stated below
#1. Loading and preparing the data
#2. Checking for correlation and eliminating the impactless factors
#3. Checking for outliers and removing them if any
#4. Building the model
#5. Evaluating the model
# ------------------------------------------------------------
#  I. Data preparation

#Loading the data and viewing it
ht_attack <- read.csv("heart.csv")
View(ht_attack)

#The coloumn names are changed as per the data description
new_colnames <- c("Age","Sex","Chest Pain type","Resting blood pressure (in mm Hg)","Cholestrol","Fasting blood sugar","Resting electrocardiographic","Maximum heart rate achieved","Exercise induced angina","Oldpeak","Slope","Number of major vessels","Thal","Target")
colnames(ht_attack)<- new_colnames
View(ht_attack)


# Check for missing data as it is vital
incomplete_data <- ht_attack[!complete.cases(ht_attack),]
incomplete_data
# the data is complete.

# visualize the data for missing variables
# for this task mice library is used
library("mice")
md.pattern((ht_attack))

#Sub-setting the required model data.
#here only the variables of requirement to the building model have been sub-setted to a new data frame. 

ht_model_data <- subset(ht_attack, select = c(1,2,4,5,6,8,12,14))
View(ht_model_data)

#--------------------------------------------------------------------------------------------------------------------------
#Checking correlation of the variables.


#Here the total correlation distribution plot of the total model data is diplayed using pairs function
pairs(ht_model_data)

#PLotting scatter plots each independent variable against the dependent variable to get better insights of correlation
#for this model the dependent variable is Target, and remaining all are dependent variables.
# plotting of dependent is done on x-axis and independent is done on y-axis of graph resepectivley.
#when a graph doesnt reveal anything then correlation value is generated using 'corr' function 
attach(ht_model_data)

#scatter plot for Age
scatter.smooth(x = Target, 
               y = Age,
               main = "Impact on chances of heart attack - Age wise",
               xlab = "Chances of Heart Attack",
               ylab = "Age")

cor(Target, Age)
# -0.22

#scatter plot for Sex
scatter.smooth(x = Target, 
               y = Sex,
               main = "Impact on chances of heart attack - Gender wise",
               xlab = "Chances of Heart Attack",
               ylab = "Sex")

cor(Target, Sex)
#-0.28

#scatter plot for Resting blood pressure 
scatter.smooth(x = Target, 
               y = `Resting blood pressure (in mm Hg)`,
               main = "Impact on chances of heart attack with respect to Blood pressure",
               xlab = "Chances of Heart Attack",
               ylab = "BP")

cor(Target, `Resting blood pressure (in mm Hg)`)
#-0.1449311

#scatter plot for Cholestrol
scatter.smooth(x = Target, 
               y = Cholestrol,
               main = "Impact of cholestrol level on chances of heart attack",
               xlab = "Chances of Heart Attack",
               ylab = "Cholestrol level")

cor(Target, Cholestrol)
#-0.08523911

#scatter plot for Fasting blood sugar
scatter.smooth(x = Target, 
               y = `Fasting blood sugar`,
               main = "Impact of Blood sugar level on chances of heart attack",
               xlab = "Chances of Heart Attack",
               ylab = "Blood sugar level")

cor(Target, `Fasting blood sugar`)
#-0.02804576

#scatter plot for Heart rate
scatter.smooth(x = Target, 
               y = `Maximum heart rate achieved`,
               main = "Impact of Heart rate on chances of heart attack",
               xlab = "Chances of Heart Attack",
               ylab = "Heart Rate (max)")

cor(Target, `Maximum heart rate achieved`)
# 0.4217409

#scatter plot for Number of major vessels.
scatter.smooth(x = Target, 
               y = `Number of major vessels`,
               main = "Impact of major vessels(count) on chances of heart attack",
               xlab = "Chances of Heart Attack",
               ylab = "Major vessels (count)")

cor(Target, `Number of major vessels`)
#-0.391724

#-0.2 < x < 0.2  values under this range fall into low correlation category and can dropped or kept in the model as per requirement,
#+ve correlation - Heart rate
#-ve correlation - Major vessels,Age, Sex
#very low -ve correlation - Cholestrol,Resting blood pressure,Fasting Blood Sugar.
#no varaiable is dropped as per the requirement of model
#---------------------------------------------------------------------------------------------------------------------------------------
#Plot for otliers and removing if there are any
# boxplot is used for plotting outliers here 

opar <- par(no.readonly = TRUE)
par(mfrow = c(3,2))

boxplot(Age, 
        main = "Age", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Age)$out))

boxplot(Sex, 
        main = "Sex", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Sex)$out))

boxplot(`Resting blood pressure (in mm Hg)`, 
        main = "BP", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(`Resting blood pressure (in mm Hg)`)$out))


boxplot(Cholestrol, 
        main = "Cholestrol", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Cholestrol)$out))

boxplot(`Fasting blood sugar`, 
        main = "Blood sugar", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(`Fasting blood sugar`)$out))

boxplot(`Maximum heart rate achieved`, 
        main = "Max Heart Rate", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(`Maximum heart rate achieved`)$out))

boxplot(`Number of major vessels`, 
        main = "Major vessels count", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(`Number of major vessels`)$out))

boxplot(Target, 
        main = "Chances of Heart attack", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Target)$out))

detach(ht_model_data)

#outliers have been spotted in 3 variables - Resting Blood pressure, cholestrol, Maximum Heart rate.
# Outliers in Major blood vessels are not removed as they are the part of data.

#  Resetting the parameters.
par(opar)
#---------------------------------------------------------------
#Identification of outliers using boxplot-stats


outliers <- boxplot.stats(`Resting blood pressure (in mm Hg)`)$out
paste(" BP Outliers: ", 
      paste(outliers, 
            collapse = ", "))
#" BP Outliers:  172, 178, 180, 180, 200, 174, 192, 178, 180"


outliers <- boxplot.stats(Cholestrol)$out
paste("Cholestorl Outliers: ", 
      paste(outliers, 
            collapse = ", "))
#"Cholestorl Outliers:  417, 564, 394, 407, 409"


outliers <- boxplot.stats(`Maximum heart rate achieved`)$out
paste("Heart rate (max) Outliers: ", 
      paste(outliers, 
            collapse = ", "))
# "Heart rate (max) Outliers:  71"

#--------------------------------------------------------
#Removing the outliers by sub-setting from the respective variable data

ht_model_data <- subset(ht_model_data, 
                        `Resting blood pressure (in mm Hg)` != 172 &
                          `Resting blood pressure (in mm Hg)`!= 178 & 
                          `Resting blood pressure (in mm Hg)` != 180 &
                          `Resting blood pressure (in mm Hg)` != 200 & 
                          `Resting blood pressure (in mm Hg)`!= 174 & 
                          `Resting blood pressure (in mm Hg)` != 192)


ht_model_data<- subset(ht_model_data, 
                       Cholestrol != 417 &
                         Cholestrol != 564 & 
                         Cholestrol != 394 & 
                         Cholestrol != 407 & 
                         Cholestrol != 409)


ht_model_data<- subset(ht_model_data, 
                       Max_heartrate != 71)

#The outliers have been removed for the above three variables 
# Confirmation of removal is doen by plotting the box plot again

attach(ht_model_data)
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 3))


boxplot(`Resting blood pressure (in mm Hg)`, 
        main = "Eliminated Outliers in BP ", 
        sub = paste("Outliers: ", 
                    boxplot.stats(`Resting blood pressure (in mm Hg)`)$out))
boxplot(Cholestrol, 
        main = "Eliminated Outliers in Cholestrol", 
        sub = paste("Outliers: ", 
                    boxplot.stats(Cholestrol)$out))



boxplot(`Maximum heart rate achieved`, 
        main = "Eliminated Outliers in Max Heart rate", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(`Maximum heart rate achieved`)$out))

#outliers have been successfully removed.

deatch(ht_model_data)
par(opar)
#--------------------------------------------------
# Checking for Normality
# Using Distribution plots and skewness 
# e1071 library is used for skewness here,

attach(ht_model_data)
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow =c(2,3))


plot(density(Age), 
     main = "Density Plot ~ Age",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(Age), 2)))
# Fill the area under the plot
polygon(density(Age), col = "green")

paste("Skewness of Age: ", round(e1071::skewness(Age), 2))
# "Skewness of Age:  -0.15"
  
plot(density(Sex), 
     main = "Density Plot ~ Sex",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(Sex), 2)))
# Fill the area under the plot
polygon(density(Sex), col = "green")

paste("Skewness of Sex: ", round(e1071::skewness(Sex), 2))
# "Skewness of Sex:  -0.88"


plot(density(`Resting blood pressure (in mm Hg)`), 
     main = "Density Plot ~ BP",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(`Resting blood pressure (in mm Hg)`), 2)))
# Fill the area under the plot
polygon(density(`Resting blood pressure (in mm Hg)`), col = "green")

paste("Skewness of BP: ", round(e1071::skewness(`Resting blood pressure (in mm Hg)`), 2))
# "Skewness of BP:  0.25"

plot(density(Cholestrol), 
     main = "Density Plot ~ Cholestrol",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(Cholestrol, 2))))
# Fill the area under the plot
polygon(density(Cholestrol), col = "green")

paste("Skewness of Cholestrol ", round(e1071::skewness(Cholestrol, 2)))
# "Skewness of Cholestrol:  0"

plot(density(`Fasting blood sugar`), 
     main = "Density Plot ~ Blood sugar",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(`Fasting blood sugar`, 2))))
# Fill the area under the plot
polygon(density(`Fasting blood sugar`), col = "green")

paste("Skewness of Blood sugar ", round(e1071::skewness(`Fasting blood sugar`, 2)))
# "Skewness of Blood sugar:  2 "

plot(density(`Maximum heart rate achieved`), 
     main = "Density Plot ~ Heart rate (max)",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(`Maximum heart rate achieved`, 2))))
# Fill the area under the plot
polygon(density(`Maximum heart rate achieved`), col = "green")

paste("Skewness of Heart Rate (max) ", round(e1071::skewness(`Maximum heart rate achieved`, 2)))
# "Skewness of Heart Rate (max):  -1"

plot(density(`Number of major vessels`), 
     main = "Density Plot ~ Major vessels (Count)",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(`Number of major vessels`, 2))))
# Fill the area under the plot
polygon(density(`Number of major vessels`), col = "green")

paste("Skewness of Major vessels (Count) ", round(e1071::skewness(`Number of major vessels`, 2)))
# "Skewness of Major vessels (Count):  1"

#Highly skewed variables - Blood sugar, 
#Moderately skewed variables - Age, Resting blood pressure, Cholestrol.
#Symmetrical(approx) Variables- Major vessels,Heart rate

detach(ht_model_data)
#----------------------------------------------------------------------
# Model Building
# A linear model is constructed using the lm function and the below variables for the multiple linear regression.

attach(ht_model_data)  
ht_mlr<- lm(Target ~ 
                   Age + 
                   Sex + 
                   Cholestrol +
                  `Resting blood pressure (in mm Hg)` +
                  `Fasting blood sugar`+
                  `Number of major vessels`+ 
                  `Maximum heart rate achieved`, 
                   data = ht_model_data)

#Viewing the summary of the model
summary(ht_mlr)
#sex, Number of major vessels, Maximum Heart rate, Cholestrol  are the variables indicating a high correlation with ***
#R-squared- 0.3853

#viewing the confindence intervals of the model
confint(ht_mlr)


#-----------------------------------

# PREDICTION

# Create training and testing data 
set.seed(1)
rows_split <- nrow(ht_model_data)
ht_sample <- sample(1: rows_split, size = round(0.7 * rows_split), 
                      replace = FALSE)

#viewing the training data
ht_sample

#passing the training and testing data to appropriate variables
training_data <- ht_model_data[ht_sample, ]
testing_data <- ht_model_data[-ht_sample, ]


# Build the model based on training data
ht_mlr <- lm( Target ~ 
                Age + 
                Sex + 
                Cholestrol +
                `Resting blood pressure (in mm Hg)` +
                `Fasting blood sugar`+
                `Number of major vessels`+ 
                `Maximum heart rate achieved`,
                data = training_data)


summary(ht_mlr)

#-----------------------------------------------------------------------------------
#Studentised model
library(car)

#qqplot of whole model data to understand the data

qqPlot(ht_mlr, 
       labels=row(ht_model_data), 
       id.method="identify", 
       simulate=TRUE, 
       main = "Q-Q Plot for fit_model")

#student fit model
student_fit_model <- rstudent(ht_mlr)
hist(student_fit_model,
     breaks=10,
     freq=FALSE,
     xlab="Studentized Residual",
     main="Distribution of Errors")

rug(jitter(student_fit_model), col="brown")

curve(dnorm(x, mean=mean(student_fit_model), sd=sd(student_fit_model)), 
      add=TRUE, col="blue", lwd=2)

lines(density(student_fit_model)$x, density(student_fit_model)$y, col="red", lwd=2, lty=2)

legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), 
       lty=1:2, col=c("blue","red"), cex=.7)

#outlier test
outlierTest(ht_mlr)

#locating and fitting outliers
training_data["93",]
training_data["183",]

fitted(ht_mlr)["93"]
fitted(ht_mlr)["183"]

par <- opar

#perform the test again 
outlierTest(ht_mlr)
ht_model_data<- ht_model_data  %>% slice(-c(183))
#the outlier 183 has to be removed the data

#splitting the data again after removal of outlier
attach(ht_model_data)
set.seed(1)
rows_split <- nrow(ht_model_data)
ht_sample <- sample(1: rows_split, size = round(0.7 * rows_split), 
                    replace = FALSE)

ht_sample

training_data <- ht_model_data[ht_sample, ]
testing_data <- ht_model_data[-ht_sample, ]

#Rebuilding Mlr again
ht_mlr <- lm( Target ~ 
                Age + 
                Sex + 
                Cholestrol +
                `Resting blood pressure (in mm Hg)` +
                `Fasting blood sugar`+
                `Number of major vessels`+ 
                `Maximum heart rate achieved`,
              data = training_data)

summary(ht_mlr)

#running the studentised model
student_fit_model <- rstudent(ht_mlr)
hist(student_fit_model,
     breaks=10,
     freq=FALSE,
     xlab="Studentized Residual",
     main="Distribution of Errors")

rug(jitter(student_fit_model), col="brown")

curve(dnorm(x, mean=mean(student_fit_model), sd=sd(student_fit_model)), 
      add=TRUE, col="blue", lwd=2)

lines(density(student_fit_model)$x, density(student_fit_model)$y, col="red", lwd=2, lty=2)

legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), 
       lty=1:2, col=c("blue","red"), cex=.7)

# the distribution of errors is normalised and the errors are between -2 to 2

#-----------------------------------------------------------------------------------------------------


#----------------------------------------
#linearity
crPlots(ht_mlr)

#Indentifying influential pieces of data with cooks distance

cutoff <- 4/(nrow(training_data) - length(ht_mlr$coefficients) - 2)
plot(ht_mlr, which = 4, cook.levels = cutoff)
13
abline(h = cutoff, lty = 2, col = "red")

library(car)
influencePlot(ht_mlr, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")
# 89, 156, 157 are the influential observations


#homocedasity
library(car)
ncvTest(ht_mlr)
#p-value is 0.68

spreadLevelPlot(ht_mlr)
#Suggested power transformation:  0.7236559 


#global validation
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(ht_mlr)
summary(gvmodel)
#All assumptions acceptable

#checking for multi correlation
library(car)
vif(ht_mlr)

# Age                                 Sex                          Cholestrol `Resting blood pressure (in mm Hg)` 
#1.421458                            1.056349                            1.073717                            1.146839 
#Fasting blood sugar`           `Number of major vessels`       `Maximum heart rate achieved` 
#1.062470                            1.147628                            1.225773 

sqrt(vif(ht_mlr)) > 2

#Age                                 Sex                          Cholestrol `Resting blood pressure (in mm Hg)` 
#FALSE                               FALSE                               FALSE                               FALSE 
#`Fasting blood sugar`           `Number of major vessels`       `Maximum heart rate achieved` 
#FALSE                               FALSE                             

#the output is false , so there is no multi correlation

#______________________________

# comparison of models with AIC

attach(ht_model_data)
sqrt_transform_Target <- sqrt(training_data$Target)
training_data$Target_sqrt <- sqrt_transform_Target


ht_model_1 <- lm(Target ~ 
                   Age + 
                   Sex + 
                   Cholestrol +
                   `Resting blood pressure (in mm Hg)` +
                   `Fasting blood sugar`+
                   `Number of major vessels`+ 
                   `Maximum heart rate achieved`,
                   data=training_data)

ht_model_2 <- lm(Target_sqrt ~ 
                   Age + 
                   Sex + 
                   Cholestrol +
                   `Resting blood pressure (in mm Hg)` +
                   `Fasting blood sugar`+
                   `Number of major vessels`+ 
                   `Maximum heart rate achieved`,
                 data = training_data)

AIC(ht_model_1,ht_model_2)

#df      AIC
#ht_model_1  9 205.0334
#ht_model_2  9 205.0334
#--------------------------------------

#Stepwise Regression with AIC
library(MASS)
ht_mlr_model1_test <- lm(Target ~ 
                       Age + 
                       Sex + 
                       Cholestrol +
                       `Resting blood pressure (in mm Hg)` +
                       `Fasting blood sugar`+
                       `Number of major vessels`+ 
                       `Maximum heart rate achieved`,
                        
                     data=training_data)
stepAIC(ht_mlr_model1_test, direction="backward")
#formula = Target ~ Sex + Cholestrol + `Number of major vessels` + `Maximum heart rate achieved` is the best combination of model


#subsets Regression
install.packages("leaps")
library(leaps)
leaps <-regsubsets(Target ~ Age + 
                     Sex + 
                     Cholestrol +
                    `Resting blood pressure (in mm Hg)` +
                     `Fasting blood sugar`+
                     `Number of major vessels`+ 
                     `Maximum heart rate achieved`, 
                      data=training_data, nbest=4)
plot(leaps, scale="adjr2")


#changing the variables in the model as per results 
ht_mlr1<-lm(Target ~ 
               Sex + 
               Cholestrol +
               `Number of major vessels`+ 
               `Maximum heart rate achieved`,
             
             data=training_data)

#predictions
predicted_heart_attack <- predict(ht_mlr1, testing_data)

#Difference between actal and pricted values
actual_prediction <- data.frame(cbind(actuals = testing_data$Target, 
                                      predicted = predicted_heart_attack))

head(actual_prediction)

correlation_accuracy <- cor(actual_prediction)
correlation_accuracy
# the model is 58 percent accurate 


sigma(ht_mlr_model1_test)/ mean(testing_data$Target)
#0.71 which is a high value , value the lower the better, This model is not the best model to predict heart attack. 
#--------------------------------------------
library("MASS")

ht_model_2_test <- lm(Target_sqrt ~ 
                   Age + 
                   Sex + 
                   Cholestrol +
                   `Resting blood pressure (in mm Hg)` +
                   `Fasting blood sugar`+
                   `Number of major vessels`+ 
                   `Maximum heart rate achieved`,
                 data = training_data)
stepAIC(ht_model_2_test, direction="backward")


install.packages("leaps")
library(leaps)
leaps <-regsubsets(Target_sqrt ~ Age + 
                     Sex + 
                     Cholestrol +
                     `Resting blood pressure (in mm Hg)` +
                     `Fasting blood sugar`+
                     `Number of major vessels`+ 
                     `Maximum heart rate achieved`, 
                   data=training_data, nbest=4)
plot(leaps, scale="adjr2")

#prediction
predicted_heart_attack <- predict(ht_model_2, testing_data)

#Differrences of actuals from predictions
actual_prediction <- data.frame(cbind(actuals = testing_data$Target, 
                                      predicted = predicted_heart_attack))

head(actual_prediction)

#correlation accuracy
correlation_accuracy <- cor(actual_prediction)
correlation_accuracy


sigma(mlr_model_test)/ mean(testing_data$Target)
#0.70 is the sigma value 


#_________________________________________________________________
#Here the coorelation accurracy of the two models are surprsingly the same (58%) but the sigma value - RSE is a bit lower for the second model i.e 
#ht_model_2,so that is th best model.
#----------------------------------------------------------------------------------------
#forecasting
#Run some output on the final model

summary(ht_model_data)

#inputting some data
df_ht_attack <- data.frame(Age = c(41),
                           Sex = c(0),  
                           `Resting blood pressure (in mm Hg)` = c(135),
                             `Fasting blood sugar`= c(0), Cholestrol = c(204), 
                           `Number of major vessels` = c(0), 
                           `Maximum heart rate achieved` = c(172))
#prediction
predicted_heart_attack <- predict(ht_model_2, df_ht_attack)
predicted_heart_attack

#the person has 81% chance of getting a heart attack 

