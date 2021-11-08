#import required libraries

library(lattice)
library(ggplot2)
library(corrplot)
library(caret)
library(ggstatsplot)
library(fastmap)
library(caTools)

#importing the data set

setwd("C:/Users/moon/Desktop/excell/Data_Analysis/first project")
insurance <- read.table("insurance.csv",header=TRUE,sep=",")


#viewing the data set

View(insurance)

# display the structure of the data set 

str(insurance)

# display the summary of the data set 

summary(insurance)


#converting sex, smoker, and region into Dummy columns

Dummy_var <- dummyVars("~.",  insurance)
insurance <- data.frame  (predict(Dummy_var, insurance))

#viewing the data set

View(insurance)



#checking if there is a missing variables in the data set or not

any(is.na(insurance))



# Checking the outliers

boxplot(insurance)

# Box blot of age and bmi  


boxplot(insurance$bmi)

boxplot(insurance$age)

boxplot(insurance$bmi, plot= FALSE)$out

#Removing the outlier from bmi

bmi_outlier <- boxplot(insurance$bmi,plot= FALSE)$out
insurance <- insurance[-which(insurance$bmi %in% bmi_outlier),]
boxplot(insurance$bmi)



#Checking the correlation

corroleation <- cor(insurance)
corrplot(corroleation, method = "number", type = "upper")


#Splitting the data into train and test

split <- sample.split(insurance$charges, SplitRatio = 0.8)
train_set = subset(insurance, split == TRUE)
test_set = subset(insurance, split == FALSE)

#Building the first regression model using train data set
#charges is dependent variable
#age,sexmale, bmi, smokeryes , children,regionnortheast,regionnorthweat and regionsoutheast are independent variables

regression_model1 = lm(formula = charges ~ age + sexmale + bmi + children +  smokeryes+regionnortheast+ regionnorthwest+regionsoutheast,
                       data = train_set)

#Predicting the output from regression_model1 using test data set

y_pred1 = predict(regression_model1, newdata = test_set)

#Building the second regression model using train data set
#charges is dependent variable
#age, bmi and smokeryes are independent variables

regression_model2 = lm(formula = charges ~ age + bmi + smokeryes,
                       data = train_set)

#Predicting the output from regression_model2 using test data set

y_pred2 = predict(regression_model2, newdata = test_set)


#Analyzing  the first model

summary(regression_model1)

#Analyzing  the second model

summary(regression_model2)

#Computing the residuals from the second model

residuals <- test_set$charges - y_pred2


#Checking residuals plot for the second model

Prediction=y_pred2

ggplot(data = test_set, aes(x = Prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 3, color = "red") +
  ggtitle("Residuals plot")



#Plotting the second model

Charges = test_set$charges


test_set$y_pred2 <- predict(regression_model2, newdata = test_set)
ggplot(test_set, aes(x = Prediction, y = Charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")

#checking the normality of errors plot for the second model

ggplot(test_set, aes(x = residuals)) + 
  geom_histogram(bins = 15, fill = "blue") +
  ggtitle("Normality plot")



#Applying prediction on the new data from the second model using predict function

Menna <- data.frame(age = 34,
                    
                    bmi = 26.5,
                    
                    smokeryes = 1
)

Mostafa <- data.frame(age = 30,
                      bmi = 31.2,
                      smokeryes = 0
)
print(paste0("Health care charges for Menna: ", round(predict(regression_model2, Menna), 2)))  
print(paste0("Health care charges for Mostafa: ", round(predict(regression_model2, Mostafa), 2)))


#Applying prediction on the new data using the regression equation function 
#Regression equation of the second model is : Y = -11348.70 + 255.77 X1 + 321.63 X2 + 23604.89 X3  

Menna=(-11348.70)+(255.77*34)+(321.63*26.5)+(23604.89)
Menna

