
########################################################################################################################################

# Created by: Prof. Valdecy Pereira, D.Sc.
# UFF - Universidade Federal Fluminense (Brazil)
# email:  valdecypereira@yahoo.com.br
# Course: Multivariate Data Analysis
# Lesson: Multiple Linear Regression
# Citation: 
# PEREIRA, V. (2016). Project: Multivariate Data Analysis, File: R-MVDA-07-MLR.R, GitHub repository: <https://github.com/Valdecy/Multivariate_Data_Analysis>

########################################################################################################################################

# Graph
library ("psych")
pairs.panels(my_data)
library("ggplot2")
ggplot(data = my_data, aes(x = 1:25, y = my_data$Y)) + geom_point(shape = 1) + labs(x = "Observations", y = "Y")
ggplot(data = my_data, aes(x = "Observations", y = my_data$Y)) + geom_boxplot() + theme(axis.title.x = element_blank()) + labs(y = "Y")
ggplot(my_data, aes(my_data$Y)) + geom_histogram(bins = 50) + labs(x = "Y")
ggplot(data = my_data, aes(x = 1:25, y = my_data$X1)) + geom_point(shape = 1) + labs(x = "Observations", y = "X1")
ggplot(data = my_data, aes(x = "Observations", y = my_data$X1)) + geom_boxplot() + theme(axis.title.x = element_blank()) + labs(y = "X1")
ggplot(my_data, aes(my_data$X1)) + geom_histogram(bins = 50) + labs(x = "X1")
ggplot(data = my_data, aes( sample = my_data$X1)) + stat_qq()+ xlab("Theorethical") + ylab("X1")
ggplot(data = my_data, aes(x = 1:25 y = my_data$X2)) + geom_point(shape = 1) + labs(x = "Observations", y = "X2")
ggplot(data = my_data, aes(x = "Observations", y = my_data$X2)) + geom_boxplot() + theme(axis.title.x = element_blank()) + labs(y = "X2")
ggplot(my_data, aes(my_data$X2)) + geom_histogram(bins = 50) + labs(x = "X2")
ggplot(data = my_data, aes( sample = my_data$X2)) + stat_qq()+ xlab("Theorethical") + ylab("X2")
# MLR
mlr <- lm(Y ~ ., data = my_data)
summary(mlr)
# Predicted Y
mlr$fitted.values
ggplot() + geom_line(data = my_data, aes(x = 1:25, y = Y), colour = "black", size = 1.2)  + xlab("Time") + ylab("Y") + geom_line(data = my_data, aes(x = 1:25, y = mlr$fitted.values), colour = "red", size = 1, linetype = 2)
# VIF (Variance Inflation Factors) 
library("car")
vif(mlr)
library("MASS")
sresid <- studres(mlr)
# QQ Plot
ggplot(data = my_data, aes( sample = sresid)) + stat_qq()+ xlab("Theorethical") + ylab("Studentized Residuals")
# Univariate Normality
shapiro.test(sresid)
# Homoscedasticity
ggplot(data = my_data, aes(x = 1:25, y = sresid)) + geom_point() + labs(x = "Observations", y = "Sresiduals")
# Breusch-pagan test 
library("car")
ncvTest(mlr)
# Prediction
new_data <- as.data.frame(cbind(2, 50))
colnames(new_data) <- c("X1", "X2")
predict(mlr, new_data)
