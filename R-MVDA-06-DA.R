########################################################################################################################################

# Created by: Prof. Valdecy Pereira, D.Sc.
# UFF - Universidade Federal Fluminense (Brazil)
# email:  valdecypereira@yahoo.com.br
# Course: Multivariate Data Analysis
# Lesson: Discriminant Analysis
# Citation: 
# PEREIRA, V. (2016). Project: Multivariate Data Analysis, File: R-MVDA-06-DA.R, GitHub repository: <https://github.com/Valdecy/Multivariate_Data_Analysis>

########################################################################################################################################

# Graph
library("ggplot2")
ggplot(my_data, aes(x = Y1, y = Y2)) + geom_point(aes(colour = ifelse(my_data[,1] == 1, "blue", ifelse(my_data[,1] == 2,"green", "red" ))), size = 2) + theme(legend.position = "none")
ggplot(data = my_data, aes(x = 1:20, y = my_data$Y1)) + geom_point(shape = 1) + labs(x = "Observations", y = "Y1")
ggplot(data = my_data, aes(x = "Observations", y = my_data$Y1)) + geom_boxplot() + theme(axis.title.x = element_blank()) + labs(y = "Y1")
ggplot(my_data, aes(my_data$Y1)) + geom_histogram(bins = 50) + labs(x = "Y1")
ggplot(data = my_data, aes( sample = my_data$Y1)) + stat_qq()+ xlab("Theorethical") + ylab("Y1")
ggplot(data = my_data, aes(x = 1:20, y = my_data$Y2)) + geom_point(shape = 1) + labs(x = "Observations", y = "Y2")
ggplot(data = my_data, aes(x = "Observations", y = my_data$Y2)) + geom_boxplot() + theme(axis.title.x = element_blank()) + labs(y = "Y2")
ggplot(my_data, aes(my_data$Y2)) + geom_histogram(bins = 50) + labs(x = "Y2")
ggplot(data = my_data, aes( sample = my_data$Y2)) + stat_qq()+ xlab("Theorethical") + ylab("Y2")
# Univariate Normality
shapiro.test(my_data$Y1)
shapiro.test(my_data$Y2) 
# Multivariate Normality
library("MVN")
mardiaTest(my_data, qqplot = FALSE)
library ("psych")
pairs.panels(my_data)
#  Relationship between cases and clusters
20/3
#  Relationship between cases and independent variables
20/2
# Box M Test
library("biotools")
boxM(my_data[,2:3], my_data$Cluster)
#  Discriminant Analysis
library("MASS")
lda <- lda(Cluster ~ ., data = my_data)
# Prediction
lda_values <- predict(lda)
# Z Values
lda_values$x 
# Z Graph
ggplot(my_data, aes(x = lda_values$x[,1], y = lda_values$x[,2])) + geom_point(aes(colour = ifelse(my_data[,1] == 1, "blue", ifelse(my_data[,1] == 2,"green", "red" ))), size = 2) + theme(legend.position = "none") + xlab("Z1") + ylab("Z2")
# Cluster Histograms
ldahist(data = lda_values$x[,1], g = my_data$Cluster)
ldahist(data = lda_values$x[,2], g = my_data$Cluster)
# U-Statistics
library("DiscriMiner")
discPower(my_data[,2:3], my_data$Cluster)
# Eigenvalue
eigen_values <- betweenSS(lda_values$x[,1:2], my_data$Cluster)/withinSS(lda_values$x[,1:2], my_data$Cluster) 
# Proportion of trace - The percentage of variance achieved by each discriminant function.
p_ev_1 <- eigen_values[1]/(eigen_values[1] + eigen_values[4])
p_ev_2 <- eigen_values[4]/(eigen_values[1] + eigen_values[4])
# Canonical Correlation
rc1 <- ((betweenSS(lda_values$x[,1:2], my_data$Cluster)[1])/(withinSS(lda_values$x[,1:2], my_data$Cluster)[1] + betweenSS(lda_values$x[,1:2], my_data$Cluster)[1]))^(1/2)
rc2 <- ((betweenSS(lda_values$x[,1:2], my_data$Cluster)[4])/(withinSS(lda_values$x[,1:2], my_data$Cluster)[4] + betweenSS(lda_values$x[,1:2], my_data$Cluster)[4]))^(1/2)
# Lambda Wilks
library("rrcov")
Wilks.test(Cluster ~ ., data = my_data, method = "c")
# Classsification
linDA(my_data[,2:3], my_data$Cluster)$scores
# Accuracy of the prediction
class_table   <- table(my_data$Cluster, lda_values$class) 
class_table_p <- (prop.table(class_table, 1))
acc           <- sum(diag(prop.table(class_table)))
# Prediction
new_data <- as.data.frame(cbind(7.10,8.88))
colnames(new_data) <- c("Y1", "Y2")
predict(lda, new_data)$class
