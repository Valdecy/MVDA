########################################################################################################################################

# Created by: Prof. Valdecy Pereira, D.Sc.
# UFF - Universidade Federal Fluminense (Brazil)
# email:  valdecypereira@yahoo.com.br
# Course: Multivariate Data Analysis
# Lesson: Exploratory Factor Analysis
# Citation: 
# PEREIRA, V. (2016). Project: Multivariate Data Analysis, File: R-MVDA-04-MDS.R, GitHub repository: <https://github.com/Valdecy/Multivariate_Data_Analysis>

########################################################################################################################################

# MDS
mds <- cmdscale(my_data, 2, eig = TRUE)
mds$points
mds$eig
mds$GOF[2]
# Variance Plot
library(ggplot2)
ggplot(data = my_data, aes(x = 1:15, y = mds$eig/sum(mds$eig[which(mds$eig > 0)]))) + geom_point(size = 2) + labs(x = "Number of Dimensions", y = "Variance") + geom_hline(yintercept = 0, color = "red")
# MDS Plot
dimension_x <- mds$points[, 1]
dimension_y <- mds$points[, 2]
ggplot(data = my_data, aes(x = dimension_x, y = dimension_y, label = row.names(my_data))) + geom_point(size = 2) + labs(x = "Dimension 1", y = "Dimension 1") 
ggplot(data = my_data, aes(x = dimension_x, y = dimension_y, label = row.names(my_data))) + labs(x = "Dimension 1", y = "Dimension 1") + geom_text(size = 4)
ggplot(data = my_data, aes(x = dimension_y, y = dimension_x, label = row.names(my_data))) + labs(x = "Dimension 1", y = "Dimension 1") + geom_text(size = 4)
# Diagnosis
d_hat <- dist(mds$points, method = "euclidean", diag = TRUE, upper = TRUE, p = 2)
cor (c(as.matrix(d_hat)),c(as.matrix(my_data)))
qplot(x = c(as.matrix(d_hat)), y = c(as.matrix(my_data))) + geom_point(size = 2) + labs(x = "Disparity", y = "Distance")
d_hat <- as.matrix(d_hat)
K_STRESS_2 <- ((sum((my_data - d_hat)^2))/sum(((d_hat - sum(my_data))^2)))^(1/2)
Y_STRESS_2 <- ((sum(((my_data)^2 - d_hat^2)^2))/sum(((d_hat^2 - sum(my_data)^2)^4)))^(1/2)
Y_STRESS_S <-  (sum(((my_data)^2 - d_hat^2)^2))/sum(((d_hat^2 - sum(my_data)^2)^4))
R_STRESS   <-   sum((my_data - d_hat)^2)
N_R_STRESS <-  (sum((my_data - d_hat)^2)/sum((my_data)^2))^(1/2)
N_STRESS   <-  (sum((my_data - d_hat)^2)/sum((my_data - sum(my_data))^2))^(1/2)
RSQ        <- ((sum((my_data - sum(my_data))*(d_hat - sum(d_hat))))^2)/(sum((my_data - sum(my_data))^2)*sum((d_hat - sum(d_hat))^2))

########################################################################################################################################
########################################################################################################################################

# Non-Metric MDS

########################################################################################################################################
########################################################################################################################################

#nMDS
my_data2 <- R.MVDA.nMDS
library(vegan)
n_mds <- metaMDS(my_data2, k = 2, trymax = 100)
# Plot nMDS Variables
library(ggplot2)
n_mds_variables <- scores(n_mds, "spec")
n_mds_variables <- cbind.data.frame(n_mds_variables, label = rownames(n_mds_variables))
ggplot(data = n_mds_variables, aes(x = NMDS1, y = NMDS2)) + geom_text(aes(label = label))
# Shepard Plot
stressplot(n_mds)
