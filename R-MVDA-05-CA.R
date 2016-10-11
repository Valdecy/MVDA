########################################################################################################################################

# Created by: Prof. Valdecy Pereira, D.Sc.
# UFF - Universidade Federal Fluminense (Brazil)
# email:  valdecypereira@yahoo.com.br
# Course: Multivariate Data Analysis
# Lesson: Correspondence Analysis
# Citation: 
# PEREIRA, V. (2016). Project: Multivariate Data Analysis, File: R-MVDA-05-CA.R, GitHub repository: <https://github.com/Valdecy/Multivariate_Data_Analysis>

########################################################################################################################################

my_data <- table(my_data)
library("FactoMineR")
ca <- CA(my_data, ncp = 2, graph = FALSE)
# Variance Percentage
ca$eig
# Trace (Correlation Between Rows and Columns)
trace <- sqrt(sum(ca$eig[,1]))
# Row Mass
ca$call$marge.row 
# Column Mass
ca$call$marge.col
# Contribution of the categories in column to dimensions
ca$col$contrib
# Contribution of the categories in row to dimensions
ca$row$contrib
# Coordinates of categories in row
ca$row$coord
# Coordinates of categories in column
ca$col$coord
# Biplot
library("factoextra")
fviz_ca_biplot(ca)
# Absolut Frequencies
P <- my_data
for (i in 1:3){
	for (j in 1:3){
		P[i, j] <- (sum(my_data[ ,j])*sum(my_data[i, ])/sum(my_data))
	}
}
# Residuals
E <- my_data - P
# Chi-Squared Table
Chi_Sq_Table <- (E)^2/P
# Chi-Squared Value
Chi_Sq <- sum(Chi_Sq_Table)
# Chi-Squared Test
qchisq(0.95, df = 2*2)
# Beta Test
Beta <- (Chi_Sq - (3 - 1)*(3 - 1))/((3 - 1)*(3- 1))^(1/2)
# Standartized Residuals
E_st <- E/(P)^(0.5)
# Adjusted Standartized Residuals
E_st_adj <- E_st
for (i in 1:3){
	for (j in 1:3){
		E_st_adj[i, j] <- E_st[i, j]/((1 - sum(my_data[ ,j])/sum(my_data))*(1 - sum(my_data[i, ])/sum(my_data)))^(0.5)
	}
}
# Quality of representation (0 = worst, 1 = Perfect) of the categories in column for each dimension 
ca$col$cos2
# Quality of representation (0 = worst, 1 = Perfect) of the categories in row for each dimension 
ca$row$cos2

########################################################################################################################################
########################################################################################################################################

# Multiple Correspondence Analysis

########################################################################################################################################
########################################################################################################################################

library("FactoMineR")
mca <- MCA(my_data2, ncp = 2, graph = FALSE)
# Variance Percentage (Inertia)
mca$eig
# Column Mass
mca$call$marge.col
# Row Mass
mca$call$marge.row
# Contribution of the categories in column to dimensions
mca$var$contrib
# Contribution of the categories in row to dimensions
mca$ind$contrib
library("factoextra")
# Biplot
fviz_mca_var(mca)
fviz_mca_ind(mca)
fviz_mca_biplot(mca)
# Chi-Squared Tests
P <- table(my_data2[,c(1,2)])
temp <- table(my_data2[,c(1,2)])
for (i in 1:3){
  for (j in 1:3){
    P[i, j] <- (sum(temp[ ,j])*sum(temp[i, ])/sum(temp))
  }
}
E <- temp - P
Chi_Sq_Table <- (E)^2/P
Chi_Sq <- sum(Chi_Sq_Table)
qchisq(0.95, df = 2*2)
Beta <- (Chi_Sq - (3 - 1)*(3 - 1))/((3 - 1)*(3- 1))^(1/2)
E_st <- E/(P)^(0.5)
E_st_adj <- E_st
for (i in 1:3){
  for (j in 1:3){
    E_st_adj[i, j] <- E_st[i, j]/((1 - sum(temp[ ,j])/sum(temp))*(1 - sum(temp[i, ])/sum(temp)))^(0.5)
  }
}
P <- table(my_data2[,c(1,3)])
temp <- table(my_data2[,c(1,3)])
for (i in 1:3){
  for (j in 1:2){
    P[i, j] <- (sum(temp[ ,j])*sum(temp[i, ])/sum(temp))
  }
}
E <- temp - P
Chi_Sq_Table <- (E)^2/P
Chi_Sq <- sum(Chi_Sq_Table)
qchisq(0.95, df = 2*1)
Beta <- (Chi_Sq - (3 - 1)*(2 - 1))/((3 - 1)*(2- 1))^(1/2)
E_st <- E/(P)^(0.5)
E_st_adj <- E_st
for (i in 1:3){
  for (j in 1:2){
    E_st_adj[i, j] <- E_st[i, j]/((1 - sum(temp[ ,j])/sum(temp))*(1 - sum(temp[i, ])/sum(temp)))^(0.5)
  }
}
P <- table(my_data2[,c(2,3)])
temp <- table(my_data2[,c(2,3)])
for (i in 1:3){
  for (j in 1:2){
    P[i, j] <- (sum(temp[ ,j])*sum(temp[i, ])/sum(temp))
  }
}
E <- temp - P
Chi_Sq_Table <- (E)^2/P
Chi_Sq <- sum(Chi_Sq_Table)
qchisq(0.95, df = 2*1)
Beta <- (Chi_Sq - (3 - 1)*(2 - 1))/((3 - 1)*(2- 1))^(1/2)
E_st <- E/(P)^(0.5)
E_st_adj <- E_st
for (i in 1:3){
  for (j in 1:2){
    E_st_adj[i, j] <- E_st[i, j]/((1 - sum(temp[ ,j])/sum(temp))*(1 - sum(temp[i, ])/sum(temp)))^(0.5)
  }
}
# Quality of representation (0 = worst, 1 = Perfect) of the categories in column for each dimension 
mca$var$cos
# Quality of representation (0 = worst, 1 = Perfect) of the categories in row for each dimension
mca$ind$cos
# Squared correlation between each variable and the dimensions.
mca$var$eta2
# Dimension Description. The function can be used to identify the most correlated variables with a given dimension.
dimdesc(mca, axes = 1, proba = 0.05)
dimdesc(mca, axes = 2, proba = 0.05)
