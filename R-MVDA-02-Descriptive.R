########################################################################################################################################

# Created by: Prof. Valdecy Pereira, D.Sc.
# UFF - Universidade Federal Fluminense (Brazil)
# email:  valdecypereira@yahoo.com.br
# Course: Multivariate Data Analysis
# Lesson: Descriptive Statistics with R
# Citation: 
# PEREIRA, V. (2016). Project: Multivariate Data Analysis, File: R-MVDA-Descriptive.R, GitHub repository: <https://github.com/Valdecy/Multivariate_Data_Analysis>

########################################################################################################################################

# Numeric Vector
a <- c(1, 3.33, 8.87, 6, -2, 7)
# To retrieve the elements of vector use [ ]
a [ c (2,4) ]  
a [ c (1:4) ]  
# Character (String) Vector
b <- c("alpha", "bravo")
# Logical Vector
c <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)  # or #  c <- c (T, T, T, F, T, F)
length(a) 
names(a)  <- c("A", "B", "C", "D", "E", "F")
# Matrix 5 x 4 (5 rows and 4 columns) with a sequence of numbers from 1 to 20.
matrix(1:20, nrow = 5, ncol = 4, byrow = FALSE)
matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE)
# Another example
my_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
rownames(my_matrix) <- c ("1st_row", "2nd_row")
colnames(my_matrix) <- c ("column_A", "column_B")
my_matrix[2,1] 
length(my_matrix) 
dim(my_matrix) 
my_matrix[ -1,  ]     
my_matrix <- cbind(my_matrix, c(5, 6)) 
colnames(my_matrix)[ 3 ] <- c("column_C")    
my_matrix <- rbind(my_matrix, c(7, 8, 9)) 
rownames(my_matrix)[ 3 ] <- c("3rd_row")
my_data_frame <- data.frame (c (1, 2, 3, 4), c ("one", "two", "five", "ten"), c (T, T, T, T))
colnames(my_data_frame) <- c("A", "B", "C") 
rownames(my_data_frame) <- c("r1", "r2", "r3", "r4") 
# my_data_frame [ ,1] ; my_data_frame$A  or my_data_frame["A"] returns the values from the first column
my_list <- list(my_matrix, my_data_frame, b)
my_list[[2]]
my_list[[2]][1]
# Strings are converted automatically in factors
factor(c("small", "medium", "large"), levels = c("small", "medium", "large"))
# Orders are created from the lowest to the greater attribute.
ordered(c("small", "medium", "large"), levels = c("small", "medium", "large"))
# For
for (i in 1:3){
print(i)
}
# If-then-else
x <- 0
if (x < 0) {
print("Negative number")
} else if (x > 0) {
print("Positive number")
} else
   print("Zero")
# While
x <- 1
while(x < 5) {
x <- x + 1 
print(x)
}
# Arithmetic mean for dataset that holds a sequence from 1 to 100
sum(1:100)/length(seq(1:100))
mean(x = 1:100) # mean(1:100)  also works!
# Weighted mean for dataset in the example
weighted.mean(x = c(80, 90, 96), w = c(0.3, 0.3, 0.4))
# Trimmed mean for data set that holds a sequence from 1 to 100 and the value 5897 as an outlier
mean(x = c(1:100, 5897))
mean(x = c(1:100, 5897), trim = 0.10) # Both sides are trimmed
mean(x = 2:100) # This is the same as reducing de data from 2 to 100
# Median
median(x = c(3, 7, 5, 5, 1, 9, 15, 13, 17, 13, 17))
# Mode
mode_table <- table(c(3, 7, 5, 5, 1, 9, 15, 13, 17, 13, 17))
names(mode_table)[mode_table  == max(mode_table)]
# Variance
var(c(2, 4, 6, 8, 10))
# Standart Deviation
sd(c(2, 4, 6, 8, 10))
# Generate a data set with 1000 observations with mean 0 and standart deviation 1.
set.seed(101)
n_data <- as.data.frame(rnorm(1000, 0, 1))  # or n_data <- as.data.frame(rnorm(n = 1000, mean = 0, sd = 1))
# Central Limit Theorem
cl <- data.frame(runif(500)) 
for (i in 1:50) {
cl[ , i] <- data.frame(runif(500)) 
}
cl$Sum <- rowSums(cl)
library("ggplot2")
ggplot(data = cl, aes( x = Sum)) + geom_density() + xlab("Normal")
# histogram
library("ggplot2")
ggplot(data = n_data, aes( x = n_data[,1])) + geom_histogram(binwidth = 0.5, colour = "black", fill = "white") + xlab("Normal")
# scatter plot
library("ggplot2")
ggplot(data = n_data, aes( x = 1:1000, y = n_data[,1])) + geom_point() + xlab("Observations") + ylab("Data")
# line plot
library("ggplot2")
ggplot(data = n_data, aes( x = 1:1000, y = n_data[,1])) + geom_line() + xlab("Observations") + ylab("Data")
# density curve
ggplot(data = n_data, aes( x = n_data[,1])) + geom_density() + xlab("Normal")
# both curves
ggplot(data =  n_data, aes( x = n_data[,1])) + geom_histogram(aes(y = ..density.. ), binwidth= 0.5, colour = "black", fill = "white") + geom_density(alpha = 0.2, fill = "red") + xlab("Normal")
# box plot
ggplot(data = n_data, aes(x = " ", y = n_data[ ,1])) + geom_boxplot() + ylab("Normal")
# Q1 - 1.5IQR, Q1, Q2, Q3, Q3 + 1.5IQR
boxplot.stats(n_data[,1])$stats
# Normal Plot
set.seed(101)
n_data <- as.data.frame(rnorm(900000, 0, 1)) 
density_n_data <- density(n_data[ , 1])
df_d <- data.frame(density_n_data$x, density_n_data$y)
colnames(df_d ) <- c ("x", "y")
ggplot(data =  df_d, aes( x = x , y = y)) + geom_line() + geom_ribbon(data = subset(df_d, x >= -1 & x <= 1), aes( ymax = y), ymin = 0, fill= "yellow", colour = NA,  alpha=0.5) + geom_ribbon(data = subset(df_d, x >= -2 & x < -1), aes( ymax = y), ymin = 0, fill = "darkorange", colour = NA,  alpha = 0.5) + geom_ribbon(data = subset(df_d, x >= 1 & x < 2), aes( ymax = y), ymin = 0, fill = "darkorange", colour = NA,  alpha=0.5) + geom_ribbon(data = subset(df_d, x >= -3 & x < -2), aes( ymax = y), ymin = 0, fill = "red", colour = NA,  alpha=0.5) + geom_ribbon(data = subset(df_d, x >= 2 & x < 3), aes( ymax = y), ymin = 0, fill = "red", colour = NA,  alpha=0.5) + geom_segment(data = df_d , x = -3, y = 0.41, xend = 3, yend = 0.41) + geom_segment(data = df_d , x = -3, y = 0, xend = -3, yend = 0.41) + geom_segment(data = df_d , x = -2, y = 0, xend = -2, yend = 0.41) + geom_segment(data = df_d , x = -1, y = 0, xend = -1, yend = 0.41) + geom_segment(data = df_d , x = 0, y = 0, xend = 0, yend = 0.41) + geom_segment(data = df_d , x = 1, y = 0, xend = 1, yend = 0.41) + geom_segment(data = df_d , x = 2, y = 0, xend = 2, yend = 0.41) + geom_segment(data = df_d , x = 3, y = 0, xend = 3, yend = 0.41) + xlab("x") + ylab("y")
library(e1071) 
skewness(n_data[ ,1])
library(e1071) 
kurtosis(n_data[ ,1])
# R Shapiro-Wilk test supports up to 5000 observations
#shapiro.test() 
# The KS test needs a reference distribution y.
set.seed(101)
n_data <- as.data.frame(rnorm(900000, 0, 1))
r_data <- as.data.frame(runif(900000))
ks.test(x = r_data[,1], y = n_data[,1])
