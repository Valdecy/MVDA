#############################################################################################################

# Created by: Prof. Valdecy Pereira, D.Sc.
# UFF - Universidade Federal Fluminense (Brazil)
# email:  valdecypereira@yahoo.com.br
# Course: Multivariate Data Analysis
# Lesson: Canonical Correlation Analysis
# Citation: 
# PEREIRA, V. (2019). Project: Multivariate Data Analysis, File: R-MVDA-11-CCA.R, GitHub repository: <https://github.com/Valdecy/Multivariate_Data_Analysis>

#############################################################################################################

# Load Data
my_data <- R_MVDA_CCA

# Dependend Variables
my_data_y <- my_data[ , 1:2]

# Independend Variables
my_data_x <- my_data[ , 3:4]

#############################################################################################################

# Correlations
R_yy <- cor(my_data_y)
R_xx <- cor(my_data_x)
R_xy <- cor(my_data_x, my_data_y)
R_yx <- cor(my_data_y, my_data_x)

R_xy %*% solve(R_yy) %*% R_yx
R_yx %*% solve(R_xx) %*% R_xy

# Lambda
R_A <- solve(R_xx) %*% R_xy %*% solve(R_yy) %*% R_yx
eigen(R_A)$values
A   <- eigen(R_A)$vectors
A_x <- eigen(R_A)$vectors[ , 1]
A_y <- eigen(R_A)$vectors[ , 2]

R_B <- solve(R_yy) %*% R_yx %*% solve(R_xx) %*% R_xy
eigen(R_B)$values
B   <- eigen(R_B)$vectors
B_x <- eigen(R_B)$vectors[ , 1]
B_y <- eigen(R_B)$vectors[ , 2]

# Canonical coefficients
CC <- diag(eigen(R_A)$values^(1/2))

# Or

CC <- diag(eigen(R_B)$values^(1/2))

# Variance
variance_A <- (t(A) %*% R_xx %*% A)
variance_A_x <- variance_A[1,1]
variance_A_y <- variance_A[2,2]

variance_B <- (t(B) %*% R_yy %*% B)
variance_B_x <- variance_B[1,1]
variance_B_y <- variance_B[2,2]

A_x_st <- A_x/(variance_A_x)^(1/2)
A_y_st <- A_y/(variance_A_y)^(1/2)

B_x_st <- B_x/(variance_B_x)^(1/2)
B_y_st <- B_y/(variance_B_y)^(1/2)

# Diagnosis - Pillai Barlett Trace
p <- ncol(my_data_x)
q <- ncol(my_data_y)
n <- nrow(my_data)
s_LH  <- min(p, q)
u_LH  <- (n - (q + 1) - p - 1)/2
t_LH  <- (abs(p - (q + 1) + 1) - 1)/2
v1_LH <- s_LH*(2*t_LH + s_LH + 1)
v2_LH <- s_LH*(2*u_LH + s_LH + 1)

G <- sum(eigen(R_A)$values)
F_test <- (G/(s_LH - G))*(v2_LH/v1_LH)
qf(0.95, df1 = v1_LH, df2 = v2_LH)

# Diagnosis - Lawley-Hotelling Trace 
p <- ncol(my_data_x)
q <- ncol(my_data_y)
n <- nrow(my_data)
s_LW <- 1
if ((p^2 + q^2) <= 5){
	s_LW <- 1
	ifelse
	s_LW  <- (((p^2)*(q^2) - 4)/((p^2) + (q^2) - 5))^(1/2)
}

u_LW  <- (p*q - 2)/2
t_LW  <- n - (q + 1) - (p - (q + 1) + 2)/2
v1_LW <- p*q
v2_LW <- t_LW*s_LW - u_LW

# Diagnosis - Wilks Lambda Test 
Lamb <- prod(1 - eigen(R_A)$values)

Wilks <- ((1 - Lamb^(1/s_LW))/Lamb^(1/s_LW))*(v2_LW/v1_LW)

qf(0.95, df1 = v1_LW, df2 = v2_LW)

# Diagnosis - F Rao Test 
p <- ncol(my_data_x)
q <- ncol(my_data_y)
n <- nrow(my_data)
s_RAO <- 1
if ((p^2 + q^2) <= 5){
	s_RAO <- 1
	ifelse
	s_RAO  <- (((p^2)*(q^2) - 4)/((p^2) + (q^2) - 5))^(1/2)
}

t_RAO  <- n - (q + 1) - (p - (q + 1) + 2)/2
v1_RAO <- p*q
v2_RAO <- 1 + t_RAO*s_RAO - p*q/2

Lamb <- prod(1 - eigen(R_A)$values)

RAO <- ((1 - Lamb^(1/s_RAO))/Lamb^(1/s_RAO))*(v2_RAO/v1_RAO)

qf(0.95, df1 = v1_RAO, df2 = v2_RAO)

# Diagnosis - Roy's Largest Root Test
EV <- eigen(R_A)$values/(1 - eigen(R_A)$values)
ROY <- max(EV)

# Structural correlation measures 
my_data_x_st <- scale(my_data_x)
my_data_y_st <- scale(my_data_y)

W1 <- my_data_x_st %*% A_x_st
V1 <- my_data_y_st %*% B_x_st

W2 <- my_data_x_st %*% A_y_st
V2 <- my_data_y_st %*% B_y_st

plot(W1,V1)
plot(W2,V2)

CE_X1_W1 <- cor(my_data_x[ , 1], W1)
CE_X2_W1 <- cor(my_data_x[ , 2], W1)
CE_Y1_V1 <- cor(my_data_y[ , 1], V1)
CE_Y2_V1 <- cor(my_data_y[ , 2], V1)

CE_X1_W2 <- cor(my_data_x[ , 1], W2)
CE_X2_W2 <- cor(my_data_x[ , 2], W2)
CE_Y1_V2 <- cor(my_data_y[ , 1], V2)
CE_Y2_V2 <- cor(my_data_y[ , 2], V2)

# Average Variance Quantity
QMV_X_W1 <- (CE_X1_W1^2 + CE_X2_W1^2)/p
QMV_X_W2 <- (CE_X1_W2^2 + CE_X2_W2^2)/p
QMV_Y_V1 <- (CE_Y1_V1^2 + CE_Y2_V1^2)/q
QMV_Y_V2 <- (CE_Y1_V2^2 + CE_Y2_V2^2)/q

MR_W1_V1 <- QMV_X_W1*eigen(R_A)$values[1]
MR_W2_V2 <- QMV_X_W2*eigen(R_A)$values[2]
MR_V1_W1 <- QMV_Y_V1*eigen(R_A)$values[1]
MR_V2_W2 <- QMV_Y_V2*eigen(R_A)$values[2]

MRT_X <- MR_W1_V1 + MR_W2_V2
MRT_Y <- MR_V1_W1 + MR_V2_W2