#############################################################################################################

# Created by: Prof. Valdecy Pereira, D.Sc.
# UFF - Universidade Federal Fluminense (Brazil)
# email:  valdecypereira@yahoo.com.br
# Course: Multivariate Data Analysis
# Lesson: Canonical Correlation Analysis
# Citation: 
# PEREIRA, V. (2018). Project: Multivariate Data Analysis, File: R-MVDA-11-CCA.R, GitHub repository: <https://github.com/Valdecy/Multivariate_Data_Analysis>

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

# Covariances
S_yy <- cov(my_data_y)
S_xx <- cov(my_data_x)
S_xy <- cov(my_data_x, my_data_y)
S_yx <- cov(my_data_y, my_data_x)

#############################################################################################################

# Lambda - Correlation
R_A <- solve(R_xx) %*% R_xy %*% solve(R_yy) %*% R_yx
eigen(R_A)$values # eigenvalues of each canonical pair
A   <- eigen(R_A)$vectors # constant ak of the canonical variables
A_x <- eigen(R_A)$vectors[ , 1]
A_y <- eigen(R_A)$vectors[ , 2]

R_B <- solve(R_yy) %*% R_yx %*% solve(R_xx) %*% R_xy
eigen(R_B)$values # eigenvalues of each canonical pair
B   <- eigen(R_B)$vectors # constant bk of the canonical variables
B_x <- eigen(R_B)$vectors[ , 1]
B_y <- eigen(R_B)$vectors[ , 2]

# Lambda - Covariance
S_A <- solve(S_xx) %*% S_xy %*% solve(S_yy) %*% S_yx
eigen(S_A)$values # eigenvalues of each canonical pair
A_r   <- eigen(S_A)$vectors # constant ak of the canonical variables
A_r_x <- eigen(S_A)$vectors[ , 1]
A_r_y <- eigen(S_A)$vectors[ , 2]

S_B <- solve(S_yy) %*% S_yx %*% solve(S_xx) %*% S_xy
eigen(S_B)$values # eigenvalues of each canonical pair
B_r   <- eigen(S_B)$vectors # constant bk of the canonical variables
B_r_x <- eigen(S_B)$vectors[ , 1]
B_r_y <- eigen(S_B)$vectors[ , 2]

# Canonical coefficients
CC <- diag(eigen(R_A)$values^(1/2))

# Or

CC <- diag(eigen(R_B)$values^(1/2))

#############################################################################################################

# Standardized Variance
variance_A <- (t(A) %*% R_xx %*% A)
variance_A_x <- variance_A[1,1]
variance_A_y <- variance_A[2,2]

variance_B <- (t(B) %*% R_yy %*% B)
variance_B_x <- variance_B[1,1]
variance_B_y <- variance_B[2,2]

# Standardized Coeficients
A_x_st <- A_x/(variance_A_x)^(1/2)
A_y_st <- A_y/(variance_A_y)^(1/2)

B_x_st <- B_x/(variance_B_x)^(1/2)
B_y_st <- B_y/(variance_B_y)^(1/2)

# Original Variance
variance_r_A <- (t(A_r) %*% S_xx %*% A_r)
variance_r_A_x <- variance_r_A[1,1]
variance_r_A_y <- variance_r_A[2,2]

variance_r_B <- (t(B_r) %*% S_yy %*% B_r)
variance_r_B_x <- variance_r_B[1,1]
variance_r_B_y <- variance_r_B[2,2]

# Original Coefficients
A_r_x_st <- A_r_x/(variance_r_A_x)^(1/2)
A_r_y_st <- A_r_y/(variance_r_A_y)^(1/2)

B_r_x_st <- B_r_x/(variance_r_B_x)^(1/2)
B_r_y_st <- B_r_y/(variance_r_B_y)^(1/2)

#############################################################################################################

# Diagnosis – Pillai Barlett Trace
p <- ncol(my_data_x)
q <- ncol(my_data_y)
n <- nrow(my_data)
s_PB  <- min(p, q)
u_PB  <- (n - (q + 1) - p - 1)/2
t_PB  <- (abs(p - (q + 1) + 1) - 1)/2
v1_PB <- s_PB*(2*t_PB + s_PB + 1)
v2_PB <- s_PB*(2*u_PB + s_PB + 1)

G <- sum(eigen(R_A)$values)
F_test <- (G/(s_PB - G))*(v2_PB/v1_PB)
qf(0.975, df1 = v1_PB, df2 = v2_PB)

# Diagnosis - Lawley-Hotelling Trace 
p <- ncol(my_data_x)
q <- ncol(my_data_y)
n <- nrow(my_data)
s_LH  <- min(p, q)
u_LH  <- (n - (q + 1) - p - 1)/2
t_LH  <- (abs(p - (q + 1) + 1) - 1)/2
v1_LH <- s_LH*(2*t_LH + s_LH + 1)
v2_LH <- 2*(s_LH*u_LH + 1)

LH <- sum(eigen(R_A)$values/(1-eigen(R_A)$values))
F_test <- (LH/s_LH)*(v2_LH/v1_LH)
qf(0.975, df1 = v1_LH, df2 = v2_LH)

# Diagnosis - Wilks Lambda Test 
p <- ncol(my_data_x)
q <- ncol(my_data_y)
n <- nrow(my_data)
s_WL <- 1
if ((p^2 + q^2) <= 5){
	s_WL <- 1
	ifelse
	s_WL  <- (((p^2)*(q^2) - 4)/((p^2) + (q^2) - 5))^(1/2)
}

u_WL  <- (p*q - 2)/2
t_WL  <- n - (q + 1) - (p - (q + 1) + 2)/2
v1_WL <- p*q
v2_WL <- t_WL*s_WL - u_WL

Lamb <- prod(1 - eigen(R_A)$values)
Wilks <- ((1 - Lamb^(1/s_WL))/Lamb^(1/s_WL))*(v2_WL/v1_WL)
qf(0.975, df1 = v1_WL, df2 = v2_WL)

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
qf(0.975, df1 = v1_RAO, df2 = v2_RAO)

# Diagnosis - Roy's Largest Root Test
EV <- eigen(R_A)$values/(1 - eigen(R_A)$values)
ROY <- max(EV)

#############################################################################################################

# Structural correlation measures 
my_data_x_st <- scale(my_data_x)
my_data_y_st <- scale(my_data_y)

W1 <- my_data_x_st %*% A_x_st
V1 <- my_data_y_st %*% B_x_st

W2 <- my_data_x_st %*% A_y_st
V2 <- my_data_y_st %*% B_y_st

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
