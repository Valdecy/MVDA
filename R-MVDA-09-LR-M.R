#############################################################################################################

# Created by: Prof. Valdecy Pereira, D.Sc.
# UFF - Universidade Federal Fluminense (Brazil)
# email:  valdecypereira@yahoo.com.br
# Course: Multivariate Data Analysis
# Lesson: Logistic Regression - Multinomial
# Citation: 
# PEREIRA, V. (2018). Project: Multivariate Data Analysis, File: R-MVDA-09-LR-M.R, GitHub repository: <https://github.com/Valdecy/Multivariate_Data_Analysis>

#############################################################################################################

library("ggplot2")
library("corrplot")
library("nnet")

########################################################################################################

# Load Data
my_data <- R_MVDA_LR_M[,2:3]
my_data <- my_data[order(my_data$On_Time),,drop = FALSE]

#############################################################################################################

# Classes
ggplot() + geom_point(data = my_data, aes(x = 1:nrow(my_data), y = On_Time, colour = as.factor(On_Time)), size = 2) + xlab("Observations") + ylab("Classes") + labs(fill='NEW LEGEND TITLE')

# Correlation Matrix
corrplot.mixed(cor(my_data), lower = "number", upper = "ellipse")

#######################################################################################################

# Betas
mlogit <- multinom(On_Time ~ ., data = my_data)
summary(mlogit)$coefficients

# LLmax
summary(mlogit)$value  

#######################################################################################################

# S.E
summary(mlogit)$standard.errors

# Betas Significance
z <- summary(mlogit)$coefficients/summary(mlogit)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2

# Betas Confidence Interval
summary(mlogit)$coefficients + 1.96*summary(mlogit)$standard.errors
summary(mlogit)$coefficients - 1.96*summary(mlogit)$standard.errors

#######################################################################################################

# Null Model
mlogit_00 <- multinom(On_Time ~ 1, data = my_data)
summary(mlogit_00)$coefficients
summary(mlogit_00)$value  

# Likelihood Ratio Test (the deviance residual is -2*log likelihood)
LL0   <- -summary(mlogit_00)$value
LLmax <- -summary(mlogit)$value
chisq <- -2*(LL0 - LLmax)
qchisq(.975, df = ncol(my_data))

# Pseudo-r2
psd_r_McFadden      = (-2*LL0 + 2*LLmax)/(-2*LL0)
psd_r_Cox_and_Snell = 1 - (exp(LL0)/exp(LLmax))^(2/nrow(my_data))
psd_r_Nagelkerke    = psd_r_Cox_and_Snell/(1 - (exp(LL0))^(2/nrow(my_data)))

# AIC & BIC
p <- 6
aic <- -2*LLmax + 2*p
bic <- -2*LLmax + p*log(nrow(my_data))

#######################################################################################################

# Probability of Occurrence
fit <- fitted(mlogit)
round(fit, 2)

# Prediction
fit_cat <- cbind(fit[,1])
for (i in 1:nrow(my_data)) {
  for (j in 1:nlevels(as.factor(my_data[ , 1]))){
    k <- max(fit[i,])
    if (fit[i,j] == k){
      fit_cat[i] <- (j - 1)
    }
  }
}

Prediction <- as.factor(fit_cat)
On_Time <- as.factor(my_data$On_Time)
Observations <- seq(1, length(my_data[,1]))

ggplot() + geom_point(aes(x = Observations, y = On_Time, colour = Prediction), size = 2) + xlab("Observations") + ylab("Classes")

# Confusion Matrix
table(On_Time, Prediction)

#######################################################################################################

# New Data Prediction
new_data <- as.data.frame(cbind(9.5, 8))
colnames(new_data) <- c("Distance", "Traffic_Lights")
predict(mlogit, new_data, type = c("probs"))
