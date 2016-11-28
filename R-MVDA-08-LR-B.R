
########################################################################################################################################

# Created by: Prof. Valdecy Pereira, D.Sc.
# UFF - Universidade Federal Fluminense (Brazil)
# email:  valdecypereira@yahoo.com.br
# Course: Multivariate Data Analysis
# Lesson: Binary Logistic Regression (Logit)
# Citation: 
# PEREIRA, V. (2016). Project: Multivariate Data Analysis, File: R-MVDA-08-LR-B.R, GitHub repository: <https://github.com/Valdecy/Multivariate_Data_Analysis>

########################################################################################################################################

library("ggplot2")
ggplot(data = my_data, aes(x = Observations, y = On_Time)) + geom_point(shape = 1)
ggplot(data = my_data, aes(x = "", y = On_Time)) + geom_boxplot() + theme(axis.title.x = element_blank())
ggplot(data = my_data, aes(On_Time)) + geom_histogram(bins = 50)
ggplot(data = my_data, aes(sample = On_Time)) + stat_qq()+ xlab("Theorethical") + ylab("On_Time")
ggplot(data = my_data, aes(x = Observations, y = Distance)) + geom_point(shape = 1)
ggplot(data = my_data, aes(x = "", y = Distance)) + geom_boxplot() + theme(axis.title.x = element_blank())
ggplot(data = my_data, aes(Distance)) + geom_histogram(bins = 50)
ggplot(data = my_data, aes(sample = Distance)) + stat_qq()+ xlab("Theorethical") + ylab("Distance")
ggplot(data = my_data, aes(x = Observations, y = Traffic_Lights)) + geom_point(shape = 1)
ggplot(data = my_data, aes(x = "", y = Traffic_Lights)) + geom_boxplot() + theme(axis.title.x = element_blank())
ggplot(data = my_data, aes(Traffic_Lights)) + geom_histogram(bins = 50)
ggplot(data = my_data, aes(sample = Traffic_Lights)) + stat_qq()+ xlab("Theorethical") + ylab("Traffic_Lights")
ggplot(data = my_data, aes(x = Observations, y = Time_Period)) + geom_point(shape = 1)
ggplot(data = my_data, aes(x = "", y = Time_Period)) + geom_boxplot() + theme(axis.title.x = element_blank())
ggplot(data = my_data, aes(Time_Period)) + geom_histogram(bins = 50)
ggplot(data = my_data, aes(sample = Time_Period)) + stat_qq()+ xlab("Theorethical") + ylab("Time_Period")
ggplot(data = my_data, aes(x = Observations, y =  Driver_Profile1)) + geom_point(shape = 1)
ggplot(data = my_data, aes(x = "", y =  Driver_Profile1)) + geom_boxplot() + theme(axis.title.x = element_blank())
ggplot(my_data, aes( Driver_Profile1)) + geom_histogram(bins = 50)
ggplot(data = my_data, aes(sample = Driver_Profile1)) + stat_qq()+ xlab("Theorethical") + ylab("Driver_Profile1")
ggplot(data = my_data, aes(x = Observations, y =  Driver_Profile2)) + geom_point(shape = 1)
ggplot(data = my_data, aes(x = "", y =  Driver_Profile2)) + geom_boxplot() + theme(axis.title.x = element_blank())
ggplot(my_data, aes( Driver_Profile2)) + geom_histogram(bins = 50)
ggplot(data = my_data, aes(sample = Driver_Profile2)) + stat_qq()+ xlab("Theorethical") + ylab("Driver_Profile2")
library ("psych")
pairs.panels(my_data)
# Logit
logit_01 <- glm(On_Time ~ ., data = my_data, family = "binomial")
summary(logit_01)
logit_02 <- glm(On_Time ~ Distance + Traffic_Lights + Time_Period + Driver_Profile2, data = my_data, family = "binomial")
summary(logit_02)
# Prediction
prob  <- predict(logit_02,type = c("response"))
ggplot(my_data, aes(x = 1:100, y = On_Time)) + geom_point(aes(colour = ifelse(my_data$On_Time >= 0.5, "red", "blue")), size = 3) + theme(legend.position = "none") + xlab ("Observations") + ylab("On_Time")
ggplot(my_data, aes(x = 1:100, y = On_Time)) + geom_point(aes(colour = ifelse(prob >= 0.5, "red", "blue")), size = 3) + theme(legend.position = "none") + xlab ("Observations") + ylab("Predicted")
# Betas 
logit_02$coefficients
# Maximum LogLikelyhood
LLmax <- logLik(logit_02)
# CIs Using Standard Errors
confint.default(logit_02,level = 0.95)
# Likelihood Ratio Test
with(logit_02, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# Pseudo-r2
LL0 <- logLik(glm(On_Time ~ 1, data = my_data, family = "binomial"))
LLmax <- logLik(logit_02)
psd_r_McFadden      = (-2*LL0 + 2*LLmax)/(-2*LL0)
psd_r_Cox_and_Snell = 1 - (exp(LL0)/exp(LLmax))^(2/nrow(my_data))
psd_r_Nagelkerke    = (1 - (exp(LL0)/exp(LLmax))^(2/nrow(my_data)))/(1 - (exp(LL0))^(2/nrow(my_data)))
# Odds Ratios
exp_coef <- exp(coef(logit_02))
# Confusion Matrix
library("SDMTools") 
confusion.matrix(my_data$On_Time, fitted(logit_02), threshold = 0.5)
# Accuracy
acc <- (30+56)/100
# ROC Curve
library("ggplot2")
library("ROCR")
pred <- prediction(prob, my_data$On_Time)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr = unlist(perf@x.values), tpr = unlist(perf@y.values), model = "GLM")
ggplot(roc.data, aes(x = fpr, ymin = 0, ymax = tpr)) + geom_ribbon(alpha = 0.2) + geom_line(aes(y = tpr)) + geom_abline(colour = "blue", intercept = 0, slope = 1) + ggtitle(paste0("AUC = ", round(auc, digits = 2)))
# Prediction
new_data <- as.data.frame(cbind(9.5, 8, 1, 0))
colnames(new_data) <- c("Distance", "Traffic_Lights", "Time_Period", "Driver_Profile2")
predict(logit_02, new_data, type = c("response"))