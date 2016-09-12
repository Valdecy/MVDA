########################################################################################################################################

# Created by: Prof. Valdecy Pereira, D.Sc.
# UFF - Universidade Federal Fluminense (Brazil)
# email:  valdecypereira@yahoo.com.br
# Course: Multivariate Data Analysis
# Lesson: Exploratory Factor Analysis
# Citation: 
# PEREIRA, V. (2016). Project: Multivariate Data Analysis, File: R-MVDA-03-EFA.R, GitHub repository: <https://github.com/Valdecy/Multivariate_Data_Analysis>

########################################################################################################################################

# Graphical Analysis
library(ggplot2)
Observations <- 1:1536
ggplot(data = my_data, aes(x = Observations, y = Moed)) + geom_point(shape = 1)
ggplot(data = my_data, aes(x = "", y = Moed)) + geom_boxplot() + theme(axis.title.x = element_blank())
ggplot(my_data, aes(Moed)) + geom_histogram(bins = 50)
ggplot(data = my_data, aes( sample = my_data[ ,1])) + stat_qq()+ xlab("Theorethical") + ylab("Moed") 
ggplot(data = my_data, aes(x = Observations, y = Faed)) + geom_point(shape = 1)
ggplot(data = my_data, aes(x = "", y = Faed)) + geom_boxplot() + theme(axis.title.x = element_blank())
ggplot(my_data, aes(Faed)) + geom_histogram(bins = 50)
ggplot(data = my_data, aes( sample = my_data[ ,2])) + stat_qq()+ xlab("Theorethical") + ylab("Faed")
ggplot(data = my_data, aes(x = Observations, y = Famin)) + geom_point(shape = 1)
ggplot(data = my_data, aes(x = "", y = Famin)) + geom_boxplot() + theme(axis.title.x = element_blank())
ggplot(my_data, aes(Famin)) + geom_histogram(bins = 50)
ggplot(data = my_data, aes( sample = my_data[ ,3])) + stat_qq()+ xlab("Theorethical") + ylab("Famin")
ggplot(data = my_data, aes(x = Observations, y = Eng)) + geom_point(shape = 1)
ggplot(data = my_data, aes(x = "", y = Eng)) + geom_boxplot() + theme(axis.title.x = element_blank())
ggplot(my_data, aes(Eng)) + geom_histogram(bins = 50)
ggplot(data = my_data, aes( sample = my_data[ ,4])) + stat_qq()+ xlab("Theorethical") + ylab("Eng") 
ggplot(data = my_data, aes(x = Observations, y = Math)) + geom_point(shape = 1)
ggplot(data = my_data, aes(x = "", y = Math)) + geom_boxplot() + theme(axis.title.x = element_blank())
ggplot(my_data, aes(Math)) + geom_histogram(bins = 50)
ggplot(data = my_data, aes( sample = my_data[ ,5])) + stat_qq()+ xlab("Theorethical") + ylab("Math")
ggplot(data = my_data, aes(x = Observations, y = Soc)) + geom_point(shape = 1)
ggplot(data = my_data, aes(x = "", y = Soc)) + geom_boxplot() + theme(axis.title.x = element_blank())
ggplot(my_data, aes(Soc)) + geom_histogram(bins = 50)
ggplot(data = my_data, aes( sample = my_data[ ,6])) + stat_qq()+ xlab("Theorethical") + ylab("Soc") 
ggplot(data = my_data, aes(x = Observations, y = Nat)) + geom_point(shape = 1)
ggplot(data = my_data, aes(x = "", y = Nat)) + geom_boxplot() + theme(axis.title.x = element_blank())
ggplot(my_data, aes(Nat)) + geom_histogram(bins = 50)
ggplot(data = my_data, aes( sample = my_data[ ,7])) + stat_qq()+ xlab("Theorethical") + ylab("Nat")
ggplot(data = my_data, aes(x = Observations, y = Vocab)) + geom_point(shape = 1)
ggplot(data = my_data, aes(x = "", y = Vocab)) + geom_boxplot() + theme(axis.title.x = element_blank())
ggplot(my_data, aes(Vocab)) + geom_histogram(bins = 50)
ggplot(data = my_data, aes( sample = my_data[ ,8])) + stat_qq()+ xlab("Theorethical") + ylab("Vocab")
# Univariate Normality
shapiro.test(my_data$Moed) 
shapiro.test(my_data$Faed) 
shapiro.test(my_data$Famin)
shapiro.test(my_data$Eng)
shapiro.test(my_data$Math)
shapiro.test(my_data$Soc)
shapiro.test(my_data$Nat)
shapiro.test(my_data$Vocab)
# Multivariate Normality
library(MVN)
mardiaTest(my_data, qqplot = FALSE)
# Linearity
library (psych)
pairs.panels(my_data)
# Sample Size
nrow(my_data)
# Ratio
nrow(my_data)/ncol(my_data)
# Correlation matrix
c_mat <- cor(my_data)
# Determinant of the correlation matrix
det(c_mat)
# KMO and Diagonal
KMO(cor(my_data))
# Barlett Sphericity Test
cortest.bartlett(cor(my_data), n = nrow(my_data)) 
# Scree plot
scree <- as.data.frame(eigen(cor(my_data))$values)
ggplot(data = scree, aes( x = 1:ncol(my_data), y = scree[,1])) + geom_point(shape = 7, size = 1.2) + geom_line() + geom_hline(yintercept = 1) + xlab("Factors") + ylab("Eigenvalue")
# Extraction and Rotation
library(psych)
fa_obl <- fa(my_data, nfactors = 2, rotate = "oblimin", fm = "pa")
fa_var <- fa(my_data, nfactors = 2, rotate = "varimax", fm = "pa")
# Loadings
fa_var$loadings
fa_obl$loadings
# Communality
fa_obl$communality
fa_var$communality
# Factor Correlation
fa_obl$Phi
# Factor Loadings Plot
L1_obl <- fa_obl$loadings[1:ncol(my_data)]
L2_obl <- fa_obl$loadings[(ncol(my_data) + 1):(2*ncol(my_data))]
FL_obl <- as.data.frame(cbind(L1_obl,L2_obl))
ggplot(data = FL_obl, aes( x = L1_obl, y = L2_obl, label = colnames(my_data))) + geom_text() + xlim(c(-1,1)) + ylim(c(-1,1)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlab("Factor 1") + ylab("Factor 2")
L1_var <- fa_var$loadings[1:ncol(my_data)]
L2_var <- fa_var$loadings[(ncol(my_data) + 1):(2*ncol(my_data))]
FL_var <- as.data.frame(cbind(L1_var,L2_var))
ggplot(data = FL_var, aes( x = L1_var, y = L2_var, label = colnames(my_data))) + geom_text() + xlim(c(-1,1)) + ylim(c(-1,1)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlab("Factor 1") + ylab("Factor 2")
# Factor Plot
fa.diagram(fa_obl)
fa.diagram(fa_var)