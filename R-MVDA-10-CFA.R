#############################################################################################################

# Created by: Prof. Valdecy Pereira, D.Sc.
# UFF - Universidade Federal Fluminense (Brazil)
# email:  valdecypereira@yahoo.com.br
# Course: Multivariate Data Analysis
# Lesson: Exploratory Factor Analysis
# Citation: 
# PEREIRA, V. (2016). Project: Multivariate Data Analysis, File: R-MVDA-03-EFA.R, GitHub repository: <https://github.com/Valdecy/Multivariate_Data_Analysis>

#############################################################################################################

# Required Libraries
#install.packages("lavaan")
#install.packages("MVN")
#install.packages("semPlot")
#install.packages("corrplot")
#install.packages("bestNormalize")
library("lavaan")
library("MVN")
library("semPlot")
library("corrplot")
library("bestNormalize")

#############################################################################################################

# Load Data
my_data <- R_MVDA_CFA_Dataset_01

# Correlation Matrix
corrplot.mixed(cor(my_data), lower = "number", upper = "ellipse")

#############################################################################################################

# Check Mulyivariate Normality
mvn(data = my_data, mvnTest = "mardia")
mvn(data = my_data, mvnTest = "mardia", univariateTest = "AD", desc = TRUE) # AD, SW (> 5000), CVM, Lillie, SF

# Create Univariate Plots
mvn(data = my_data, mvnTest = "mardia", univariatePlot = "qqplot")
mvn(data = my_data, mvnTest = "mardia", univariatePlot = "histogram")
par(mfrow = c(1,1))

# Outliers
#mvn(data = my_data, mvnTest = "mardia", multivariateOutlierMethod = "quan")
#mvn(data = my_data, mvnTest = "mardia", multivariateOutlierMethod = "adj" ) # Adjusted Mahalanobis distance
#outliers <- mvn(data = my_data, mvnTest = "mardia", multivariateOutlierMethod = "adj" , showNewData = TRUE)
#my_data <- outliers$newData

# Transform to Normal
#moed_nm <- bestNormalize(my_data$Moed)
#faed_nm <- bestNormalize(my_data$Faed)
#famn_nm <- bestNormalize(my_data$Famin)
#eng_nm  <- bestNormalize(my_data$Eng)
#mat_nm  <- bestNormalize(my_data$Math)
#soc_nm  <- bestNormalize(my_data$Soc)
#nat_nm  <- bestNormalize(my_data$Nat)
#voc_nm  <- bestNormalize(my_data$Vocab)

# Last Resort - Binarize
#moed_nm <- binarize(my_data$Moed) 
#faed_nm <- binarize(my_data$Faed)
#famn_nm <- binarize(my_data$Famin)
#eng_nm  <- binarize(my_data$Eng)
#mat_nm  <- binarize(my_data$Math)
#soc_nm  <- binarize(my_data$Soc)
#nat_nm  <- binarize(my_data$Nat)
#voc_nm  <- binarize(my_data$Vocab)

#par(mfrow = c(1, 2))
#MASS::truehist(my_data$Vocab)
#MASS::truehist(voc_nm$x.t)
#par(mfrow = c(1, 1))

#my_data$Moed  <- moed_nm$x.t
#my_data$Faed  <- faed_nm$x.t
#my_data$Famin <- famn_nm$x.t
#my_data$Eng   <- eng_nm$x.t
#my_data$Math  <- mat_nm$x.t
#my_data$Soc   <- soc_nm$x.t
#my_data$Nat   <- nat_nm$x.t
#my_data$Vocab <- voc_nm$x.t

#boxplot(log10(nat_nm$oos_preds), yaxt = 'n')

# Check Mulyivariate Normality
#mvn(data = my_data, mvnTest = "mardia")

# Create Univariate Plots
#mvn(data = my_data, mvnTest = "mardia", univariatePlot = "qqplot")
#mvn(data = my_data, mvnTest = "mardia", univariatePlot = "histogram")
#par(mfrow = c(1,1))

# CFA - Model Specification
cfa_model <- "
# Measurement Model
factor_1 =~ Eng + Math + Soc + Nat + Vocab
factor_2 =~ Moed + Faed + Famin

"

# Running CFA
cfa_fit <- cfa(cfa_model, data = my_data, orthogonal = FALSE, estimator = "mlr") # orthogonal = TRUE (Uncorrelated Factors); estimator ml robust.

# CFA Results
summary(cfa_fit, fit.measures = TRUE, standardized = TRUE, rsq = TRUE)
parameterEstimates(cfa_fit)   # Coefficients with se, z-values and 95% CI
fitted(cfa_fit)               # Implied variance-covariance matrix
fitMeasures(cfa_fit)          # Fit measures

# Visualize this model with semPlot
semPaths(object = cfa_fit, what = "std", whatLabels = "std", title = FALSE, curvePivot = TRUE, intercepts = FALSE, residuals = TRUE, style = "lisrel") # what = "path", "par", "std", "eq", "col"

# estimator: 
# "ML"   for maximum likelihood, 
# "GLS"  for generalized least squares, 
# "WLS"  for weighted least squares (sometimes called ADF estimation), 
# "ULS"  for unweighted least squares and 
# "DWLS" for diagonally weighted least squares. 
# These are the main options that affect the estimation. For convenience, the "ML" option can be extended as "MLM", "MLMV", "MLMVS", "MLF", and "MLR". The estimation will still be plain "ML", but now with robust standard errors and a robust (scaled) test statistic. 

# For "MLM", "MLMV", "MLMVS", classic robust standard errors are used (se = "robust.sem"); 
# For "MLF", standard errors are based on first-order derivatives (information = "first.order"); 
# For "MLR", 'Huber-White' robust standard errors are used (se = "robust.huber.white"). 
# "MLM" will compute a Satorra-Bentler scaled (mean adjusted) test statistic (test = "satorra.bentler"), 
# "MLMVS" will compute a mean and variance adjusted test statistic (Satterthwaite style) (test = "mean.var.adjusted"), 
# "MLMV" will compute a mean and variance adjusted test statistic (scaled and shifted) (test = "scaled.shifted"), and 
# "MLR" will compute a test statistic which is asymptotically equivalent to the Yuan-Bentler 50 lavOptions T2-star test statistic (test = "yuan.bentler.mplus"). 

# Analogously, the estimators "WLSM" and "WLSMV" imply the "DWLS" estimator (not the "WLS" estimator) with robust standard errors and a mean or mean and variance adjusted test statistic. Estimators "ULSM" and "ULSMV" imply the "ULS" estimator with robust standard errors and a mean or mean and variance adjusted test statistic


# Write a lavaan model:
lavaan_cfa_model <- "

# Factor loadings:
factor_1 =~ 1*Eng + Math + Soc + Nat + Vocab
factor_2 =~ 1*Moed + Faed + Famin

# Variances:
factor_1 ~~ factor_1
factor_2 ~~ factor_2

# Covariances:
factor_1 ~~ factor_2

# Residuals:
Eng   ~~ Eng
Math  ~~ Math
Soc   ~~ Soc
Nat   ~~ Nat
Vocab ~~ Vocab
Moed  ~~ Moed
Faed  ~~ Faed
Famin ~~ Famin

"

# Fit in lavaan:
cfa_fit <- lavaan(lavaan_cfa_model, my_data, orthogonal = FALSE, estimator = "mlr")

# Assess fit:
cfa_fit
summary(cfa_fit)
parameterEstimates(cfa_fit)   # Coefficients with se, z-values and 95% CI
fitted(cfa_fit)               # Implied variance-covariance matrix
fitMeasures(cfa_fit)          # Fit measures

# Visualize this model with semPlot
semPaths(object = cfa_fit, what = "std", whatLabels = "std", title = FALSE, curvePivot = TRUE, intercepts = FALSE, residuals = TRUE, style = "lisrel")
