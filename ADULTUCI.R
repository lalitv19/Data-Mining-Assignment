#**************************************************************************************
# ADULT UCI data analytics -                                                     *
# *************************************************************************************
# Loading required packages
library(arules)  # package to load the data AdultUCI
library(reshape2)# package to handle matrix data
library(ggplot2) # package to plot graphs
library(xtable)  # package to import tables to latex code
library(dplyr)   # package to handle data frames
library(DMwR)    # package to use KNN imputation
library(corrplot)# package to explore correlation analysis
library(ROSE)    # package to explore sampling options

# We use h2o package for classification. it is a powerful package which can
# do algorthms at a fast pace in a different enviornment. For h2o package to 
# work perfectly java JDK and JRE must be installed and updated.
library(h2o)    # package for classification
# Defing a function to plot missing data

ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}
# importing data from package to enviornment
data(AdultUCI)
# exploring the data types of AdultUCI
str(AdultUCI)

# SECTION 1
# Missing data Analysis
# The missing data of the loaded data frame is plotted using this pre defined function
ggplot_missing(AdultUCI)

# Since the income data is missing in a number of data points. it is deleted from the data bse
df<-AdultUCI[!is.na(AdultUCI$income),]

#SECTION 2
# The missing data plot shows only few missing data is observed. Hence we use KNN imputation 
# to fill the data points missing
Df_imp <- knnImputation(df)

# SECTION 3
# Outlier treatment 
# We define a user defined function to calculate the parameters required to
# identify outliers
mystats <- function(x) {
  a <- x[!is.na(x)]
  m <- mean(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c( outlier_flag=outlier_flag, mean=m, stdev=s,min = min,p99=p99,max=max, UC=UC, LC=LC ))
}
# The user defined function is applied on the numeric data which has potential for outliers
vars <- c( 'age','fnlwgt','education-num','capital-gain','capital-loss','hours-per-week')
diag_stats<-t(data.frame(apply(Df_imp[vars], 2, mystats)))
View(diag_stats)

# Based on the insights from above analysis was used to curtail the data to 99 quartile
Df_imp$age[Df_imp$age>74]<-74
Df_imp$fnlwgt[Df_imp$fnlwgt>510072]<-510072
Df_imp$`education-num`[Df_imp$`education-num`>16]<-16
Df_imp$`capital-gain`[Df_imp$`capital-gain`>15024]<-15024
Df_imp$`capital-loss`[Df_imp$`capital-loss`>1980]<-1980
Df_imp$`hours-per-week`[Df_imp$`hours-per-week`>80]<-80

# SECTION 4
# Correlation Analysis.
# In this section we explore the impact of numerical, binary categorical and ordered factor 
# variables on income class prediction
Df_imp$education<-as.numeric(Df_imp$education)
Df_imp$sex<-as.numeric(Df_imp$sex)
Df_imp$income<-as.numeric(Df_imp$income)

# The variable selection is done to select the varaiables
vars <- c( 'age','fnlwgt','education-num','capital-gain','capital-loss','hours-per-week','sex','income')

# Correlation plot of the variables
M<-cor(Df_imp[vars])
corrplot(M, method="circle")

# The plot indiactes that fnlwgt predictor is not correlated to income class. 
# As income is a categorical variable and fnlwgt is a quantitative variable 
# ANOVA is used to confirm our analysis
res.aov <- aov(fnlwgt ~ income, data = Df_imp)
summary(res.aov)

# The ANOVA analysis also confirms that fnlwgt is not useful to predict income
# Further education and education num are highly correlated so we only take the numeric value
Df_imp$fnlwgt<-NULL
Df_imp$education<-NULL

#SECTION 5
# In this section we explore the effect of categorical variable on income 
# As both variable as categorical we use chisquare test 
# marital-status vs income analysis
car.data = table(Df_imp$`marital-status`, Df_imp$income) 
print(chisq.test(car.data))

#occupation vs income analysis
car.data = table(Df_imp$occupation, Df_imp$income) 
print(chisq.test(car.data))

#relationship vs income analysis
car.data = table(Df_imp$relationship, Df_imp$income) 
print(chisq.test(car.data))

# race vs income analysis
car.data = table(Df_imp$race, Df_imp$income) 
print(chisq.test(car.data))

#native-country vs income analysis
car.data = table(Df_imp$`native-country`, Df_imp$income) 
print(chisq.test(car.data))
# All variables are found to have good impact on income

#SECTION 6
# preparing data for calssification
mydat2<-Df_imp
mydat2$workclass<-as.factor(mydat2$workclass)
mydat2$income<-as.factor(as.numeric(mydat2$income))

# We analyse the income class distribution
table(mydat2$income)

# intialize h2o cluster
h2o.init()

# Remove running clusters to be more efficient
h2o.removeAll()

# Convert data frame to h2o enviornment
df<-as.h2o(mydat2)

# The following code split the data into test and train on 60% train 20% for validation
# and 20% test
splits <- h2o.splitFrame(
  df,           
  c(0.6,0.2),   ##  create splits of 60% and 20%; 
  seed=1234)
# The split is used to get train data
train <- h2o.assign(splits[[1]], "train.hex")   ## R train, H2O train.hex
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex

#SECTION 7
# Classification Algorithms
# No.1 Algorithm
# Random forest
rf1 <- h2o.randomForest(         # h2o.randomForest 
  training_frame = train,        # training data
  validation_frame = valid,      # validation data
  x=1:12,                        # the predictor columns, by column index
  y=13,                          # the dependent variable
  model_id = "rf_covType_v1",    # model name
  ntrees = 200,                  # number of trees
  stopping_rounds = 2,           # Stoping condition
  score_each_iteration = T,      # training and validation 
  seed = 1000000)  
# Train data performance
rf1_tr_perf<-h2o.performance(h2o.getModel('rf_covType_v1'))
# Validation data performance: ( although we didnt use cross validation)
rf1_vali_perf<-rf1@model$validation_metrics 
# Test data performance
rf1_test_perf<-h2o.performance(h2o.getModel('rf_covType_v1'), newdata = test)
# Test data prediction
rf1_pred<-h2o.predict(rf1, test)
# Model variable impact
rf1_sum <- summary(rf1)  

# No.2 Algorithm
# Logistic Regression

train.glm <- h2o.glm(                            # Logistic Regression Model
                     training_frame = train,     # training data
                     validation_frame = valid,   # validation data (not required)
                     x=1:12,                     # predictors
                     y=13,                       # dependent variable
                     model_id = "glm_covType_v1",# Model name
                     seed = 1234567,             # Seed for random numbers
                     family = "binomial",        # Outcome variable
                     lambda_search = TRUE,       # regularisation lambda
                     alpha = 0.5                 # regularization alpha
)
# Train data performance
LR_train_perf<-h2o.performance(h2o.getModel('glm_covType_v1'))
# Validation data performance
LR_vali_perf<-train.glm@model$validation_metrics 
# test data performance
LR_test_perf<-h2o.performance(h2o.getModel('glm_covType_v1'), newdata = test)
# Model summary
LR_sum<-summary(train.glm)  
# Model prediction
LR_pred<-h2o.predict(train.glm, test)

# No.3 Algorithm
# Neural Network

nn_model <- h2o.deeplearning( # Neural Network Model
  model_id="dl_model_nn",     # Model Name
  training_frame=train,       # training data
  validation_frame=valid,     # validation data 
  x=1:12,                     # Predictors
  y=13,                       # dependent variable
  hidden=c(20),               # No of hidden layers and hidden nodes
  epochs=10                   # number of runs
)
# Train data performance
nn_train_perf<-h2o.performance(h2o.getModel('dl_model_nn'))
# Validation data performance
nn_vali_perf<-nn_model@model$validation_metrics 
# Test data performance
nn_test_perf<-h2o.performance(h2o.getModel('dl_model_nn'), newdata = test)
# Model Summary
nn_sum<-summary(nn_model) 
# Model prediction
nn_pred<-h2o.predict(nn_model, test)

# No.4 Algorithm
# Naive Bayes

nb_model <- h2o.naiveBayes(  # Naive Bayes Model
  model_id="dl_model_nb",    # Model name
  training_frame=train,      # Train data
  validation_frame=valid,    # validation data
  x=1:12,                    # predictors
  y=13,                      # dependent variable
  laplace = 3                # Laplace factor
)
# Train data performance
nb_train_perf<-h2o.performance(h2o.getModel('dl_model_nb'))
# Validation data performance
nb_vali_perf<-nb_model@model$validation_metrics 
# Test Data performance
nb_test_perf<-h2o.performance(h2o.getModel('dl_model_nb'), newdata = test)
# Model Summary
nb_sum<-summary(nb_model)  
# Model prediction
nb_pred<-h2o.predict(nb_model, test)

# No.5 Algorithm
# GBM

gbm_model <- h2o.gbm(       # Gradient Boosting Model
  model_id="dl_model_gbm",  # Model Name
  training_frame=train,     # Train data
  validation_frame=valid,   # validation data
  x=1:12,                   # predictors
  y=13                      # Dependent variable
)
# Train data performance
gbm_train_perf<-h2o.performance(h2o.getModel('dl_model_gbm'))
# Validation Data performance
gbm_vali_perf<-gbm_model@model$validation_metrics 
# Test Data performance
gbm_test_perf<-h2o.performance(h2o.getModel('dl_model_gbm'), newdata = test)
# Model summary
gbm_sum<-summary(gbm_model)  
# Model prediction
gbm_pred<-h2o.predict(gbm_model, test)


# SECTION 8
# Sensitvity
# A comparision of method predictions in eval results
evalResults = as.data.frame(test$income )          # put in the truth
evalResults$income<-as.numeric(evalResults$income) # Convert to number
evalResults$RF = as.data.frame(rf1_pred)[2]        # Random Forest Prediction
evalResults$LR = as.data.frame(LR_pred)[2]         # Logistic Regression
evalResults$NN= as.data.frame(nn_pred)[2]          # Neural Network
evalResults$NB= as.data.frame(nb_pred)[2]          # Naive Bayes
evalResults$GBM = as.data.frame(gbm_pred)[2]       # Gradient Boosting Method

# ROC curve plot values of models on validation  data
# Logistic Regression
x_glm<-h2o.performance(train.glm,valid=T)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_glm$col<-'GLR'
# Random Forest
x_rf<-h2o.performance(rf1,valid=T)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_rf$col<-'RF'
# Neural Network
x_nn<-h2o.performance(nn_model,valid=T)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_nn$col<-'NN'
# Naive Bayes
x_nb<-h2o.performance(nb_model,valid=T)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_nb$col<-'NB'
# Gradient Boosting
x_gbm<-h2o.performance(gbm_model,valid=T)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_gbm$col<-'GBM'
# Combining all data
x<-rbind(x_glm,x_rf,x_nn,x_nb,x_gbm)
# Plot sensitivity for different models

ggplot(data=x, aes(x=fpr, y=tpr)) + 
  geom_line(aes( color=col),size=1.5) + 
  xlab('False Positive Rate') + 
  ylab("True Positive Rate")

# ROC curve plot values of models on train  data
x_glm<-h2o.performance(train.glm)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_glm$col<-'GLR'
x_rf<-h2o.performance(rf1)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_rf$col<-'RF'
x_nn<-h2o.performance(nn_model)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_nn$col<-'NN'
x_nb<-h2o.performance(nb_model)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_nb$col<-'NB'
x_gbm<-h2o.performance(gbm_model)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_gbm$col<-'GBM'

x<-rbind(x_glm,x_rf,x_nn,x_nb,x_gbm)
# ROC plot
ggplot(data=x, aes(x=fpr, y=tpr)) + 
  geom_line(aes( color=col),size=1.5) + 
  xlab('False Positive Rate') + 
  ylab("True Positive Rate")

# ROC curve plot values of models on test data
x_glm<-h2o.performance(train.glm,newdata = test)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_glm$col<-'GLR'
x_rf<-h2o.performance(rf1,newdata = test)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_rf$col<-'RF'
x_nn<-h2o.performance(nn_model,newdata = test)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_nn$col<-'NN'
x_nb<-h2o.performance(nb_model,newdata = test)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_nb$col<-'NB'
x_gbm<-h2o.performance(gbm_model,newdata = test)@metrics$thresholds_and_metric_scores[c('tpr','fpr')]
x_gbm$col<-'GBM'

x<-rbind(x_glm,x_rf,x_nn,x_nb,x_gbm)
# ROC plot
ggplot(data=x, aes(x=fpr, y=tpr)) + 
  geom_line(aes( color=col),size=1.5) + 
  xlab('False Positive Rate') + 
  ylab("True Positive Rate")

# combined Gain chart plot data

y_glm<-h2o.gainsLift(gbm_model)
y_glm$col<-'GLR'
y_rf<-h2o.gainsLift(rf1)
y_rf$col<-'RF'
y_nn<-h2o.gainsLift(nn_model)
y_nn$col<-'NN'
y_nb<-h2o.gainsLift(nb_model)
y_nb$col<-'NB'
y_gbm<-h2o.gainsLift(gbm_model)
y_gbm$col<-'GBM'

ys<-rbind(y_glm,y_rf,y_nn,y_nb,y_gbm)

# Plot Gains plot
ggplot(data=ys, aes(x=cumulative_data_fraction, y=cumulative_lift)) + 
  geom_line(aes( color=col),size=1.5) + 
  xlab('data fraction') + 
  ylab("cumulative lift")


# Plot Lift curve
ggplot(data=ys, aes(x=cumulative_data_fraction, y=cumulative_capture_rate)) + 
  geom_line(aes( color=col),size=1.5) + 
  xlab('data fraction') + 
  ylab("cumulative capture rate")


table(mydat2$income)

#******************************************Concluded***********************************
