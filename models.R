#Set working directory
setwd("~/Cars/Final/Github")

#Import the dataset
test_set_pca <- readRDS("~/Cars/Final/Github/test_set_pca.rds")
training_set_pca <- readRDS("~/Cars/Final/Github/training_set_pca.rds")
under_pca <- readRDS("~/Cars/Final/Github/under_pca.rds")
#Import Libraries to build classifiers
library(e1071)
library(rpart)
library(caret)
library(ROCR)
library(randomForest)

#Train model on blanced set with PCA
#Random Forest
classifier_rf = randomForest(INITIAL_RESULT~., data = under_pca, ntree = 39)
prob_rf = predict(classifier_rf, test_set_pca)
y_pred_rf = ifelse(prob_rf> 0.3, 1, 0)
#confusion matrix
table(y_pred_rf, test_set_pca$INITIAL_RESULT)
#Using Caret evaluating classifier
result_rf = confusionMatrix(y_pred_rf, test_set_pca$INITIAL_RESULT)
plot(classifier_rf)
result_rf$byClass


#SVM classifier
classifier_svm = svm(formula = INITIAL_RESULT ~ ., data = under_pca, type = "nu-classification", kernel = "radial")
result_svm = confusionMatrix(predict(classifier_svm, test_set_pca), test_set_pca$INITIAL_RESULT)
result_svm$byClass
result_svm


#logistic regression
classifier_log = glm(formula = INITIAL_RESULT ~., family = binomial(link="logit"),
                     data = under_pca)
prob_pred_log = predict(classifier_log,type = 'response', newdata = test_set_pca[-3])
y_pred_logistic = ifelse(prob_pred_log> 0.3, 1, 0)
cm_logistic = table(test_set_pca[, 3], y_pred_logistic)
result_log = confusionMatrix(cm_logistic)
result_log$byClass

