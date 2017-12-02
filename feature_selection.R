#Getting Data
setwd("~/Cars/Final/Github")

#import prepared data
test_set <- readRDS("~/Cars/Final/Github/test_set.rds")
training_set <- readRDS("~/Cars/Final/Github/training_set.rds")
under <- readRDS("~/Cars/Final/Github/under.rds")

training_set$INITIAL_RESULT = as.factor(training_set$INITIAL_RESULT)

#Feature Selection
#Applying Boruta to get most important features  (takes around 30 mins)
#install.packages("Boruta")
library(Boruta)
boruta.train = Boruta(INITIAL_RESULT~., data = training_set, doTrace = 2)
#See important variables
print(boruta.train)
#Plot variables in terms of importance
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.6)

#Removing tentative features
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
#Finally selected features 
getSelectedAttributes(final.boruta, withTentative = F)

training_set$CYL_12 = NULL
test_set$CYL_12 = NULL 
under$CYL_12 = NULL

sapply(training_set, class)
#apply PCA for reduction of variables to 2 
library(caret)
car.pca = prcomp(training_set[-64])
plot(car.pca, type = "line")
pca = preProcess(x = training_set[-64], method = "pca", pcaComp = 2)
training_set =  predict(pca, training_set)
training_set = training_set[c(2,3,1)]
test_set =  predict(pca, test_set)
test_set = test_set[c(2,3,1)]
under = predict(pca, under)
under = under[c(2,3,1)]

saveRDS(under, "under_pca.rds")
saveRDS(training_set, "training_set_pca.rds")
saveRDS(test_set, "test_set_pca.rds")

