##### b.Decision Tree Model #####
# instruction: https://cran.r-project.org/web/packages/caret/caret.pdf 
# install.packages('caret')
library(caret)
library(rpart)

# b.1 data preparation
# b.1.1 load the data
df1 <- read.csv('df1.csv')
df2 <- read.csv('df2.csv')
# b.1.2 split training set and test set
# instruction: https://cran.r-project.org/web/packages/caTools/caTools.pdf 
# install.packages('caTools')
library(caTools)
set.seed(696)
split1 = sample.split(df1$credit, SplitRatio = 0.5)
training <- subset(df1, split1 == TRUE)
not_training <- subset(df1, split1 == FALSE)

split2 <- sample.split(not_training$credit, SplitRatio = 0.5)
testing <- subset(not_training, split2 == TRUE)
validation <- subset(not_training, split2 == FALSE)

nrow(training)      # number of observations in training set: 318   (training = 50% dataset)
nrow(validation)    # number of observations in training set: 160   (validation = 25% dataset)
nrow(testing)       # number of observations in test set: 160       (testing = 25% dataset)


# b.2 hyperparameter tuning
# setting conditions for trainControl
fitControl <- trainControl(method = "cv",   # set up cross validation
                           ## 5-fold cross validation
                           number = 5,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using the following function which
                           ## computes sensitivity, specificity & area under the ROC
                           summaryFunction = twoClassSummary)
# search for parameter(s) for tuning random forest
modelLookup('rpart2')
# setting different values of maxdepth for the model 
man_grid <-  expand.grid(maxdepth = c(1:20))
# doing the grid search                                                              
set.seed(696)
tree_model1 <- train(credit ~.-id, 
                     data = training, 
                     method = "rpart2", 
                     ## Specify which metric to optimize, by default, this is the accuracy
                     metric = "ROC",
                     trControl = fitControl,
                     tuneGrid = man_grid)
tree_model1
plot(tree_model1)
summary(tree_model1)
plot(varImp(tree_model1))
rpart.plot::rpart.plot(tree_model1$finalModel)

# b.3. build the model by manually selecting the parameter after the grid search
# setting conditions for trainControl
fitControl <- trainControl(method = "cv",   # set up cross validation
                           ## 5-fold cross validation
                           number = 5,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using the following function which
                           ## computes sensitivity, specificity & area under the ROC
                           summaryFunction = twoClassSummary)
# choosing maxdepth = 4 as the best tree model is given by doing the grid search above
hyperparams <- expand.grid(maxdepth = 4)
# building the model
set.seed(696)
tree_model2 <- train(credit ~.-id, 
                     data = training, 
                     method = "rpart2", 
                     tuneGrid = hyperparams,
                     metric = "ROC",
                     trControl = fitControl)
plot(varImp(tree_model2))
rpart.plot::rpart.plot(tree_model2$finalModel)

# b.4 using validation set to choose the best threshold
# extracting class posterior probabilities
prob <- predict(tree_model2, newdata = validation, type = 'prob')
# creating a vector of probability from tree prediction on the validation set
tree_val_prob <- prob[[2]]


# choosing the best therold for the classifer
library(ROCR)
threshold_pred <- prediction(tree_val_prob, validation$credit)
evaluation <- performance(threshold_pred, 'acc')
# Identify the best threshold 
max <- which.max(evaluation@y.values[[1]])
val_accuracy <- evaluation@y.values[[1]][max]
tree_threshold <- evaluation@x.values[[1]][max]
# reduce the threshold value a small degree so that the model could cover the true positives better
tree_threshold <- tree_threshold - 0.001
hist(tree_val_prob, breaks = 50, main = 'Histogram of the probability of the prediction')
abline(v = tree_threshold, col = 'red')
plot(evaluation, main = 'Threshold vs Accuracy in Decision Tree Model', xlab = 'Threshold', ylab = 'Accuracy')
abline(h = val_accuracy, col = 'red')
abline(v = tree_threshold, col = 'red')
tree_threshold

# b.5 predict on the testing set
# extracting class posterior probabilities
prob <- predict(tree_model2, newdata = testing, type = 'prob')
# creating a vector of probability from tree prediction on the validation set
tree_test_prob <- prob[[2]]
tree_test_pred <- as.factor(ifelse(prob[[2]] >= tree_threshold, "Good", "Bad"))
# accuracy level of the model on the testing set = 78.75%
confusionMatrix(testing$credit, tree_test_pred, positive = 'Good')
tree_test_acc <- confusionMatrix(testing$credit, tree_test_pred, positive = 'Good')[[3]][[1]]
tree_test_sens <- confusionMatrix(testing$credit, tree_test_pred, positive = 'Good')[[4]][[1]]
tree_test_spec <- confusionMatrix(testing$credit, tree_test_pred, positive = 'Good')[[4]][[2]]

# b.6 making predictions on df2, i.e. scoring sheet
df2$tree_prob <- predict(tree_model2, newdata = df2, type = 'prob')[[2]]
df2$tree_pred <- as.factor(ifelse(df2$tree_prob >= tree_threshold, "Good", "Bad"))
View(df2[,c('id','tree_pred')])


##### d.1 random forest #####
# install.packages('rpart')
library(rpart)

# d.1.1 hyperparameter tuning
# setting conditions for trainControl
fitControl <- trainControl(method = "cv",   # set up cross validation
                           ## 3-fold cross validation
                           number = 5,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using the following function which
                           ## computes sensitivity, specificity & area under the ROC
                           summaryFunction = twoClassSummary)
# search for parameter(s) for tuning random forest
modelLookup('rf')
# setting different values of mtry for the model 
man_grid <-  expand.grid(mtry = c(1:13))
# doing the grid search                                                              
set.seed(696)
# building the model                        NB. This takes some time to run           
forest_model1 <- train(credit ~.-id, 
                       data = training, 
                       method = "rf", 
                       ## Specify which metric to optimize, by default, this is the accuracy
                       metric = "ROC",
                       trControl = fitControl,
                       verbose = FALSE,
                       tuneGrid = man_grid)
forest_model1 
forest_model1$bestTune    # mtry = 3
plot(forest_model1, xlab = 'mtry')



# d.1.2 build the model by selecting the mtry given by the grid searches (mtry = 3)
hyperparams <- expand.grid(mtry=3)
fitControl <- trainControl(method = "cv",   # set up cross validation
                           ## 5-fold cross validation
                           number = 5,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using the following function which
                           ## computes sensitivity, specificity & area under the ROC
                           summaryFunction = twoClassSummary)
set.seed(696)
forest_model2 <- train(credit ~.-id, 
                       data = training, 
                       method = 'rf', 
                       tuneGrid = hyperparams,
                       metric = "ROC",
                       trControl = fitControl,
                       verbose = F)
forest_model2
summary(forest_model2)
forest_var <- varImp(forest_model2)
plot(forest_var)
ggplot(forest_var)

# d.1.3 using validation set to choose the best threshold
# extracting class posterior probabilities
prob <- predict(forest_model2, newdata = validation, type = 'prob')
# creating a vector of probability from tree prediction on the validation set
forest_val_prob <- prob[[2]]

# choosing the best therold for the classifer
library(ROCR)
threshold_pred <- prediction(forest_val_prob, validation$credit)
evaluation <- performance(threshold_pred, 'acc')
# Identify the best threshold 
max <- which.max(evaluation@y.values[[1]])
val_accuracy <- evaluation@y.values[[1]][max]
forest_threshold <- evaluation@x.values[[1]][max]
# reduce the threshold value a small degree so that the model could cover the true positives better
forest_threshold <- forest_threshold - 0.001
hist(tree_val_prob, breaks = 20, main = 'Histogram of the probability of the prediction')
abline(v = forest_threshold, col = 'red')
plot(evaluation, main = 'Threshold vs Accuracy - Random Forest', xlab = 'Threshold', ylab = 'Accuracy')
abline(h = val_accuracy, col = 'red')
abline(v = forest_threshold, col = 'red')
forest_threshold

# d.1.4 predict on the testing set
# extracting class posterior probabilities
prob <- predict(forest_model2, newdata = testing, type = 'prob')
# creating a vector of probability from tree prediction on the validation set
forest_test_prob <- prob[[2]]
forest_test_pred <- as.factor(ifelse(prob[[2]] >= forest_threshold, "Good", "Bad"))
# accuracy level of the model on the testing set = 63%
confusionMatrix(testing$credit, forest_test_pred, positive = 'Good')
forest_test_acc <- confusionMatrix(testing$credit, forest_test_pred, positive = 'Good')[[3]][[1]]
forest_test_sens <- confusionMatrix(testing$credit, forest_test_pred, positive = 'Good')[[4]][[1]]
forest_test_spec <- confusionMatrix(testing$credit, forest_test_pred, positive = 'Good')[[4]][[2]]

# d.1.5 making predictions on df2, i.e. scoring sheet
df2$forest_prob <- predict(forest_model2, newdata = df2, type = 'prob')[[2]]
df2$forest_pred <- as.factor(ifelse(df2$forest_prob >= forest_threshold, "Good", "Bad"))
View(df2[,c('id','forest_pred')])


##### d.2 gradient boosted trees #####
library(gbm)

# d.2.1 hyperparameter tuning
# d.2.1.1 grid search 1 
# setting conditions for trainControl
fitControl <- trainControl(method = "repeatedcv",   # set up cross validation
                           ## 3-fold cross validation
                           number = 3,
                           ## repeat cross validation 1 time
                           repeats = 1,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using the following function which
                           ## computes sensitivity, specificity & area under the ROC
                           summaryFunction = twoClassSummary)
# setting different values for gbm's parameters 
man_grid <-  expand.grid(n.trees = c(100,200,500,800,1000,1200,1500),
                         shrinkage = c(0.05, 0.01,0.1),
                         interaction.depth = c(1,3,5,8,10),            
                         n.minobsinnode = c(5,10,15,20))

# doing the grid search                  NB. THIS TAKE AROUND 15 MINUTES - SKIP THIS AND GO TO SECTION d.2.2
set.seed(696)
gbm_model1 <- train(credit ~.-id, 
                    data = training, 
                    method = "gbm", 
                    ## Specify which metric to optimize, by default, this is the accuracy
                    metric = "ROC",
                    trControl = fitControl,
                    verbose = FALSE,
                    tuneGrid = man_grid)
gbm_model1
gbm_model1$bestTune # n.trees = 100, interaction.depth = 3, shrinkage = 0.05, n.minobsinnode = 5
plot(gbm_model1)
summary(gbm_model1)

# d.2.1.1 grid search 2 
# setting conditions for trainControl
fitControl <- trainControl(method = "repeatedcv",   # set up cross validation
                           ## 3-fold cross validation
                           number = 3,
                           ## repeat cross validation 1 time
                           repeats = 1,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using the following function which
                           ## computes sensitivity, specificity & area under the ROC
                           summaryFunction = twoClassSummary)
# setting different values for gbm's parameters 
man_grid <-  expand.grid(n.trees = c(20,50,80,100,120,150,180,200),
                         shrinkage = c(0.05, 0.01,0.1),
                         interaction.depth = c(1,2,3,4,5),            
                         n.minobsinnode = c(1,2,3,4,5,6,7))

# doing the grid search                  NB. THIS TAKE AROUND 15 MINUTES - SKIP THIS AND GO TO SECTION d.2.2
set.seed(696)
gbm_model2 <- train(credit ~.-id, 
                    data = training, 
                    method = "gbm", 
                    ## Specify which metric to optimize, by default, this is the accuracy
                    metric = "ROC",
                    trControl = fitControl,
                    verbose = FALSE,
                    tuneGrid = man_grid)
gbm_model2
gbm_model2$bestTune # n.trees = 20, interaction.depth = 4, shrinkage = 0.01, n.minobsinnode = 5
plot(gbm_model2)
summary(gbm_model2)


# d.2.2 build the model by manually selecting the parameters after the grid searches
hyperparams <- expand.grid(n.trees = 20,
                           interaction.depth = 4, 
                           shrinkage = 0.01,
                           n.minobsinnode = 5)
fitControl <- trainControl(method = "repeatedcv",   # set up cross validation
                           ## 5-fold cross validation
                           number = 5,
                           ## repeat cross validation 5 times
                           repeats = 5,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using the following function which
                           ## computes sensitivity, specificity & area under the ROC
                           summaryFunction = twoClassSummary,
                           savePredictions = "all")
set.seed(696)
gbm_model8 <- train(credit ~.-id, 
                    data = training, 
                    method = "gbm", 
                    tuneGrid = hyperparams,
                    metric = "ROC",
                    trControl = fitControl,
                    verbose = F)

gbm_model8 
summary(gbm_model8) # ranking variables' significance: 1st historyCritical, 2nd employmentShort, 3rd historyCurrent, 4th historyDelay, 5th savingLow & 6th months
gbm_var <- varImp(gbm_model8)
plot(gbm_var)
ggplot(gbm_var)


# d.1.3 using validation set to choose the best threshold
# extracting class posterior probabilities
prob <- predict(gbm_model8, newdata = validation, type = 'prob')
# creating a vector of probability from tree prediction on the validation set
forest_val_prob <- prob[[2]]

# choosing the best therold for the classifer
library(ROCR)
threshold_pred <- prediction(forest_val_prob, validation$credit)
evaluation <- performance(threshold_pred, 'acc')
# Identify the best threshold 
max <- which.max(evaluation@y.values[[1]])
val_accuracy <- evaluation@y.values[[1]][max]
gbm_threshold <- evaluation@x.values[[1]][max]
# reduce the threshold value a small degree so that the model could cover the true positives better
gbm_threshold <- gbm_threshold - 0.001
hist(tree_val_prob, main = 'Histogram of the prediction probability')
abline(v = gbm_threshold, col = 'red')
plot(evaluation, main = 'Threshold vs Accuracy - GBM model', xlab = 'Threshold', ylab = 'Accuracy')
abline(h = val_accuracy, col = 'red')
abline(v = gbm_threshold, col = 'red')
gbm_threshold

# d.1.4 predict on the testing set
# extracting class posterior probabilities
prob <- predict(gbm_model8, newdata = testing, type = 'prob')
# creating a vector of probability from tree prediction on the validation set
gbm_test_prob <- prob[[2]]
gbm_test_pred <- as.factor(ifelse(prob[[2]] >= gbm_threshold, "Good", "Bad"))
# accuracy level of the model on the testing set = 63%
confusionMatrix(testing$credit, gbm_test_pred, positive = 'Good')
gbm_test_acc <- confusionMatrix(testing$credit, gbm_test_pred, positive = 'Good')[[3]][[1]]
gbm_test_sens <- confusionMatrix(testing$credit, gbm_test_pred, positive = 'Good')[[4]][[1]]
gbm_test_spec <- confusionMatrix(testing$credit, gbm_test_pred, positive = 'Good')[[4]][[2]]

# d.1.5 making predictions on df2, i.e. scoring sheet
df2$gbm_prob <- predict(gbm_model8, newdata = df2, type = 'prob')[[2]]
df2$gbm_pred <- as.factor(ifelse(df2$gbm_prob >= gbm_threshold, "Good", "Bad"))
View(df2[,c('id','gbm_pred')])


##### Evaluating models' performances ####


# calculating the area under the ROC
prob_list <- list(tree_test_prob, forest_test_prob, gbm_test_prob)
auc <- c()
for(i in 1:3){
  t <- prediction(prob_list[[i]], testing$credit)
  v <- performance(t, "auc")
  auc <- c(auc, v@y.values[[1]])
}
# creating a vector of models' accuracy
acc <- c(tree_test_acc, forest_test_acc, gbm_test_acc)
# creating a vector of models' sensitivity
sens <- c(tree_test_sens, forest_test_sens, gbm_test_sens)
# creating a vector of models' specificity
spec <- c(tree_test_spec, forest_test_spec, gbm_test_spec)
# creating a matrix of metrics evaluating models' performaces
model_performance <- cbind(auc, acc, sens, spec)
rownames(model_performance) <- c('tree', 'forest', 'gbm')
model_performance

write.csv(model_performance, 'model_performance.csv')

# final predictions of all models on the new dataset, i.e. df2
write.csv(df2[,c('id', 'tree_pred', 'forest_pred', 'gbm_pred')], 'a.csv')


# In my opinion, Decision Tree is better when the dataset have a “Feature” that is 
# really important to take a decision. Random Forest, 
# select some “Features” randomly to build the Trees, 
# if a “Feature” is important, sometimes Random Forest will build trees 
# that will not have the significance that the“Feature” has in the final decision.
# I think that Random Forest is good to avoid low quality of data,
# example: Imagine a dataset that shows (all houses that doors are green have a high cost), 
# in Decision Trees this is a bias in the data that can be avoid in Random Forest

# Greedy search of decision tree could be good this time





# create a dataframe containing prediction probabilities 
# given by tree, forest & gbm models on the test set
# the dataframe also includes numeric column of the credit (1 for Good & 0 for Bad)
numeric_credit_testing <- as.numeric(testing$credit) - 1
models_prob_predictions <- data.frame(tree_test_prob, 
                                      forest_test_prob,
                                      gbm_test_prob, 
                                      numeric_credit_testing)
View(models_prob_predictions)
# write a csv file so that the following question, i.e. h, can be refered to 
write.csv(models_prob_predictions, 'models_prob_predictions.csv', row.names = FALSE)



