##### f. Information Gain #####
# 1. load the data
df1 <- read.csv('df1.csv')
df2 <- read.csv('df2.csv')
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

# 2. information gain
# 2.1 entropy of the parent, i.e entropy of the credit (response variable)
prop_credit <- prop.table(table(training$credit))
parent_entropy <- - sum((prop_credit * log2(prop_credit)))
parent_entropy    # parent_entropy = 0.9792732

# 2.2 entropy of the children, i.e. entropy of other predictor variables
# 2.2.1 categorical variables
# calculate the proportions of each value of categorical variables against 'credit'
prop_elements <- function(x){
  prop.table(table(training[[x]], training$credit) + 1e-6, margin = 1)
}
# visualising the proportions -- click 'Previous Plot' in the Plots Window to see all of the plots
for(i in 2:10){
  plot(prop_elements(i), 
       col =c('red', 'blue'), 
       main = names(training[i]), 
       ylab = 'credit')
}
# calculate the entropy of categorical variables as the children of 'credit'
categorical_child_entropy <- function(x){
  prop_elements <- prop.table(table(training[[x]], training$credit) + 1e-6, margin = 1)
  sum(prop.table(table(training[[x]]))*rowSums(-prop_elements(x)*log2(prop_elements(x))))
}
for(i in 2:10){
  cat('\nEntropy of', names(training[i]), ':', categorical_child_entropy(i))
}

# 2.2.2 numeric variables
library(tree)
# choosing the best splits for numeric variables, 
# i.e. finding the split that the numeric variable has the lowest entropy
# 2.2.2.1 months 
# building a simple tree to choose a split 
# in which the information gain is maxisimised for that split
tree_month <- tree(training$credit ~ training$months)
tree_month  # choosing the split at 26.5
# split the data into two groups, 
# i.e. one group having greater than 26.5 months, the other less than 26.5
upper_months <- training$credit[training$months > 26.5]
lower_months <- training$credit[training$months < 26.5]
prop_upper_months <- length(upper_months)/length(training$credit)
prop_lower_months <- length(lower_months)/length(training$credit)
upper_cut <- table(upper_months)
lower_cut <- table(lower_months)
# calculate proportions for each group
c <- rbind(upper_cut, lower_cut)
d <- prop.table(c, margin = 1)
d
# calculate the entropy for months at the split of 26.5
entropy_months = prop_upper_months * rowSums(-d*log2(d))[[1]] + 
  prop_lower_months * rowSums(-d*log2(d))[[2]]
cat('\nEntropy of', names(training[11]), ':', entropy_months)

# 2.2.2.2 residence & age
# building a simple tree to choose a split 
# in which the information gain is maxisimised for that split
tree_residence <- tree(training$credit ~ training$residence)
tree_age <- tree(training$credit ~ training$age)
# examine the trees
tree_residence    # there is only root which does not grow its subsequent branches
tree_age          # there is only root which does not grow its subsequent branches
# conclusion: the trees could not choose splits for those two variables
# it implies that the entropies of those two children are greater than their parent, i.e. credit
# therefore, the trees decide not to split 
# when the information gain (parent's entropy - children's entropy) is not positive

# 2.3 the information gain
# create vector info_gain
info_gain <- c()
for(i in 2:13){
  if(i<=10){
    info_gain <- c(info_gain, parent_entropy - categorical_child_entropy(i))
  }else if(i == 11){
    info_gain <- c(info_gain, parent_entropy - entropy_months)
  }else{
    info_gain <- c(info_gain, 0)
  }
}
# create vector colnames, i.e. names for the correspondong info_gain values
colnames <- c()
for(i in 2:13){
  colnames <- c(colnames, names(training[i]))
}
info_gain_matrix <- matrix(info_gain, dimnames = list(colnames))
info_gain_matrix
# checking again whether there is a split for single-predictor tree models with response variable "credit" 
tree(training$credit ~ training$checking)       # split available for "checking" variable
tree(training$credit ~ training$history)        # split available for "history" variable
tree(training$credit ~ training$loan_reason)    # split available for "loan_reason" variable
tree(training$credit ~ training$saving)         # split unavailable for "saving" variable
tree(training$credit ~ training$employment)     # split available for "employment" variable
tree(training$credit ~ training$status)         # split unavailable for "status" variable
tree(training$credit ~ training$housing)        # split unavailable for "housing" variable
tree(training$credit ~ training$job)            # split unavailable for "job" variable
tree(training$credit ~ training$foreign)        # split unavailable for "foreign" variable
tree(training$credit ~ training$months)         # split available for "months" variable
tree(training$credit ~ training$residence)      # split unavailable for "residence" variable
tree(training$credit ~ training$age)            # split unavailable for "age" variable
rownames(info_gain_matrix)
# plot
bp <- barplot(info_gain_matrix, 
              beside = T,
              names.arg = c('checking', 'history', 'loan_reason', 'saving*', 
                            'employment', 'status*', 'housing*', 'job*',
                            'foreign*', 'months', 'residence*', 'age*'), 
              las=1, cex.names=0.93, 
              ylim = c(0, 0.25),
              space = 0.3,
              col= 'darkblue',border= 'white', 
              main = 'Information Gain for the 1st Split')
abline(h = 0.0134, col = 'grey47', lty = 2)
text(bp, 
     info_gain_matrix + 0.005, 
     labels = round(info_gain_matrix, digits = 4), 
     cex=0.9, 
     col = 'red3')
legend(8, 0.175, 
       legend = c('Approximation of IG threshold for splitting ( ~ 0.0135)', 
                  'No splitting for hypothetical single-predictor tree models'),
       lty = c(2, NA),
       pch = c(NA, 8),
       col = c('grey47', 'black'),
       cex = 0.85,
       box.lty=0)
# CONCLUSION: decision tree choose history as the root 
# as it achieved the greatest information gain when the tree splits



##### h. ROC #####
# h.1 load the dataframe of models' prediction probabilities
models_prob <- read.csv('models_prob_predictions.csv')
View(models_prob)

# h.2 calculating sensitivity and specificity for the tree model on the testing set
tree_prob_sort <- sort(models_prob$tree_test_prob)
tree_prob_no <- models_prob$tree_test_prob[models_prob$numeric_credit_testing == 0]
tree_prob_yes <- models_prob$tree_test_prob[models_prob$numeric_credit_testing == 1]
tree_sens <- c()         # create a new empty vector for the tree model's sensitivity
tree_spec <- c()         # create a new empty vector for the tree model's specificity
for(i in 1:nrow(models_prob)){
  tree_sens <- c(tree_sens, mean(tree_prob_yes >= tree_prob_sort[i]))
  tree_spec <- c(tree_spec, mean(tree_prob_no >= tree_prob_sort[i]))
}

# h.3 calculating sensitivity and specificity for the forest model on the testing set
forest_prob_sort <- sort(models_prob$forest_test_prob)
forest_prob_no <- models_prob$forest_test_prob[models_prob$numeric_credit_testing == 0]
forest_prob_yes <- models_prob$forest_test_prob[models_prob$numeric_credit_testing == 1]
forest_sens <- c()       # create a new empty vector for the forest model's sensitivity
forest_spec <- c()       # create a new empty vector for the forest model's specificity
for(i in 1:nrow(models_prob)){
  forest_sens <- c(forest_sens, mean(forest_prob_yes >= forest_prob_sort[i]))
  forest_spec <- c(forest_spec, mean(forest_prob_no >= forest_prob_sort[i]))
}


# h.4 calculating sensitivity and specificity for the gbm model on the testing set
gbm_prob_sort <- sort(models_prob$gbm_test_prob)
gbm_prob_no <- models_prob$gbm_test_prob[models_prob$numeric_credit_testing == 0]
gbm_prob_yes <- models_prob$gbm_test_prob[models_prob$numeric_credit_testing == 1]
gbm_sens <- c()           # create a new empty vector for the gbm model's sensitivity
gbm_spec <- c()           # create a new empty vector for the gbm model's specificity
for(i in 1:nrow(models_prob)){
  gbm_sens <- c(gbm_sens, mean(gbm_prob_yes >= gbm_prob_sort[i]))
  gbm_spec <- c(gbm_spec, mean(gbm_prob_no >= gbm_prob_sort[i]))
}

# h.5 plotting the ROC curve
plot(tree_spec, tree_sens, 
     xlim = c(0.035, 0.965), 
     ylim = c(0.035, 0.965), 
     xlab = "false positive rate",
     ylab = "true positive rate",
     main = 'ROC curve',
     type = "l", col = 'goldenrod1')
lines(forest_spec, forest_sens, col = 'purple')
lines(gbm_spec, gbm_sens, col = 'deepskyblue2')
abline(0,1, col = 'grey47', lty =3)
legend(0.01,0.99,
       legend = c("Tree", "Forest", "GBM"),
       col = c("goldenrod1", "purple", "deepskyblue2"),
       lty =1, cex = 0.75, box.lty=0)

