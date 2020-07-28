##### 1. data preparation #####
# 1.1. list the excel file's sheets 
library(tidyverse)
library(readxl)
excel_sheets('Credit_Risk6_final.xlsx')

# 1.2. load the dataset 
# 1.2.1 dataframe df1 from 'Training_Data' sheet
df1 <- read_excel('Credit_Risk6_final.xlsx', sheet = 'Training_Data')
View(df1)
nrow(df1)
ncol(df1)
str(df1)
# 1.2.2 dataframe df2 from 'Scoring_Data' sheet
df2 <- read_excel('Credit_Risk6_final.xlsx', sheet = 'Scoring_Data')
View(df2)
nrow(df2)
ncol(df2)
str(df2)

# 1.3. change columns' names 
# df1
colnames(df1)
names(df1)[names(df1) == 'ID'] <- 'id'
names(df1)[names(df1) == 'Checking Acct'] <- 'checking'
names(df1)[names(df1) == 'Credit History'] <- 'history'
names(df1)[names(df1) == 'Loan Reason'] <- 'loan_reason'
names(df1)[names(df1) == 'Savings Acct'] <- 'saving'
names(df1)[names(df1) == 'Employment'] <- 'employment'
names(df1)[names(df1) == 'Personal Status'] <- 'status'
names(df1)[names(df1) == 'Housing'] <- 'housing'
names(df1)[names(df1) == 'Job Type'] <- 'job'
names(df1)[names(df1) == 'Foreign National'] <- 'foreign'
names(df1)[names(df1) == 'Months since Checking Acct opened'] <- 'months'
names(df1)[names(df1) == 'Residence Time (In current district)'] <- 'residence'
names(df1)[names(df1) == 'Age'] <- 'age'
names(df1)[names(df1) == 'Credit Standing'] <- 'credit'
# df2
colnames(df2)
names(df2)[names(df2) == 'ID'] <- 'id'
names(df2)[names(df2) == 'Checking Acct'] <- 'checking'
names(df2)[names(df2) == 'Credit History'] <- 'history'
names(df2)[names(df2) == 'Loan Reason'] <- 'loan_reason'
names(df2)[names(df2) == 'Savings Acct'] <- 'saving'
names(df2)[names(df2) == 'Employment'] <- 'employment'
names(df2)[names(df2) == 'Personal Status'] <- 'status'
names(df2)[names(df2) == 'Housing'] <- 'housing'
names(df2)[names(df2) == 'Job Type'] <- 'job'
names(df2)[names(df2) == 'Foreign National'] <- 'foreign'
names(df2)[names(df2) == 'Months since Checking Acct opened'] <- 'months'
names(df2)[names(df2) == 'Residence Time'] <- 'residence'
names(df2)[names(df2) == 'Age'] <- 'age'

# 1.4 checking duplications and overlaps
# 1.4.1 checking column id
# building a function to check whether a vector is consecutive or not
check_consecutive <- function(x){
  if(all(diff(x) == 1)){
    print('This is consecutive')
  }else{
    print('This is not consecutive')
    print('Positions are not consecutive')
    print(which(diff(x) != 1))
  }
}
check_consecutive(df1$id)
check_consecutive(df2$id)

# 1.4.1a IDs vs credit - checking the normality of the 'credit' entry
# percentage of the good credit
prop_good <- prop.table(table(df1$credit))[[2]]
prop_good
# percentage of the bad credit
prop_bad <- prop.table(table(df1$credit))[[1]]
prop_bad
# getting the lengths of consecutive credits which are similar 
consecutive_credit <- rle(df1$credit)
credit_freq <- consecutive_credit[[1]]
credit_value <- consecutive_credit[[2]]
# identify the ids positions where consecutive similar credits start
id_start <- c(1, cumsum(consecutive_credit[[1]]) + 1)
id_start <- id_start[1:length(id_start)-1]
# identify the ids positions where consecutive similar credits end
id_end <- id_start + consecutive_credit[[1]] - 1 
# calculate chance of events associated with consecutive similar credits
chance <- c()
for(i in 1:length(id_start)){
  if(credit_value[i] == "Good"){
    chance <- c(chance, prop_good^credit_freq[i]*100)
  }else{
    chance <- c(chance, prop_bad^credit_freq[i]*100)
  }
}
# creating a dataframe for suspicious entries
# i.e. number of consecutive credit entries are greater than 6
frame_a <- data.frame(id_start, id_end, credit_freq, credit_value, chance)
colnames(frame_a) <- c('id_start', 'id_end', 'frequency', 'value', 'chance_in_percentage')
good_entry_limit <- frame_a %>%
  filter(value == 'Good') %>%
  filter(frequency >= 6)
bad_entry_limit <- frame_a %>%
  filter(value == 'Bad') %>%
  filter(frequency >= 4)
suspicious_entry <- bind_rows(good_entry_limit, bad_entry_limit)
suspicious_entry <- suspicious_entry %>% arrange(id_start)
suspicious_entry


# 1.4.2 check all columns apart from the ids
# 1.4.2.1 regardless of the ids column, checking duplications in the dataset df1
a <- duplicated(df1[,2:14])
# a vector containing rows are duplicated with other rows in the dataset df1
row_dups <- df1$id[a]
# the number of duplications in the dataset df1
cat('Number of duplications in the dataset df1:',length(row_dups))
# print all the ids having similar rows in the dataset df1
for(j in row_dups){
  for(i in 1:nrow(df1)){
    if(j != i){
      if(identical(df1[j,2:14], df1[i, 2:14])){
        cat('\n In the dataset df1, the id number',df1[j,1]$id, 'is having similar row to the id number',df1[i,1]$id)
      }
    }
  }
}
# for example, id 470 has similar row to id 7
df1[df1$id == 470,]
df1[df1$id == 7,]

# 1.4.2.2 regardless of the ids column, checking duplications in the dataset df2
b <- duplicated(df2[,2:13])
b # there is no duplication in the dataset df2

# 1.4.2.3 checking any overlap between df1 & df2 regardless of the ids columns 
# i.e. checking whether any new observations are from the past observations 
for(i in 1:nrow(df2)){
  for(j in 1:nrow(df1)){
    if(identical(df2[i,2:13], df1[j,2:13])){
      cat('\nThe id', df2[i,1]$id,'in dataset df2 is having similar row to the id', df1[j,1]$id, 'in dataset df1')
    }
  }
}
# for example, id 782 in dataset df2 is similar to the id 607 in dataset df1
df2[df2$id == 782,]
df1[df1$id == 607,]
# for example, id 783 in dataset df2 is similar to the id 603 in dataset df1
df2[df2$id == 783,]
df1[df1$id == 603,]
# checking whether these df1's ids are in duplicated rows which was checked in the step 1.4.2.1 above, i.e. row_dups
c(607,603) %in% row_dups 

# 1.4.3 removing duplications in dataset df1
df1 <- df1[-row_dups,]
str(df1)

# 1.5. checking categorical variables
# 1.5.1. factorise columns whose classes are characters
# df1
str(df1)
for(i in 2:ncol(df1)){
  if(is.character(df1[[i]])){
    df1[[i]] <- as.factor(df1[[i]])
  }
}
str(df1)
# df2
str(df2)
for(i in 2:ncol(df2)){
  if(is.character(df2[[i]])){
    df2[[i]] <- as.factor(df2[[i]])
  }
}
str(df2)

# 1.5.2. changing level's name from '0Balance' to 'No Balance' in the checking column
# df1
levels(df1$checking)[levels(df1$checking) == '0Balance'] <- 'No Balance'
levels(df1$checking)
# df2
levels(df2$checking)[levels(df2$checking) == '0Balance'] <- 'No Balance'
levels(df2$checking)

# 1.5.3 checking missing values for each categorical variable
for(i in 2:ncol(df1)){
  if(is.factor(df1[[i]])){
    cat('\n','This is the column', names(df1[i]), " - column's position",i, '\n')
    cat('No. of missing values', sum(is.na(df1[[i]])), '\n')
  }
}

# 1.5.4. investigating different values of each categorical variable
for(i in 2:ncol(df1)){
  if(is.factor(df1[[i]])){
    cat('\n', 'This is the column', names(df1[i]), " - column's position", i, '\n')
    cat('Number of factors -', length(levels(df1[[i]])), '\n')
    print(levels(df1[[i]]))
  }
}

# 1.6. checking numeric columns
# 1.6.1 checking missing values
for(i in 2:ncol(df1)){
  if(is.numeric(df1[[i]])){
    cat('\n','This is the column', names(df1[i]), " - column's position",i, '\n')
    cat('No. of missing values', sum(is.na(df1[[i]])), '\n')
  }
}

# 1.7 dealing with missing values
# instruction: https://cran.r-project.org/web/packages/mice/mice.pdf
# install.packages('mice')
library(mice)
# 1.7.1 visualising the missing-data matrix
md.pattern(df1)
# 1.7.2 imputing the missing data
impute <- mice(df1[,2:14], m=5, seed = 696)   # m: Number of multiple imputations
print(impute)   # for catogerical variables having missing values, i.e. employment, status & housing, multinomial logistic regression is applied
# printing imputed values which are grouped in 5 imputations
impute$imp$employment
impute$imp$status
impute$imp$housing
# complete data
df1 <- bind_cols(as.data.frame(df1$id), complete(impute, 1))
names(df1)[names(df1) == 'df1$id'] <- 'id'
str(df1)

# # 1.8 export csv files from dataframes df1 & df2 
# write.csv(df1, 'df1.csv', row.names = FALSE)
# write.csv(df2, 'df2.csv', row.names = FALSE)

##### a.EDA #####
# a.0. setting positions of numeric variables and categorical variables
nume_pos <- c(11,12,13)
cate_pos <- c(2,3,4,5,6,7,8,9,10,14)

# a.1 investigating correlations among variables 
# a.1.1 correlations among numeric variables
# a.1.1.1 create a dataframe having numeric variables
df1numeric <- df1[,nume_pos]
str(df1numeric)
# a.1.1.2 building correlation matrix among variables
cor.mat <- cor(df1numeric, use = 'complete.obs')
# a.1.1.3 building p-value matrix among variables
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df1numeric)
# a.1.1.4 plotting correlation matrix
# install.packages("corrplot")
# instruction: http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
library(corrplot)
col1 <- colorRampPalette(c("#083c5d",'white', "#d98310"))
corrplot(cor.mat, 
         method = 'ellipse',
         # choosing lower half of the corr. plot
         # type = 'lower',
         # add the correlation
         addCoef.col = 'black', number.cex = 0.7,
         # changing the axis's color
         tl.col="black", 
         tl.srt=0,
         # dropping the correlation between a variabe with itself
         diag=F,
         # grey coloring the cells with insignificant level of p-value being greater than 0.01 - NB. view full screen
         p.mat = p.mat, sig.level = 0.01, pch.col = 'grey91', pch.cex = 11, pch = 19,
         col = col1(100))

# a.1.2 correlations among categorical variables - chi-squared test
# install.packages('greybox')
# instruction: https://rdrr.io/cran/greybox/man/cramer.html
library(greybox)
# a.1.2.1 create a dataframe having categorical variables
df1cate <- df1[,cate_pos]
View(df1cate)
# a.1.2.2 create chi-square matrix & corresponding p-value matrix
chi_elements <- c()
pchi_elements <- c()
for(i in 1:length(df1cate)){
  for(j in 1:length(df1cate)){
    chi <- cramer(df1cate[[i]], df1cate[[j]], use = 'complete.obs')
    chi_elements <- c(chi_elements, chi$value)
    pchi_elements <- c(pchi_elements, chi$p.value)
  }
}
chi.mat <- matrix(chi_elements, 
                  nrow = length(df1cate), 
                  dimnames = list(names(df1cate), names(df1cate)))
pchi.mat <- matrix(pchi_elements, 
                   nrow = length(df1cate), 
                   dimnames = list(names(df1cate), names(df1cate)))
View(chi.mat)
View(pchi.mat)
# a.1.2.3 plotting chi-square matrix
col1 <- colorRampPalette(c("#083c5d",'white', "#d98310"))
corrplot(chi.mat, 
         method = 'ellipse',
         # choosing lower half of the corr. plot
         type = 'lower',
         # add the correlation
         addCoef.col = 'black', number.cex = 0.7,
         # changing the axis's color
         tl.col="black", 
         tl.srt=45,
         # dropping the correlation between a variabe with itself
         diag=F,
         # grey coloring the cells with insignificant level of p-value being greater than 0.01 - NB. view full screen
         p.mat = pchi.mat, sig.level = 0.01, pch.col = 'grey91', pch.cex = 7.6, pch = 19,
         col = col1(100))

# a.1.3 intraclass correlations between categorical variables vs numerical variables (ANOVA)
# a.1.3.1 create intraclass coefficient matrix and corresponding p-value matrix
# instruction: https://cran.r-project.org/web/packages/ICC/ICC.pdf
# install.packages('ICC')
library(ICC)
intra_elements <- c()
p.intra_elements <- c()
for(i in nume_pos){
  for(j in cate_pos){
    # create a vector of intraclass coefficients
    ano <- ICCest(df1[[j]], df1[[i]], alpha = 0.01)
    intra_elements <- c(intra_elements, ano$ICC)
    # create a vector of p-values
    anova_test <- aov(df1[[i]] ~ df1[[j]])
    p.intra <- summary(anova_test)[[1]][["Pr(>F)"]][1]
    p.intra_elements <- c(p.intra_elements, p.intra)
  }
}
# View(intra_elements)
# View(p.intra_elements)
intra.mat <- matrix(intra_elements,
                    nrow = length(nume_pos),
                    byrow = TRUE,
                    dimnames = list(names(df1numeric), names(df1cate)))
p.intra.mat <- matrix(p.intra_elements,
                      nrow = length(nume_pos), 
                      byrow = TRUE,
                      dimnames = list(names(df1numeric), names(df1cate)))
View(intra.mat)
View(p.intra.mat)

# a.2 visualisations 
# a.2.1 univariate visualisations
# a.2.1.1 barplots for categorical variables
for(i in 2:ncol(df1)){
  # choose the categorical variables only
  if(is.factor(df1[[i]])){
    # select columns having missing values
    if(any(is.na(df1[[i]]))){
      df <- as.data.frame(table(df1[[i]], useNA = 'always'))
      levels(df[[1]]) <- c(levels(df[[1]]), '"missing"')
      df[[1]][is.na(df[[1]])] <- '"missing"'
      bp <- barplot(df[[2]], 
                    names.arg=df[[1]], 
                    las = 1,
                    border = F, 
                    ylim = c(0,max(df[[2]]) + 100), 
                    main = names(df1[i]), 
                    col = '#083c5d',
                    cex.names = 1)
      text(bp, df[[2]] + 20, labels = df[[2]], cex=1, col = 'black') 
      # select columns not having missing values
    }else{
      df <- as.data.frame(table(df1[[i]]))
      bp <- barplot(df[[2]], 
                    names.arg=df[[1]], 
                    las = 1,
                    border = F, 
                    ylim = c(0,max(df[[2]]) + 100), 
                    main = names(df1[i]), 
                    col = '#083c5d',
                    cex.names = 1)
      text(bp, df[[2]] + 20, labels = df[[2]], cex=1, col = 'black') 
    }
  }
}
# a.2.1.2 histograms for numeric variables
par(mfrow=c(1,3))
for(i in 2:ncol(df1)){
  if(is.numeric(df1[[i]])){
    hist(df1[[i]], main = names(df1[i]), xlab = names(df1[i]), 
         col = '#083c5d',
         ylim = c(0, 350))
  }
}
par(mfrow=c(1,1))

# a.2.1.3 boxplots for numeric variables 
for(i in 2:ncol(df1)){
  if(is.numeric(df1[[i]])){
    plt <- ggplot(df1, aes(x=1, y=df1[[i]])) +
      geom_boxplot(fill = '#d98310', alpha = 1) +
      labs(y = names(df1[i]), title = paste(names(df1[i]), 'boxplot'))
    print(plt)
  }
}

# a.2.1.4 density plots for numeric variables 
for(i in 2:ncol(df1)){
  if(is.numeric(df1[[i]])){
    plt <- ggplot(df1, aes(x=df1[[i]])) +
      geom_density(color="black", fill="#d98310") +
      labs(x = names(df1[i]), title = paste(names(df1[i]), 'density plot'))
    print(plt)
  }
}

# a.2.2 bivariate visualisations

# a.2.3 multivariate visualisations

