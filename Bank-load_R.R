# %% [code] {"_execution_state":"idle"}
# This R environment comes with many helpful analytics packages installed
# It is defined by the kaggle/rstats Docker image: https://github.com/kaggle/docker-rstats
# For example, here's a helpful package to load

library(tidyverse) # metapackage of all tidyverse packages

# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory



# You can write up to 20GB to the current directory (/kaggle/working/) that gets preserved as output when you create a version using "Save & Run All" 
# You can also write temporary files to /kaggle/temp/, but they won't be saved outside of the current session

# %% [code]
list.files(path = "../input")

# %% [code]
# Load the data into the variable
bank_data = read.csv("f://bank-loan.csv")

# %% [code]
#check first few entries of the data
head(bank_data, 10)

# %% [code]
#check the structure of the data frame
str(bank_data)

# %% [code]
#check the summary
summary(bank_data)

# %% [code]
#check attributes
min(bank_data$othdebt)
max(bank_data$othdebt)
max(bank_data$creddebt)
min(bank_data$creddebt)
min(bank_data$address)

# %% [code]
# Missing Values
miss_val <- data.frame(apply(bank_data,2,function(x){sum(is.na(x))}))
names(miss_val)[1]='missing_val'
miss_val

# %% [code]
#check unique values
unique(bank_data$default)

# %% [code]
#Count
count(bank_data, default)

# %% [code]
#replace missing values with 0
bank_data[is.na(bank_data)] <- 0

# %% [code]
#Check last few entries of the data
tail(bank_data,10)

# %% [code]
#Save column names
cnames = c("age", "ed", "employ", "address", "income", "debtinc", "creddebt", "othdebt")

# %% [code]
cnames

# %% [code]
#Outlier analysis box plot
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "default"), data = subset(bank_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of count for",cnames[i])))
}

# %% [code]
gridExtra::grid.arrange(gn1,gn2,ncol=3)

# %% [code]
gridExtra::grid.arrange(gn3,gn4,ncol=3)

# %% [code]
gridExtra::grid.arrange(gn5,gn6,gn7,ncol=2)

# %% [code]
#Normaility check
qqnorm(bank_data$age)
hist(bank_data$income)

# %% [code]
# Normalization of the data
for(i in cnames){
  print(i)
  bank_data[,i] = (bank_data[,i] - min(bank_data[,i]))/(max(bank_data[,i] - min(bank_data[,i])))
}

# %% [code]
head(bank_data,10)

# %% [code]
# To remove all the outliers
for(i in cnames){
  print(i)
  val = bank_data[,i][bank_data[,i] %in% boxplot.stats(bank_data[,i])$out]
  #print(length(val))
  bank_data = bank_data[which(!bank_data[,i] %in% val),]
}


# %% [code]
#dimensions of the bank _data
dim(bank_data)

# %% [code]
# Standardisation of the data
for(i in cnames){
  print(i)
  bank_data[,i] = (bank_data[,i] - mean(bank_data[,i]))/sd(bank_data[,i])
}

# %% [code]
head(bank_data,10)

# %% [code]
## Correlation Plot 
library(corrgram)
corrgram(bank_data[cnames], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

# %% [code]
factor(bank_data$default, levels = c(0,1))
library(rpart)
library(MASS)

#divide the data into train and test data.
train_index = sample(1:nrow(bank_data), 0.7 * nrow(bank_data))
train = bank_data[train_index,]
test = bank_data[-train_index,]

# %% [code]
dim(train)

# %% [code]
#Logistic Regression
logit_model = glm(default ~ ., data = train, family = "binomial")

# %% [code]
summary(logit_model)

# %% [code]
#predictions using the model
logit_Predictions = predict(logit_model, newdata = test, type = "response")


# %% [code]
logit_Predictions

# %% [code]
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)

# %% [code]
summary(logit_Predictions)

# %% [code]
# Confusion Matrix
ConfMatrix_RF = table(test$default, logit_Predictions)

# %% [code]
ConfMatrix_RF

# %% [code]
#accuracy = 82.3 %
#FNR = 77
accuracy = (146+8)/(146+6+8+27)
print(accuracy)
FNR = 27/(27+8)
print(FNR)

# %% [code]
#plotting roc curve
library(pROC)
ROC <- roc(test$default, logit_Predictions)

# %% [code]
plot(ROC, col = 'blue')

# %% [code]
#ROCscore
auc(ROC)

# %% [code]
##Decision tree for classification
#Develop Model on training data
library(rpart)
loan_model <- rpart(default ~ ., data = train, method = "class", control = rpart.control(cp = 0))


# %% [code]
loan_model

# %% [code]
loan_rpart_pred = predict(loan_model, test, type = "class")


# %% [code]
library(rpart.plot)
rpart.plot(loan_model, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)

# %% [code]
ConfMatrix_rpart = table(test$default, loan_rpart_pred)
#confusionMatrix(ConfMatrix_rpart)

# %% [code]
ConfMatrix_rpart

# %% [code]
#accuracy = 80.7 %
#FNR = 57.14 %
accuracy_1 = (136+15)/(136+16+15+20)
print(accuracy_1)
FNR_1 = 20/(20+15)
print(FNR_1)

# %% [code]


# %% [code]
