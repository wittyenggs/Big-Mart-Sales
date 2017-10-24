library('caret')
library('Boruta')
library('randomForest')

#Load Datasets
train <- read.csv('Train.csv',sep = ',',header = TRUE,na.strings = c('',' '))
test <- read.csv('Test.csv',sep = ',',header = TRUE, na.strings = c('', ' '))

#Define target variable name
target <- "Item_Outlet_Sales"

#Exclude Item Identifier variable
train <- train[, -1]
test <- test[,-1]

#Save target variable values
Sales <- train[,target]

#Combine train and test datasets
data <- rbind(train[,-ncol(train)],test)

#Data Cleansing
data$Item_Fat_Content[data$Item_Fat_Content == 'LF'] <- 'Low Fat'
data$Item_Fat_Content[data$Item_Fat_Content == 'low fat'] <- 'Low Fat'
data$Item_Fat_Content[data$Item_Fat_Content == 'reg'] <- 'Regular'
data$Item_Fat_Content <- factor(data$Item_Fat_Content)

#Conversion into factor variable
data$Outlet_Establishment_Year <- as.factor(data$Outlet_Establishment_Year)


categorical_columns <- names(Filter(is.factor,data)) 
numerical_columns <- names(Filter(is.numeric,data))

#Converting NA's in Categorical variable to unknown level
for (col_name in categorical_columns){
  a <- as.character(data[,col_name])
  a[is.na(a)] <- 'Unknown'  
  data[,col_name] <- as.factor(a)
  
}

train_modified <- data[1:nrow(train),]
test_modified <- data[(nrow(train) + 1):nrow(data),]

for (col_name in numerical_columns){
  train_modified[,col_name][is.na(train_modified[,col_name])] <- mean(train_modified[,col_name][!is.na(train_modified[,col_name])])
  test_modified[,col_name][is.na(test_modified[,col_name])] <- mean(test_modified[,col_name][!is.na(test_modified[,col_name])])
}

stats_whisker <- list()
for (col_name in numerical_columns){

  box <- boxplot(train_modified[,col_name],xlab = col_name)
  stats <- list(col_name,box$stats[1],box$stats[5])
  stats_whisker <- append(stats_whisker,list(stats))
}

for (i in 1:length(stats_whisker)){
  col_name <- stats_whisker[[i]][[1]]
  lower <- stats_whisker[[i]][[2]]
  upper <- stats_whisker[[i]][[3]]
  
  train_modified[,col_name][(train_modified[,col_name] > upper) | (train_modified[,col_name] < lower)] <-
    mean(train_modified[,col_name][(train_modified[,col_name] <= upper) & 
                            (train_modified[,col_name] >= lower)])
} 

#train_modified <- cbind(train_modified, Sales)

train_modified <- predict(preProcess(train_modified,method = c('range')),train_modified)
#boruta_model <- Boruta(Sales ~., train_modified)
#final.model <- TentativeRoughFix(boruta_model)
#variables <- getSelectedAttributes(final.model, withTentative = F)

#control <- rfeControl(functions=rfFuncs, method="cv", number=10)
#rfe.train <- rfe(train_modified[,2:11], train_modified[,12], sizes=1:12, rfeControl=control)


dummy_train <- dummyVars(~., data = train_modified, fullRank = T)
train_dummy <- data.frame(predict(dummy_train,train_modified))

#col_names <- sapply(train_dummy, function(col) length(unique(col)) < 40)
#train_dummy[ , col_names] <- lapply(train_dummy[ , col_names] , factor)


train_control <- trainControl(method = "repeatedcv",
                              number = 10,repeats = 10)

# model_lm <- train(Sales ~ ., data = train_dummy, method = "lm", 
#                  trControl = train_control,verbose = TRUE)
 
#model_rf <- model_lm <- train(Sales ~ ., data = train_dummy, method = "rf",
#                              trControl = train_control,verbose = TRUE)

pca.train <- train_dummy[1:nrow(train_dummy),]
pca.test <- train_dummy[-(1:nrow(train_dummy)),]

prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)