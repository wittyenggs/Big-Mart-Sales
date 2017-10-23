library('caret')

#Load Datasets
train <- read.csv('Train.csv',sep = ',',header = TRUE,na.strings = c('',' '))
test <- read.csv('Test.csv',sep = ',',header = TRUE, na.strings = c('', ' '))

#Define target variable name
target <- "Item_Outlet_Sales"

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
  print (paste(col_name,lower,upper))
  
  train_modified[,col_name][(train_modified[,col_name] > upper) | (train_modified[,col_name] < lower)] <-
    mean(train_modified[,col_name][(train_modified[,col_name] <= upper) & 
                            (train_modified[,col_name] >= lower)])
} 

for (col_name in numerical_columns){
  
  box <- boxplot(train_modified[,col_name],xlab = col_name)
  stats <- list(col_name,box$stats[1],box$stats[5])
  stats_whisker <- append(stats_whisker,list(stats))
}