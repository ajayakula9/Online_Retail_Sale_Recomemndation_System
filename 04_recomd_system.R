### remove variables in the environment
rm(list=ls())

library(methods)
library(recommenderlab)
library(data.table)
library(ggplot2)
library(knitr)

## load the data from the excel

df_data <- fread('C:/Users/Vijay/Desktop/KPISOFT/retail_product.csv')
df_data[ ,InvoiceDate := as.Date(InvoiceDate)]

setkeyv(df_data, c('StockCode', 'Description'))
itemCode <- unique(df_data[, c('StockCode', 'Description')])
setkeyv(df_data, NULL)

df_train_ori <- dcast(df_data, CustomerID ~ StockCode, value.var = 'Quantity',fun.aggregate = sum, fill=0)

CustomerId <- df_train_ori[,1] #!

df_train_ori <- df_train_ori[,-c(1,3390:3394)]

#Fill NA with 0
for (i in names(df_train_ori))
  df_train_ori[is.na(get(i)), (i):=0]

df_train <- as.matrix(df_train_ori)
df_train <- df_train[rowSums(df_train) > 5,colSums(df_train) > 5] 
df_train <- binarize(as(df_train, "realRatingMatrix"), minRatin = 1)

## Split the data into training set and test set

which_train <- sample(x = c(TRUE, FALSE), size = nrow(df_train),replace = TRUE, prob = c(0.8, 0.2))
y <- df_train[!which_train]
x <- df_train[which_train]

recommender_models <- recommenderRegistry$get_entries(dataType ="binaryRatingMatrix")
recommender_models$IBCF_binaryRatingMatrix$parameters

## Item based collborative filtering

method <- 'IBCF'
parameter <- list(method = 'Jaccard')
n_recommended <- 5
n_training <- 3269

# built the model using the IBCF parameters
recc_model <- Recommender(data = x, method = method, parameter = parameter)
model_details <- getModel(recc_model)

# test the model with test data
recc_predicted <-predict(object = recc_model, newdata=y,n = n_recommended, type="topNList")


as(recc_predicted,"list")[1:5]

user_1 <- CustomerId[as.integer(names(recc_predicted@items[200]))]

Item_rec <- recc_predicted@items[[200]]
Item_rec <- rownames(model_details$sim)[Item_rec]
itemCode[Item_rec]
user_1_buy <- df_data[CustomerID== 13694, sum(Quantity), by=StockCode]
merge(itemCode,user_1_buy, by='StockCode')
