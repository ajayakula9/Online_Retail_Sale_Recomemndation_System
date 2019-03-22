
### remove variables in the environment
rm(list=ls()) 
install.packages("tidyverse")
installed.packages("lubridate")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(plyr)

#Read the csv file
retail_dataimpor <- read_csv("C:/Users/Vijay/Desktop/KPISOFT/Online_retail_Data.csv",
                   col_types = cols(
                     InvoiceNo = col_character(),
                     StockCode = col_character(),
                     Description = col_character(),
                     Quantity = col_integer(),
                     InvoiceDate = col_datetime("%m/%d/%Y %H:%M"),
                     UnitPrice = col_double(),
                     CustomerID = col_integer(),
                     Country = col_character()
                   )) %>%
  mutate(day = parse_date(format(InvoiceDate, "%Y-%m-%d")),
         day_of_week = wday(day, label = TRUE),
         time = parse_time(format(InvoiceDate, "%H:%M")),
         month = format(InvoiceDate, "%m"),
         income = Quantity * UnitPrice,
         income_return = ifelse(Quantity > 0, "income", "return"))


#creating a fresh copy of the data to work 

retail <- retail_dataimpor
glimpse(retail)

# outlier detection

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}

outlierKD(retail, UnitPrice)
outlierKD(retail, Quantity)
outlierKD(retail, income)

summary(retail)

#How many missing value are present corresponding to each column
retail %>% 
  map(., ~sum(is.na(.)))

#We ignore the entire row if row has missing value
retail <- retail[complete.cases(retail), ]

retail %>% 
  map(., ~sum(is.na(.)))


retail_cleaned <- retail %>%
  mutate(Description = factor(Description, levels = unique(Description))) %>% 
  mutate(Country = factor(Country, levels = unique(Country)))%>%
  mutate(InvoiceNo = factor(InvoiceNo, levels = unique(InvoiceNo))) 

glimpse(retail_cleaned)

write.csv(retail_cleaned, file = "C:/Users/Vijay/Desktop/KPISOFT/retail_cleaned.csv")

retail_pid <- transform(retail_cleaned, ProductID = match(retail_cleaned$'Description', unique(retail_cleaned$'Description')))
all.equal(retail_pid,retail_pid)  ## Only first 8 Column Names are left uncoded.

write.csv(retail_pid, file = "C:/Users/Vijay/Desktop/KPISOFT/retail_product.csv")

getTotalQuantity <- function(pid, npar = FALSE, print = FALSE)
{
  totalQuantity <- sum(subset(retail_pid, ProductID == pid, select = c(Quantity)))
  return(totalQuantity)
}

getTotalCustomers <- function(pid, npar = FALSE, print = FALSE)
{
  return(length(unique(retail_pid$CustomerID[retail_pid$ProductID==pid])))
}

getMeanQuantityPerTransaction <- function(pid, npar = FALSE, print = FALSE)
{
  
  meanQuantity <- mean(subset(retail_pid, ProductID == pid, select = c(Quantity))$Quantity)
  return(meanQuantity)
}

##products range

range(retail_pid$ProductID)

productstatdf <- count(subset(retail_pid, ProductID = 1:3561, select = c(ProductID, Description, UnitPrice)), c('ProductID', 'Description'))
colnames(productstatdf)[3] <- "TransactionFreq"

totQuantity <- c()
totCustomers <- c()
meanQuantityPerTransaction <- c()

for (netproductcount in 1:3561)
{
  totQuantity[netproductcount] <- getTotalQuantity(netproductcount)
  totCustomers[netproductcount] <- getTotalCustomers(netproductcount)
  meanQuantityPerTransaction[netproductcount] <- getMeanQuantityPerTransaction(netproductcount)
}

productstatdf$"TotalQuantity" <- totQuantity
productstatdf$"Customers" <- totCustomers
productstatdf$"MeanQuantityPerTransaction" <- meanQuantityPerTransaction


meanQuantityPerCustomer <- c()
for (netproductcount in 1:3561)
{
  meanQuantityPerCustomer[netproductcount] <- productstatdf$"TotalQuantity"[productstatdf$ProductID==netproductcount]/productstatdf$"Customers"[productstatdf$ProductID==netproductcount]
}
productstatdf$"MeanQuantityPerCustomer" <- meanQuantityPerCustomer
rm(totCustomers, totQuantity, meanQuantityPerCustomer, meanQuantityPerTransaction)


plotProductsInitial <- function(){ 
  
  head(productstatdf)

  for (n in c(1,3,4,5,6,7))
  {
    hist(productstatdf[,n], xlab = colnames(productstatdf)[n], xlim= c(100,500), breaks = 100)
    hist(productstatdf[,n], xlab = colnames(productstatdf)[n])
  }
  
  hist(productstatdf[,3], xlab = colnames(productstatdf)[3], breaks=100, xlim = c(0,2015), ylab = "No. of Different Products")
  max(productstatdf[,3])
  
  hist(productstatdf[,4], xlab = colnames(productstatdf)[4], breaks=100, xlim = c(0,81000), ylab = "No. of Different Products")
  max(productstatdf[,4])
  
}




###Splitting Dataset into High Frequency, Medium Frequency and Low Frequency Transactions
orderedallproducts <- productstatdf[with(productstatdf, order(-TransactionFreq, TotalQuantity, ProductID)),]


orderdProductsStats <- function(orderedallproducts){
  head(orderedallproducts)
  tail(orderedallproducts)
  max(orderedallproducts$ProductID)
  summary(orderedallproducts$TransactionFreq)
}
orderdProductsStats(orderedallproducts)
transfreqsummary <- summary(orderedallproducts$TransactionFreq)
lowfreqproducts <- subset(orderedallproducts, (ProductID = 1:3561)&(TransactionFreq < transfreqsummary[4]), select = c(ProductID, Description, TransactionFreq, TotalQuantity, Customers, MeanQuantityPerTransaction, MeanQuantityPerCustomer))
midfreqproducts <- subset(orderedallproducts, (ProductID = 1:3561)&(TransactionFreq > transfreqsummary[4])&(TransactionFreq < transfreqsummary[5]), select = c(ProductID, Description, TransactionFreq, TotalQuantity, Customers, MeanQuantityPerTransaction, MeanQuantityPerCustomer))
highfreqproducts <- subset(orderedallproducts, (ProductID = 1:3561)&(TransactionFreq >= transfreqsummary[5]), select = c(ProductID, Description, TransactionFreq, TotalQuantity, Customers, MeanQuantityPerTransaction, MeanQuantityPerCustomer))
#UPDATED
finaldataset <- orderedallproducts


#Reattach UnitPrice of each product
upricein <- c()
earningpertransactions <- c()
index <- 0
for (currentid in finaldataset$ProductID) #3877 Total Products
{
  index = index + 1
  upricein[index] <- retail_pid$UnitPrice[retail_pid$ProductID==currentid]
  earningpertransactions[index] <- retail_pid$UnitPrice[retail_pid$ProductID==currentid] * finaldataset$MeanQuantityPerTransaction[finaldataset$ProductID==currentid]
}
rm(index)
finaldataset$"UnitPrice" <- upricein
finaldataset$"MeanEarningPerTransaction" <- earningpertransactions
rm(upricein, earningpertransactions)

write.csv(orderedallproducts, "C:/Users/Vijay/Desktop/KPISOFT/orderedallproductsNEW.csv")
write.csv(finaldataset, "C:/Users/Vijay/Desktop/KPISOFT/finaldatasetNEW.csv")