####marketbasket analysis to analyse which product is more close to be sold with which product

### remove variables in the environment

rm(list=ls()) 
library(arules)
library(arulesViz)
library(tidyverse)
library(plyr)

# Load the data and devide InvoiceNo, Description into transaction retail data frame

retail_mba <- read_csv('C:/Users/Vijay/Desktop/KPISOFT/retail_product.csv')
transaction_retail <-  select(retail_mba, 'InvoiceNo', 'Description')


#How many unique levels of InvoiceNo and Description of the product are there
str(transaction_retail)
#save transaction id and commodities in one file for future reference
write.csv(transaction_retail,'C:/Users/Vijay/Desktop/KPISOFT/transaction_retail.csv', row.names = FALSE)

#creating a itemList from the Description column of the data.
#for each InvoiceNo,description of all the products brought together are written together

itemList <- plyr :: ddply(transaction_retail, c("InvoiceNo"), 
                  function(transaction_retail)paste(transaction_retail$Description, 
                                     collapse = ","))
#itemList
#deleting the InvoiceNO from the itemList data as this is not required anymore

itemList$InvoiceNo <- NULL

#Write out the itemlist per transaction in a csv file
write.csv(itemList,'C:/Users/Vijay/Desktop/KPISOFT/market_basket_tr.csv', row.names = FALSE)

#Read the csv in 'basket' format
#rm.duplicates removes duplicate items in a particular transaction.
transaction <- read.transactions('C:/Users/Vijay/Desktop/KPISOFT/market_basket_tr.csv', format = 'basket', quote = "", cols = NULL, sep=',', skip = 1, rm.duplicates = T)
transaction
summary(transaction)

# Make a frequency plot of the transactions with a support of 0.05 or greater.
# This shows the the most popular gift items sold.
itemFrequencyPlot(transaction, support = .05, col = rainbow(4))


# create association rules with a minimum support value ,where support indicates appearance of 
#commodity A and B together out of total transactions of all items.
rules <- apriori(transaction, parameter = list(supp = 0.01, conf = 0.5, minlen = 2))
options(digits=2)
top10rules <-rules[1:10]
inspect(top10rules)

plot(top10rules, method = "graph", engine = 'interactive')

#if A => B is the rule, confidence shows the proportion of transactions having both A and B,
#out of total transactions having A.

#sort the rules by decreasing confidence and show top 10 rules
rules_by_confidence <- sort(rules, by ='confidence', decreasing = TRUE)
summary(rules_by_confidence)
toprules_by_confidence <- rules_by_confidence[1:10]
options(digits=2)
inspect(toprules_by_confidence)

plot(toprules_by_confidence, method="graph",engine = 'interactive',shading = NA)

 #Lift is the factor by which, the co-occurence of A and B exceeds the expected
 #probability of A and B co-occuring, had they been independent. So, higher the
 #lift, higher the chance of A and B occurring together.
 # sort the rules by decreasing lift and show top 10 rules

rules_by_lift <- sort(rules, by='lift', decreasing = TRUE)
summary(rules_by_lift)
toprules_by_lift <- rules_by_lift[1:10]
options(digits=2)
inspect(toprules_by_lift)

plot(toprules_by_lift, method="graph",engine = 'interactive',shading = NA)

#Since WHITE HANGING HEART T-LIGHT HOLDER  is the most popular item, we are
#interested in the items bought with it.
rules_lhs_white_hanging_heart_t_shirt_holder<-apriori(data=transaction, parameter=list(supp=0.001,conf = 0.1, minlen = 2), 
                                  appearance = list(default="rhs",lhs="WHITE HANGING HEART T-LIGHT HOLDER"),
                                  control = list(verbose=F))
rules_lhs_white_hanging_heart_t_shirt_holder <- sort(rules_lhs_white_hanging_heart_t_shirt_holder, decreasing=TRUE,by="confidence")
inspect(rules_lhs_white_hanging_heart_t_shirt_holder)
gifts_with_tshirtholder <- rules_lhs_white_hanging_heart_t_shirt_holder[1:10]

plot(gifts_with_tshirtholder, method="graph",engine = 'interactive',shading = NA)


#Since ASSORTED COLOUR BIRD ORNAMENT  is the most popular item, we are
#interested in the items bought with it.
rules_lhs_ASSORTED_COLOUR_BIRD_ORNAMENT<-apriori(data=transaction, parameter=list(supp=0.001,conf = 0.1, minlen = 2), 
                                                      appearance = list(default="rhs",lhs="ASSORTED COLOUR BIRD ORNAMENT"),
                                                      control = list(verbose=F))
rules_lhs_ASSORTED_COLOUR_BIRD_ORNAMENT <- sort(rules_lhs_ASSORTED_COLOUR_BIRD_ORNAMENT, decreasing=TRUE,by="confidence")
inspect(rules_lhs_ASSORTED_COLOUR_BIRD_ORNAMENT)
gifts_with_tshirtholder <- rules_lhs_ASSORTED_COLOUR_BIRD_ORNAMENT[1:3]

plot(gifts_with_tshirtholder, method="graph",engine = 'interactive',shading = NA)


