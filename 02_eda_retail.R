
rm(list=ls()) 


library(tidyverse)
library(lubridate)
library(forcats)
library(sqldf)

retail_data <- read_csv("C:/Users/Vijay/Desktop/KPISOFT/retail_product.csv")

glimpse(retail_data)


country_mostsales <- sqldf("select Country, sum(income) as 'countrywise_totalrevenue' from retail_data group by Country ORDER BY income ASC")
  

by_top10_countries <- country_mostsales %>% 
  top_n(n = 10, wt = countrywise_totalrevenue)


by_top10_countries %>% 
  mutate(Country = fct_reorder(Country, countrywise_totalrevenue)) %>% 
  ggplot(aes(Country, countrywise_totalrevenue))+
  geom_point()+
  coord_flip()

# Most valued product-The product briging largest turnover

mostvalued_product <- sqldf("select Description, sum(income) as 'value_of_product' from retail_data group by Description ORDER BY income ASC")

top10_valued_products <- mostvalued_product %>% 
  top_n(n = 10, wt = value_of_product)


top10_valued_products %>% 
  mutate(Description = fct_reorder(Description, value_of_product)) %>% 
  ggplot(aes(Description, value_of_product))+
  geom_point()+
  coord_flip()

# which product is most sold worldwide?

mostsold_product <- sqldf("select Description, sum(Quantity) as 'total_units_sold' from retail_data group by Description ORDER BY Quantity ASC")


# top10 most sold products--
top10_mostsoldproducts <- mostsold_product %>% 
  top_n(n = 10, wt = total_units_sold)

top10_mostsoldproducts %>% 
  mutate(Description = fct_reorder(Description, total_units_sold)) %>% 
  ggplot(aes(Description, total_units_sold))+
  geom_point()+
  coord_flip()


# which customers are most valuable for the company?

by_mostvaluable_customer <- sqldf("select CustomerID, sum(income) as 'customer_amount' from retail_data group by CustomerID ORDER BY income ASC")

top10_mostvaluable_customer <- by_mostvaluable_customer %>% 
  top_n(n = 10, wt = customer_amount)
View(top10_mostvaluable_customer)



