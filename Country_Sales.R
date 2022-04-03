#### Country Sales


knitr::opts_chunk$set(echo = TRUE, warn.conflicts = FALSE)

library(tidyverse)
library(rsample)
library(broom)
library(yardstick)
library(rpart)
library(randomForest)
library(parsnip)
library(data.table)
library(kableExtra)



sales <- read_csv("C:/Data/1500000 Sales Records.csv",show_col_types = FALSE)





df<-sales  %>% 
  
  filter(Country == c("Canada","Ukraine","Luxembourg","New Zealand","United Kingdom","Vietnam","Norway","Australia","Singapore","Cuba")) %>%
  
  select("Order Date","Item Type","Units Sold","Unit Price","Total Profit","Country") %>%
  
  arrange(desc("Order Date"))


head(df,20)




data<-cbind(df,"Year" = substr(df$`Order Date`,nchar(df$`Order Date`)-3,nchar(df$`Order Date`)))
data$Revenue <-  data$`Units Sold`  * data$`Unit Price`
data$Unit_Cost <- (data$Revenue - data$`Total Profit`) / data$`Units Sold`
data$Cost_Rank <- min_rank(data$Unit_Cost)




skimr::skim(data)


data<-data[complete.cases(data), ]
data



data<- data.table(data[, c("Order Date","Item Type","Units Sold","Unit Price","Total Profit","Country","Year","Revenue","Unit_Cost","Cost_Rank")] )
data<-data %>%
  arrange(desc(Year)) %>%
  mutate(Count = n())


str(data)


kable(data)  %>%
  kable_styling(full_width = TRUE, position = "float_right")


skimr::skim(data)


str(data)



write.csv(data,"Country_Sales.csv",row.names=FALSE)





