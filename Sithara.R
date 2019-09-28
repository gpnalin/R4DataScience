#R Assignment#

#install packages
install.packages("tidyverse")
install.packages("psych")
install.packages("PerformanceAnalytics")
install.packages("ggplot2")
install.packages("rcompanion")
install.packages("gmodels")
install.packages("e1071")
install.packages("devtools")
install.packages("sparklyr")
install.packages("psych")
install.packages("caTools")
install.packages("arules")
install.packages("splitstackshape")
install.packages("e1071")
library(tidyverse)
library(RColorBrewer)
library(splitstackshape)
library(arules)
library(ggplot2)
library(caTools)
library(tidyverse)
library(sparklyr)
library(readxl)
library(psych)
library(data.table)
library(dplyr)
library(knitr)
library(stringr)
library(DT)
library(plotly)
library(arulesViz)
library(visNetwork)
library(igraph)
library(kableExtra)
library(PerformanceAnalytics)
library(rcompanion)
library(e1071)

getwd()
setwd("C:/Users/Nalin Perera/Documents/R4DataScience/")
getwd()


#load dataset into R envirenement

#Set/Configure R envirenment/ data description

#Load CSV files into R
tbl_ItemDetails_dataset <- read.csv("./Dataset/item.csv", header = TRUE, stringsAsFactors=FALSE)
tbl_promotion_dataset <- read.csv("./Dataset/promotion.csv", header = TRUE, stringsAsFactors=FALSE)
tbl_Sales_dataset <- read.csv("./Dataset/sales.csv", header = TRUE, stringsAsFactors=FALSE)
tbl_Supermarket_dataset <- read.csv("./Dataset/supermarkets.csv", header = TRUE, stringsAsFactors=FALSE)

#check the structure of the datasets
str(tbl_Supermarket_dataset)
str(tbl_ItemDetails_dataset)
str(tbl_Sales_dataset)
str(tbl_promotion_dataset)

#check first few records of the datsets
head(tbl_Supermarket_dataset)
head(tbl_ItemDetails_dataset)
head(tbl_Sales_dataset)
head(tbl_promotion_dataset)
tail(tbl_Supermarket_dataset)
view(tbl_Supermarket_dataset)

#check dataset quality

#check first few records of the datsets
summary(tbl_Supermarket_dataset)
summary(tbl_ItemDetails_dataset)
summary(tbl_Sales_dataset)
summary(tbl_promotion_dataset)

#data cleaning and prepare dataset for Analysis/ Preprocessing


#merge datasets

#Merge Items + sales + supermarket data frames
merged_sales_item <- merge(x = tbl_Sales_dataset, y = tbl_ItemDetails_dataset, by = "code", all.x = TRUE)
merged_sales_item_supermarkets <- merge(x = merged_sales_item, y = tbl_Supermarket_dataset,
                                        by.x = "supermarket", by.y = "supermarket_No", all.x = TRUE)


str(merged_sales_item_supermarkets)

summary(merged_sales_item_supermarkets)
is.na(merged_sales_item_supermarkets)

#remove NA values

#remove negative values
merged_sales_item_supermarkets <- merged_sales_item_supermarkets  %>% 
  filter(amount>0)


merged_sales_item_supermarkets[["typeID"]] <- as.numeric(factor(merged_sales_item_supermarkets$type,
                                                                levels = c('Type 1','Type 2','Type 3','Type 4'),
                                                                labels = c(1,2,3,4)))


#remove columns
merged_sales_item_supermarkets$descrption =NULL


#check plot - amount column
plot(merged_sales_item_supermarkets$amount)


summary(merged_sales_item_supermarkets)

par(mfrow=c(2, 3),oma=c(0,0,2,0))
boxplot(merged_sales_item_supermarkets$amount,main="Item Unit Price")
title("Outliers", outer=TRUE)

#get IQR value

#apply IQR function to buid the condition to remove outliers # 3rd Qu.:2.190  # Median :1.500
IQR_value = 2.19 + 1.5 * IQR(merged_sales_item_supermarkets$amount)
IQR_value


#Remove outliers
merged_sales_item_supermarkets$amount[merged_sales_item_supermarkets$amount > IQR_value] = IQR_value

par(mfrow=c(2, 3),oma=c(0,0,2,0))
boxplot(merged_sales_item_supermarkets$amount,main="Item Unit Price")
title("Outliers", outer=TRUE)


#Simple Linear Regression------------------------------------------------------------

#summerize the item wise sale into week wise sales for multiple reggression
summaried_sales_ItemType <- merged_sales_item_supermarkets   %>%
  group_by(typeID) %>%
  summarise(totalAmount = sum(amount*units, na.rm = TRUE))

# split data sets in to test and trainingset 
set.seed(123)
split=sample.split(summaried_sales_ItemType$typeID,SplitRatio = 2/3)

training_set =subset(summaried_sales_ItemType,split==TRUE)
test_set =subset(summaried_sales_ItemType,split==FALSE)

#graphical visualization of the relationships between columns
pairs(test_set) 
pairs.panels(summaried_sales_ItemType) 

#filling Simple Linear regression to the training data set
regr = lm(formula=totalAmount~typeID,data=test_set)

summary(regr)


#Visualising the training set results
ggplot()+
  geom_point(aes(x=test_set$typeID,y=test_set$totalAmount),
             colour='red')+
  geom_line(aes(x=test_set$typeID, y=predict(regr,newdata=test_set)),
            colour='blue')+ ggtitle("Total Sales vs Weeks(Testg set)")+
  xlab('Item Type') +
  ylab('Invoice values')


#Visualising the test set results
ggplot()+
  geom_point(aes(x=training_set $typeID,y=test_set$totalAmount),
             colour='red')+
  geom_line(aes(x=test_set$typeID, y=predict(regr,newdata=training_set)),
            colour='blue')+ ggtitle("Total Sales vs Weeks(Training set)")+
  xlab('Item Type') +
  ylab('Invoice values')
#End Simple Linear Regression--------------------------------------------------------------


#Multi Linear Regression------------------------------------------------------------

reg_invoice <- merged_sales_item_supermarkets   %>%
  group_by(province,supermarket,typeID) %>%
  summarise(InvoiceAmount = sum(amount*units, na.rm = TRUE))

# train the model
set.seed(123)
split=sample.split(reg_invoice$province,
                   SplitRatio = 2/3)
training_set =subset(reg_invoice,split==TRUE)

test_set =subset(reg_invoice,split==FALSE)

#apply the regression formula.
regMlt = lm(formula =InvoiceAmount~province+supermarket+typeID,
            data=training_set)

regMlt

#----------------------------------------------------------------------------------