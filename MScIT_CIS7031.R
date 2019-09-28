######### Setup and Configure the environment #########

# Set working directory
setwd("C:/Users/Nalin Perera/Documents/R4DataScience/")

# installing the necessary packages
install.packages("readr")
install.packages("caret")
install.packages("janitor")
install.packages("sjmisc")
install.packages("skimr")
install.packages("tidyverse")
install.packages("vtreat")
install.packages("dplyr")

# loading the libraries
library(readr)
library(caret)
library(janitor)
library(sjmisc)
library(skimr)
library(tidyverse)
library(vtreat)
library(dplyr)


###### collecting data ######

# loading datasets into R environment
df_item <- read_csv(file = "Dataset/item.csv")
df_sales <- read_csv(file = "Dataset/sales.csv")
df_promotion <- read_csv(file = "Dataset/promotion.csv")
df_supermarkets <- read_csv(file = "Dataset/supermarkets.csv")

#### Explore datasets ####

# View structure of datasets
glimpse(df_item) 
glimpse(df_sales) 
glimpse(df_promotion) 
glimpse(df_supermarkets)

# summarize datasets
df_item %>%
  sjmisc::descr() -> descr_stats_item
print(descr_stats_item)

df_sales %>%
  sjmisc::descr() -> descr_stats_sales
print(descr_stats_sales)

df_promotion %>%
  sjmisc::descr() -> descr_stats_promotion
print(descr_stats_promotion)

df_supermarkets %>%
  sjmisc::descr() -> descr_stats_supermarkets
print(descr_stats_supermarkets)


# check for duplicate observations (indicate either TRUE or FALSE where TRUE is a duplicate)
duplicates_item <- duplicated(df_item)
duplicates_sales <- duplicated(df_sales)
duplicates_promotion <- duplicated(df_promotion)
duplicates_supermarkets <- duplicated(df_supermarkets)

# SHow's the count of duplicate observations
table(duplicates_item)
table(duplicates_sales)
table(duplicates_promotion)
table(duplicates_supermarkets)

# show the observations with duplicate data
which(duplicates_item == "TRUE")
which(duplicates_sales == "TRUE")
which(duplicates_promotion == "TRUE")
which(duplicates_supermarkets == "TRUE")

# clean duplicate
df_cd_item <- dplyr::distinct(df_item, .keep_all = TRUE)
df_cd_sales <- dplyr::distinct(df_sales, .keep_all = TRUE)
df_cd_promotion <- dplyr::distinct(df_promotion, .keep_all = TRUE)
df_cd_supermarkets <-
  dplyr::distinct(df_supermarkets, .keep_all = TRUE)

# review dataset statistics
df_cd_item %>%
  sjmisc::descr() -> descr_stats_item
df_cd_sales %>%
  sjmisc::descr() -> descr_stats_sales
df_cd_promotion %>%
  sjmisc::descr() -> descr_stats_promotion
df_cd_supermarkets %>%
  sjmisc::descr() -> descr_stats_supermarkets

#Merge Items + sales + supermarket data frames
merged_sales_item <- merge(x = df_cd_sales, y = df_cd_item, by = "code", all.x = TRUE)
merged_sales_item_supermarkets <- merge(x = merged_sales_item, y = df_cd_supermarkets,
                                        by.x = "supermarket", by.y = "supermarket_No", all.x = TRUE)


###### review sales.csv ###### 

# load the data file
sales <- read_csv(file = "Dataset/sales.csv")

# check the sales column names
colnames(sales)

# check dimensions to understand sales data (Row and columns)
dim(sales)

# check for duplicate observations (indicate either TRUE or FALSE where TRUE is a duplicate)
sales_duplicates <- duplicated(sales)

# SHow's the count of duplicate observations
table(sales_duplicates)

# show the observations with duplicate data
which(sales_duplicates == "TRUE")

# clean duplicate
sales <- dplyr::distinct(sales, .keep_all = TRUE)

# review dataset statistics
sales %>%
  sjmisc::descr() -> sales_descr_stats

# write the statistic description
readr::write_csv(sales_descr_stats, 'reports/sales_descr_stats.csv')
view(sales_descr_stats)

sales_cat <-
  sales[, sapply(sales, class) == 'character']


sales_cat %>%
  dplyr::summarise_all(dplyr::funs(dplyr::n_distinct(.)))
sales_cat

sales_na_count <-
  sapply(sales, function(y)
    sum(length(which(is.na(
      y
    )))))

sales_na_df <- data.frame(sales_na_count)

View(sales_na_df)

sales_feature_variance <- caret::nearZeroVar(sales, saveMetrics = TRUE)
head(sales_feature_variance)
which(sales_feature_variance$zeroVar == 'TRUE')
which(sales_feature_variance$nzv == 'TRUE')
row.names(sales_feature_variance[11, ])




###### review item.csv ######

# load the data file
item <- read_csv(file = "Dataset/item.csv")

# check the sales column names
colnames(item)

# check dimensions to understand sales data (Row and columns)
dim(item)

# check for duplicate observations (indicate either TRUE or FALSE where TRUE is a duplicate)
item_duplicates <- duplicated(item)

# SHow's the count of duplicate observations
table(item_duplicates)

# show the observations with duplicate data
which(item_duplicates == "TRUE")

# clean duplicate
item <- dplyr::distinct(item, .keep_all = TRUE)

item %>%
  sjmisc::descr() -> item_descr_stats

readr::write_csv(item_descr_stats, 'reports/item_descr_stats.csv')

item_cat <-
  item[, sapply(item, class) == 'character']

item_cat %>%
  dplyr::summarise_all(dplyr::funs(dplyr::n_distinct(.)))

# type
item_cat %>% 
  dplyr::group_by(type) %>%
  dplyr::summarize(num_rows = n()) -> item_count_by_type
readr::write_csv(item_count_by_type, 'reports/item_count_by_type.csv')

# brand
item_cat %>% 
  dplyr::group_by(brand) %>%
  dplyr::summarize(num_rows = n()) -> item_count_by_brand
readr::write_csv(item_count_by_brand, 'reports/item_count_by_brand.csv')

item_na_count <-
  sapply(item, function(y)
    sum(length(which(is.na(
      y
    )))))

item_na_df <- data.frame(item_na_count)

View(item_na_df)


###### review supermarkets.csv ######

# load the data file
supermarkets <- read_csv(file = "Dataset/supermarkets.csv")

# check the sales column names
colnames(supermarkets)

# check dimensions to understand sales data (Row and columns)
dim(supermarkets)

# check for duplicate observations (indicate either TRUE or FALSE where TRUE is a duplicate)
supermarkets_duplicates <- duplicated(supermarkets)

# SHow's the count of duplicate observations
table(supermarkets_duplicates)

# show the observations with duplicate data
which(supermarkets_duplicates == "TRUE")

# clean duplicate
supermarkets <- dplyr::distinct(supermarkets, .keep_all = TRUE)

supermarkets %>%
  sjmisc::descr() -> supermarkets_descr_stats

readr::write_csv(supermarkets_descr_stats, 'reports/supermarkets_descr_stats.csv')


###### review promotion.csv ######

# load the data file
promotion <- read_csv(file = "Dataset/promotion.csv")

# check the sales column names
colnames(promotion)

# check dimensions to understand promotion data (Row and columns)
dim(promotion)

# check for duplicate observations (indicate either TRUE or FALSE where TRUE is a duplicate)
promotion_duplicates <- duplicated(promotion)

# SHow's the count of duplicate observations
table(promotion_duplicates)

# show the observations with duplicate data
which(promotion_duplicates == "TRUE")

# clean duplicate
promotion <- dplyr::distinct(promotion, .keep_all = TRUE)

promotion %>%
  sjmisc::descr() -> promotion_descr_stats

readr::write_csv(promotion_descr_stats, 'reports/promotion_descr_stats.csv')