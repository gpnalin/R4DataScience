# install easypackages for 
install.packages("easypackages")
library("easypackages")

# Loading necessary packages
packages("readr", "dplyr", "ggplot2", "forecast","lubridate","tm","measurements","units")


# Load the datasets
item <- read.csv("./Dataset/item.csv", header = TRUE, stringsAsFactors=FALSE)
promotion <- read.csv("./Dataset/promotion.csv", header = TRUE, stringsAsFactors=FALSE)
sales <- read.csv("./Dataset/sales.csv", header = TRUE, stringsAsFactors=FALSE)
supermarkets <- read.csv("./Dataset/supermarkets.csv", header = TRUE, stringsAsFactors=FALSE)

# View its class
class(item) 

# View its dimensions (Rows|Columns)
dim(item) 

# Look at column names 
names(item)

# View structure of item
glimpse(item) 

# View a summary
summary(item) 

# View histogram
unique(item$brand)
hist(item$type)

sales_item <- merge(x = sales, y = item, by = "code", all.x = TRUE)
sales_item_supermarkets <- merge(x = sales_item, y = supermarkets, by.x = "supermarket", by.y = "supermarket_No", all.x = TRUE)


item$size_lc1 = stripWhitespace(tolower(item$size))
item
#gsub("[:alpha:]", "", item$size_lc1)
item$size_lc2 <- sapply(item$size_lc1, function(x) gsub(pattern = "n ", replacement = "", x))
item

conv_unit(10, "cm", "inch")

# Data Cleaning
## check duplicates which(duplicated(supermarket_Details))
## Categorical data setup, 
## outliers balanna... iqr function eken mean eka balanna eken replace karanna outliers eken eliye theiyen ewa
## remove unwanted columns
## Data modeling part
# Merging


which(duplicated(item$type))
