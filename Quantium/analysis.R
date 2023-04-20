#installing packages
install.packages('tidyverse')
#loading the packages
library('data.table')
library('ggplot2')
library('readr')
library('dplyr')
library('ggmosaic')
library('tidyverse')

#loading the dataset
transactionalData <- readxl::read_excel('~/Desktop/Quantium/QVI_transaction_data.xlsx')
customerData <- read.csv('~/Desktop/Quantium/QVI_purchase_behaviour.csv',header = TRUE)

#examining transactional data
head(transactionalData)
str(transactionalData)

#change the integer to date fomart
transactionalData$DATE <- as.Date(transactionalData$DATE, origin = "1899-12-30")

#summary of PROD name
PROD_NAME <- data.frame(unique(transactionalData$PROD_NAME))
colnames(PROD_NAME) <- "unique_products"

#remove digits & special characters
PROD_NAME$unique_products <- gsub("[^[:alpha:]]","_" ,PROD_NAME$unique_products)
transactionalData$PROD_NAME <- gsub("[^[:alpha:]]","_" ,transactionalData$PROD_NAME)

PROD_NAME$unique_products <- gsub("___","_" ,PROD_NAME$unique_products)
transactionalData$PROD_NAME <- gsub("___","_" ,transactionalData$PROD_NAME)

PROD_NAME$unique_products <- gsub("__","_" ,PROD_NAME$unique_products)
transactionalData$PROD_NAME <- gsub("___","_" ,transactionalData$PROD_NAME)

#sort words by frequency
frequency <- transactionalData %>%
  group_by(PROD_NAME) %>%
  summarize(freq = n()) 
frequency <- frequency[order(-frequency$freq),]
rm(PROD_NAME)

#remove salsa products
transactionalData2 <- transactionalData[!grepl("Salsa",transactionalData$PROD_NAME),]
frequency <- frequency[!grepl("Salsa",frequency$PROD_NAME),]

#check null data
summary(transactionalData)
nulls <- sum(is.na(transactionalData2))

#check for outliers
transactionalData2 %>% filter(transactionalData2$PROD_QTY == 200)

#check other transactions with loyalty card 226000
transactionalData2 %>% filter(transactionalData2$LYLTY_CARD_NBR == 226000)

#remove customer 
transactionalData3 <- transactionalData$LYLTY_CARD_NBR != 226000

