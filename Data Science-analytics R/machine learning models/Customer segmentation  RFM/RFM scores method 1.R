
#imports
library('tidyverse')
library('readxl')
library('lubridate')



#data 
customer <- read_xlsx('G:/Dennis Personal Files/Learning/machine learning/Rdata/Customer segmentation  RFM/retail.xlsx')
customer$price <- customer$Quantity*customer$UnitPrice

#eda
summary(customer)

#filter out negative values
customer <- customer %>% filter(Quantity > 0)

#group by country and drop duplicates
customer <- unique(customer)

customer2 <- customer %>% group_by(country=customer$Country,customerID=customer$CustomerID) %>%  summarise(n(),.groups ='drop' )

customer3 <-customer %>% group_by(country=customer$Country,customerID=customer$CustomerID) %>% tally()

#work with uk data
customer4 <- subset(customer, Country=='United Kingdom')
summary(customer4)
customer4 <-customer4%>% filter(!is.na(CustomerID))

#RFM modelling

#set analysis date
max(customer4$InvoiceDate)
analysis_date <- as.Date('2011-12-10')
customer4$InvoiceDate <- as.Date(customer4$InvoiceDate)


#rfm table 2 method 2
rfm_df <- customer4 %>% group_by(InvoiceNo) %>% 
  summarise(Recency = as.numeric(analysis_date- max(InvoiceDate)), Frequency = n(), Monetary = sum(price))



#RFM score method 2
summary(rfm_df)

rfm_df$R_score <- 0
rfm_df$R_score[rfm_df$Recency >= 255.0] <- 1
rfm_df$R_score[rfm_df$Recency >= 152.0 & rfm_df$Recency <255.0] <- 2
rfm_df$R_score[rfm_df$Recency >= 59.0 & rfm_df$Recency <152.0] <- 3
rfm_df$R_score[rfm_df$Recency < 59.0] <- 4

rfm_df$F_score<- 0
rfm_df$F_score[rfm_df$Frequency >=27 ] <- 4
rfm_df$F_score[rfm_df$Frequency <27 & rfm_df$Frequency >= 15] <- 3
rfm_df$F_score[rfm_df$Frequency <15 & rfm_df$Frequency >= 6] <- 2
rfm_df$F_score[rfm_df$Frequency <6] <- 1

rfm_df$M_score <- 0
rfm_df$M_score[rfm_df$Monetary >= 444.4] <- 4
rfm_df$M_score[rfm_df$Monetary < 444.4 & rfm_df$Monetary >= 299.5] <- 3
rfm_df$M_score[rfm_df$Monetary >= 152.8 & rfm_df$Monetary < 299.5] <- 2
rfm_df$M_score[rfm_df$Monetary <152.8] <- 1


#rfm score
rfm_df$score <- rfm_df$R_score+rfm_df$F_score+rfm_df$M_score

unique(rfm_df$score)

rfm_df$loyalty <- cut(rfm_df$score,breaks = c(0,3,6,9,12),labels = c('bronze','silver','gold','platinum'))


#hist
barplot(table(unique(rfm_df)$loyalty))




