#
install.packages('rfm')

#imports
library('tidyverse')
library('readxl')
library('lubridate')
library('rfm')


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



#rfm table 1
rfm_result <- rfm_table_order(customer4,InvoiceNo,InvoiceDate,price,analysis_date)
rfm_result

#RFM score  method 1

#segments 4 bins  (bronze,silver,gold,platinum)
segment_names  <- c('bronze','silver','gold','platinum') 

recency_lower <- c(1,1,2,3)
recency_upper <- c(1,3,3,4)
frequency_lower <- c(1,1,2,3)
frequency_upper <- c(1,3,3,4)
monetary_lower  <- c(1,1,2,3)
monetary_upper <- c(1,3,3,4)

segments <- rfm_segment(rfm_result,segment_names,recency_lower,recency_upper,frequency_lower,
                        frequency_upper,monetary_lower,monetary_upper)

#check distribution
segments %>% 
  count(segment)%>%
  arrange(desc(n))%>%
  rename(Segment=segment, count=n)

#plots
rfm_plot_median_frequency(segments)
rfm_plot_median_monetary(segments)
rfm_plot_median_recency(segments)

