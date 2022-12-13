root <- '~/Online Retail/'
functions <- '~/Online Retail/function RFM/'

library(lubridate)
library(tidyverse)

analysis <- readxl::read_xlsx('~/Online Retail.xlsx')

analysis$Total <- analysis$Quantity*analysis$UnitPrice

summary(analysis)
analysis <- analysis %>% filter(!is.na(analysis$CustomerID))
analysis$Total <- analysis$Quantity*analysis$UnitPrice

#separting dates and times
analysis <- analysis%>%separate(InvoiceDate,into = c('Year','Month','Day'),sep = '-')
analysis <- analysis%>% separate(Day, into = c('Day','Time'),sep = ' ')
analysis <- analysis%>%separate(Time, into = c('Hour','Minutes','Seconds'), sep = ':')
analysis<-analysis[-10]

#Total sales by diffrent demographics 
country <- analysis %>% group_by(Country) %>% summarise(Total_sales=sum(Total),.groups = 'drop')
products_by_country <- analysis %>% group_by(Country,Description) %>% tally()
products3 <- products_by_country %>%                                     
arrange(desc(n))
Average_per_month <- analysis %>% group_by(Year,Month) %>% summarise(average_sales=mean(Total))
Total_per_month <- analysis %>% group_by(Year,Month) %>% summarise(average_sales=sum(Total))





#group products bought together 
UK <- analysis %>% filter(Country=='United Kingdom')

ordered_together <- UK%>%group_by(InvoiceNo)%>%
  summarise(product=paste(Description ,collapse = ", "),.groups = 'drop')

count_ordered_together <-ordered_together %>% group_by(product)%>%tally()

ordered_together_Count <- count_ordered_together %>% separate(product, into = c('product 1','product 2','product 3'), sep = ',')

ordered_together <- ordered_together_Count %>% filter(!is.na(ordered_together_Count$`product 2`))

# Top N highest values by group
df <- ordered_together %>%                                    
  arrange(desc(n)) %>% 
  group_by(n) %>%
  slice(1:3)

#customer segmenttation UK

#RFM modelling

#set analysis date
max(UK$InvoiceDate)
analysis_date <- as.Date('2011-12-10')
UK$InvoiceDate <- as.Date(UK$InvoiceDate)


#rfm table 2 method 2
rfm_df <- UK %>% group_by(InvoiceNo) %>% 
  summarise(Recency = as.numeric(analysis_date- max(InvoiceDate)), Frequency = n(), Monetary = sum(Total))

#RFM function
RFM <- rfm_scores(ID=rfm_df$InvoiceNo,Recency = rfm_df$Recency,
                  Monetary = rfm_df$Monetary,Frequency = rfm_df$Frequency)



setwd(functions)
dump('rfm_scores',file = 'RFMFunction.R')