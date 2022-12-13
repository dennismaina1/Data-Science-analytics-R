library(lubridate)
library(dplyr)

df <- df[-1]

summary(df)

df <- drop_na(df)

#feature engineering
df['Sale'] <- df['Price Each']*df['Quantity Ordered']
df['date'] <- df['Order Date']
df <- df[-5]

   #date
df <- df %>%
  separate(date, sep="/", into = c("month", "day", "year"))
df <- df %>%
  separate(year, sep=" ", into = c( "year", 'Time'))

  #city
df <- df %>%
  separate(`Purchase Address`, sep=",", into = c("street", "City", "State"))

df <- df %>%
  separate(State, sep=" ", into = c("State Abbr","postal code"))

names(df)[names(df)=='postal code']<- 'State Abbr'


#question 1
#grouping sales by month and plotting 
df2 <- df%>%group_by(month)%>%
  summarise(sales=sum(Sale),.groups = 'drop')

ggplot(data = df2, aes(x = month,y=sales)) +
  geom_bar(stat = "identity")+scale_fill_hue(c = 40) 

#question 2
df3 <- df%>%group_by(City)%>%
  summarise(sales=sum(Sale),.groups = 'drop')

ggplot(data = df3, aes(x = City,y=sales)) +
  geom_bar(stat = "identity")

#question 3
df4 <- df%>%group_by(Time)%>%
  summarise(sales=sum(Sale),.groups = 'drop')

ggplot(data = df4, aes(x = Time,y=sales))+geom_line(aes(group=60))
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

 x=df4['Time']
 y=df4['sales']
   
#question 4

#group products bought together 
df5 <- df%>%group_by(`Order ID`)%>%
  summarise(product=paste(Product ,collapse = ", "),.groups = 'drop')

#get top 30 value counts
df6 <-df5 %>% group_by(product)%>%summarise(count=n())%>% top_n(30)

#separate products into different columns
df7 <- df6 %>%
  separate(product, sep=",", into = c("product 1","product 2","product 3"))

#drop single products 
df7 <- df7 %>% drop_na(`product 2`)

#arrange in descending order 
df8 <- df7 %>%                                      # Top N highest values by group
  arrange(desc(count)) %>% 
  group_by(count) %>%
  slice(1:3)

