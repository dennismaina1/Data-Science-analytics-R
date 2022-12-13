
#group by function
region2 <- file%>%group_by(Month)%>%
  summarise(sales=sum(Sales),.groups = 'drop')


#change month name to abbreviation
region2 <- region2 %>% mutate(Month=month.abb[Month])

#ensure the data is diplayed from Jan to Dec in order
region2$Month <- factor(region2$Month, levels = month.abb)


