#packages
install.packages('leaflet')
install.packages('ggmap')
install.packages('sf')
install.packages('tidyverse')
install.packages('tidycensus')
install.packages('tmap')
install.packages('gganimate')
install.packages('devtools')
devtools::install_github('thomasp85/transformr')
install.packages("gifski")
install.packages('png')
install.packages('av')

rm(ies)
rm(Asia,countries,mapdataAsia,meanLife)
#load data
countries <- read.csv('C:/Users/hp/Documents/countries.csv',header=TRUE)

#Question 1
Asia <- countries %>% filter(continent=='Asia')
meanLife <- Asia%>% group_by(Asia$country)%>%summarise(meanLife=mean(lifeExp))


#map data
mapdataAsia <-map_data('world')

#rename countries to region
Asia$region <- Asia$country
Asia <- Asia[-2]
meanLife$region <-meanLife$`Asia$country` 
meanLife<- meanLife[-1]

#join mapdata to new data
mapdata<- left_join(mapdataAsia,meanLife,by='region')

#filter na data
mapdata <- mapdata %>% filter(!is.na(mapdata$meanLife))

#maps
map1<-ggplot(mapdata, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=meanLife,color='red'))


map1 <- map1+scale_fill_gradient(name='Life Expectancy',low="Red",high="Black")

map1


#question 2
mapdataEuro <- map_data('world')
Europe<- countries %>% filter(continent=="Europe")
SAmerica <- countries %>% filter(country=='Paraguay'|country== 'Brazil' | country=='Uruguay'
                                 | country=='Argentina' | country=='Bolivia'| country=='Chile' |
                                   country=='Colombia' | country=='Venezuela'| country=='Peru' | 
                                   country=='Ecuador')

EuroAmerica <- rbind(SAmerica,Europe)

names(EuroAmerica)[names(EuroAmerica)=="country"]<-"region"
mapdataEuro <- left_join(mapdataEuro,EuroAmerica,by='region')

mapdataEuro <- mapdataEuro %>% filter(!is.na(mapdataEuro$gdpPercap))

map1<-ggplot(mapdataEuro, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=gdpPercap,color='red'))

map1 <- map1+scale_fill_gradient(name='Gdp Per capita',low="black",high="green")


map2 <-map1+ labs(title = 'year: {frame_time}') +transition_time(year, range = c(1952L,2007L)) 

#render to a gif
animate(map2,nframes = 100,renderer = gifski_renderer("gdp.gif"),
        height=700,width=1000)

#render to video
animate(map2,nframes = 500,renderer =av_renderer('output.mp4'),
        height=700,width=1000)
map



#question 3
mapdataAfrica <- map_data('world')
Africa<- countries %>% filter(continent=="Africa")


names(Africa)[names(Africa)=="country"]<-"region"
mapdataAfrica <- left_join(mapdataAfrica,Africa,by='region')

mapdataAfrica <- mapdataAfrica %>% filter(!is.na(mapdataAfrica$pop))

map3<-ggplot(mapdataAfrica, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=gdpPercap,color='red'))

map3 <- map3+scale_fill_gradient(name='Gdp Per capita',low="blue",high="red")


map3 <-map3+ labs(title = 'year: {frame_time}') +transition_time(year, range = c(1952L,2007L)) 

#render to a gif
animate(map3,nframes = 300,renderer = gifski_renderer("gdp.gif"),
        height=700,width=1000)

#render to video
animate(map2,nframes = 500,renderer =av_renderer('output.mp4'),
        height=700,width=1000)
map

