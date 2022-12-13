
#necessary packages
install.packages('rjson')
installed.packages('jsonlite')
install.packages('plotly')
install.packages("RCurl")

library('rjson')
library('plotly')
library("RCurl")


#using encoded data
countries <- read.csv('C:/Users/hp/Documents/countries.csv',header=TRUE)

mapdataAfrica <- map_data('world')
Africa<- countries %>% filter(continent=="Africa")



fig <- plot_ly(Africa,
               type='choropleth',
               locations=Africa$iso_alpha, 
               z=Africa$pop,
               text=Africa$country,
               frame = Africa$year)
fig


#using json data
#African data json file
data <- 'https://raw.githubusercontent.com/statsbomb/open-data/master/data/events/7298.json'
mydata <- jsonlite::read_json(data)

g <- list(
  scope = 'Africa',
  projection = 'orthographic',
  showlakes = TRUE,
  rivercolor=toRGB('blue'),
  fitbounds = "locations",
  visible = FALSE
)

fig <- plot_ly(frame = Africa$year)
fig <- fig %>% add_trace(
  type="choropleth",
  locations=Africa$iso_alpha,
  geojson=mydata,
  z=Africa$pop,
  colorscale="Viridis",
  featureidkey="properties.ISO3"
 

)
fig <- fig %>% colorbar(title = "population deviation" )
fig <- fig %>% layout(
  title = "population deviation from 1952 to present"
)

fig <- fig %>% layout(
  geo = g
)

fig
