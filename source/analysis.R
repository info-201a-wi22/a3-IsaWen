#Introduction + Summary Information
# Question:
#  1, What is the average value of my variable across all the counties in all years?
#  2, When is my variable the highest?
#  3, When is my variable the lowest?
#  4, How much has my variable change over the last 10 years?
#  5, What is the standard division of the data?

library("dplyr")
library ("ggplot2")
library ("plotly")
library("leaflet")
library("rjson")

incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")



# the average value of total population in jail
mean_total_jail_pop <- mean(incarceration$total_jail_pop,na.rm = TRUE)

# when is the max of the value of total population in jail
max_total_jail_pop <- max(incarceration$total_jail_pop,na.rm = TRUE)
time_max_total_jail_pop <- incarceration%>%
  filter(total_jail_pop == max_total_jail_pop)%>%
  pull(year)

# when is the min of the value of total population in jail
min_total_jail_pop <- min(incarceration$total_jail_pop,na.rm = TRUE)
time_min_total_jail_pop <- incarceration%>%
  filter(total_jail_pop == min_total_jail_pop)%>%
  pull(year)

# the changes of the value of total population in jail from 2008-2018
total_pop_10 <- incarceration %>%
  filter(year >= 2008 & year <= 2018)
max_total_pop_10 <- max(total_pop_10$total_jail_pop,na.rm = TRUE)
min_total_pop_10 <- min(total_pop_10$total_jail_pop,na.rm = TRUE)

change_total_jail_pop <- max_total_pop_10 - min_total_pop_10

# the standard division of the value of total population from in jail
SD_total_jail_pop <- sd(incarceration$total_jail_pop,na.rm = TRUE)





# the average value of aapi population in jail
mean_aapi_jail_pop <- mean(incarceration$aapi_jail_pop,na.rm = TRUE)

# when is the max of the value of aapi population in jail
max_aapi_jail_pop <- max(incarceration$aapi_jail_pop,na.rm = TRUE)
time_max_aapi_jail_pop <- incarceration%>%
  filter(aapi_jail_pop == max_aapi_jail_pop)%>%
  pull(year)

# when is the min of the value of aapi population in jail
min_aapi_jail_pop <- min(incarceration$aapi_jail_pop,na.rm = TRUE)
time_min_aapi_jail_pop <- incarceration%>%
  filter(aapi_jail_pop == min_aapi_jail_pop)%>%
  pull(year)

time_min_aapi_jail_pop_base <- incarceration%>%
  filter(aapi_jail_pop == min_aapi_jail_pop)%>%
  pull(year)
time_min_aapi_jail_pop <- unique(time_min_aapi_jail_pop_base)

# the changes of the value of aapi population in jail from 2008-2018
aapi_pop_10 <- incarceration %>%
  filter(year >= 2008 & year <= 2018)
max_aapi_pop_10 <- max(aapi_pop_10$aapi_jail_pop,na.rm = TRUE)
min_aapi_pop_10 <- min(aapi_pop_10$aapi_jail_pop,na.rm = TRUE)

change_aapi_jail_pop <- max_aapi_pop_10 - min_aapi_pop_10

# the standard division of the value of aapi population from in jail
SD_aapi_jail_pop <- sd(incarceration$aapi_jail_pop,na.rm = TRUE)




# the average value of black population in jail
mean_black_jail_pop <- mean(incarceration$black_jail_pop,na.rm = TRUE)

# when is the max of the value of black population in jail
max_black_jail_pop <- max(incarceration$black_jail_pop,na.rm = TRUE)
time_max_black_jail_pop <- incarceration%>%
  filter(black_jail_pop == max_black_jail_pop)%>%
  pull(year)

# when is the min of the value of black population in jail
min_black_jail_pop <- min(incarceration$black_jail_pop,na.rm = TRUE)
time_min_black_jail_pop <- incarceration%>%
  filter(black_jail_pop == min_black_jail_pop)%>%
  pull(year)

# the changes of the value of black population in jail from 2008-2018
black_pop_10 <- incarceration %>%
  filter(year >= 2008 & year <= 2018)
max_black_pop_10 <- max(black_pop_10$black_jail_pop,na.rm = TRUE)
min_black_pop_10 <- min(black_pop_10$black_jail_pop,na.rm = TRUE)

change_black_jail_pop <- max_black_pop_10 - min_black_pop_10

# the standard division of the value of black population from in jail
SD_black_jail_pop <- sd(incarceration$black_jail_pop,na.rm = TRUE)





# the average value of white population in jail
mean_white_jail_pop <- mean(incarceration$white_jail_pop,na.rm = TRUE)

# when is the max of the value of white population in jail
max_white_jail_pop <- max(incarceration$white_jail_pop,na.rm = TRUE)
time_max_white_jail_pop <- incarceration%>%
  filter(white_jail_pop == max_white_jail_pop)%>%
  pull(year)

# when is the min of the value of white population in jail
min_white_jail_pop <- min(incarceration$white_jail_pop,na.rm = TRUE)
time_min_white_jail_pop <- incarceration%>%
  filter(white_jail_pop == min_white_jail_pop)%>%
  pull(year)

# the changes of the value of white population in jail from 2008-2018
white_pop_10 <- incarceration %>%
  filter(year >= 2008 & year <= 2018)
max_white_pop_10 <- max(white_pop_10$white_jail_pop,na.rm = TRUE)
min_white_pop_10 <- min(white_pop_10$white_jail_pop,na.rm = TRUE)

change_white_jail_pop <- max_white_pop_10 - min_white_pop_10

# the standard division of the value of white population from in jail
SD_white_jail_pop <- sd(incarceration$white_jail_pop,na.rm = TRUE)





# Trends over time chart
incarceration_NA <- incarceration %>%
  filter(na.rm = TRUE, 
         year >= 2000 & year<=2018,
         region == "West")
hist <- ggplot(incarceration_NA) +
  geom_col(mapping = aes(x = year, y = white_jail_pop), position = "dodge") +
  labs(
    title = "white people population in jail depends on year in King county",
    x = "time in year",
    y = "white people population in jail",
  )
ggplotly(hist)


#Variable comparison chart
scatter <- ggplot(incarceration_NA) +
  geom_point(mapping = aes(x = black_jail_pop, y = white_jail_pop, color = year)) +
  labs(
    title = "white people population and black people population in jail in King county realation",
    x = "black people population in jail in King county",
    y = "white people population in jail in King county",
  )
ggplotly(scatter)

# Map
url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)

incarceration_2018 <- incarceration %>%
  filter(na.rm = TRUE, 
         year==2018)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_ly()
fig <- fig %>% add_trace(
  type="choropleth",
  geojson=counties,
  locations = incarceration_2018$fips,
  z=incarceration_2018$black_jail_pop,
  colorscale="Viridis",
  marker=list(line=list(
    width=0)
  )
)
fig <- fig %>% colorbar(title = "population black people in jail in 2018")
fig <- fig %>% layout(
  title = "2018 distribution of black population in jail"
)
fig <- fig %>% layout(
  geo = g
)

fig






