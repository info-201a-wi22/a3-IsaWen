#Introduction + Summary Information
# Question:
#  1, What is the average value of my variable across all the counties in all years?
#  2, When is my variable the highest?
#  3, When is my variable the lowest?
#  4, How much has my variable change over the last 10 years?
#  5, What is the standard division of the data?

library("dplyr")
library("maps")
library ("ggplot2")
library("usmap")
library ("plotly")
library("leaflet")

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

# the changes of the value of total population in jail between 2008 and 2018
total_pop_08 <- incarceration %>%
  filter(year == 2008 )
total_pop_18 <- incarceration %>%
  filter(year == 2018 )
max_total_pop_08 <- max(total_pop_08$total_jail_pop,na.rm = TRUE)
max_total_pop_18 <- max(total_pop_18$total_jail_pop,na.rm = TRUE)

change_total_jail_pop <- max_total_pop_08 - max_total_pop_18

# the standard division of the value of total population from in jail
SD_total_jail_pop <- sd(incarceration$total_jail_pop,na.rm = TRUE)





# the average value of aapi population in jail
mean_aapi_jail_pop <- mean(incarceration$aapi_jail_pop,na.rm = TRUE)

# when is the max of the value of aapi population in jail
max_aapi_jail_pop <- max(incarceration$aapi_jail_pop,na.rm = TRUE)
time_max_aapi_jail_pop <- incarceration%>%
  filter(aapi_jail_pop == max_aapi_jail_pop)%>%
  pull(year)

# when is the min of the value of aapi population in jail and How many year is that
min_aapi_jail_pop <- min(incarceration$aapi_jail_pop,na.rm = TRUE)
time_min_aapi_jail_pop <- incarceration%>%
  filter(aapi_jail_pop == min_aapi_jail_pop)%>%
  pull(year)
time_range <- length(time_min_aapi_jail_pop)

time_min_aapi_jail_pop_base <- incarceration%>%
  filter(aapi_jail_pop == min_aapi_jail_pop)%>%
  pull(year)
time_min_aapi_jail_pop <- unique(time_min_aapi_jail_pop_base)

# the changes of the value of aapi population in jail between 2008 and 2018
aapi_pop_08 <- incarceration %>%
  filter(year == 2008 )
aapi_pop_18 <- incarceration %>%
  filter(year == 2018 )
max_aapi_pop_08 <- max(aapi_pop_08$aapi_jail_pop,na.rm = TRUE)
max_aapi_pop_18 <- max(aapi_pop_18$aapi_jail_pop,na.rm = TRUE)

change_aapi_jail_pop <- max_aapi_pop_08 - max_aapi_pop_18

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

# the changes of the value of black population in jail between 2008 and 2018
black_pop_08 <- incarceration %>%
  filter(year == 2008 )
black_pop_18 <- incarceration %>%
  filter(year == 2018 )
max_black_pop_08 <- max(black_pop_08$black_jail_pop,na.rm = TRUE)
max_black_pop_18 <- max(black_pop_18$black_jail_pop,na.rm = TRUE)

change_black_jail_pop <- max_black_pop_08 - max_black_pop_18

# the standard division of the value of black population from in jail
SD_black_jail_pop <- sd(incarceration$black_jail_pop,na.rm = TRUE)






# Trends over time chart
incarceration_NA <- incarceration %>%
  filter(na.rm = TRUE, 
         year >= 2008 & year<=2018,
         county_name == "King County")
hist <- ggplot(incarceration_NA) +
  geom_line(mapping = aes(x = year, y = white_jail_pop,color = state)) +
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
incarceration_2018 <- incarceration %>%
  filter(na.rm = TRUE, 
         year==2018)

map <- plot_usmap(data = incarceration_2018, values = "black_jail_pop") + 
  scale_fill_continuous(name = "Population in jail (2018)", label = scales::comma) + 
  theme(legend.position = "right")+labs(title = "2018 distribution of black population in jail")

map



