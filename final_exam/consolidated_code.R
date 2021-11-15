#-------------------------------------------------------------------------------

#loading all the libraries
library(tidyverse)
library(mosaic)
library(gapminder)
library(here)
library(lubridate)
library(skimr)
library(patchwork)
library(janitor)

#============================================================================================

#----------------S A M P L E    E X A M ------------------------------------------------

#Q1
ggplot(data = gapminder , mapping = aes(x= gdpPercap, y = lifeExp)) + geom_point(colour = "blue")


#Q2 ---Constructing a tibble/table---------------
pregnant <- c("yes", "no")
male <- c(NA, 20)
female <- c(10, 12)

untidy_before <- data.frame(pregnant, male, female)

untidy_before

tidy_pregnant <- pivot_longer(untidy_before,cols = c(2:3), values_to = "Count" , values_drop_na = TRUE,  names_to = "Gender" )
tidy_pregnant




#-----------G A P M I N D E R --------------------------------------------------

movies <- read_csv(here::here("data", "movies.csv"))
glimpse(movies)

ggplot(movies, aes(x = cast_facebook_likes, y = gross)) +
  geom_point( color="blue" , size = 1, shape =2) +
  geom_smooth(method = "lm" , color="red" ,  alpha = 0.2)+
  labs(
    title = "Relationship of Facebook Likes vs Gross Revenue of the Movie",
    x = "Facebook Likes",
    y = "Gross Revenue"
  )+
  NULL




#-----------NORMAL DISTRIBUTIONS----------------------------------------------------

# Test scores are normally distributed with a mean of 525 and standard deviation of 55. 
# Automatic accepts have exam score > = 600 and automatic rejects score < = 425
# Calculate the percentage of applicants who are automatic accepted and rejected

# For those automatically accepted
xpnorm(600, mean = 525, sd = 55) 
xpnorm(25, mean = 22.5, sd = 0.75) 

# For those automatically rejected
xpnorm(425, mean = 525, sd = 55)

xpnorm(21.5, mean = 22.5, sd = 1.5) 

xpnorm(21, mean = 22.5, sd = 0.15) 


#z value at the 99th percentile (which is the same as z value at the 1st percentile)
qnorm(0.01)
qnorm(0.95)

#============================================================================================

#calculate for CI for proportions
sqrt( 0.46*0.54/198)

#-------------BIKE DATASET------------------------------------------------------------


#read the CSV file
bike <- read_csv(here::here("data", "london_bikes.csv"))

# fix dates using lubridate, and generate new variables for year, month, month_name, day, and day_of _week
bike <- bike %>%   
  mutate(
    #year=year(day),
    month = month(day),
    month_name=month(day, label = TRUE),
    day_of_week = wday(day, label = TRUE)) 

# generate new variable season_name to turn seasons from numbers to Winter, Spring, etc
bike <- bike %>%  
  mutate(
    season_name = case_when(
      month_name %in%  c("Dec", "Jan", "Feb")  ~ "Winter",
      month_name %in%  c("Mar", "Apr", "May")  ~ "Spring",
      month_name %in%  c("Jun", "Jul", "Aug")  ~ "Summer",
      month_name %in%  c("Sep", "Oct", "Nov")  ~ "Autumn",
    ),
    season_name = factor(season_name, 
                         levels = c("Winter", "Spring", "Summer", "Autumn"))
  )

# examine what the resulting data frame looks like
glimpse(bike)
skim(bike)


# Time series plot of bikes rented
ggplot(bike, aes(x=day, y=bikes_hired))+
  geom_smooth()+
  geom_point(alpha = 0.4, color="blue")+
  theme_bw()+
  NULL


#summary statistics
favstats(~ bikes_hired, data= bike)

# if we wanted to get summary statistics by `year`, `day_of_week`,  `month_name`, or `season_name`
# we use mosaic's syntax `Y ~ X` that allows us to facet our analysis of a variable Y by variable X 
# using the syntax `favstats( Y ~ X, data=...)`

favstats(bikes_hired ~ year, data=bike)
favstats(bikes_hired ~ day_of_week, data=bike)
favstats(bikes_hired ~ month_name, data=bike)
favstats(bikes_hired ~ season_name, data=bike)


# Histogram of bikes rented
ggplot(bike, aes(x=bikes_hired, color = "red"))+
  geom_histogram( binwidth =  8000 )+
  theme_bw()+
  NULL

# Histogram faceted by season_name
ggplot(bike, aes(x=bikes_hired))+
  geom_histogram()+
  facet_wrap(~season_name)+
  theme_bw()+
  NULL

# Histogram faceted by month_name
ggplot(bike, aes(x=bikes_hired))+
  geom_histogram()+
  facet_wrap(~month_name)+
  theme_bw()+
  NULL

# Histogram faceted by month_name in 4 rows
ggplot(bike, aes(x=bikes_hired))+
  geom_histogram()+
  facet_wrap(~month_name, nrow = 4)+
  theme_bw()+
  NULL


# Density plot 
ggplot(bike, aes(x=bikes_hired))+
  geom_density()+
  theme_bw()+
  NULL

# Density plot filled by season_name 
ggplot(bike, aes(x=bikes_hired))+
  geom_density(aes(fill=season_name), alpha = 0.3)+
  theme_bw()+
  NULL

# Density plot filled by season_name, and faceted by season_name
ggplot(bike, aes(x=bikes_hired))+
  geom_density(aes(fill=season_name), alpha = 0.3)+
  facet_wrap(~season_name, nrow = 4)+
  theme_bw()+
  NULL

# Density plot filled by season_name, and faceted by month_name
ggplot(bike, aes(x=bikes_hired))+
  geom_density(aes(fill=season_name), alpha = 0.3)+
  facet_wrap(~month_name, nrow = 4)+
  theme_bw()+
  theme(legend.position="none")+
  NULL

#Boxplot of bikes_hired  by month
# since 'month' is a number, it treats it as a continuous variable; hence we get just one box
ggplot(bike, aes(x=month, y= bikes_hired))+
  geom_boxplot()+
  theme_bw()+
  NULL


#Boxplot by month_name
ggplot(bike, aes(x=month_name, y= bikes_hired, fill=season_name))+
  geom_boxplot()+
  theme_bw()+
  NULL


#Violin plot by month_name
ggplot(bike, aes(x=month_name, y= bikes_hired))+
  geom_violin()+
  theme_bw()+
  NULL


# Summary stats of bikes hired vs rain and snow
favstats(bikes_hired ~ i_rain_drizzle, data=bike)
favstats(bikes_hired ~ i_rain_drizzle + season_name, data=bike)


favstats(bikes_hired ~ i_snow_ice, data=bike)
favstats(bikes_hired ~ i_snow_ice + season_name, data=bike)

#Boxplot of bikes_hired temperature by rain (TRUE/FALSE)
bike %>% filter(!is.na(i_rain_drizzle)) %>% 
  ggplot( aes(x=factor(i_rain_drizzle), y= bikes_hired))+
  geom_boxplot()+
  theme_bw()+
  NULL

#Boxplot of bikes_hired temperature by snow (TRUE/FALSE)
bike %>% filter(!is.na(i_snow_ice)) %>% 
  ggplot(aes(x=factor(i_snow_ice), y= bikes_hired))+
  geom_boxplot()+
  theme_bw()+
  NULL


# bikes_hired vs. `temp`, `rh` (relative humidity)`, `slp` (pressure), and `wdsp` (windspeed)
ggplot(bike, aes(x=temp, y= bikes_hired))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  NULL

ggplot(bike, aes(x=rh, y= bikes_hired))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  NULL

ggplot(bike, aes(x=slp, y= bikes_hired))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  NULL

ggplot(bike, aes(x=wdsp, y= bikes_hired))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  NULL

#============================================================================================
#=====================M O V I E S ===========================================================


movies <- read_csv(here::here("data", "movies.csv"))
glimpse(movies)

movies_tim <- movies %>% filter(movies$director  == "Tim Burton")
movies_stv <- movies %>% filter(movies$director  == "Steven Spielberg")

favstats(data = movies_tim , ~rating  )
favstats(data = movies_stv , ~rating  )

nrow(movies)

movies %>% 
group_by(genre) %>% 
  summarise(count=n()) %>%
  mutate( percentage = count/n()) %>%
  arrange(-count)


movies %>% 
    summarise(total_rows = nrow(movies))
    group_by(genre) %>% 
    summarise(count=n()) %>%
    arrange(-count)

    
    
  movies <-  movies %>%
    mutate(is_cancelled=ifelse(genre %in% c( "Action" , "Adventure" , "Comedy" ) , 0 , 1 )) %>% 
    mutate ( hotel  = ifelse ( director %in% c ( "Shane Black
" , "Gary Ross" , "Andrew Stanton") , "Resort Hotel" , "City Hotel"  ) )
        
           
                                    
    
    movies
    
    
    Untidy_Hotel_bookings <- movies %>%
      group_by(hotel) %>%
      summarise (cancel_prop = 100 * (sum(is_cancelled == 1) / n())) %>%
      mutate (not_cancel_prop = 1 - cancel_prop) 
    
    Tidy_Hotel_bookings <- Untidy_hotel_booking %>%
      Pivot_longer (cols = c(cancel_prop, cancel_prop), 
                    name_to = “Cancellation_Status”, 
                    values_to = “proportion”)
    
    ggplot (Tidy_Hotel_bookings, mapping = aes (x = hotel, y = proportion, fill = Cancellation_Status)) +
      geom_col (position = “dodge”)+
      labs (x = “Hotel Type”, y = “ddd”, title = “Cancellation Status % by Hotel Type”)
    
    
    #Part c (5 pts)For those customers who did stay, 
    #what proportion had a booking that involved kids (either children or babies). 

  
    
    movies_kids <- movies %>%  
      # make rev not 0 and not canceled
      filter((duration <100 | year==2004 ) & gross >10000 %>%
      #kids = babies + children
      mutate ( kids = duration+ votes) %>% 
      mutate ( yes_kids  = ifelse(  ( kids > 100000 ) , "Yes" , "No" )) %>%
      summarise(total_rows = n() , yes_kids) 
    
    prop_kids<- movies_kids %>% 
      group_by (yes_kids)%>%
      summarise(count=n(), total_rows) %>%
      mutate( percentage = count/total_rows ) %>%
      group_by(count)
      
    
    
    %>% 
      
    
    
    %>% 
 
    
      
    
