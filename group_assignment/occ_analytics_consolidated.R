library(tidyverse)
library(janitor) 
library(skimr)
library(countrycode) # to clean up country names
library(broom)
library(car)
library(ggfortify)
library(mosaic)
library(dplyr)
library(huxtable)
library(ggthemes)
library(tidyquant)
library(hrbrthemes)
library(viridis)
library(infer)


weather <-  read_csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.csv", #retrieving data 
           skip = 1, #skipping first line 
           na = "***") 


## Data Frame 

tidyweather <- weather %>%  select(1:13) %>% pivot_longer(cols=2:13, names_to = "Month", values_to="delta")






# https://www.askamanager.org/
ask_a_manager_2021 <- read_csv(here::here("data","ask_a_manager.csv"))

#going through the data on a high level
skimr::skim(ask_a_manager_2021)


# Data cleaning (exchange rate: Alex)

# After skimming the dataset, we took the below steps to clean the raw data :
  
#   - Removing NA values
# - Removing outliers (out of 3$\sigma$)
# - Transforming certain datatype into factor
# - Selecting key industries from 1000+ industries reported
# - Unifying currencies

## Removing NA values


# remove the NA values
ask_a_manager_2021 <- ask_a_manager_2021 %>% 
  filter(!gender %in% c("NA","Other or prefer not to answer","Prefer not to answer", "Non-binary", NA) &
           race!="NA" &
           highest_level_of_education_completed!="NA")

## Removing outliers: 3 places outside z score
# using z-score. remove data out of 3 sigma

ask_a_manager_2021 <- ask_a_manager_2021[-which(abs(scale(ask_a_manager_2021$annual_salary))>3),]



## Transforming into ordered `factor` datatype

#Namely, we want to make age, years of working experience, education into `factor`, 
#and take the log of salary for the next step of EDA.


ask_a_manager_2021 <- ask_a_manager_2021 %>% 
  mutate(
    country = countrycode::countryname(country),
    age_group = factor(how_old_are_you,levels=c("under 18","18-24","25-34","35-44","45-54","55-64","65 or over")),
    field_exp = factor(years_of_experience_in_field,levels=c("1 year or less","2 - 4 years","5-7 years","8 - 10 years","11 - 20 years","21 - 30 years","31 - 40 years","41 years or more")),
    pro_exp = factor(overall_years_of_professional_experience,levels=c("1 year or less","2 - 4 years","5-7 years","8 - 10 years","11 - 20 years","21 - 30 years","31 - 40 years","41 years or more")),
    education = factor(highest_level_of_education_completed, labels=c("High School","Some college","College degree","Master's degree","Professional degree", "PhD")),
    higher_edu = ifelse(education %in% c("College degree","Master's degree", "PhD", "Professional degree"),1,0),
    salary = annual_salary,
    log_salary = log(annual_salary)
  ) %>% 
  janitor::clean_names() # clean columns names


## Selecting industries

#Since there are over 1000 reported industries in the dataset, 
# we will only keep the top 25 industries which account for 92% of total entries

# Industry is messy... it has > 1000 different industries.
# we only keep the names of top 25 industries. (24597(92%) out of 26765 entries)
industry_list <- ask_a_manager_2021 %>% 
  count(industry, sort=TRUE) %>% 
  mutate(percent = 100* n/sum(n)) %>% 
  slice_max(order_by = n, n = 25) %>% 
  select(industry)

ask_a_manager_2021 <- ask_a_manager_2021 %>% 
  mutate(industry = ifelse(industry %in% industry_list$industry,
                           industry,
                           "Others"))





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


favstats(bikes_hired ~ year, data=bike)
favstats(bikes_hired ~ day_of_week, data=bike)
favstats(bikes_hired ~ month_name, data=bike)
favstats(bikes_hired ~ season_name, data=bike)


# Histogram of bikes rented
ggplot(bike, aes(x=bikes_hired, color = "red"))+
  geom_histogram( binwidth =  8000 )+
  theme_bw()+
  NULL
