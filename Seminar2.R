library("tidyverse")
library("readr")
library("dplyr")

urlfile = "https://raw.githubusercontent.com/CWWhitney/teaching_R/master/participants_data.csv"

participants_data <- read_csv(url(urlfile))
# Change the number of rows displayed to 7
head(participants_data, 
     n = 7)
names(participants_data)
str(participants_data)

# Change the variable to gender
participants_data$gender

# Change the selection to batch and age
select(participants_data, 
       batch,
       age)

# Change the selection without batch and age
select(participants_data,
       -c(batch, age))

# Change the selection to 
# those who work more than 5 hours a day
filter(participants_data, 
       working_hours_per_day > 5)

# Change the filter to those who work more than 5 hours a day and 
# names are longer than three letters
filter(participants_data, 
       working_hours_per_day > 5 & 
         letters_in_first_name > 3)

# Rename the variable km_home_to_office as commute
rename(participants_data, 
       name_length = letters_in_first_name,
       commute = km_home_to_office)

# Mutate a new column named age_mean that is a function of the age multiplied by the mean of all ages in the group
mutate(participants_data, 
       labor_mean = working_hours_per_day*
         mean(working_hours_per_day),
       age_mean = age* mean(age))

# Mutate new column named response_speed populated by 'slow' if it took you more than a day to answer my email
# and 'fast' for others
mutate(participants_data, 
       response_speed = ifelse(days_to_email_response > 1, "slow", "fast"))

# Create a summary of the participants_mutate data with the mean number of siblings and median years of study
summarize(participants_data,
          mean(number_of_siblings),
          median(years_of_study))

# Use the magrittr pipe to summarize the mean days to email response, 
# median letters in first name, and maximum years of study by gender
participants_data %>% 
  group_by(gender) %>% 
  summarize(mean(days_to_email_response), 
            median(letters_in_first_name), 
            max(years_of_study))

# Use the magrittr pipe to create a new column called commute, 
# where those who travel more than 10km to get to the office are called "commuter" and others are "local". 
# Summarize the mean days to email response, median letters in first name, and maximum years of study. 
participants_data %>% 
  mutate(commute = ifelse(
    km_home_to_office > 10, 
    "commuter", "local")) %>% 
  group_by(commute) %>% 
  summarize(mean(days_to_email_response), 
            median(letters_in_first_name), 
            max(years_of_study))

# Split the data frame by batch, fit a linear model formula 
# (days to email response as dependent and working hours as independent) 
# to each batch, compute the summary, then extract the R^2.
participants_data %>%
  split(.$batch) %>% 
  map(~ 
        lm(days_to_email_response ~ 
             working_hours_per_day, 
           data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")


# Your turn to perform
# 
# Up until this point the code has been provided for you to work on. 
# Now it is time for you to apply your new found skills. 
# Please work through the wrangling tasks we just went though. 
# Use the diamonds data and make the steps in long format (i.e. assigning each step to an object)
# and short format with (i.e. with the magrittr pipeline):
   
my_diamonds <- diamonds

# select: carat and price
my_diamonds <- select(my_diamonds, carat, price)
my_diamonds
# filter: only where carat is > 0.5
my_diamonds <- filter(my_diamonds, carat > 0.5)
my_diamonds
# rename: rename price as cost
my_diamonds <- mutate(my_diamonds, cost = price, .keep = "unused")
my_diamonds
# mutate: create a variable with ‘expensive’ if greater than mean of cost and ‘cheap’ otherwise
my_diamonds <- mutate(my_diamonds, deal = ifelse(cost>mean(cost), "expensive", "cheap"))
my_diamonds
# group_by: split into cheap and expensive
my_diamonds <- group_by(my_diamonds, deal)
# summarize: give some summary statistics of your choice
summarize(my_diamonds, mean(cost), mean(carat))
 
# The diamonds data is built in with the ggplot2 library.
# It is already available in your R environment. 
# Look at the help file with ?diamonds to learn more about it.
