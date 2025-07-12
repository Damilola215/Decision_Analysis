
library(tidyverse)
participant_data %>%
  na.omit()

install.packages("tidyverse")
library(tidyverse)


#read file from github

library(readr)

urlfile = "https://raw.githubusercontent.com/CWWhitney/teaching_R/master/participants_data.csv"

participants_data <- read_csv(url(urlfile))
a <- read.csv("participants_data.csv")
participants_file

#look at data
participants_data

# Change the number of rows displayed to 7
head(a, 
     n = 7)

#check names
names(participants_data)

#see structure of data
str(participants_data)

# Change the variable to gender
participants_data$age

#select function
# Change the selection to batch and age
select(participants_data, 
       academic_parents,
       working_hours_per_day)

#negative selection
# Change the selection 
# without batch and age
select(participants_data,
       -academic_parents,
       -working_hours_per_day)

#filtering
# Change the selection to 
# those who work more than 5 hours a day
filter(participants_data, 
       working_hours_per_day >10)

#rename function
# Rename the variable km_home_to_office as commute
rename(participants_data, 
       name_length = letters_in_first_name)
#mutate
# Mutate a new column named age_mean that is a function of the age multiplied by the mean of all ages in the group
mutate(participants_data, 
       labor_mean = working_hours_per_day*
         mean(working_hours_per_day))

#more mutate
# Mutate new column named response_speed 
# populated by 'slow' if it took you 
# more than a day to answer my email and 
# 'fast' for others
mutate(participants_data, 
       commute = 
         ifelse(km_home_to_office > 10, 
                "commuter", "local"))

#summarize
# Create a summary of the participants_mutate data 
# with the mean number of siblings 
# and median years of study
summarize(participants_data,
          mean(years_of_study),
          median(letters_in_first_name))

#summarize by different groups
# Use the magrittr pipe to summarize 
# the mean days to email response, 
# median letters in first name, 
# and maximum years of study by gender
participants_data %>% 
  group_by(research_continent) %>% 
  summarize(mean(days_to_email_response), 
            median(letters_in_first_name), 
            max(years_of_study))

# Use the magrittr pipe to create a new column 
# called commute, where those who travel 
# more than 10km to get to the office 
# are called "commuter" and others are "local". 
# Summarize the mean days to email response, 
# median letters in first name, 
# and maximum years of study. 
participants_data %>% 
  mutate(response_speed = ifelse(
    days_to_email_response > 1, 
    "slow", "fast")) %>% 
  group_by(response_speed) %>% 
  summarize(mean(number_of_siblings), 
            median(years_of_study), 
            max(letters_in_first_name))

Split the data frame by batch, 
# fit a linear model formula 
# (days to email response as dependent 
# and working hours as independent) 
# to each batch, compute the summary, 
# then extract the R^2.
participants_data %>%
  split(.$gender) %>% 
  map(~ 
        lm(number_of_publications ~ 
             number_of_siblings, 
           data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")
# Create a scatterplot of days to email response (y) 
# as a function of the letters in your first name (x) 
ggplot(data = participants_data, 
       aes(x = age, 
           y = number_of_siblings)) + 
  geom_point()

