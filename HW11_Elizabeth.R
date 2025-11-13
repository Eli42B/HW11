# HW 11 ###########################
# Nathan Lin, Kristine Schoenecker, Elizabeth Braatz 

# Here is my (Elizabeth's) code behind my data. I will update my data separately in a csv file so that Nathan or Kristine can use it without looking at this code. 
# Since we were in the same group earlier, I think we will need to generate at least two other false datasets if we want to stay in the same group 


###########################################
# Exercise  1 
###########################################

#A ------ Create fake data with 
  # A continuous Y variable 
  # One continuous X variables 
  # A two-level categorical X value 

#Calling libraries 

library(tidyverse)

#Creating data 

set.seed(42)

#true equations 
# y = mx*ai + b + e
# acres on fire = 100*2 + 1 + e when ai = 1 white dragons 
# acres on fire = 100*1 + 1 + e when ai = 0 red dragons
# y =  dependent variable  = acres on fire 
# ai = categorical variable of level i, where i = 0 is red and i = 1 is white 
# m = continuous independent variable = size 
# b = intercept = minimum acres, regardless of size or color, that dragons set on fire 
# e = error term 


color = c(rep("White", times = 50), rep("Red", times = 50))
color = as.factor(color) #changing to factor is important 
# creating color column 

size = rnorm(100, mean = 35, sd = 9)
#creating size column 

y_white = size[1:50]*1.3 + 1 
y_red = size[51:100]*1 + 1 
acres_on_fire = c(y_white, y_red)
#creating the y variable 

dragons = tibble(
  color = color, 
  size = size, 
  acres_on_fire = acres_on_fire
)
dragons

write.csv(dragons, "elizabeth_data.csv", row.names = FALSE)

# B ------ Ecological Scenario 

# Uther Pendragon is having some trouble with dragons in his medieval kingdom. White dragons and red dragons keep setting fire to the kingdom. He wants to hire a private contractor named Merlin (occupation: Wizard) to relocate the dragons, but private contractors are really expensive and Merlin can only capture half the dragons. The dragons are also all different sizes. Some people think red dragons are bigger, but other people think white dragons are bigger. Bigger dragons are more problematic. Should Uther have Merlin capture the red dragons, the white dragons, or does it not matter? Oddly enough, Uther has excellent ecological data on all the dragons.

#Question: Do the numbers of acres set on fire depend on the size of white and red dragons? 
# Continuous response variable: Number of acres set on fire 
  # type = numeric
  # unit = acre 
# Continuous predictor variable: Dragon size 
  # type = numeric 
  # unit = tons 
# Two level factor: Color of dragon 
  # type =  categorical 


# # Twist / quetsion for Olaf: Let's say also we know the dragons' age. Assume that there are no interactions between them. How would you model that? 
# acres set on fire ~ age + size + color  


###########################################
# Exercise  2 
###########################################

#I used Kristine's data. Here's her question for me: 
##Average weight gain in bear cubs at two sites after 6 months
#disclaimer: I know nothing about bear cubs
#Did cubs at one site gain significantly more weight on average than at another site?

# Researchers are interested in the growth of bear cubs at two national park sites.
# They weighed 50 bear cubs at each site, and then six months later, reweighed those cubs.
# They want to know if the bear cubs are significantly heavier at one site or another.






