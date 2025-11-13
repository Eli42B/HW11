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


########################################
# Part II 
########################################

# I used Nathan's tick data 

# Instructions: 
# Ecological Scenario: For our newly-discovered tick species Tickus bittus, we are interested in seeing if the different life stages (nymphs and adults) have different vulnerability to desiccation, and how this might affects their questing patterns.

#Our question: How does sensitivity of questing duration to a humidity index differ between tick nymphs and adults?

#Continuous response variable (questing_duration, in minutes), continuous predictor variable (humidity_index, does not exist outside of this exercise), and two-level factor (life_stage).

#loading libraries 
library(tidyverse)   #data manipulation, dplyr 
library(ggpubr)      #creating easily publication ready plots
library(rstatix)     # pipe friendly stat analyses
library(broom)       #printing summary of stat tests as data frames
library(datarium)   #contains example datasets from this
library(dplyr)       #so we can pipe 
library(emmeans)     # so we can do a post-hoc test like Tukey for ANCOVA

#loading data 
ticks = read.csv("tick_data.csv")

#Initial data visualization 

#y = mx + b 
#questing_duration ~ humidity*life_stage + b + e 

#Base plot 
base_plot = ggplot(ticks, aes(x = humidity_index, y = questing_duration)) + 
  geom_point(aes(color = life_stage))
base_plot + 
  labs(title = "Ticks by Lifestage", 
       x = "Humidity Index",
       y = "Questing Duration") 
# we want to post the base plot + a title, but we don't want to save the title to the baseplot because we will be changing this quite a lot 

base_plot + 
  facet_wrap(~life_stage, nrow = 1)  #visualizing the baseplot but facet wrapped 

# A ---------------------- 
# Fit a model 

aov_model = aov(questing_duration~life_stage*humidity_index, data = ticks)
summary(aov_model)

#life stage is not interacting, so we can include it as an independent covariate

fit = aov(questing_duration~life_stage + humidity_index, data = ticks)

# Run the ANCOVA and keep checking assumptions 

#Assumptions of both ANCOVA and ANOVA 
# Homogeneity of residuals variance (homoscedasticity) 
# Normally distributed  outcome variable 
# No sign. outliers 

resid = ticks$questing_duration - predict.lm(fit)
plot(resid ~ predict.lm(fit), ylab = "residuals", xlab = "Predicted Y")
abline(a = 0, b = 0, col = "red")
#residuals are the difference between the actual and predicted fit 
#Testing homogeneity of residuals 
#this is looking good 

stdRes = rstandard(fit)
qqnorm(stdRes,ylab="Standardized Residuals", xlab="Theoretical Quantiles")
qqline(stdRes, col=2,lwd=2)
hist(stdRes)
#Testing normality 
#   QQline is great
#   Histogram is great

#What does our fit say? 
summary(fit)
# questing duration is significantly related to the humidity index, but life stage does not impact questing duration 

#Posthoc test 
emmeans(fit, pairwise ~ life_stage, adjust = "tukey")$contrasts 
#tukey base R will not work here becuase its ancova 
#We need to use emmeans to compare pairwise means 
#basically like tukey but for ancova 
# no significance of life stage 

# B --------------------------------

#Our question: How does sensitivity of questing duration to a humidity index differ between tick nymphs and adults?
#As humidity increases, ticks start questing for longer periods of time. However, their life stage does not impact  how long they quest. 