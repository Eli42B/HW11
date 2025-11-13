#### In class activity November 11, 2025 ----- 
# In class activity will also be the homework

#### Load libraries ----

library(tidyverse)
library(car)
library(emmeans)

#### Exercise: simulation and estimation of linear models with one categorical and one continuous predictor

# With last week's code simulate 100 observations of two level categorical data

# Using your code from last week's homework, add a two-level categorical X-value
# and simulate sufficient data (say 100 obs total, split evenly
# between the two levels) to estimate the parameters of an ANCOVA model.
# You can decide whether there is an interaction or not 
# (same slopes but different intercepts vs different slopes and intercepts)
# or even whether the categorical variable matters at all.

#### Part 1 ----

# since we want two levels, let's say they are treatment 1 and treatment 2
# and let's we got to have equal sample sizes, so 50 of treatment 1 and 50 of treatment 2

## save treatment 1

t1_x <- rnorm(50, mean = 15, sd = 2) #t1 _x for treatment 1 x-values
t1_error <- rnorm(50, mean = 15, sd =2) # save errors for these x-values. for ease, let's normally distribute them

slope_1 <- 3
intercept_1 <- 20

# simulate y values from the first treatment

t1_y <- intercept_1 + (slope_1 * t1_x) + t1_error

# save this as a data frame
location <- "Site_A"
individual_id <- seq(1:50)
sex1 <- sample(c("male", "female"), size = 50, replace = TRUE)
treat1 <- data.frame(site = location, individual = individual_id, sex = sex1, initial_weight = t1_x, final_weight = t1_y)

## save treatment 2 

t2_x <- rnorm(50, mean = 10, sd = 2) #t2_x for treatment 2 x-values
t2_error <- rnorm(50, mean = 10, sd = 2) # save errors for the x-values

slope_2 <- 3
intercept_2 <- 21

# simulate y values from the second treatment

t2_y <- intercept_2 + (slope_2 * t2_x) + t2_error

# save treatment two as a data frame
location2 <- "Site_B"
individual_id <- seq(1:50)
sex2 <- sample(c("male", "female"), size = 50, replace = TRUE)
treat2 <- data.frame(site = location2, individual = individual_id, sex = sex2, initial_weight = t2_x, final_weight = t2_y)


# combine the two data frames into one

df_both <- rbind(treat1, treat2)

## Preliminary data exploration

# make boxplots!! They are fun!

site_boxplots <- ggplot(data = df_both, mapping = aes(x = site, y = final_weight, fill = sex)) +
  geom_boxplot() +
  theme_minimal()

site_boxplots
# well, looking at these boxplots, I would certainly expect the ancova to say these are significant results

# so let's actually run the ancova
# to do this, first build out our simple linear model

mod1 <- lm(final_weight ~ site + sex, data = df_both) #model of final_weight as a result of site, sex as a covariate?

# Summary of the model
summary(mod1)

# Type II ANOVA table (common for ANCOVA)
Anova(mod1, type = "II")

emmeans(mod1, ~ site)

# Write a brief (2-3 sentences) ecological scenario for these data 
# and give the variables related names and units
# Finish with an ecological question that fits the ANCOVA format. 
# For example, "Does the depth distribution of pigmented and unpigmented Daphnia
# respond similarly to changes in light intensity?"
# There's a continuous response variable, a continuous predictor, and a 2-level factor.

#Average weight gain in bear cubs at two sites after 6 months
#disclaimer: I know nothing about bear cubs
#Did cubs at one site gain significantly more weight on average than at another site?

# Researchers are interested in the growth of bear cubs at two national park sites.
# They weighed 50 bear cubs at each site, and then six months later, reweighed those cubs.
# They want to know if the bear cubs are significantly heavier at one site or another.
