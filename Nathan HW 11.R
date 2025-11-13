# Quantitative Methods in Ecology and Evolution, Homework 11
# Nathan Lin, Kristine Schoenecker, Elizabeth Braatz
# November 13, 2025
# ANOVA and ANCOVA

# Simulating and estimating linear models (ANCOVA) with one categorical and one continuous predictor

library(dplyr)
library(ggplot2)

### Objective 1A ###
# Make a two-level categorical X and simulate sufficient data to estimate parameters of ANCOVA model
# 100 observations total to split evenly between the two levels
# We can decide whether there is an interaction or not, or if the categorical var matters at all
# Normally distributed match all the assumptions etc

# OK ACTUALLY I WILL SPLIT INTO 50 AND 50 AND FOR EACH ONE ILL DO A DIFFERENT REGRESSION?
  # Or I could just make the error really large and see how that goes

set.seed(1234)

# Simulate normally-distributed data
continuous_predictor <- rnorm(100, mean=50, sd=10)

# Simulate the errors and set parameters for regression
slope <- 4
intercept <- 8
errors <- rnorm(100, mean=0, sd=50)

# Calculate our response variable values
continuous_response <- intercept + (continuous_predictor * slope) + errors

# Put everything into a dataframe
tick_data <- data.frame(humidity_index=continuous_predictor,
                        questing_duration=continuous_response)

# Evenly split 50/50 adults and nymphs
tick_data$life_stage <- rep(c("Adults", "Nymphs"), times=50)

# Let's see what it looks like
ggplot(tick_data, aes(x=humidity_index, y=questing_duration, col=life_stage)) + 
  geom_point()

# Make it a csv
write.csv(tick_data, file="./HW 11/ticks.csv")

###


### Objective 1B ###
# Write a brief 2-3 sentence ecological summary for the data and give variables related names and units
# Finish with ecological question for ANCOVA format

# Ecological Scenario: For our newly-discovered tick species Tickus bittus, we are interested in seeing if the different life stages (nymphs and adults) have different vulnerability to desiccation, and how this might affects their questing patterns.
# Our question: How does sensitivity of questing duration to a humidity index differ between tick nymphs and adults?
# Continuous response variable (questing_duration, in minutes), continuous predictor variable (humidity_index, does not exist outside of this exercise), and two-level factor (life_stage).

# Do the full model first 
full_model <- lm(questing_duration ~ humidity_index + life_stage + humidity_index:life_stage,
                 data=tick_data)
# View it
summary(full_model)
# So humidity index is significant but the intercept, categorical variable, and the interaction term are not significant.

# Take out the interaction term
no_interaction_model <- lm(questing_duration ~ humidity_index + life_stage,
                           data=tick_data)
# View it
summary(no_interaction_model)
# Humidity index once again is significant but the life stage is not

# Take out the categorical variable
basic_lm <- lm(questing_duration ~ humidity_index, data=tick_data)
# View it
summary(basic_lm)
# Intercept once again is not significant

# CONCLUSION: there is no difference in how humidity index affects questing duration between nymphs and adults. Life stage did not predict questing duration as well, and the intercepts were not significantly different.
  # The estimated slope was 3.86 (close to the true one of 4), and the estimated intercept was 16 but there was no significance here (the true intercept was 8).
# If I were to do this again I would do it that two simulations way and change the intercepts but maybe keep the slope the same for fun 

### 

### Objective 2 ###

# Kristine's ecological scenario:
  # Researchers are studying brown bear cubs at two sites. 
  # They are interested in if the amount of weight gained after their survey period is related to their average daily forage distance. 
  # Do bears that, on average, forage further gain more weight? 
  # Is there a difference between male and female bear cubs, or cubs from one site or another?

# Load in the data
bear_data <- read.csv("./HW 11/bear_data.csv")

# Forage distance will be the predictor, difference between initial and final weight will be the response

# Get the difference
bears_final <- bear_data %>% mutate(weight_gain = final_weight - initial_weight)

# Check it out
ggplot(bears_final, aes(x=forage_distance, y=weight_gain, col=site)) + geom_point()

# First set of tests will be with sex as the categorical
full_model_bears_sex <- lm(weight_gain ~ forage_distance + sex + forage_distance:sex,
                       data=bears_final)
# View it
summary(full_model_bears_sex)
# No significant interaction term for sex and forage distance affecting weight gain

# Interaction-less model
no_interaction_model_bears_sex <- lm(weight_gain ~ forage_distance + sex,
                                     data=bears_final)
summary(no_interaction_model_bears_sex)

# Only forage distance
only_distance_model_bears_sex <- lm(weight_gain ~ forage_distance, data=bears_final)
summary(only_distance_model_bears_sex)

# Here there's only one significant term which is forage distance. 
  # There is a positive relationship between forage distance and weight gain (slope of 0.18).
  # Bears that did not forage are expected to gain 27 units of weight (that's the intercept).

# Second set of tests will be with site as the categorical
full_model_bears_site <- lm(weight_gain ~ forage_distance + site + forage_distance:site,
                            data=bears_final)
# View it
summary(full_model_bears_site)

# Interaction-less model
no_interaction_model_bears_site <- lm(weight_gain ~ forage_distance + site, data=bears_final)
# View it
summary(no_interaction_model_bears_site)

# Just sites
only_site_model_bears <- lm(weight_gain ~ site, data=bears_final)
# View it
summary(only_site_model_bears)

# Final summary: When site is not considered, bears that forage farther gain more weight, independent of sex. 
  # However, when site is considered, that becomes the most important determinant of weight gain, and now forage distance is no longer an important factor.
  # On average, site B mean weight gain was -13.58 units lower than site A mean weight gain, it does check out graphically (~50 vs ~65).
  # The estimate here also described the mean weight gain for the reference level (site A), which was 64.89.

# Checking back with Kristine, the simulations intended for sites to be a major determinant of weight gain, but sites were also 
  # not supposed to have an effect on the slopes of the forage distance / weight gain relationship. So there truly was no interaction.
  # The slopes were both 3 and the intercepts were 20 and 21 for the sites. These were a bit different from my pure forage_distance and weight gain model.
  # Although I am realizing now that I am not seeing the full picture of Kristine's simulations since the 3/20/21 values I am looking at were apparently with x value being the initial weight and y being the final? 
    # Not sure when forage distance came in and what those relationships were simulated to be.

# (I did Nurul's as well but did not get to compare results with him at the end)

# Nurul's ecological scenario:
  # We are investigating the impact of Chronic Wasting Disease (CWD) on white-tailed deer. 
  # We used GPS collars with accelerometers to measure the average daily activity level (odba) of 100 deer. 
  # We also know the age of each deer and its CWD test status (positive or negative).

# Does CWD status affect the daily activity level (odba) of deer? 
# After accounting for the natural decline in activity due to age, do CWD-positive deer have different activity levels than CWD-negative deer? 
# Furthermore, does the relationship between age and activity differ between CWD-positive and negative animals (i.e., do CWD-positive animals become lethargic more rapidly with age)?

# Load in data
cwd_data <- read.csv("./HW 11/cwd_ancova_data_to_share.csv")

# Take a look
ggplot(cwd_data, aes(x=age_yrs, y=odba, col=cwd_status)) + geom_point()

# Full model
cwd_full_model <- lm(odba ~ age_yrs + cwd_status + age_yrs:cwd_status, data=cwd_data)
# View it
summary(cwd_full_model)

# Final conclusion: There is a clear interaction between CWD status and deer activity. 
  # It seems like deer with CWD have a greater rate of activity decline with age, compared to deer that do not have CWD.
