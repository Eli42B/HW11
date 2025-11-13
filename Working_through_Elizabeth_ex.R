#### Working through Elizabeth's example data ----

#### load libraries
library(tidyverse)


# Ecological context: King Pendragon is trying to determine which dragons to hunt.
# Red dragons or white dragons, our just hunt based on size
# he needs to know so he can limit damage to the kingdom (acres burned)

# let's read in the data 

dragon_df <- read.csv("elizabeth_data.csv")

#since color is a categorical piece of info, we should save it as a factor instead of a character, right?

dragon_df$color <- as.factor(dragon_df$color)

# let's do a little bit of data exploration before fitting a model

# boxplots are a nice visualization

boxplot1 <- ggplot(data = dragon_df, mapping = aes(x = color, y = size)) +
  geom_boxplot()

boxplot1 # looking at this the dragons seem pretty even in size 

# do they appear to burn different amounts of land?

boxplot2 <- ggplot(data = dragon_df, mapping = aes(x = color, y = acres_on_fire)) +
  geom_boxplot()

boxplot2

# it definitely looks like the white dragons set more land on fire than the red dragons

# we could even do a scatterplot of the data

scatterplot1 <- ggplot(data = dragon_df, mapping = aes(x = size, y = acres_on_fire, shape = color)) +
  geom_point() +
  stat_smooth(method = "lm") #can fit a linear regression line directly in ggplot

scatterplot1

# looking at the scatterplot, it definitely looks like there might be a difference between the two
# categories of dragons (with red ones burning more on average, but acres burned also increasing with size)

# now we can use a linear model to look at the data

mod_dragons <- lm(acres_on_fire ~ size*color, data = dragon_df)
# the multiplication in the formula should count for the interaction effect?

# if I am understanding that correctly, we should also plot this model

plot(mod_dragons)

# let's see a summary of this model

summary(mod_dragons)

# the bottom of the summary is essentially the anova, correct?
# 270 - 272 in Faraway ch 15
# an interaction being significant means you cannot remove that from your model and look at main effects


# interpretation of the ANOVA results!

# Based on the summary of the first model,
# there is a significant interaction between the size and color of the dragon on the 
# number of acres it burns. White dragons tend to be larger and burn more acres than red dragons.
# As dragons get larger, the white dragons cause even more damage than red dragons 
# (compared to the amount of damage done when both are small)
# I would recommend the king focus his hunting efforts mainly on white dragons.