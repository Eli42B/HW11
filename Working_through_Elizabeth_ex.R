#### Working through Elizabeth's example data ----

#### load libraries
library(tidyverse)


# Ecological context: King Pendragon is trying to determine which dragons to hunt.
# Red dragons or white dragons, our just hunt based on size
# he needs to know so he can limit damage to the kingdom (acres burned)

# let's read in the data 

dragon_df <- read.csv("elizabeth_data.csv")

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
  geom_point()

scatterplot1

# looking at the scatterplot, it definitely looks like there might be a difference between the two
# categories of dragons (with red ones burning more on average, but acres burned also increasing with size)

# now we can use a linear model to look at the data

mod_dragons <- lm(acres_on_fire ~ size*color, data = dragon_df)
# the multiplication in the formula should count for the interaction effect?
# if I am understanding that correctly

# let's see a summary of this model

summary(mod_dragons)
