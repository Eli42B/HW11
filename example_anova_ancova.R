######################################
# ANOVA AND ANCOVA
######################################

#Notes from Elizabeth Braatz 
#Compiled using websites below and Chat GPT 


#https://www.datanovia.com/en/lessons/ancova-in-r/

#loading libraries 
library(tidyverse)   #data manipulation, dplyr 
library(ggpubr)      #creating easily publication ready plots
library(rstatix)     # pipe friendly stat analyses
library(broom)       #printing summary of stat tests as data frames
library(datarium)   #contains example datasets from this
library(dplyr)       #so we can pipe 
library(emmeans)     # so we can do a post-hoc test like Tukey for ANCOVA

#loading data 
data("anxiety", package = "datarium")
anxiety = anxiety %>% 
  select(id, group, t1, t3) %>% 
  rename(pretest = t1, posttest = t3)
anxiety # note that the categorical groups are factors, this is important 


#Let's say we want to do stratified sampling (ie randomly sample iwthin each group) 
set.seed(42)
anxiety %>% sample_n_by(group, size = 1) 
#      sample_n_by() is a function from dplyr 
#      group = column used to define the grouping, in this case 'group'
#      size = 1 means we want 1 random row from each group 

#Initial data visualization 

#Base plot 
base_plot = ggplot(anxiety, aes(x = group, y = posttest)) + 
  geom_point(aes(color = group))
base_plot + 
  labs(title = "Anxiety by Group", 
       x = "group",
       y = "Post-test Score") 
# we want to post the base plot + a title, but we don't want to save the title to the baseplot because we will be changing this quite a lot 

base_plot + 
  facet_wrap(~group, nrow = 1)  #visualizing the baseplot but facet wrapped 

# Run ANOVA 
#----------------------------------

#ANOVA test 
fit = aov(posttest ~ group, data = anxiety)

#Check assumptions 

# Linearity
# Homogeneity of variances 
# Constant variance (homoscedasticity) 
# Normally distributed 
# Independence 

resid = anxiety$posttest - predict.lm(fit) 
plot(resid ~ predict.lm(fit), ylab = "residuals", xlab = "Predicted Y")
abline(a = 0, b = 0, col = "red")
#residuals are the difference between the actual and predicted fit 
#Testing homogeneity of residuals 
#It's OK, the straight lines makes me think I'm doing this wrong a bit 

stdRes = rstandard(fit)
qqnorm(stdRes,ylab="Standardized Residuals", xlab="Theoretical Quantiles")
qqline(stdRes, col=2,lwd=2)
hist(stdRes)
#Testing normality 
#   QQline is OK 
#   Histogram likewise is not great, looks bimodal  

#What does our fit say? 
summary(fit)
#anxiety is significantly related to group, but I'm not convinced of linearity 

#Posthoc test 
TukeyHSD(fit)
# group 3 is sign. different from group 1 and 2, but group 1 and 2 are indistinguishable 


# Now let's do a fancier test with ANCOVA to control for pre-tests 

# ANCOVA 
# ----------------------------------

#Check assumptions unique to ANCOVA 

# Linearity between covariate and outcome variable 
# Homogeneity of regression slopes  


base_plot = ggplot(anxiety, aes(x = pretest, y = posttest)) + 
  geom_point(aes(color = group)) + 
  geom_smooth(aes(color = group), method = lm, se = FALSE, fullrange = TRUE) 
base_plot
#https://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization 
#we want three linear lines, if it's not linear, we can't do it 

aov_model = aov(posttest~group*pretest, data = anxiety)
summary(aov_model)

anxiety %>% anova_test(posttest ~ group*pretest)
# ChatGPT 
# Checking for homogeneity of regression slopes 
# This means that covariate (pretest) and dependent variable (posttest) should be the same between all groups. If they aren't then we can't use ANCOVA becuase it's based around the idea that we can just account for a consistent factor across all the groups that's confounding things. 
# So we add a *pretest as an interaction term with group (group*pretest) and check whether it's significant 
# We want a NONSIGNIFICANT p for the interaction 
# In this case, p = 0.415 >> 0.05 for the group:pretest interaction, so we can go ahead 
# ges means the Generalized Eta Squared 
#   Measure of Effect Size 
#   IE How much of the total variance in dependent variable (posttest) is explained by a     particular effect (group, pretest, or their interaction) 
#   Cohen's rough guidelines to interpret ges 
#   0.01 = small 
#   0.06 = medium 
#   0.14 = large   

# Run the ANCOVA and keep checking assumptions 

#Assumptions of both ANCOVA and ANOVA 
# Homogeneity of residuals variance (homoscedasticity) 
# Normally distributed  outcome variable 
# No sign. outliers 

#Fit the model 
fit = lm(posttest ~ pretest + group, data = anxiety) 
#covariate goes first 

resid = anxiety$posttest - predict.lm(fit) 
plot(resid ~ predict.lm(fit), ylab = "residuals", xlab = "Predicted Y")
abline(a = 0, b = 0, col = "red")
#residuals are the difference between the actual and predicted fit 
#Testing homogeneity of residuals 
#this is looking better 

stdRes = rstandard(fit)
qqnorm(stdRes,ylab="Standardized Residuals", xlab="Theoretical Quantiles")
qqline(stdRes, col=2,lwd=2)
hist(stdRes)
#Testing normality 
#   QQline is better
#   Histogram is better, though skewed 

#What does our fit say? 
summary(fit)
#anxiety is significantly related to group and pretest 
#Wait this is just a normal anova, I'm not actually sure it's important for the covariate to be 'first' 
#Note: summary(fit) is not the same as anova(lm)!! It's kind of like type 3 anova table where you are accounting for everything else and telling it to just barf out everything it knows. Whereas if you ask for anova(fit), it'll do a sequential sum of squares (add terms in formula order) to give you a pretty table. So ask for summary(fit), not anova. 
anova(fit) #don't do this 

#Posthoc test 
emmeans(fit, pairwise ~ group, adjust = "tukey")$contrasts 
#tukey base R will not work here becuase its ancova 
#We need to use emmeans to compare pairwise means 
#basically like tukey but for ancova 
#note that we are asking for the pairwise pairs ~ group, if our column name was not named group we would need it to match the column of groups we are comparing 
#And now we see that when we adjust for pretest, all 3 groups are actually significantly different 

# A note about whether order matters for ANCOVA
# Sometimes it matters but not always, it depends on if you are doing sum of squares type I, II, or III. See Google Doc for notes. 
