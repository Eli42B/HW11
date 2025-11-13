# HW11
HW 11 for Kristine Schoenecker, Nathan Lin, and Elizabeth Braatz


# @ Kristine and Nathan 
Hi! I have created data for Assignment 11. Don't look at HW11_Elizabeth.R yet because that is the code that created the data and you'll be able to see what the true equations are there. Here is my question prompt: 

King Uther Pendragon is having some trouble with dragons in his medieval kingdom. White dragons and red dragons keep setting fire to the kingdom. He wants to hire a private contractor named Merlin (occupation: Wizard) to relocate the dragons, but private contractors are really expensive and Merlin can only capture half the dragons. The dragons are also all different sizes. Should Uther have Merlin capture the red dragons, the white dragons, or does it only matter how large the dragons are matter? Oddly enough, Uther has excellent ecological data on all the dragons.

Question: Do the numbers of acres set on fire depend on the size of white and red dragons? 
 Continuous response variable: Number of acres set on fire 
   type = numeric
   unit = acre 
 Continuous predictor variable: Dragon size 
   type = numeric 
   unit = tons 
 Two level factor: Color of dragon 
   type =  categorical 

You can create your own R files and read in my data (use read.csv("elizabeth_data.csv")) and it should load OK, and then analyze it to answer Uther's question. 

#elizabeth's notes from lecture 
The lectures nad readings confused me so I tried to make my own notes on the topic, you can see them in Week 11 ANCOVA AND ANOVA.docx word document. Maybe they will help if you are also confused! 

# example_anova_ancova.R 
I made an example anova and ancova, you can check it out here! 

# @ Elizabeth and Nathan
I uploaded my data example, I hope it is in the right format. (I can definitely adjust this if my example does not make sense, so let me know!)

Ecological Scenario: Researchers are studying brown bear cubs at two sites. They are interested in if the amount of weight gained after their survey period is related to their average daily forage distance. Do bears that, on average, forage further gain more weight? Is there a difference between male and female bear cubs, or cubs from one site or another?


# @ Elizabeth and Kristine
Ecological Scenario: For our newly-discovered tick species Tickus bittus, we are interested in seeing if the different life stages (nymphs and adults) have different vulnerability to desiccation, and how this might affects their questing patterns.
Our question: How does sensitivity of questing duration to a humidity index differ between tick nymphs and adults?
Continuous response variable (questing_duration, in minutes), continuous predictor variable (humidity_index, does not exist outside of this exercise), and two-level factor (life_stage).
