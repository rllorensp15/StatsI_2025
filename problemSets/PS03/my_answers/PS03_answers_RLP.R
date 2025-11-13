#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("~/GitHub/StatsI_2025/problemSets/PS03/my_answers")

getwd

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

View(inc.sub)

#I first create a model which includes both variables and then summarise it.

model_q1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(model_q1)

stargazer(model_q1)

modelsummary(model_q1, output = "latex", title = "OLS Results 1 PS03")

#To create this scatterplot of the two variables, I will load the system
#"ggplot2", which is know for having good visuals.

library(ggplot2)
library(stargazer)

#Then, I will create the scatterplot using, again, "difflog" as the explanatory
#variable, and "voteshare" as the outcome variable.

ggplot1 <- ggplot(inc.sub, aes(x = difflog, y = voteshare)) +
  geom_point(colour = "darkgrey") +
  geom_smooth(method = "lm", colour = "blue", se = FALSE) +
  labs(
    title = "Vote share of the presidential candidate of the incumbents party by Difference in Campaign Spending between the Incumbent and the Challenge",
    x = "Difference in Campaign Spending between the Incumbent and the Challenger",
    y = "Incumbent's Vote Share"
  ) +
  theme_minimal()

ggsave("scatterplot1_PS03.png", plot = ggplot1, width = 9, height = 7, dpi = 300)

#I will create a new object using the function "residuals()" and I will name it
#"residuals_model_q1)

residuals_model_q1 <- residuals(model_q1)
mean(residuals_model_q1)
residuals_model_q1

#Prediction equation:
#The general formula is Y = a + bX
#In this case, it is Incumbent vote share = Intercept + slope * difflog

# Y =  0.579031 + 0.041666 * X
# voteshare =  0.579031 + 0.041666 * difflog

#SECOND EXERCISE

model_q2 <- lm(presvote ~ difflog, data = inc.sub)
summary(model_q2)

stargazer(model_q2)

ggplot2 <- ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point(colour = "darkgrey") +
  geom_smooth(method = "lm", colour = "blue", se = FALSE) +
  labs(
    title = "Vote share of the presidential candidate of the incumbents party by Campaign Spending",
    x = "Campaign Spending",
    y = "Vote share of the presidential candidate of the incumbents party"
  ) +
  theme_minimal()
ggsave("scatterplot2_PS03.png", plot = ggplot2, width = 8, height = 6, dpi = 300)


residuals_model_q2 <- residuals(model_q2)
mean(residuals_model_q2)
residuals_model_q2


#Prediction equation:
#The general formula is Y = a + bX
#In this case, it is Incumbent vote share = Intercept + slope * difflog

# Y = 0.507583 + 0.023837 * X
# presvote = 0.507583 + 0.023837 * difflog

#THIRD EXERCISE

# We are interested in knowing how the vote share of the presidential candidate
#of the incumbents party is associated with the incumbents electoral success.

model_q3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(model_q3)

stargazer(model_q3)

ggplot3 <- ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point(colour = "darkgrey") +
  geom_smooth(method = "lm", colour = "blue", se = FALSE) +
  labs(
    title = "Vote share of the presidential candidate of the incumbents party by Incumbents electoral success",
    x = "Vote share of the presidential candidate of the incumbents party",
    y = "Incumbents electoral success"
  ) +
  theme_minimal()
ggsave("scatterplot3_PS03.png", plot = ggplot3, width = 8, height = 6, dpi = 300)


residuals_model_q3 <- residuals(model_q3)
mean(residuals_model_q3)
residuals_model_q3

#Prediction equation:
#The general formula is Y = a + bX
#In this case, it is Incumbent vote share = Intercept + slope * difflog

# Y = 0.441330 + 0.388018 * X
# voteshare = 0.441330 + 0.388018 * presvote

#FOURTH EXERCISE

#The residuals from part (a) tell us how much of the variation in voteshare is
#not explained by the difference in spending between incumbent and challenger.
#The residuals in part (b) tell us how much of the variation in presvote is not
#explained by the difference in spending between incumbent and challenger in the
#district.

#1. Run a regression where the outcome variable is the residuals from Question 1
#and the explanatory variable is the residuals from Question 2.

model_residuals <- lm(residuals_model_q1 ~ residuals_model_q2, data = inc.sub)
summary(model_residuals)
stargazer(model_residuals)

ggplot4 <- ggplot(inc.sub, aes(x = residuals_model_q2, y = residuals_model_q1)) +
  geom_point(colour = "darkgrey") +
  geom_smooth(method = "lm", colour = "blue", se = FALSE) +
  labs(
    title = "Residuals of Questions 1 and 2",
    x = "Residuals Question 2",
    y = "Residuals Question 1"
  ) +
  theme_minimal()
ggsave("scatterplot4_PS03.png", plot = ggplot4, width = 8, height = 6, dpi = 300)


#Prediction equation:
#The general formula is Y = a + bX
#In this case, it is residuals_model_q1 = Intercept + slope * residuals_model_q2

# Y = -5.520e-18 + 2.569e-01 * X
# residuals_model_q1 = -5.520e-18 + 2.569e-01 * residuals_model_q2

#FIFTH EXERCISE

#What if the incumbents vote share is a ected by both the presidents popularity
#and the difference in spending between incumbent and challenger?
  
#1. Run a regression where the outcome variable is the incumbents voteshare and
#the explanatory variables are difflog and presvote.

model_q5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(model_q5)
stargazer(model_q5)

#Prediction equation:
#The general formula is Y = a + bX
#In this case, it is voteshare = Intercept + slope1 * difflog + slope2 * presvote

# Y = 0.4486442 + 0.0355431 * X1 + 0.2568770 * X2
# voteshare = 0.4486442 + 0.0355431 * difflog + 0.2568770 * presvote
