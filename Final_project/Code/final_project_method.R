


############################### Final Project  ####################################


# Author: Md Salman Rahman
# Course: MATH 6364 Statistical Methods
# Course Instructor: Dr. George Yanev
setwd("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Methods/Final_project")

library("readxl")
library(ISLR2)
library(ggplot2)
data("Boston")

df<- Boston

summary(df)
str(df)

########################  Checking Missing Value ################# 
library(tidyr)
library(dplyr)
summary(df)
table(is.na(df))
  
##################### descriptive analysis  ###################
head(df)
# correlation analysis 
library(corrplot)
# correlation matrix 
corrplot(cor(df[,]),
         method = "number",
         type = "upper" # show only upper side
)

# scatter plot of several variable 
pairs(df[,])


# histogram 
hist(df$medv)
hist(df$crim)
hist(df$zn)
hist(df$indus)
hist(df$chas)
hist(df$nox)


#################### linear regression #########################

#model 1
lm.model <- lm(medv~., data = df)
names(lm.model)
coef(lm.model)
#confidence interval 
confint(lm.model)
summary(lm.model)

# anova analysis
anova(lm.model)

#variance inflation factor

library(car)
vif(lm.model)


plot(lm.model)


#model 2

lm.model2 <- update(lm.model, . ~. + zn:tax+dis:rad)

summary(lm.model2)

#confidence interval 
confint(lm.model2)
#variance inflation factor

library(car)
vif(lm.model2)

# anova analysis
anova(lm.model2)


plot(lm.model2)

#model 3

lm.model3 <- lm(medv ~. -age-indus, data=df)

summary(lm.model3)

#confidence interval 
confint(lm.model3)
#variance inflation factor

library(car)
vif(lm.model3)

# anova analysis
anova(lm.model3)

plot(lm.model3)
