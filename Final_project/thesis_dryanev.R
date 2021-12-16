


############################### Final Project  ####################################


# Author: Md Salman Rahman
# Course: MATH 6364 Statistical Methods
# Course Instructor: Dr. George Yanev
setwd("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Methods/Final_project")

library("readxl")
library(ggplot2)
data_ir<- iris

source("dataadj.R")

########################  data Preparation and Cleaning  ################# 
library(tidyr)
library(dplyr)
summary(data_ir)
table(is.na(data_ir))
  
##################### descriptive analysis  ###################

x1.retail <- DB[["United States"]][["google"]][["retail_and_recreation"]]
x2.grocery <- DB[["United States"]][["google"]][["grocery_and_pharmacy"]]
x3.resid <- DB[["United States"]][["google"]][["residential"]]
x4.transit <- DB[["United States"]][["google"]][["transit_stations"]]
x5.parks <- DB[["United States"]][["google"]][["parks"]]
x6.workplace <- DB[["United States"]][["google"]][["workplaces"]]
new.cases <- DB[["United States"]][["disease"]][["new_cases"]]

x1.retail[is.na(x1.retail)] <- 0
x2.grocery[is.na(x2.grocery)] <- 0
x3.resid[is.na(x3.resid)] <- 0
x4.transit[is.na(x4.transit)] <- 0
x5.parks[is.na(x5.parks)] <- 0
x6.workplace[is.na(x6.workplace)] <- 0

df.covid <- data.frame(x1.retail,x2.grocery,x3.resid,x4.transit,x5.parks,x6.workplace,new.cases)


lm.model <- lm(new.cases ~ ., data = df.covid)

summary(lm.model)

