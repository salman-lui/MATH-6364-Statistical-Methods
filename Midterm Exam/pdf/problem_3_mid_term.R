
# Author: Md Salman Rahman
# Course: MATH 6364 Statistical Methods
# Course Instructor: Dr. George Yanev
# Problem 3

library("readxl")
library(ggplot2)
data_3<- read_excel("C:/Users/User/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Methods/HW_and R/Midterm Exam/Exercise_2_11_data.xlsx")



########################  data Preparation  ################# 
library(tidyr)
library(dplyr)


split_data <- data_3 %>% separate(`Ave Punting Distance`, c("Ave Punting Distance in ft","Unit_Ft", "Ave Punting Distance in Inch", "Unit_Inch"))


# removing unit column 
new_data<- subset(split_data, select = -c(Unit_Ft,Unit_Inch))

final_data <- lapply(new_data,as.numeric)

df<- as.data.frame(final_data)

# converting inch into feet 

inch_to_feet <- (df[,5])/12

df$Ave.Punting.Distance.in.ft = df$Ave.Punting.Distance.in.ft + inch_to_feet


# final data 


data<- subset(df, select = -c(Ave.Punting.Distance.in.Inch))





## first Regression model

reg_1 <- lm(Ave.Punting.Distance.in.ft~ `Right Leg (lb)`,data=data)
summary(reg_1)


## second Regression model

reg_2<-lm(Ave.Punting.Distance.in.ft~`Right Leg (lb)` + `Left Leg (lb)`,data=data)
summary(reg_2)



## third Regression model

reg_3<-lm(Ave.Punting.Distance.in.ft~`Left Leg (lb)`,data=df)
summary(reg_3)


# first residuals plot 

residuals(reg_1)
plot(residuals(reg_1),xlab=" ",ylab=" ",main=)



# second residuals plot 

residuals(reg_2)
plot(residuals(reg_2),xlab=" ",ylab=" ",main=)


# third residuals plot 

residuals(reg_3)
plot(residuals(reg_3),xlab=" ",ylab=" ",main=)


# hypothesis testing and confidence interval for beta_2

n= 13

beta_hat_2 = 111111111111111111111

se = 111111111111

t_statistics = qt(1-0.05/2, df=n-3)  # 95% CI
c(betahat1-t_statistics*se, betahat1+t_statistics*se)




