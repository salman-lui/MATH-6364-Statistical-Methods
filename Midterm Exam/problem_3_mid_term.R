


############################### Problem 3  ####################################


# Author: Md Salman Rahman
# Course: MATH 6364 Statistical Methods
# Course Instructor: Dr. George Yanev


library("readxl")
library(ggplot2)
data_3<- read_excel("C:/Users/User/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Methods/HW_and R/Midterm Exam/Exercise_2_11_data.xlsx")



########################  data Preparation and Cleaning  ################# 
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

reg_1 <- lm(data$Ave.Punting.Distance.in.ft~ data$Right.Leg..lb.,data=data)
summary(reg_1)


## second Regression model

reg_2<-lm(data$Ave.Punting.Distance.in.ft~data$Right.Leg..lb. + data$Left.Leg..lb.,data=data)
summary(reg_2)



## third Regression model

reg_3<-lm(data$Ave.Punting.Distance.in.ft~data$Left.Leg..lb.,data=data)
summary(reg_3)


# first residuals plot 

residuals(reg_1)
plot(residuals(reg_1),xlab="Observation Number", ylab="Residuals",main="Residual vs obs no. for Model 1")



plot(reg_1$fitted.values,reg_1$residuals,xlab="Fitted values", ylab="Residuals",main="Residuals vs Fitted Values for Model 1")


#plot(data$Ave.Punting.Distance.in.ft,residuals(reg_1), main="First model")


# second residuals plot 

plot(residuals(reg_2),xlab="Observation Number", ylab="Residuals",main="Residual vs obs no. for Model 2")



residuals(reg_2)
plot(reg_2$fitted.values,reg_2$residuals,xlab="Fitted values", ylab="Residuals",main="Residuals vs Fitted Values for Model 2")


# third residuals plot 

plot(residuals(reg_3),xlab="Observation Number", ylab="Residuals",main="Residual vs obs no. for Model 3")

residuals(reg_3)
plot(reg_3$fitted.values,reg_3$residuals,xlab="Fitted values", ylab="Residuals",main="Residuals vs Fitted Values for Model 3")



# comparison 

summary(reg_1)$r.squared
summary(reg_2)$r.squared
summary(reg_3)$r.squared

# anova table: 
anova(reg_1)
anova(reg_2)
anova(reg_3)


# confidence interval

confint(reg_2)

confint(reg_3)






