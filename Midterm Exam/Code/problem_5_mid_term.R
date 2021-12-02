


############################ Problem 5 #########################



# Author: Md Salman Rahman
# Course: MATH 6364 Statistical Methods
# Course Instructor: Dr. George Yanev


x <-c(4,8,12.5,16,20,25,31,36,40,40)
y<-c(3.7,7.8,12.1,15.6,19.8,24.5,31.1,35.5,39.4,39.5)

data<-cbind(x,y)
d_frame <- as.data.frame(data)
########### (a) simple linear regression  ###########

reg5<-lm(y~ x)

summary(reg5)

plot(reg5)



########### (b) and (c) computing the 95% confidence interval and slope for intercept ###########

# confidence interval 
confint(reg5, level=0.95)


## (d),(e), and (f)

model_e<- lm(y~0+x, data=d_frame)

summary(model_e)

confint(model_e, level=0.95)

