## HW 3 

# Problem 1

x_verbal_score<- c(66,56,65,43,78,52,29,78,55,76)

y_final_grade <- c(76,68,76,51,92,62,34,92,70,87)

linear_model= lm(y_final_grade ~ x_verbal_score)

predict(linear_model, x_verbal_score =100, interval = "confidence")

plot(x_verbal_score,y_final_grade)

summary(linear_model)


### Problem 2 



### Problem 4

x<- c(-1,0,6,8,10)
y<-c(0,7,13,20,26)

pre=lm(y~x)


### Problem 6
x<- c(-1,0,5)
y<-c(2,6,15)

model1<-lm(y ~ (5+2x))

prediction <- predict(y~5+2x, newdata = x)


# Problem 10

x<- c(-2,3,5,7,11)

y<-c(-1,1,6,6,11)

model= lm(y~x)
summary(model)


# Problem 10 of pro

x<-c(-3,0,5,9,12)
y<-c(-2,0,4,7,9)
model=lm(y~x)
summary(model)


