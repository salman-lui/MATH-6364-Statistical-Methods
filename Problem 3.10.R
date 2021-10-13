### Problem 1

temperature_x<-c(30,30,30,30,40,40,40,50,50,50,60,60,60,60)
hardness_y<-c(55.8,59.1,54.8,54.6,43.1,42.2,45.2,31.6,30.9,30.8,17.5,20.5,17.2,16.9)

# linear regression model 

l_model <- lm(hardness_y ~ temperature_x)

matrix_model = model.matrix(l_model)

# converting into matrix 14 * 2 matrix 

m <- matrix(matrix_model, nrow = 14)

# transpose of the matrix 

transpose_m <- t(m)

# multiplication of transpose matrix into original

multil <- traspose_m %*% m

# inverse

library(matlib)

a<- inv(multil)

# this gives the value of parameter beta
beta<- a %*% transpose_m %*% hardness_y

