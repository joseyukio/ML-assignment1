library(data.table)
## to be executed only once: install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)

## As requested by the assignment use the first 463715 examples as training
trainYMus <- fread("YearPredictionMSD.txt", nrows = 463715)
## And the last 51630  for test
test <- fread("YearPredictionMSD.txt", skip = 463715)

## Split training into training and validation. Let's use the validation
## size equals to the test size.
## Ref: https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf

## Set seed so the same results can be reproduced.
set.seed(1)
## the outcome data are needed
## The percentage of data in the
## training set
inTrain <- createDataPartition(y = trainYMus$V1, p = .75, list = FALSE)

## The format of the resul
## The output is a set of integers for the rows of Sonar
## that belong in the training set.
str(inTrain)
## By default, createDataPartition does a stratified random split of the data. 
## To partition the data. Add as.data.frame since it does not work with
## data table.
training <- as.data.frame(trainYMus)[inTrain,]
validation <- as.data.frame(trainYMus)[-inTrain,]

## Just to check
nrow(validation)
nrow(training)
nrow(test)

## I can remove objects that will not be used
rm(trainYMus)
rm(inTrain)

## Function to generate the model. 
generateModel <- function(x){
## Compute regression estimate V1(year) ~ x
## Store into x in order to avoid the warning problem
## Reference: http://stackoverflow.com/questions/27464893/getting-warning-newdata-had-1-row-but-variables-found-have-32-rows-on-pred
model <- lm(training$V1 ~ x)
return (model)
}

## Using the training data and feature V2
V2x <- training$V2
V2x_model <- generateModel(V2x)
# print regression output
summary(V2x_model)
## See the coeficients
coef(V2x_model)

## Plot the training data. V2 to predict V1 (year).
plot(training$V1 ~ training$V2)
# plot fitted regression line as obtained before.V2 to predict V1 (year)
## So we can see how our line obtained from the model looks like.
abline(V2x_model, col = "red")


## Let's now consider featuring scaling for prediction
scaled_V2x <- (training$V2 - mean(training$V2))/sd(training$V2)
scaled_model <- generateModel(scaled_V2x)
# print regression output
summary(scaled_model)
## See the coeficients
coef(scaled_model)

## Let's now consider featuring scaling for test
x <- (test$V2 - (mean(test$V2)))/sd(test$V2)
## Predict new values using the test data.
## Get only the column V2
x <- test$V2
##testV1V2 <- data.frame(V2 = test$V2)
## Use the model and predict the test data
predictedTestV1V2_scaled <- predict(model1, newdata = as.data.frame(x))

## Plot the test data. V2 to predict V1 (year).
plot(test$V1 ~ test$V2)

# plot fitted regression line as obtained before.
## So we can see how our line obtained from the model looks like.
abline(scaled_model, col = "green") ## strange line!!!Check





## Print regression output. V2 to predict V1 (year).
par(mfrow=c(2,2))
plot(model1)
## reset
par(mfrow=c(1,1))




####################
## Gradient Descent
# http://www.r-bloggers.com/linear-regression-by-gradient-descent/

## Load x and y
x <- training$V2
y <- training$V1
m <- length(y)
## x <- runif(1000, -5, 5)
## y <- x + rnorm(1000) + 3


# squared error cost function
cost <- function(X, y, theta) {
        sum( (X %*% theta - y)^2 ) / 2 %*% m
}

# learning rate and iteration limit
alpha <- 0.01
num_iters <- 1000
# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
theta <- matrix(c(1900,0), nrow=2)

# add a column of 1's for the intercept coefficient
X <- cbind(1, matrix(x))

# gradient descent
pb <- txtProgressBar(min = 0, max = num_iters, style = 3) ## progress bar
for (i in 1:num_iters) {
        error <- (X %*% theta - y)
        delta <- t(X) %*% error / length(y)
        theta <- theta - alpha * delta
        cost_history[i] <- cost(X, y, theta)
        theta_history[[i]] <- theta
        
        setTxtProgressBar(pb, i)
        }

close(pb) ## close progress bar
print(theta)

## Plot the cost history
plot(cost_history, type='points', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations', ylim = c(0,1000))


## Let's see GD for scaled V2
x <- (training$V2 - mean(training$V2))/sd(training$V2)


## Analytical result?
solve(t(x)%*%x)%*%t(x)%*%y 


####




