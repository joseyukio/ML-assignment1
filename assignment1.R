library(data.table) # to handle the data frame
library(dplyr) # for subsetting
## to be executed only once: install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret) # to handle the data partition

## As requested by the assignment use the first 463715 examples as training
trainYMus <- fread("YearPredictionMSD.txt", nrows = 463715)
## And the last 51630  for test
test <- fread("YearPredictionMSD.txt", skip = 463715)

## Check if there is NA values
TRUE %in% is.na(trainYMus)
TRUE %in% is.na(test)
## The results are 'FALSE" which means there is no NA value in the data.

## Split training into training and validation. Let's use the validation
## size equals to the test size.
## Ref: https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf

## Set seed so the same results can be reproduced.
set.seed(1)
## the outcome data are needed
## The percentage (75%) of data in the training set.
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
rm(inTrain)

## In order to run the PreTest3.R do this:
xaux <- as.matrix(training)

## Analyse the input
## Check the distribution of the dependent variable V1 (year)
hist(trainYMus$V1, xlab = "Year", ylab = "# Musics", main = "Training - Year distribution")
hist(training$V1, xlab = "Year", ylab = "# Musics", main = "Training - Year distribution")
hist(training$V1, xlab = "Year", ylab = "# Musics", main = "Training - Year distribution")
hist(validation$V1, xlab = "Year", ylab = "# Musics", main = "Validation - Year distribution")
#hist(test$V1, main = "Test - Year distribution")
## See that the number of musics in the recent years is much higher then the oldest years.
## One alternative is to remove part of the observations that have those years so that
## high number or observation will not bias the result.

## Check the outliers. Ref: http://global.oup.com/us/companion.websites/9780195089653/rprogram/
## Example using boxplot for V87
boxplot(training$V87)
##identify(rep(1, length(training$V87)), training$V87, labels = seq_along(training$V87))
## Use identify to mark the outliers
hist(training$V87, freq = TRUE, breaks = c(100), xlab = "Feature values", main = "Feature 87 distribution")

hampel.proc <- function(my.x, t = 3, RemoveNAs = FALSE){
        #
        #  This procedure returns an index of x values declared
        #  outliers according to the Hampel detection rule, if any
        #
        mu <- median(my.x, na.rm = RemoveNAs)
        sig <- mad(my.x, na.rm = RemoveNAs)
        indx <- which( abs(my.x - mu) > t*sig)
        #
        indx
}


## Check the distribution of each feature
hist(training$V2, main = "Training - V2 distribution")
hist(training$V3, main = "Training - V3 distribution")
hist(training$V4, main = "Training - V4 distribution")
## ...

## Check the correlation of the variables
corr.training <- cor(training)
symnum(corr.training)
## Looking at the result, it seems there is no clear correlation between the
## year and the individual variables.

## Function to generate the model. 
##generateModel <- function(x){
## Compute regression estimate V1(year) ~ x
## Store into x in order to avoid the warning problem
## Reference: http://stackoverflow.com/questions/27464893/getting-warning-newdata-had-1-row-but-variables-found-have-32-rows-on-pred
##model <- lm(training$V1 ~ x)
##return (model)
##}

################################################
## Using the training data and all feature V2toV91
V2toV91x.model <- lm(training)
# print regression output
summary(V2toV91x.model)
## See the coeficients
V2toV91x.coef <- coef(V2toV91x.model)
V2toV91x.residual <- resid(V2toV91x.model)
V2toV91x.predicted <- fitted(V2toV91x.model)
## Calculate the root mean squared Error
#RMSE <- sqrt(mean((y-y_pred)^2))
V2to91x.RMSE <- sqrt(mean((training$V1 - V2toV91x.predicted) ^ 2 ))
## Or sqrt(mean(residuals(V2toV91x.model) ^ 2))


## Use the model and predict the validation data
## Predict new values using the validation data.
V2to91.predictedValidation <- predict(V2toV91x.model, newdata = as.data.frame(validation))
## Quick check on the difference between the predicted and actual values
V2to91.differenceValidation <- validation$V1 - V2to91.predictedValidation
## Calculate the root mean squared Error
V2to91.validation.RMSE <- sqrt(mean((validation$V1 - V2to91.predictedValidation) ^ 2 ))

plot(validation$V1, V2to91.predictedValidation)

## Try to select the best model using stepwise
step(V2toV91x.model) ## It takes several minutes to run

################################################
## Using the training data and feature V2
V1 <- training$V1
V2x <- training$V2
V2x.model <- lm(V1 ~ V2x)
# print regression output
summary(V2x.model)
## See the coeficients
v2x.coef <- coef(V2x.model)
V2x.residual <- resid(V2x.model)
V2x.predicted <- fitted(V2x.model)

################################################

################################################
## Using the training data and feature V3
V3x <- training$V3
V3x.model <- lm(training$V1 ~ V3x)
# print regression output
summary(V3x.model)
## See the coeficients
v3x.coef <- coef(V3x.model)
V3x.residual <- resid(V3x.model)
V3x.predicted <- fitted(V3x.model)

################################################

################################################
## Using the training data and feature V87 (according to stepwise output it is the best)
V87x <- training$V87
V87x.model <- lm(training$V1 ~ V87x)
# print regression output
summary(V87x.model)
## See the coeficients
v87x.coef <- coef(V87x.model)
V87x.residual <- resid(V87x.model)
V87x.predicted <- fitted(V87x.model)

plot(training$V1, training$V87)
abline(V87x.model, col = "red")

#Analyse the Results
#Reference:
#http://www.r-bloggers.com/r-tutorial-series-graphic-analysis-of-regression-assumptions/
##########

## Plot the training data. V2 to predict V1 (year).
plot(training$V1 ~ training$V2, main = "Feature V2")
#Plot the residual against the predicted values
plot(V2x.predicted, V2x.residual, main = "Feature V2")
#Plot the residual against the training feature (V2)
plot(training$V2, V2x.residual, main = "Feature V2")

# plot fitted regression line as obtained before.V2 to predict V1 (year)
## So we can see how our line obtained from the model looks like.
abline(V2x_model, col = "red")


## Use the model and predict the validation data
## Predict new values using the validation data.
## Get only the column V2
V2x <- validation$V2
predictedValidationV1V2 <- predict(V2x.model, newdata = as.data.frame(V2x))

## Quick check on  predicted and actual values. Ideally the values should fit a line.
plot(validation$V1, predictedValidationV1V2)

## Quick check on the difference between the predicted and actual values
differenceValidation <- validation$V1 - predictedValidationV1V2
plot(validation$V1, differenceValidation)

abline(h=-20)
abline(h=0)
abline(h=20)
abline(v=1970)
abline(v=2011)



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

################################################
##Gradient Descent for multivariate
## Reference: http://www.statsblogs.com/2011/10/24/stanford-ml-3-multivariate-regression-gradient-descent-and-the-normal-equation/


# Gradient descent function
grad <- function(x, y, theta) {
        gradient <- (1 / nrow(y)) * (t(x) %*% ((x %*% t(theta)) - y))
        return (t(gradient))
}

gradient.descent <- function(x, y, alpha=0.1, num.iterations=500, threshold=1e-5, output.path=FALSE) {
        # Add x_0 = 1 as the first column
        m <- if(is.matrix(x)) nrow(x) else length(x)
        if(is.vector(x) || (!all(x[,1] == 1))) x <- cbind(rep(1, m), x)
        x <- apply(x, 2, as.numeric)
        
        num.features <- ncol(x)
        
        # Initialize the parameters
        theta <- matrix(rep(0, num.features), nrow=1)
       
        # Look at the values over each iteration
        theta.path <- theta
        
        pb <- txtProgressBar(min = 0, max = num.iterations, style = 3) ## progress bar
        for (i in 1:num.iterations) {
                theta <- theta - alpha * grad(x, y, theta)
                cost.history[i] <<- grad(x, y, theta) ## <<- since it's a global variable
                if(all(is.na(theta))) break
                theta.path <- rbind(theta.path, theta)
                if(i > 2) if(all(abs(theta - theta.path[i-1,]) < threshold)) break 
                
                setTxtProgressBar(pb, i)
                
        }
        close(pb) ## close progress bar
        
        
        if(output.path) return(theta.path) else return(theta.path[nrow(theta.path),])
}


# Function to standardize input values
zscore <- function(x, mean.val=NA) {
        if(is.matrix(x)) return(apply(x, 2, zscore, mean.val=mean.val))
        if(is.data.frame(x)) return(data.frame(apply(x, 2, zscore, mean.val=mean.val)))
        if(is.na(mean.val)) mean.val <- mean(x)
        sd.val <- sd(x)
        if(all(sd.val == 0)) return(x) # if all the values are the same
        (x - mean.val) / sd.val 
}


## Load myx and myy
myx <- as.matrix(select(training, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13))
myy <- as.matrix(select(training, V1))
#myy <- matrix(training$V1, ncol=1)
# Standardize the features
myx.scaled <- zscore(myx)

## Number of iterations
my.num.iterations = 1000
## Alpha
my.alpha = 0.001
## My cost history. A better approach is to return it from the function grad-gradient.descent
cost.history = matrix(nrow=my.num.iterations,ncol=1)

## Calculate the GD
unscaled.theta <- gradient.descent(x=myx, y=myy, alpha=my.alpha, num.iterations=my.num.iterations, output.path=TRUE)
scaled.theta <- gradient.descent(x=myx.scaled, y=myy, alpha=my.alpha, num.iterations=my.num.iterations, output.path=TRUE)

## Check the theta
my.theta <- unscaled.theta[my.num.iterations + 1, 2]
##
my.result <- 45.14986 %*% training$V2
## Calculate the difference between actual and predicted
difference.result <- training$V1 - my.result


plot(cost.history)

##
qplot(1:(nrow(scaled.theta)), scaled.theta[,1], geom=c("line"), xlab="iteration", ylab="theta_1")
qplot(1:(nrow(scaled.theta)), scaled.theta[,2], geom=c("line"), xlab="iteration", ylab="theta_2")



################################################
## Gradient Descent  for one variable
# http://www.r-bloggers.com/linear-regression-by-gradient-descent/

## Load myx and myy
myx <- as.matrix(select(training, V2))
myy <- as.matrix(select(training, V1))

#x <- training$V2
#y <- training$V1
m <- length(myy)
## x <- runif(1000, -5, 5)
## y <- x + rnorm(1000) + 3


# squared error cost function
cost <- function(X, myy, theta) {
        sum( (X %*% theta - myy)^2 ) / 2 %*% m
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
X <- cbind(1, matrix(myx))

# gradient descent
pb <- txtProgressBar(min = 0, max = num_iters, style = 3) ## progress bar
for (i in 1:num_iters) {
        error <- (X %*% theta - myy)
        delta <- t(X) %*% error / m
        theta <- theta - alpha * delta
        cost_history[i] <- cost(X, myy, theta)
        theta_history[[i]] <- theta
        
        setTxtProgressBar(pb, i)
        }

close(pb) ## close progress bar
print(theta)

## Plot the cost history
plot(cost_history, type='points', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations', ylim = c(0,1000))


## Let's see GD for scaled V2
x <- (training$V2 - mean(training$V2))/sd(training$V2)

################################################
## Analytical result, Normal Equation.
## With all features
myx <- as.matrix(select(training, V2:V91))
myx <- cbind(1,myx) # Add 1 to represent the intercept
myy <- as.matrix(select(training, V1))

## Normal equation
NE.coef <- solve(t(myx) %*% myx) %*% t(myx) %*% myy 
## The resulting coefficients are very similar to the ones obtained from lm
## The difference is probably caused by the rounding applied by the different methods.


## With 12 features
myx <- as.matrix(select(training, V2:V13))
myx <- cbind(1,myx) # Add 1 to represent the intercept
myy <- as.matrix(select(training, V1))

## Normal equation
NE.coef <- solve(t(myx) %*% myx) %*% t(myx) %*% myy 

## using lm
V1 <- training$V1
##V2xtoV13x <- (select(training, V2:V13))
V2xtoV13x.model <- lm(V1 ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13, data = training)
# print regression output
summary(V2xtoV13x.model)
## See the coeficients
V2xtoV13x.coef <- coef(V2xtoV13x.model)
V2xtoV13x.residual <- resid(V2xtoV13x.model)
V2xtoV13x.predicted <- fitted(V2xtoV13x.model)

################################################

## Increase the complexity of the model by adding more columns.
## In this case we multiply the column 2 by all other columns and add each result as a 
## new column. At the end 180 columns
V2toV181 <- mutate(training,V92=V2*V3,V93=V2*V4,V94=V2*V5,V95=V2*V6,V96=V2*V7,V97=V2*V8,V98=V2*V9,V99=V2*V10,V100=V2*V11,V101=V2*V12)
V2toV181 <- mutate(V2toV181,V102=V2*V13,V103=V2*V14,V104=V2*V15,V105=V2*V16,V106=V2*V17,V107=V2*V18,V108=V2*V19,V109=V2*V20,V110=V2*V21)
V2toV181 <- mutate(V2toV181,V111=V2*V22,V112=V2*V23,V113=V2*V24,V114=V2*V25,V115=V2*V26,V116=V2*V27,V117=V2*V28,V118=V2*V29,V119=V2*V30)
V2toV181 <- mutate(V2toV181,V120=V2*V31,V121=V2*V32,V122=V2*V33,V123=V2*V34,V124=V2*V35,V125=V2*V36,V126=V2*V37,V127=V2*V38,V128=V2*V39)
V2toV181 <- mutate(V2toV181,V129=V2*V40,V130=V2*V41,V131=V2*V42,V132=V2*V43,V133=V2*V44,V134=V2*V45,V135=V2*V46,V136=V2*V47,V137=V2*V48)
V2toV181 <- mutate(V2toV181,V138=V2*V49,V139=V2*V50,V140=V2*V51,V141=V2*V52,V142=V2*V53,V143=V2*V54,V144=V2*V55,V145=V2*V56,V146=V2*V57)
V2toV181 <- mutate(V2toV181,V147=V2*V58,V148=V2*V59,V149=V2*V60,V150=V2*V61,V151=V2*V62,V152=V2*V63,V153=V2*V64,V154=V2*V65,V155=V2*V66)
V2toV181 <- mutate(V2toV181,V156=V2*V67,V157=V2*V68,V158=V2*V69,V159=V2*V70,V160=V2*V71,V161=V2*V72,V162=V2*V73,V163=V2*V74,V164=V2*V75)
V2toV181 <- mutate(V2toV181,V165=V2*V76,V166=V2*V77,V167=V2*V78,V168=V2*V79,V169=V2*V80,V170=V2*V81,V171=V2*V82,V172=V2*V83,V173=V2*V84)
V2toV181 <- mutate(V2toV181,V174=V2*V85,V175=V2*V86,V176=V2*V87,V177=V2*V88,V178=V2*V89,V179=V2*V90,V180=V2*V91)

## With 179 features
myx <- as.matrix(select(V2toV181, V2:V180))
myx <- cbind(1,myx) # Add 1 to represent the intercept
myy <- as.matrix(select(training, V1))

## Normal equation
NE.coef <- solve(t(myx) %*% myx) %*% t(myx) %*% myy 

## using lm
V2xtoV181.model <- lm(V2toV181)
# print regression output
summary(V2xtoV181.model)
## See the coeficients
V2xtoV181.coef <- coef(V2xtoV181.model)
V2xtoV181.residual <- resid(V2xtoV181.model)
V2xtoV181.predicted <- fitted(V2xtoV181.model)

## Calculate RMSE
V2to181x.RMSE <- sqrt(mean((training$V1 - V2xtoV181.predicted) ^ 2 ))







