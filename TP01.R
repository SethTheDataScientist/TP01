'''
Team 7: Seth Lanza, Sophia Liu, Kyle Kaufmann, Emma Minnick
Excersize 9.7.5 Support Vector Machines
'''


"""
## 9.75. We have seen that we can fit an SVM with a non-linear kernel in order to perform classification using a non-linear decision boundary.We will now see that we can also obtain a non-linear decision boundary by performing logistic regression using non-linear transformations of the features.

### a) 
Generate a data set with n = 500 and p = 2, such that the observations
belong to two classes with a quadratic decision boundary
between them. For instance, you can do this as follows:
"""

set.seed(1693)
x1 <- runif (500) - 0.5
x2 <- runif (500) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)

"""### b) 
Plot the observations, colored according to their class labels.
Your plot should display X1 on the x-axis, and X2 on the yaxis.
"""

#if y=0, red; if y=1, blue
plot(x1[y == 1], x2[y == 1], col = "blue", pch = 1, xlab = "X1", ylab = "X2", xlim = c(-.5,.5), ylim = c(-.5,.5))
points(x1[y == 0], x2[y == 0], col = "red", pch = 2)

"""### c) 
Fit a logistic regression model to the data, using X1 and X2 as
predictors.
"""

lm.fit <- glm(y ~ x1 + x2, family = binomial)

summary(lm.fit)

"""### d) 
Apply this model to the training data in order to obtain a predicted
class label for each training observation. Plot the observations,
colored according to the predicted class labels. The
decision boundary should be linear.
"""

data = data.frame(x1 = x1, x2 = x2, y = y)

lm.prob = predict(lm.fit, data, type = "terms")

  #lm.prob = predict(lm.fit, data, type = "response")
  #lm.pred = ifelse(lm.prob > 0.5, 1, 0)

data.pos = data[lm.pred == 1, ]
data.zero = data[lm.pred == 0, ]

plot(data.pos$x1, data.pos$x2, col = "blue", xlab = "X1", ylab = "X2", pch = 1, xlim = c(-.5,.5), ylim = c(-.5,.5))
points(data.zero$x1, data.zero$x2, col = "red", pch = 2)

"""### e) 
Now fit a logistic regression model to the data using non-linear
functions of X1 and X2 as predictors (e.g. X2
1 , X1×X2, log(X2),
and so forth).
"""

lm.fit = glm(y ~ x1 + poly(x2, 2) + I(x1 * x2), data = data, family = binomial)

summary(lm.fit)

"""### f) 
Apply this model to the training data in order to obtain a predicted
class label for each training observation. Plot the observations,
colored according to the predicted class labels. The
decision boundary should be obviously non-linear. If it is not,
then repeat (a)-(e) until you come up with an example in which
the predicted class labels are obviously non-linear.
"""

lm.prob = predict(lm.fit, data, type = "terms")

  #lm.prob = predict(lm.fit, data, type = "response")
  #lm.pred = ifelse(lm.prob > 0.5, 1, 0)

data.pos = data[lm.pred == 1, ]
data.zero = data[lm.pred == 0, ]

plot(data.pos$x1, data.pos$x2, col = "blue", xlab = "X1", ylab = "X2", pch = 1, xlim = c(-.5,.5), ylim = c(-.5,.5))
points(data.zero$x1, data.zero$x2, col = "red", pch = 2)

"""### g) 
Fit a support vector classifier to the data with X1 and X2 as
predictors. Obtain a class prediction for each training observation.
Plot the observations, colored according to the predicted
class labels.
"""

#install.packages("e1071")
library(e1071)

svm.fit = svm(as.factor(y) ~ x1 + x2, data, kernel = "linear", cost = 0.1)
svm.pred = predict(svm.fit, data, type = 'class')

data.pos = data[svm.pred == 1, ]
data.zero = data[svm.pred == 0, ]

plot(data.pos$x1, data.pos$x2, col = "blue", xlab = "X1", ylab = "X2", pch = 1, xlim = c(-.5,.5), ylim = c(-.5,.5))
points(data.zero$x1, data.zero$x2, col = "red", pch = 2)

"""### h) 
Fit a SVM using a non-linear kernel to the data. Obtain a class
prediction for each training observation. Plot the observations,
colored according to the predicted class labels.
"""

svm.fit = svm(as.factor(y) ~ x1 + x2, data, kernel = "polynomial", degree = 2, gamma = 1)
  #OR
#svm.fit = svm(as.factor(y) ~ x1 + x2, data, gamma = 1)

svm.pred = predict(svm.fit, data)
data.pos = data[svm.pred == 1, ]
data.zero = data[svm.pred == 0, ]

plot(data.pos$x1, data.pos$x2, col = "blue", xlab = "X1", ylab = "X2", pch = 1, xlim = c(-.5,.5), ylim = c(-.5,.5))
points(data.zero$x1, data.zero$x2, col = "red", pch = 2)


