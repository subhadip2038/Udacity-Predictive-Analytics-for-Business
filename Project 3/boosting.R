## Extreme Gradient Boosting Models

install.packages("adabag")
library(adabag)
library(caret)
credit_test
credit_train


boost_tr <- boosting(`Credit-Application-Result`~., data = credit_train, boos = TRUE)
print(boost_tr)

boost_pr <- predict(boost_tr, credit_test)
